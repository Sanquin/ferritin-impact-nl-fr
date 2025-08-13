library(ggplot2)
library(dplyr)

#create folder to save the files to
FolderName <-  paste0(getwd(), "/results/")
if (!file.exists(FolderName)){
  dir.create(file.path(FolderName))
}
  
showfigs=T
savefigs=T

data <- readRDS("~/Amber/Data/FINDEM/FINDEM_data.rds")     # 37621 obs
alledonaties <- readRDS("~/Amber/Data/Algemeen/alledonaties_2008_2022.rds") %>% select(c("EINnummer", "Donatiesoortcode", "KeyID", "Donatiedatum", "AfgenomenVolume")) %>% mutate(EINnummer = substr(EINnummer, start = 1, stop = 13)) %>% filter(KeyID %in% data$KeyID) 


#merge the donor data with the actual FIND'EM data
data <- merge(data, alledonaties, by.x=c("Einnummer", "KeyID"), by.y=c("EINnummer","KeyID"), all.x=T, all.y=F)

data <- data[(data$Geslacht == "Vrouw" & data$Hb >= 7.8) | 
               (data$Geslacht == "Man" & data$Hb >= 8.4), ]             # 8741 obs
data <- data%>% mutate(Geslacht = ifelse(Geslacht == "Vrouw", "F", "M"))

## Meetweek 0 dataset
Meetweek0 <- data %>% filter(Meetweek==0) #9180

newdonors <- Meetweek0[Meetweek0$Donatiesoortcode == "N", ] #757
regdonors <- Meetweek0[Meetweek0$Donatiesoortcode == "V", ] #7984
newdonors <- newdonors[!is.na(newdonors$Ferritine_FINDEM), ]      # 732 obs
regdonors <- regdonors[!is.na(regdonors$Ferritine_FINDEM), ]      # 6955 obs

newdonors$NewReg <- "New donor"
regdonors$NewReg <- "Repeat donor"

Meetweek0 <- rbind(newdonors, regdonors)
Meetweek0$NewReg <- as.factor(Meetweek0$NewReg)
Meetweek0 <- Meetweek0[order(Meetweek0$KeyID, Meetweek0$Donatiedatum.x), ]           # 7687 obs
Meetweek0$AgeCat <- cut(as.numeric(Meetweek0$LeeftijdBijDOnatie),
                   breaks = c(18, 25, 35, 50, Inf),  # The intervals
                   labels = c("18-24", "25-34", "35-49", "50+"),  # The factor labels
                   right = FALSE) 

## number of donations in past two years for repeat donors

# Create AgeCat column for later grouping
regdonors <- regdonors %>%
  mutate(AgeCat = cut(as.numeric(LeeftijdBijDOnatie),
                      breaks = c(18, 25, 35, 50, Inf),
                      labels = c("18-24", "25-34", "35-49", "50+"),
                      right = FALSE))

# Get each repeat donor's Meetweek0 donation date
reference_dates <- regdonors %>%
  select(KeyID, Donatiedatum.x) %>%
  rename(meetweek_date = Donatiedatum.x)

# Join reference dates into alledonaties for filtering
alledonaties_with_dates <- alledonaties %>%
  filter(KeyID %in% regdonors$KeyID) %>%
  left_join(reference_dates, by = "KeyID")

# Filter donations within 2 years before Meetweek0 and AfgenomenVolume > 400
recent_donations <- alledonaties_with_dates %>%
  filter(Donatiesoortcode %in% c("V"),
         AfgenomenVolume > 400,
         Donatiedatum < meetweek_date,
         Donatiedatum >= (meetweek_date - (365)))

# Count recent large donations per donor
recent_counts <- recent_donations %>%
  group_by(KeyID) %>%
  summarise(num_recent_large_donations = n())

# Add those counts to repeat donors
regdonors_recent <- regdonors %>%
  left_join(recent_counts, by = "KeyID") %>%
  mutate(num_recent_large_donations = ifelse(is.na(num_recent_large_donations), 0, num_recent_large_donations))

# Summarize by sex and age group
summary_recent <- regdonors_recent %>%
  group_by(Geslacht, AgeCat) %>%
  summarise(
    avg_recent_donations = mean(num_recent_large_donations, na.rm = TRUE),
    n = n()
  )

saveRDS(summary_recent, file = paste0(FolderName, "donations_1year.rds"))

# Median ferritin values per group (log10 transformed)
medians <- Meetweek0 %>%
  group_by(NewReg, Geslacht, AgeCat) %>%
  summarise(median_ferritin = median(log10(Ferritine_FINDEM), na.rm = TRUE)) %>%
  ungroup()

saveRDS(medians, file = paste0(FolderName, "median_ferritin_by_group.rds"))

# Median ferritin values per group (log10 transformed)
means <- Meetweek0 %>%
  group_by(NewReg, Geslacht, AgeCat) %>%
  summarise(mean_ferritin = mean(log10(Ferritine_FINDEM), na.rm = TRUE)) %>%
  ungroup()

saveRDS(means, file = paste0(FolderName, "mean_ferritin_by_group.rds"))

## make the plot
g <- ggplot(Meetweek0, aes(log10(Ferritine_FINDEM), group = NewReg)) +
  geom_density(aes(fill = NewReg, color = NewReg), lwd = 0, alpha = 0.8) + 
  geom_vline(data = medians, aes(xintercept = median_ferritin, color = NewReg),
             linetype = "solid", linewidth = 0.8)+
  geom_vline(data = means, aes(xintercept = mean_ferritin, color = NewReg),
             linetype = "dashed", linewidth = 0.8)+
  facet_grid(rows = vars(Geslacht), cols = vars(AgeCat)) + 
  xlim(-2,3.5) +
  xlab("Ferritin (ng/mL)") + ylab("Density") +
  scale_color_manual(values = c("steelblue4", "darkslategray3")) + 
  scale_fill_manual(values = c("steelblue4", "darkslategray3")) +
  theme_minimal() + theme(
                          legend.title = element_blank(), 
                          legend.position = "bottom",
                          legend.text = element_text(margin = margin(l = 8, r = 20, unit = "pt")))

## export the plot data (not individual donor data)
# Build the plot
gb <- ggplot_build(g)

# Extract density data
density_data <- gb$data[[1]]

# Extract the mapping from PANEL to facet values
# 'layout' gives us the mapping from PANEL number to AgeCat and Geslacht
facet_info <- gb$layout$layout %>%
  select(PANEL, AgeCat, Geslacht)

# Merge the facet info into the density data
density_export <- left_join(density_data, facet_info, by = c("PANEL"))

# Clean columns for export
density_export_clean <- density_export %>%
  select(x, y, group, AgeCat, Geslacht) %>%
  arrange(AgeCat, Geslacht, group, x)

# Write to file for sharing
saveRDS(density_export_clean, file = paste0(FolderName, "density_data_by_agecat.rds"))


## show the figure
if (showfigs) {
  print(g)
}

## save the figure file
if (savefigs) {
  ggsave("~/Amber/EFS_collab/figures/newregdistr_week0.pdf", width = 15, height = 10, unit = "cm")
  ggsave("~/Amber/EFS_collab/figures/newregdistr_week0.png", width = 15, height = 10, unit = "cm", bg="white")
  
}


# g <- ggplot(data, aes(Ferritine_FINDEM, group = NewReg)) +
#   geom_density(aes(linetype = NewReg), lwd = 0.5) +
#   facet_grid(rows = vars(Geslacht), cols = vars(AgeCat)) +
#   xlim(0,200) + xlab("Ferritin (ng/mL)") + ylab("Density") +
#   theme_minimal() + theme(
#                           legend.title = element_blank(),
#                           legend.position = "right",
#                           legend.text = element_text(margin = margin(l = 8, r = 20, unit = "pt")))
# g
# 
# ## Meetweek 3
# 
# Meetweek3 <- data %>% filter(Meetweek==3) #9180
# 
# newdonors <- Meetweek3[Meetweek3$Donatiesoortcode == "N", ] #1774
# regdonors <- Meetweek3[Meetweek3$Donatiesoortcode == "V", ] #7717
# newdonors <- newdonors[!is.na(newdonors$Ferritine_FINDEM), ]      # 21 obs
# regdonors <- regdonors[!is.na(regdonors$Ferritine_FINDEM), ]      # 7308 obs
# 
# newdonors$NewReg <- "New donor"
# regdonors$NewReg <- "Repeat donor"
# 
# Meetweek3 <- rbind(newdonors, regdonors)
# Meetweek3$NewReg <- as.factor(Meetweek3$NewReg)
# Meetweek3 <- Meetweek3[order(Meetweek3$KeyID, Meetweek3$Donatiedatum), ]           # 7687 obs
# Meetweek3$AgeCat <- cut(as.numeric(Meetweek3$LeeftijdBijDOnatie),
#                         breaks = c(18, 25, 35, 50, Inf),  # The intervals
#                         labels = c("18-24", "25-34", "35-49", "50+"),  # The factor labels
#                         right = FALSE) 
# 
# g <- ggplot(Meetweek3, aes(Ferritine_FINDEM, group = NewReg)) +
#   geom_density(aes(fill = NewReg, color = NewReg), lwd = 0, alpha = 0.8) + 
#   facet_grid(rows = vars(Geslacht), cols = vars(AgeCat)) + 
#   xlim(0,200) + xlab("Ferritin (ng/mL)") + ylab("Density") +
#   scale_color_manual(values = c("steelblue4", "darkslategray3")) + #@Lucile: I only changed the colours here
#   scale_fill_manual(values = c("steelblue4", "darkslategray3")) +
#   theme_minimal() + theme(
#     legend.title = element_blank(), 
#     legend.position = "bottom",
#     legend.text = element_text(margin = margin(l = 8, r = 20, unit = "pt")))
# if (showfigs) {
#   print(g)
# }
# 
# if (savefigs) {
#   ggsave("~/Amber/EFS_collab/figures/newregdistr_week3.pdf", width = 15, height = 10, unit = "cm")
# }
