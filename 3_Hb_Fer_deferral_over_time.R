#install.packages('binom') #if not present already
#install.packages("Kendall") #if not present already

library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(binom)
library(Kendall)

#create folder to save the files to
FolderName <-  paste0(getwd(), "/results/")
if (!file.exists(FolderName)){
  dir.create(file.path(FolderName))
}

data <- readRDS("~/Amber/Data/Algemeen/alledonaties_2008_2023.rds") %>% select(Geslacht, Hb, Ferritine, Donatiedatum, Donatiesoortcode) %>% filter(Donatiedatum > "2019-11-01" & Donatiedatum < "2023-01-01") # i took all data from the time the policy was fully implemented until first of jan 2023

data2 <- data %>% filter(Donatiesoortcode == "N" | Donatiesoortcode == "V") %>% filter((Hb < 15 & Hb > 3 & Ferritine < 900) | is.na(Hb) | is.na(Ferritine)) %>% #remove weird values
  mutate(quarter = paste0(year(Donatiedatum), " Q", quarter(Donatiedatum)), #Donatiedatum = donation date
         Fer15 = ifelse(Ferritine < 15, 1, 0), #Deferred for ID
           Fer30 = ifelse(Ferritine < 30 & Ferritine >= 15, 1, 0), #deferred for low fer
         Hbdef = case_when((Geslacht=="M"&Hb<8.4)~1, (Geslacht=="F"&Hb<7.8)~1, TRUE~0) #deferred for Hb #Geslacht means Sex
         )

results <- data2 %>%
  group_by(quarter, Geslacht) %>%
  summarise(
    total = n(),
    totalfer = sum(!is.na(Ferritine)), #not using this for now, but maybe later
    ID = (sum(Fer15, na.rm=T)/total)*100,
    LowFer = (sum(Fer30, na.rm=T)/total)*100,
    Hb = (sum(Hbdef, na.rm=T)/total)*100
  )

results_ci <- data2 %>%
  group_by(quarter, Geslacht) %>%
  summarise(
    total = n(),
    ID_events = sum(Fer15, na.rm = TRUE),
    LowFer_events = sum(Fer30, na.rm = TRUE),
    Hbdef_events = sum(Hbdef, na.rm = TRUE)
  ) %>%
  rowwise() %>%
  mutate(
    ID_ci = list(binom.confint(ID_events, total, method = "wilson")),
    LowFer_ci = list(binom.confint(LowFer_events, total, method = "wilson")),
    Hb_ci = list(binom.confint(Hbdef_events, total, method = "wilson"))
  ) %>%
  unnest_wider(ID_ci, names_sep = "_") %>%
  rename(ID = ID_ci_mean, ID_lower = ID_ci_lower, ID_upper = ID_ci_upper) %>%
  unnest_wider(LowFer_ci, names_sep = "_") %>%
  rename(LowFer = LowFer_ci_mean, LowFer_lower = LowFer_ci_lower, LowFer_upper = LowFer_ci_upper) %>%
  unnest_wider(Hb_ci, names_sep = "_") %>%
  rename(Hb = Hb_ci_mean, Hb_lower = Hb_ci_lower, Hb_upper = Hb_ci_upper)

#Plot preparation

# Reshape the data into long format for easier plotting
# df_long <- results %>%
#   select(quarter, Geslacht, ID, LowFer, Hb) %>%
#   pivot_longer(cols = c(ID, LowFer, Hb), names_to = "Variable", values_to = "Percentage") %>% 
#   mutate(Variable = case_when(Variable == "ID" ~ "Iron deficiency",
#                               Variable == 'LowFer' ~ "Low ferritin",
#                               Variable == "Hb"~ "Hb deferral"))

df_long_ci <- results_ci %>%
  select(quarter, Geslacht,
         ID, ID_lower, ID_upper,
         LowFer, LowFer_lower, LowFer_upper,
         Hb, Hb_lower, Hb_upper) %>%
  pivot_longer(
    cols = -c(quarter, Geslacht),
    names_to = "name",
    values_to = "value"
  ) %>%
  separate(name, into = c("Variable", "stat"), sep = "_") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(
    Variable = recode(Variable,
                      "ID" = "Iron deficiency",
                      "LowFer" = "Low ferritin",
                      "Hb" = "Hb deferral")
  )%>%
  rename(mean = `NA`) %>% mutate(mean = mean*100,
                                 lower = lower*100,
                                 upper = upper*100)

# View reshaped data
head(df_long_ci)

# Plot the data
# p <- ggplot(df_long, aes(x = quarter, y = Percentage, color = Variable, group = Variable)) +
#   geom_line(linewidth = 1) +  # Create lines
#   geom_point(size = 2) +  # Add points
#   theme_minimal() +
#   scale_color_manual(NULL, values = c("Iron deficiency" = "steelblue4", "Low ferritin" = "lightsteelblue2", "Hb deferral" = "indianred"), ) + #@Lucile: only thing i changed here is the colour names, you should then remove low ferritin
#   labs(
#     title = "Percentage of Variables per Quarter",
#     x = "Time (Quarter)",
#     y = "Percentage",
#     color = "Variable"
#   ) +
#   facet_wrap(~Geslacht) +
#   ylim(0,13)+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate X-axis labels for better readability

p <- ggplot(df_long_ci, aes(x = quarter, y = mean, group = Variable, color = Variable)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, linewidth = 0.8) +
  facet_wrap(~Geslacht) +
  scale_color_manual(values = c("Iron deficiency" = "steelblue4",
                                "Low ferritin" = "lightsteelblue2",
                                "Hb deferral" = "indianred")) +
  ylim(0, 13) +
  labs(title = "Deferral Rates per Quarter (with 95% CI)",
       x = "Time (Quarter)", y = "Percentage", color = "Deferral type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

gb <- ggplot_build(p)

# Extract density data
plot_data <- gb$data[[1]]
facet_info <- gb$layout$layout %>%
  select(PANEL, Geslacht)

plot_data2 <- gb$data[[3]]
facet_info <- gb$layout$layout %>%
  select(PANEL, Geslacht)

# Merge the facet info into the density data
data_export <- left_join(plot_data, facet_info, by = c("PANEL"))
data_export2 <- left_join(plot_data2, facet_info, by = c("PANEL"))

# Clean columns for export
data_export_clean <- data_export %>%
  select(x, y, group, Geslacht) %>%
  arrange(Geslacht, group, x)

data_export2_clean <- data_export2 %>%
  select(x, ymin, ymax, group, Geslacht) %>%
  arrange(Geslacht, group, x)

# Write to file for sharing
saveRDS(data_export_clean, file = paste0(FolderName, "deferral_data_by_sex.rds"))
saveRDS(data_export2_clean, file = paste0(FolderName, "deferral_data_by_sex_CI.rds"))

ggsave(p, filename="~/Amber/EFS_collab/figures/deferralrate_per_quarter.pdf", height = 10, width = 25, unit="cm")
ggsave(p, filename="~/Amber/EFS_collab/figures/deferralrate_per_quarter.png", height = 10, width = 25, unit="cm", bg="white")

# Kendall Mann test

df_long_ci <- df_long_ci %>%
  group_by(Geslacht, Variable) %>%
  mutate(time = row_number()) %>%
  ungroup()

#Mann-Kendall test per group
trend_results <- df_long_ci %>%
  group_by(Geslacht, Variable) %>%
  summarise(
    p_value = Kendall::MannKendall(mean)$sl,
    tau = Kendall::MannKendall(mean)$tau
  )

#export results
saveRDS(trend_results, file = paste0(FolderName, "test_for_trend.rds"))

# # Prepare the dataset for plotting distributions
# distribution_data <- data2 %>%
#   select(quarter, Geslacht, Hb, Ferritine) %>%
#   filter(!is.na(Hb), !is.na(Ferritine))  # Ensure there are no NA values in Hb and Ferritine
# 
# # Hb Distribution (Density Plot)
# ggplot(distribution_data, aes(x = Hb, fill = Geslacht)) +
#   geom_density(alpha = 0.5) +  # Create density plots with transparency
#   facet_wrap(~quarter) +  # Facet by quarter
#   labs(
#     title = "Distribution of Hemoglobin (Hb) by Quarter and Sex",
#     x = "Hemoglobin (Hb)",
#     y = "Density",
#     fill = "Sex"
#   ) +
#   xlim(0,15)+
#   theme_minimal()
# 
# ggsave("~/Amber/EFS_collab/figures/distribution_hb_per_quarter.pdf", height = 10, width = 25, unit="cm")
# 
# # Ferritin Distribution (Density Plot)
# ggplot(distribution_data, aes(x = Ferritine, fill = Geslacht)) +
#   geom_density(alpha = 0.5) +  # Create density plots with transparency
#   facet_wrap(~quarter) +  # Facet by quarter
#   labs(
#     title = "Distribution of Ferritin by Quarter and Sex",
#     x = "Ferritin",
#     y = "Density",
#     fill = "Sex"
#   ) +
#   xlim(0,150)+
#   theme_minimal()
# 
# ggsave("~/Amber/EFS_collab/figures/distribution_ferritin_per_quarter.pdf", height = 10, width = 25, unit="cm")
