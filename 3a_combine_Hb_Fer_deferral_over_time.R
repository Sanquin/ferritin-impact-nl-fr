library(ggplot2)
library(dplyr)
library(rstudioapi)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#functions
number_to_quarter_forward <- function(n, end_year, end_quarter) {
  # We reverse the counting: 10 → 2024Q3, 9 → 2024Q2, etc.
  # How many quarters back from the end
  quarters_back <- max(numbers) - n
  
  # Calculate new quarter and year
  new_quarter <- end_quarter - quarters_back
  new_year <- end_year
  
  # Adjust when quarter <= 0
  while (new_quarter <= 0) {
    new_quarter <- new_quarter + 4
    new_year <- new_year - 1
  }
  
  paste0(new_year, "-Q", new_quarter)
}



#data NL
deferral_NL <- readRDS("results/deferral_data_by_sex_NL.rds") %>% mutate(Country = "The Netherlands", group=case_when(group==1~"Low Hb", group==2~"Iron deficiency", group==3~"Low ferritin"))%>% rename(Sex = Geslacht) 
deferral_NL_CI <- readRDS("results/deferral_data_by_sex_CI_NL.rds")%>% mutate(Country = "The Netherlands", group=case_when(group==1~"Low Hb", group==2~"Iron deficiency", group==3~"Low ferritin"))%>% rename(Sex = Geslacht) 

#quarters
numbers <- deferral_NL$x[deferral_NL$group=="Low Hb"& deferral_NL$Sex=="M"]
year_quarters <- sapply(numbers, function(x) number_to_quarter_forward(x, end_quarter = 4, end_year = 2022))

quarters_NL <- data.frame(x = numbers, YearQuarter = year_quarters)
deferral_NL <- merge(deferral_NL, quarters_NL, by="x")

#data FR
deferral_FR <- readRDS("results/deferral_data_by_sex_FR.rds")%>% mutate(Country = "France", group=case_when(group==1~"Low Hb", group==2~"Iron deficiency", group==3~"Low ferritin"))%>% filter(group != "Low ferritin")
deferral_FR_CI <- readRDS("results/deferral_data_by_sex_CI_FR.rds")%>% mutate(Country = "France", group=case_when(group==1~"Low Hb", group==2~"Iron deficiency", group==3~"Low ferritin"))%>% filter(group != "Low ferritin")

#quarters
numbers <- deferral_FR$x[deferral_FR$group=="Low Hb"& deferral_FR$Sex=="M"]
year_quarters <- sapply(numbers, function(x) number_to_quarter_forward(x, end_quarter = 3, end_year = 2024))

quarters_FR <- data.frame(x = numbers, YearQuarter = year_quarters)
deferral_FR <- merge(deferral_FR, quarters_FR, by="x")

deferral <- rbind(deferral_FR, deferral_NL) %>% mutate(Sex = ifelse(Sex =='M', "Males", "Females"))
deferral$facet_label <- paste(deferral$Country, deferral$Sex, sep = " - ")

#plot
p <- ggplot(deferral, aes(x = YearQuarter, y = y)) +
  geom_line(aes(color = group, group = interaction(group, Country)), linewidth = 0.8) + 
  geom_point(aes(color = group, shape = Country, group = interaction(group, Country)), size = 1.5) + 
  theme_bw() +
  scale_color_manual("Deferral", values = c("Iron deficiency" = "steelblue4", "Low ferritin" = "lightsteelblue2", "Low Hb" = "indianred")) +
  labs(
    title = "Deferral per quarter from full implementation of ferritin policy",
    x = "Time (Quarter)",
    y = "Deferral rate",
    color = "Variable"
  ) +
  guides(shape = "none", linetype = "none") +
  facet_wrap(~ facet_label, scales = "free_x") +  # use custom label
  ylim(0, 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_rect(fill = "white")
  )

p

ggsave(p, file="~/Amber/EFS_collab/figures/combined_deferralrates.pdf", width = 20, height = 15, unit = "cm")
ggsave(p, file="~/Amber/EFS_collab/figures/combined_deferralrates.png", width = 20, height = 15, unit = "cm", bg="white")
