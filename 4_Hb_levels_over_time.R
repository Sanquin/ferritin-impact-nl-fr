library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)

data <- data <- readRDS("~/Amber/Data/Algemeen/alledonaties_2008_2023.rds") %>% select(Geslacht, Hb, Ferritine, Donatiedatum, Donatiesoortcode) %>% filter(Donatiedatum > "2015-01-01")  # i took all data from two years before the policy was implemented

data2 <- data %>% filter(Donatiesoortcode == "N" | Donatiesoortcode == "V") %>% filter(Geslacht=="M" | Geslacht=="F") %>% filter(Hb < 15 & Hb > 3) %>%
  mutate(quarter = paste0(year(Donatiedatum), " Q", quarter(Donatiedatum)) #Donatiedatum = donation date
  ) %>% mutate(Hb_gpl = Hb / 0.6206,year = year(Donatiedatum))

# Calculate mean Hb for each period
mean_hb_by_period <- data2 %>%
  mutate(period = case_when(
    year >= 2015 & year <= 2017 ~ "2015-2017",
    year >= 2020 & year <= 2022 ~ "2020-2022",
    TRUE ~ NA_character_  # Exclude other years
  )) %>%
  filter(!is.na(period)) %>%
  group_by(Geslacht, period) %>%
  summarize(mean_Hb = mean(Hb, na.rm = TRUE), .groups = "drop")

# Reshape the data for easier calculation of differences
mean_hb_wide <- mean_hb_by_period %>%
  pivot_wider(names_from = period, values_from = mean_Hb)

# Calculate the differences between periods
mean_hb_wide <- mean_hb_wide %>%
  mutate(difference = `2020-2022` - `2015-2017`)

print(mean_hb_wide)

# Calculate overall mean and differences
overall_stats <- data2%>%
  filter(!is.na(year), year >= 2015 & year <= 2022) %>%
  mutate(period = case_when(
    year >= 2015 & year <= 2017 ~ "2015-2017",
    year >= 2020 & year <= 2022 ~ "2020-2022"
  )) %>%
  filter(!is.na(period)) %>%
  group_by(period) %>%
  summarize(mean_Hb = mean(Hb, na.rm = TRUE)) %>%
  pivot_wider(names_from = period, values_from = mean_Hb) %>%
  mutate(difference = `2020-2022` - `2015-2017`) %>%
  mutate(Geslacht = "Overall") # Add column to match format with gender-specific data

# Combine gender-specific and overall stats
final_result <- bind_rows(mean_hb_wide, overall_stats)

print(final_result)

# Calculate the number of observations for each group
results <- data2 %>%
  group_by(quarter, Geslacht) %>%
  summarise(
    meanHb = mean(Hb, na.rm = TRUE),
    sdHb = sd(Hb, na.rm = TRUE),
    n = n(),  # Number of observations
    .groups = 'drop'  # Prevents group from being preserved
  ) %>%
  mutate(
    # Calculate the 95% confidence interval
    lower_ci = meanHb - 1.96 * (sdHb / sqrt(n)),
    upper_ci = meanHb + 1.96 * (sdHb / sqrt(n))
  )

# Plot with Mean Hb, SD as error bars, and 95% CI as shaded area
ggplot(results, aes(x = quarter, y = meanHb, group = Geslacht, color = Geslacht)) +
  geom_line(size = 1) +  # Create lines
  geom_point(size = 2) +  # Add points
  geom_errorbar(aes(ymin = meanHb - sdHb, ymax = meanHb + sdHb), width = 0.2, size = 0.8) +  # Add error bars
  #geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = Geslacht), alpha = 0.2) +  # Add shaded CI area
  theme_minimal() +
  labs(
    title = "Mean Hb with 95% Confidence Interval and Standard Deviation per Quarter",
    x = "Time (Quarter)",
    y = "Mean Hemoglobin (Hb)"
  ) +
  facet_wrap(~Geslacht, scales='free_y') + # Separate plots for males and females
  geom_vline(xintercept="2019 Q4")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="none")  # Rotate X-axis labels for readability

# Save the plot
ggsave("~/Amber/EFS_collab/figures/meanHb_per_quarter_with_SD.pdf", height = 10, width = 20, unit = "cm")

