library(ggplot2)
library(dplyr)
library(patchwork)
library(rstudioapi)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#data 
distr_NL <- readRDS("results/density_data_by_agecat_NL.rds") %>% mutate(Country = "NL", group = ifelse(group == 1, "New donor", "Repeat donor"))
don_NL <- readRDS("results/donations_1year_NL.rds")%>% mutate(Country = "The Netherlands")
mean_NL <- readRDS("results/mean_ferritin_by_group_NL.rds")%>% mutate(Country = "The Netherlands")
median_NL <- readRDS("results/median_ferritin_by_group_NL.rds")%>% mutate(Country = "The Netherlands")

distr_FR <- readRDS("results/density_data_by_agecat_FR.rds")%>% mutate(Country = "FR")%>% rename(AgeCat = age_gr, Geslacht = Sexe) %>% mutate(group = ifelse(group == 1, "New donor", "Repeat donor"))
don_FR <- readRDS("results/donations_1year_FR.rds")%>% rename(AgeCat = age_gr, Geslacht = Sexe)%>% mutate(Country = "France")
mean_FR <- readRDS("results/mean_ferritin_by_group_FR.rds")%>% rename(AgeCat = age_gr, Geslacht = Sexe, NewReg = Statut_donneur)%>% mutate(Country = "France")
median_FR <- readRDS("results/median_ferritin_by_group_FR.rds")%>% rename(AgeCat = age_gr, Geslacht = Sexe, NewReg = Statut_donneur)%>% mutate(Country = "France")

distr_data <- rbind(distr_FR, distr_NL)
don_data <- rbind(don_NL, don_FR)

#plot distributions
g2 <- ggplot(distr_data, aes(x = x, y = y, group = Country)) +
  geom_area(aes(fill = Country), alpha = 0.7, position = "identity") +
  #geom_line(aes(color = group), linewidth = 1) +
  facet_grid(rows = vars(group), cols = vars(Geslacht, AgeCat)) +
  xlim(-0.4,3.5) +
  xlab("Ferritin (ng/mL)") +
  ylab("Density") +
  scale_color_manual(values = c("royalblue4", "darkorange2")) + #darkslategray3
  scale_fill_manual(values = c("royalblue4", "darkorange2")) +
  theme_bw() +
  theme(
    legend.title = element_blank(), 
    legend.position = "bottom",
    legend.text = element_text(margin = margin(l = 8, r = 20, unit = "pt"))
  ) #+ coord_trans(x = "log10")

g2

# barplot donations in past year
ggplot(don_data, aes(x = AgeCat, y = avg_recent_donations, fill = Country)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("royalblue4", "darkorange2")) +
  facet_wrap(~ Geslacht) +
  labs(
    x = "Age category",
    y = "Average donations",
    title = "Average number of donations in past year",
    fill = "Country"
  ) +
  theme_minimal()


library(dplyr)

# Area plots for females and males

male_data <- filter(distr_data, Geslacht == "M")

# Split by Country
male_blue  <- filter(male_data, Country == "FR")  # or whatever your 'blue' country is
male_orange <- filter(male_data, Country == "NL")  # the orange one

female_data <- filter(distr_data, Geslacht == "F")

# Split by Country
female_blue  <- filter(female_data, Country == "FR")  # or whatever your 'blue' country is
female_orange <- filter(female_data, Country == "NL")  # the orange one

g2_female <- ggplot()+geom_area(data = female_blue, aes(x = x, y = y, group = Country),
                                fill = "royalblue4", alpha = 1, position = "identity") +
  geom_area(data = female_orange, aes(x = x, y = y, group = Country),
            fill = "darkorange2", alpha = 0.7, position = "identity") +
  facet_grid(rows = vars(group), cols = vars(AgeCat)) +
  xlim(-0.4, 3.5) +
  xlab("Log10 ferritin (ng/mL)") +
  ylab("Density") +
  labs(title="Distribution of ferritin by age group and donor type", subtitle = "Females")+
  scale_fill_manual(values = c("royalblue4", "darkorange2")) +
  theme_bw() +
  theme(legend.position = "none",strip.background =element_rect(fill="white"))

g2_male <- ggplot()+geom_area(data = male_blue, aes(x = x, y = y, group = Country),
                     fill = "royalblue4", alpha = 1, position = "identity") +
  geom_area(data = male_orange, aes(x = x, y = y, group = Country),
            fill = "darkorange2", alpha = 0.6, position = "identity") +
  facet_grid(rows = vars(group), cols = vars(AgeCat)) +
  xlim(-0.4, 3.5) +
  xlab("Log 10 ferritin (ng/mL)") +
  ylab("Density") +
  labs(subtitle="Males")+
  scale_fill_manual(values = c("royalblue4", "darkorange2")) +
  scale_alpha_manual(values = c(1, 0.7)) +
  theme_bw() +
  theme(legend.position = "none",strip.background =element_rect(fill="white"))

# Bar plots for females and males
bar_female <- ggplot(filter(don_data, Geslacht == "F"), aes(x = AgeCat, y = avg_recent_donations, fill = Country)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("royalblue4", "darkorange2")) +
  labs(x = "Age category", y = "Average donations", title = "Average number of donations in past year", fill = "Country") +
  theme_minimal() +
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5))

bar_male <- ggplot(filter(don_data, Geslacht == "M"), aes(x = AgeCat, y = avg_recent_donations, fill = Country)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("royalblue4", "darkorange2")) +
  labs(x = "Age category", y = "Average donations", fill = "Country") +
  theme_minimal() +
  theme(legend.position = "none")

row_female <- g2_female + bar_female + plot_layout(widths = c(2, 1))

# Row 2: Male plots
row_male <- g2_male + bar_male + plot_layout(widths = c(2, 1))

# Combine rows
combined <- row_female / row_male
combined

ggsave(combined, file = "figures/combined_distr_avgdon.pdf", height = 7, width = 10)
ggsave(combined, file = "figures/combined_distr_avgdon.png", height = 7, width = 10, bg='white')


MEAN_COL   <- "mean_ferritin"    
MEDIAN_COL <- "median_ferritin"  
STATS_ARE_LOG10 <- TRUE         


# Bind as-is (no extra renaming/mapping needed)
#mean_data   <- dplyr::bind_rows(mean_NL, mean_FR)
median_data <- dplyr::bind_rows(median_NL, median_FR)

# Long format for vlines (per sex x age x donor type x country)
stats_long <- dplyr::bind_rows(
  #mean_data   %>% dplyr::mutate(stat = "Mean",   val = .data[[MEAN_COL]]),
  median_data %>% dplyr::mutate(stat = "Median", val = .data[[MEDIAN_COL]])
) %>%
  dplyr::mutate(
    x_line = if (STATS_ARE_LOG10) val else log10(val)
  ) %>%
  dplyr::select(Country, Geslacht, AgeCat, NewReg, stat, x_line) %>% dplyr::rename(group = NewReg)

# -----------------------------
# DENSITY PLOTS with vlines
# -----------------------------
male_data    <- dplyr::filter(distr_data, Geslacht == "M")
female_data  <- dplyr::filter(distr_data, Geslacht == "F")
male_blue    <- dplyr::filter(male_data, Country == "FR")
male_orange  <- dplyr::filter(male_data, Country == "NL")
female_blue  <- dplyr::filter(female_data, Country == "FR")
female_orange<- dplyr::filter(female_data, Country == "NL")

lt_scale <- scale_linetype_manual(values = c("Mean" = "solid", "Median" = "dashed"))

g2_female <- ggplot() +
  geom_area(data = female_blue,   aes(x = x, y = y, group = Country),
            fill = "royalblue4", alpha = 0.8, position = "identity") +
  geom_area(data = female_orange, aes(x = x, y = y, group = Country),
            fill = "darkorange2", alpha = 0.7, position = "identity") +
  geom_vline(data = dplyr::filter(stats_long, Geslacht == "F"),
             aes(xintercept = x_line, color = Country, linetype = stat),
             linewidth = 0.4, alpha = 0.9, show.legend = TRUE) +
  facet_grid(rows = vars(group), cols = vars(AgeCat)) +
  xlim(-0.4, 3.5) +
  ylim(0,1.8)+
  xlab("Log10 ferritin (ng/mL)") + ylab("Density") +
  labs(title = "Distribution and median of ferritin by age group and donor type",
       subtitle = "Females") +
  scale_color_manual(values = c("France" = "royalblue4", "FR" = "royalblue4",
                                "The Netherlands" = "darkorange2", "NL" = "darkorange2")) +
  lt_scale +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "none",
        strip.background = element_rect(fill = "white"),
        plot.title = element_text(size=12))

g2_male <- ggplot() +
  geom_area(data = male_blue,   aes(x = x, y = y, group = Country),
            fill = "royalblue4", alpha = 0.8, position = "identity") +
  geom_area(data = male_orange, aes(x = x, y = y, group = Country),
            fill = "darkorange2", alpha = 0.6, position = "identity") +
  geom_vline(data = dplyr::filter(stats_long, Geslacht == "M"),
             aes(xintercept = x_line, color = Country, linetype = stat),
             linewidth = 0.4, alpha = 0.9, show.legend = TRUE) +
  facet_grid(rows = vars(group), cols = vars(AgeCat)) +
  xlim(-0.4, 3.5) +
  ylim(0,1.8)+
  xlab("Log10 ferritin (ng/mL)") + ylab("Density") +
  labs(subtitle = "Males") +
  scale_color_manual(values = c("France" = "royalblue4", "FR" = "royalblue4",
                                "The Netherlands" = "darkorange2", "NL" = "darkorange2")) +
  lt_scale +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "none",
        strip.background = element_rect(fill = "white"))

# -----------------------------
# BAR PLOTS with repeated subtitles
# -----------------------------
bar_female <- ggplot(dplyr::filter(don_data, Geslacht == "F"),
                     aes(x = AgeCat, y = avg_recent_donations, fill = Country)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("royalblue4", "darkorange2")) +
  labs(x = "Age category", y = "Average donations",
       title = "Average number of donations in past year",
       subtitle = "Females", fill = "Country") +
  ylim(0,3)+
  theme_minimal() +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0,size=12))

bar_male <- ggplot(dplyr::filter(don_data, Geslacht == "M"),
                   aes(x = AgeCat, y = avg_recent_donations, fill = Country)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("royalblue4", "darkorange2")) +
  labs(x = "Age category", y = "Average donations",
       subtitle = "Males", fill = "Country") +
  ylim(0,3)+
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))

row_female <- g2_female + bar_female + patchwork::plot_layout(widths = c(2, 1))
row_male   <- g2_male   + bar_male   + patchwork::plot_layout(widths = c(2, 1))
combined   <- row_female / row_male
combined

ggsave(combined, file = "figures/combined_distr_avgdon.pdf", height = 7, width = 10)
ggsave(combined, file = "figures/combined_distr_avgdon.png", height = 7, width = 10, bg = "white")

