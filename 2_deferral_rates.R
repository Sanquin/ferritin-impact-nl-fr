#install.packages('binom') #if not present already

library(binom)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)

setwd("~/Amber/EFS_collab")

#create folder to save the files to
FolderName <-  paste0(getwd(), "/results/")
if (!file.exists(FolderName)){
  dir.create(file.path(FolderName))
}


data <- readRDS("~/Amber/Data/Algemeen/alledonaties_2008_2024.rds") %>% select(KeyID, Geslacht, Hb, Ferritine, Donatiedatum, Donatiesoortcode) %>% filter(Donatiedatum > "2019-11-01" & (Donatiesoortcode == "N"|Donatiesoortcode == "V" | Donatiesoortcode == "H")& Donatiedatum < "2023-01-01")

#how many donations, by sex (geslacht)
table(data$Geslacht)

#how many of each type, by sex (geslacht)
table(data$Donatiesoortcode,data$Geslacht)

#how many ferritin measurements
table(data$Geslacht[!is.na(data$Ferritine)])

table(data$Donatiesoortcode[!is.na(data$Ferritine)],data$Geslacht[!is.na(data$Ferritine)])
table(data$Donatiesoortcode[!is.na(data$Ferritine)],data$Geslacht[!is.na(data$Ferritine)])/table(data$Donatiesoortcode,data$Geslacht)*100

#Hb deferrals
data <- data %>% mutate(Hbdef = case_when(Geslacht == "M" & Hb < 8.4 ~1, Geslacht == "F"&Hb<7.8 ~1, TRUE~0), Ferdef = case_when(Ferritine < 15 ~1,Ferritine >= 15 & Ferritine < 30 ~ 2, TRUE~0), FerdefCat = case_when(Ferritine < 15 ~ "IronDef",Ferritine >= 15 & Ferritine <= 30 ~ "LowFer", TRUE~"Normal"), Hb_gpl = Hb/0.626) 

# Count Hb deferrals and totals by sex
hb_def_summary <- data %>%
  group_by(Geslacht) %>%
  summarise(
    n = n(),
    hb_deferrals = sum(Hbdef, na.rm = TRUE)
  ) %>%
  rowwise() %>%
  mutate(
    binom = list(binom.confint(hb_deferrals, n, method = "wilson"))
  ) %>%
  unnest_wider(binom, names_sep = "_") %>%
  select(Geslacht, n, hb_deferrals, rate = binom_mean, lower = binom_lower, upper = binom_upper) %>%
  mutate(across(rate:upper, ~ .x * 100)) %>% # convert to percentage
  mutate(data = "all")  

#how many ferritin deferrals
fer_def_summary <- data %>%
  filter(!is.na(FerdefCat)) %>%
  count(Geslacht, FerdefCat, name = "deferrals") %>%   # per cell
  group_by(Geslacht) %>%
  mutate(total = sum(deferrals),
         rate = deferrals / total) %>%
  rowwise() %>%
  mutate(binom = list(binom.confint(deferrals, total, method = "wilson"))) %>%
  ungroup() %>%
  unnest_wider(binom, names_sep = "_") %>%
  transmute(
    Geslacht, FerdefCat, deferrals, total,
    rate  = binom_mean  * 100,
    lower = binom_lower * 100,
    upper = binom_upper * 100,
    data = "all"
  )
# FIND'EM data 

data_FINDEM  <- readRDS("~/Amber/Data/FINDEM/FINDEM_data.rds")  %>% filter(Meetweek == 0) %>% mutate(Hbdef = case_when(Geslacht == "Man" & Hb < 8.4 ~1, Geslacht == "Vrouw"&Hb<7.8 ~1, TRUE~0), Ferdef = case_when(Ferritine_FINDEM < 15 ~1,Ferritine_FINDEM >= 15 & Ferritine_FINDEM < 30 ~ 2, TRUE~0), FerdefCat = case_when(Ferritine_FINDEM < 15 ~ "IronDef",Ferritine_FINDEM >= 15 & Ferritine_FINDEM <= 30 ~ "LowFer", TRUE~"Normal"), Hb_gpl = Hb/0.626)  # 37621 obs

#number of measurements
table(data_FINDEM$Geslacht)

#number with low Hb
table(data_FINDEM$Geslacht, data_FINDEM$Hbdef)
prop.table(table(data_FINDEM$Geslacht, data_FINDEM$Hbdef), margin=1)*100

hb_def_summary_FINDEM <- data_FINDEM %>%
  group_by(Geslacht) %>%
  summarise(
    n = n(),
    hb_deferrals = sum(Hbdef, na.rm = TRUE)
  ) %>%
  rowwise() %>%
  mutate(
    binom = list(binom.confint(hb_deferrals, n, method = "wilson"))
  ) %>%
  unnest_wider(binom, names_sep = "_") %>%
  select(Geslacht, n, hb_deferrals, rate = binom_mean, lower = binom_lower, upper = binom_upper) %>%
  mutate(across(rate:upper, ~ .x * 100))%>% # convert to percentage
  mutate(data = "FINDEM")  

#Ferritin categories
table(data_FINDEM$Geslacht, data_FINDEM$Ferdef)
prop.table(table(data_FINDEM$Geslacht, data_FINDEM$Ferdef), margin=1)*100

ID <- data_FINDEM %>% filter(Ferdef==1)
summary(ID$Ferritine[ID$Geslacht=="Vrouw"])
summary(ID$Ferritine[ID$Geslacht=="Man"])

LowFer <- data_FINDEM %>% filter(Ferdef==2)
summary(LowFer$Ferritine[LowFer$Geslacht=="Vrouw"])
summary(LowFer$Ferritine[LowFer$Geslacht=="Man"])

fer_def_summary_FINDEM <- data_FINDEM %>%
  filter(!is.na(FerdefCat)) %>%
  group_by(Geslacht, FerdefCat) %>%
  summarise(
    n = n(),
    deferrals = n(),
    total = nrow(filter(data_FINDEM, Geslacht == first(Geslacht)))  # total per sex
  ) %>%
  rowwise() %>%
  mutate(
    binom = list(binom.confint(deferrals, total, method = "wilson"))
  ) %>%
  unnest_wider(binom, names_sep = "_") %>%
  select(Geslacht, FerdefCat, deferrals, total, rate = binom_mean, lower = binom_lower, upper = binom_upper) %>%
  mutate(across(rate:upper, ~ .x * 100)) %>% # convert to percentage
  mutate(data = "FINDEM")  

#Hb at deferral
summary(data_FINDEM$Hb_gpl[data_FINDEM$Hbdef==1&data_FINDEM$Geslacht=="Vrouw"])
summary(data_FINDEM$Hb_gpl[data_FINDEM$Hbdef==1&data_FINDEM$Geslacht=="Man"])

#export results

ferritin_summaries <- rbind(fer_def_summary, fer_def_summary_FINDEM)
saveRDS(ferritin_summaries, file = paste0(FolderName, "ferritin_def_summaries.rds"))

hb_summaries <- rbind(hb_def_summary, hb_def_summary_FINDEM)
saveRDS(hb_summaries, file = paste0(FolderName, "hb_def_summaries.rds"))