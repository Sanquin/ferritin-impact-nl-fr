library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)

setwd("~/Amber/EFS_collab")

data <- readRDS("~/Amber/Data/Algemeen/alledonaties_2008_2024.rds") %>% select(KeyID, Geslacht, Hb, Ferritine, Donatiedatum, Donatiesoortcode) %>% filter(Donatiedatum > "2017-09-01")

# Order data by KeyID and Donatiedatum, then point to the next donation
data <- data[order(data$KeyID, data$Donatiedatum),] 
idx <- 1:nrow(data)
next_rec <- idx + 1
next_rec[nrow(data)] <- NA  # Handle the last row

# See if the next donation has a ferritin measurement
data$nextFer <- NA
data$nextFer <- data$Ferritine[next_rec]
data$nextFer[data$KeyID != data$KeyID[next_rec]] <- NA  # Set to NA if next donation is for a different KeyID

# Calculate time difference with the next donation
data$dTimeNext <- NA
data$dTimeNext <- data$Donatiedatum[next_rec] - data$Donatiedatum
data$dTimeNext[data$KeyID != data$KeyID[next_rec]] <- NA  # Set to NA if next donation is for a different KeyID

#select only donations where previous ferritin was < 15
data_deferred <- data %>% filter(Ferritine < 15 & Donatiedatum < "2023-01-01") %>% mutate(returned = ifelse(!is.na(dTimeNext),1,0), returned_twoyears = ifelse(dTimeNext < (2*365.25), 1,0), returned_twoyears = ifelse(is.na(returned_twoyears), 0, returned_twoyears)) %>% mutate(fermeas_at_return = ifelse(returned_twoyears==1 & !is.na(nextFer), 1,0), fermeas_at_return = ifelse(returned_twoyears==0, NA, fermeas_at_return))

#number of males and females
table(data_deferred$Geslacht)

#number returned
table(data_deferred$Geslacht, data_deferred$returned)
prop.table(table(data_deferred$Geslacht, data_deferred$returned), margin=1)

#number returned within a year of first possible donation date
table(data_deferred$Geslacht, data_deferred$returned_twoyears)
prop.table(table(data_deferred$Geslacht, data_deferred$returned_twoyears), margin=1)

#ferritin measured upon return
table(data_deferred$Geslacht, data_deferred$fermeas_at_return)
prop.table(table(data_deferred$Geslacht, data_deferred$fermeas_at_return), margin=1)

#see how many returned with increased ferritin
returned_donors <- data_deferred %>% filter(fermeas_at_return == 1) %>% mutate(dFer = nextFer-Ferritine, changeperday = dFer/as.numeric(dTimeNext), IncreasedFer = ifelse(dFer > 0, 1,0))

# median time until return
summary(as.numeric(returned_donors$dTimeNext[returned_donors$Geslacht=="F"]))

summary(as.numeric(returned_donors$dTimeNext[returned_donors$Geslacht=="M"]))

#number returned with increased ferritin
table(returned_donors$Geslacht, returned_donors$IncreasedFer)
prop.table(table(returned_donors$Geslacht, returned_donors$IncreasedFer), margin=1)

#median change
summary(returned_donors$dFer[returned_donors$Geslacht=="F"])

summary(returned_donors$dFer[returned_donors$Geslacht=="M"])

#median change per day
summary(returned_donors$changeperday[returned_donors$Geslacht=="F"])

summary(returned_donors$changeperday[returned_donors$Geslacht=="M"])

#what is their ferritin after deferral?
returned_donors_highHb <- returned_donors %>% mutate(nextFerCat = case_when(nextFer < 15 ~ 1, nextFer >= 15 & nextFer < 30 ~ 2, nextFer> 30 ~ 3)) %>% filter((Geslacht == "M" & Hb >= 8.4) |(Geslacht == "F" & Hb >= 7.8))

table(returned_donors_highHb$Geslacht, returned_donors_highHb$nextFerCat)
prop.table(table(returned_donors_highHb$Geslacht, returned_donors_highHb$nextFerCat), margin=1)*100

#select only donations where previous ferritin was 15 - 30
data_deferred <- data %>% filter(Ferritine >= 15 & Ferritine < 30 & Donatiedatum < "2023-01-01")%>% mutate(returned = ifelse(!is.na(dTimeNext),1,0), returned_twoyears = ifelse(dTimeNext < (1.5*365.25), 1,0), returned_twoyears = ifelse(is.na(returned_twoyears), 0, returned_twoyears)) %>% mutate(fermeas_at_return = ifelse(returned_twoyears==1 & !is.na(nextFer), 1,0), fermeas_at_return = ifelse(returned_twoyears==0, NA, fermeas_at_return))

#number of males and females
table(data_deferred$Geslacht)

#number returned
table(data_deferred$Geslacht, data_deferred$returned)
prop.table(table(data_deferred$Geslacht, data_deferred$returned), margin=1)

#number returned within a year of first possible donation date
table(data_deferred$Geslacht, data_deferred$returned_twoyears)
prop.table(table(data_deferred$Geslacht, data_deferred$returned_twoyears), margin=1)

#ferritin measured upon return
table(data_deferred$Geslacht, data_deferred$fermeas_at_return)
prop.table(table(data_deferred$Geslacht, data_deferred$fermeas_at_return), margin=1)

#see how many returned with increased ferritin
returned_donors <- data_deferred %>% filter(fermeas_at_return == 1) %>% mutate(dFer = nextFer-Ferritine, changeperday = dFer/as.numeric(dTimeNext), IncreasedFer = ifelse(dFer > 0, 1,0))

# median time until return
summary(as.numeric(returned_donors$dTimeNext[returned_donors$Geslacht=="F"]))

summary(as.numeric(returned_donors$dTimeNext[returned_donors$Geslacht=="M"]))

#number returned with increased ferritin
table(returned_donors$Geslacht, returned_donors$IncreasedFer)
prop.table(table(returned_donors$Geslacht, returned_donors$IncreasedFer), margin=1)

#median change
summary(returned_donors$dFer[returned_donors$Geslacht=="F"])

summary(returned_donors$dFer[returned_donors$Geslacht=="M"])

#median change per day
summary(returned_donors$changeperday[returned_donors$Geslacht=="F"])

summary(returned_donors$changeperday[returned_donors$Geslacht=="M"])

#what is their ferritin after deferral?
returned_donors_highHb <- returned_donors %>% mutate(nextFerCat = case_when(nextFer < 15 ~ 1, nextFer >= 15 & nextFer < 30 ~ 2, nextFer> 30 ~ 3)) %>% filter((Geslacht == "M" & Hb >= 8.4) |(Geslacht == "F" & Hb >= 7.8))

table(returned_donors_highHb$Geslacht, returned_donors_highHb$nextFerCat)
prop.table(table(returned_donors_highHb$Geslacht, returned_donors_highHb$nextFerCat), margin=1)*100
