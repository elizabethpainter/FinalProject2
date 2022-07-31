library(RColorBrewer)
library(tidyr)
library(mcmcplots)
library(R2jags)
library(dplyr)
library(lubridate)
library(survival)
library(survminer)
library(ggplot2)
library(ggpubr)
library(gtsummary)
library(broom)
library(cmprsk)
library(forcats)
library(timereg)
library(mstate)
library(RColorBrewer)


Adults <- read.csv(here::here("data","Adults2019_2021.csv"))
str(Adults)




#### RENAMING VARIABLES
Adults$Age.Class <- ifelse(Adults$Age.Class =="16", "6 Month Fawn", 
                           ifelse(Adults$Age.Class =="17", "Yearling", 
                                  ifelse(Adults$Age.Class == "18", "Adult", 
                                         ifelse( Adults$Age.Class == "15", "Neonate", NA)
                                  )
                           )
)

Adults$Year <- as.factor(Adults$Year)
Adults$Age.Class <- as.factor(Adults$Age.Class)
Adults$Sex <- as.factor(Adults$Sex)

Adults$Capture.date <- as.Date(Adults$Capture.date, format = "%m/%d/%Y")
Adults$Partruition <- as.Date(Adults$Partruition, format = "%m/%d/%Y" )
Adults$FateDate <- as.Date(Adults$FateDate, format = "%m/%d/%Y")


is(Adults$Capture.date)
is(Adults$FateDate)

##### CALCUALATING DAYS OF SURVIVAL
Adults$days <- ifelse(Adults$Age.Class == "Neonate", (Adults$FateDate - Adults$Partruition), (Adults$FateDate - Adults$Capture.date))
is(Adults$days)

Adults$days <- as.numeric(Adults$days)

####subset(Adults, duplicated(EarTag))

###### MD
##########
MD <- dplyr::select( Adults, Survival, Sex, Species, Fate, Age.Class,Year, Description) %>%
  filter(Species == "MD") %>%
  arrange(Year)
MD
########


Adults <- subset(Adults, Adults$Species == "WTD")
Adults$Description


AdultsSummary <- select( Adults, Species, Sex, GMU, Year, Age.Class, Description) %>%
                filter(Species != "MD") %>%
                group_by(Sex, Age.Class) %>%
                tally()
AdultsSummary
ggplot(AdultsSummary, aes(x= Year, y = n, fill = Age.Class)) +
  geom_bar(stat = "identity", position = "dodge")




AdultsSummary <- select( Adults, Species, Sex, GMU, Year, Age.Class, Description) %>%
  filter(Species != "MD") %>%
  group_by(Year, Age.Class) %>%
  tally()
AdultsSummary