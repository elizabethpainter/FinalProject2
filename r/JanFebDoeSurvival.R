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


Adults <- read.csv(here::here("data","Adults2019_2021.csv"))
str(Adults)
Adults <- subset(Adults, Adults$Species == "WTD")
Adults <- subset(Adults, Adults$Sex == "F")


###### Date Formats

Adults$FateDate <- as.Date(Adults$FateDate, format = "%m/%d/%Y")
Adults$Capture.date <- as.Date(Adults$Capture.date, format = "%m/%d/%Y")
Adults$Age.Class <- ifelse(Adults$Age.Class =="16", "6 Month Fawn", 
                           ifelse(Adults$Age.Class =="17", "Yearling", 
                                  ifelse(Adults$Age.Class == "18", "Adult", NA)))

Adults$Year <- as.factor(Adults$Year)
Adults$Age.Class <- as.factor(Adults$Age.Class)

is(Adults$Capture.date)
is(Adults$FateDate)
str(Adults$Year)



##### DAYS SURVIVAL CALCULATION
########

Adults$days <- Adults$FateDate - Adults$Capture.date
is(Adults$days)


Adults$JanSurvival <- Adults$FateDate - Adults$JanStart2019
is(Adults$days)

Adults$days <- as.numeric(Adults$days)
Adults$JanSurvival <- as.numeric(Adults$JanSurvival)


#############

January2019 <- Adults %>% filter(Year == "2019") %>%
  filter(FateDate > "2019-12-31") %>%
 # filter(Age.Class == "Adult") %>%
  group_by(Collar.ID, Capture.date, FateDate, Fate, Description, Survival, days, Year, GMU)
  

summary(January2019)
January2019$JanStart = as.Date("2019-12-31")



January2020 <- Adults %>% filter(Year == "2020") %>%
  filter(FateDate > "2020-12-31") %>%
  #filter(Age.Class == "Adult") %>%
  group_by(Collar.ID, Capture.date, FateDate, Fate, Description, Survival, days, Year, GMU) 
January2020
summary(January2020)
January2020$JanStart = as.Date("2020-12-31")


January2021 <- Adults %>% filter(Year == "2021") %>%
  filter(FateDate > "2021-12-31") %>%
  #filter(Age.Class == "Adult") %>%
  group_by(Collar.ID, Capture.date, FateDate, Fate, Description, Survival, days, Year, GMU) 
January2021
summary(January2021)
January2021$JanStart = as.Date("2021-12-31")



January2022 <- Adults %>% filter(Year == "2022") %>%
  filter(FateDate > "2022-12-31") %>%
  #filter(Age.Class == "Adult") %>%
  group_by(Collar.ID, Capture.date, FateDate, Fate, Description, Survival, days, Year, GMU) 
January2022
summary(January2022)
January2022$JanStart = as.Date("2022-12-31")



JanSurvival <- full_join(January2019,January2020)
str(JanSurvival)

JanSurvival$JanDays <- JanSurvival$FateDate - JanSurvival$JanStart
is(JanSurvival$JanDays)


########## JANUARY 1st START DATE


NullAdultSurvival <- survfit(Surv(JanDays, Survival) ~ 1,
                             data = JanSurvival)
summary(NullAdultSurvival)
ggsurvplot(NullAdultSurvival, data = JanSurvival, risk.table = TRUE)

##probability of survival to 31 days (January)
summary(survfit(Surv(JanDays, Survival) ~ 1,
                data = JanSurvival), times=31 )
summary(survfit(Surv(JanDays, Survival) ~ Year, data = JanSurvival), times=31 )
summary(survfit(Surv(JanDays, Survival) ~ Age.Class, data = JanSurvival), times=31 )
summary(survfit(Surv(JanDays, Survival) ~ GMU, data = JanSurvival), times=31 )
summary(survfit(Surv(JanDays, Survival) ~ Sex, data = JanSurvival), times=31)

##probability of survival to 59 days (February)
summary(survfit(Surv(JanDays, Survival) ~ 1,
                data = JanSurvival), times=59 )
summary(survfit(Surv(JanDays, Survival) ~ Year, data = JanSurvival), times=59 )
summary(survfit(Surv(JanDays, Survival) ~ Age.Class, data = JanSurvival), times=59 )
summary(survfit(Surv(JanDays, Survival) ~ GMU, data = JanSurvival), times=59, extend = TRUE )
summary(survfit(Surv(JanDays, Survival) ~ Sex, data = JanSurvival), times=59)

####
#### Difference between Ages####
####
Adult_Age <- survfit(Surv(JanDays, Survival) ~ Age.Class, data = JanSurvival)
                     
survdiff(Surv(JanDays, Survival) ~ Age.Class, data = JanSurvival)

ggsurvplot(fit = Adult_Age, data = JanSurvival, risk.table = TRUE, 
           pval = TRUE, conf.int = TRUE, xlim = c(0,365), break.time.by = 50 )


####
####GMU#######No Difference
####

Adult_GMU <- survfit(Surv(JanDays, Survival) ~ GMU, data = JanSurvival)

survdiff(Surv(JanDays, Survival) ~ GMU, data = JanSurvival)

ggsurvplot(fit = Adult_GMU, data = JanSurvival, risk.table = TRUE, pval = TRUE, conf.int = TRUE )


####
#### Difference between Years
####

Adult_Year <- survfit(Surv(JanDays, Survival) ~ Year, data = JanSurvival)

survdiff(Surv(JanDays, Survival) ~ Year, data = JanSurvival)

ggsurvplot(fit = Adult_Year, data = JanSurvival, risk.table = TRUE, pval = TRUE, conf.int = TRUE )





####
#### Difference between Sexes####
####
Adult_Sex <- survfit(Surv(JanDays, Survival) ~ Sex, data = JanSurvival)


survdiff(Surv(JanDays, Survival) ~ Sex, data = JanSurvival)


ggsurvplot(fit = Adult_Sex, data = JanSurvival, risk.table = TRUE, pval = TRUE, conf.int = TRUE )


