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
library(ggfortify)




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
Adults$GMU <- as.factor(Adults$GMU)

Adults$Capture.date <- as.Date(Adults$Capture.date, format = "%m/%d/%Y")
Adults$Partruition <- as.Date(Adults$Partruition, format = "%m/%d/%Y" )
Adults$FateDate <- as.Date(Adults$FateDate, format = "%m/%d/%Y")


is(Adults$Capture.date)
is(Adults$FateDate)


##### CALCUALATING DAYS OF SURVIVAL
Adults$days <- ifelse(Adults$Age.Class == "Neonate", (Adults$FateDate - Adults$Partruition), (Adults$FateDate - Adults$Capture.date))
is(Adults$days)

Adults$days <- as.numeric(Adults$days)




####WTD AND FEMALE SUBSET
######
Adults <- subset(Adults, Adults$Species == "WTD")
#Adults <- subset(Adults, Adults$Age.Class == "Adult")
#Adults <- subset(Adults, Adults$Age.Class == "Yearling")
#Adults <- subset(Adults, Adults$Age.Class == "6 Month Fawn")
Adults <- subset(Adults, Adults$Age.Class == "Neonate")

#Adults <- subset(Adults, Adults$Sex == "F")
#Adults <- subset(Adults, Adults$Sex == "M")

Adults$days
str(Adults)

WrongDate <- subset(Adults, Adults$days < 0)






###### NULL SURVIVAL MODEL KM

NullAdultSurvival <- survfit(Surv(days, Survival) ~ 1,
                             data = Adults)
plot(NullAdultSurvival)
summary(NullAdultSurvival)
survminer::ggsurvplot(NullAdultSurvival, data = Adults, risk.table = T,
           conf.int = T, xlim = c(0,365), break.time.by = 50)

##GMU
NullAdultSurvival_GMU <- survfit(Surv(days, Survival) ~ GMU,
                                 data = Adults)
summary(NullAdultSurvival_GMU)
ggsurvplot(NullAdultSurvival_GMU, data = Adults, risk.table = TRUE)

## YEAR
NullAdultSurvival_Year <- survfit(Surv(days, Survival) ~ Year,
                                  data = Adults)
summary(NullAdultSurvival_Year, extend = TRUE$surv)
ggsurvplot(NullAdultSurvival_Year, data = Adults, risk.table = TRUE)


## AGE CLASS
NullAdultSurvival_Age <- survfit(Surv(days, Survival) ~ Age.Class, 
                                 data = Adults)
summary(NullAdultSurvival_Age)
ggsurvplot(NullAdultSurvival_Age, data = Adults, risk.table = TRUE, pval = TRUE)

####### SEX
NullAdultSurvival_Sex <- survfit(Surv(days, Survival) ~ Sex,
                                  data = Adults)
summary(NullAdultSurvival_Sex)
ggsurvplot(NullAdultSurvival_Sex, data = Adults, risk.table = TRUE)



# Visualize with survminer
ggsurvplot(NullAdultSurvival)
ggsurvplot(NullAdultSurvival, data = Adults, risk.table = TRUE)


##probability of survival to 43 days (6 weeks) (~ time to heel)
summary(survfit(Surv(days, Survival) ~ 1, data = Adults), times=43 )
summary(survfit(Surv(days, Survival) ~ Age.Class, data = Adults), times=43 )
summary(survfit(Surv(days, Survival) ~ GMU, data = Adults), times=43 )
summary(survfit(Surv(days, Survival) ~ Year, data = Adults), times=43 )
summary(survfit(Surv(days, Survival) ~ Sex, data = Adults), times=43 )

## probability of survival to 120 days ~ beginning of hunting season

summary(survfit(Surv(days, Survival) ~ 1, data = Adults), times=120 )
summary(survfit(Surv(days, Survival) ~ Age.Class, data = Adults), times=120 )
summary(survfit(Surv(days, Survival) ~ GMU, data = Adults), times=120 )
summary(survfit(Surv(days, Survival) ~ Year, data = Adults), times=120 )
summary(survfit(Surv(days, Survival) ~ Sex, data = Adults), times=120 )

#### probability of survival to 180 days ~ 6 month mark, post hunting

summary(survfit(Surv(days, Survival) ~ 1, data = Adults), times=180 )
summary(survfit(Surv(days, Survival) ~ Age.Class, data = Adults), times=180 )
summary(survfit(Surv(days, Survival) ~ GMU, data = Adults), times=180 )
summary(survfit(Surv(days, Survival) ~ Year, data = Adults), times=180 )
summary(survfit(Surv(days, Survival) ~ Sex, data = Adults), times=180)


#### probability of survival to 305 days ~~~ 10 Months

summary(survfit(Surv(days, Survival) ~ 1, data = Adults), times=305)
summary(survfit(Surv(days, Survival) ~ GMU, data = Adults), times=305, extend = TRUE )
summary(survfit(Surv(days, Survival) ~ Year, data = Adults), times=305, extend = TRUE )
summary(survfit(Surv(days, Survival) ~ Age.Class, data = Adults), times=305 )
summary(survfit(Surv(days, Survival) ~ Sex, data = Adults), times= 305 )

###### probability of survival to 365 days ~~~~~~ 12 months

summary(survfit(Surv(days, Survival) ~ 1, data = Adults), times=365)
summary(survfit(Surv(days, Survival) ~ GMU, data = Adults), times=365, extend = TRUE )
summary(survfit(Surv(days, Survival) ~ Year, data = Adults), times=365, extend = TRUE )
summary(survfit(Surv(days, Survival) ~ Age.Class, data = Adults), times=365 )
summary(survfit(Surv(days, Survival) ~ Sex, data = Adults), times= 365 )

####
#### Difference between Ages####
####
Adult_Age <- survfit(Surv(days, Survival) ~ Age.Class,
                     data = Adults)
survdiff(Surv(days, Survival) ~ Age.Class,
         data = Adults)
ggsurvplot(fit = Adult_Age, data = Adults, risk.table = TRUE, 
           pval = TRUE, conf.int = TRUE, xlim = c(0,365), break.time.by = 50 )
ggsurvplot(fit = Adult_Age, data = Adults, pval = TRUE, conf.int = TRUE)

####
####GMU#######No Difference
####

Adult_GMU <- survfit(Surv(days, Survival) ~ GMU,
                     data = Adults)
survdiff(Surv(days, Survival) ~ GMU,
         data = Adults)
ggsurvplot(fit = Adult_GMU, data = Adults, risk.table = TRUE, pval = TRUE, conf.int = TRUE )
ggsurvplot(fit = Adult_GMU, data = Adults, pval = TRUE, conf.int = TRUE )


####
#### Difference between Years
####

Adult_Year <- survfit(Surv(days, Survival) ~ Year + Age.Class,
                      data = Adults)
survdiff(Surv(days, Survival) ~ Year + Age.Class,
         data = Adults)
ggsurvplot(fit = Adult_Year, data = Adults, risk.table = TRUE, pval = TRUE, conf.int = TRUE )
ggsurvplot(fit = Adult_Year, data = Adults, pval = TRUE, conf.int = TRUE )



####
#### Difference between Ages####
####
Adult_Age <- survfit(Surv(days, Survival) ~ Age.Class +Sex,
                     data = Adults)
survdiff(Surv(days, Survival) ~ Age.Class +Sex,
         data = Adults)
ggsurvplot(fit = Adult_Age, data = Adults, risk.table = TRUE, pval = TRUE, conf.int = TRUE )
ggsurvplot(fit = Adult_Age, data = Adults, pval = TRUE, conf.int = TRUE)


####
#### Difference between Sexes####
####
Adult_Sex <- survfit(Surv(days, Survival) ~ Sex,
                     data = Adults)
survdiff(Surv(days, Survival) ~ Sex,
         data = Adults)
ggsurvplot(fit = Adult_Sex, data = Adults, risk.table = TRUE, pval = TRUE, conf.int = TRUE )
ggsurvplot(fit = Adult_Sex, data = Adults, pval = TRUE, conf.int = TRUE)



####
######COX 
###
###Nullmodel
coxph(Surv(days, Survival) ~ 1, data = Adults)


###Age
coxphAge <- coxph(Surv(days, Survival) ~ Age.Class, data = Adults) %>% 
  gtsummary::tbl_regression(exp = TRUE) 
coxph(Surv(days, Survival) ~ Age.Class, data = Adults)
coxphAge

###GMU - no difference

coxphGMU <- coxph(Surv(days, Survival) ~ GMU, data = Adults)%>% 
  gtsummary::tbl_regression(exp = TRUE) 
coxph(Surv(days, Survival) ~ GMU, data = Adults)
coxphGMU

####Year - no difference
coxphYear <- coxph(Surv(days, Survival) ~ Year, data = Adults)%>% 
  gtsummary::tbl_regression(exp = TRUE) 
coxphYear


###Sex - no difference
coxphSex <- coxph(Surv(days, Survival) ~ Sex, data = Adults) %>% 
  gtsummary::tbl_regression(exp = TRUE) 
coxph(Surv(days, Survival) ~ Sex, data = Adults)

####COX GLOBAL
CoxAge_Year <- coxph(Surv(days, Survival) ~ Age.Class +
                  
                   Year,
                 data = Adults) %>% 
  gtsummary::tbl_regression(exp = TRUE) 


CoxAge_Year


CoxAge_GMU <- coxph(Surv(days, Survival) ~ Age.Class +
                       
                       + GMU,
                     data = Adults) %>% 
  gtsummary::tbl_regression(exp = TRUE) 


CoxAge_GMU

Cox_Global <- coxph(Surv(days, Survival) ~ Age.Class +
                   
                   + GMU
                   + Year ,
                 data = Adults) %>% 
  gtsummary::tbl_regression(exp = TRUE) 


Cox_Global

autoplot(aareg(Surv(days, Survival) ~ Age.Class +
                 
                 + GMU
               + Year ,
               data = Adults))

plot_ly(Cox_Global, x = time, color = surv, type = "box")

####### WHO SURVIVED JANUARY
saveRDS(AdultS, file = "AdultS.Rds")
saveRDS(SixMonthYearlyS, file = "6MonthS.Rds")
saveRDS(YearlingS, file = "YearlingS.Rds")

readRDS(file = "AdultS.Rds")
readRDS(file = "6MonthS.Rds")
readRDS(file = "YearlingS.Rds")