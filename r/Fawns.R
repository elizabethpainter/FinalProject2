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
###
###
######NEONATES#####
####
####
        

Deer <- read.csv(here::here("data","Adults2019_2021.csv"))
## FORMAT DATE COLUMNS
Deer$Capture_date <- as.Date(Deer$Capture_date, "%m/%d/%y" )
Deer$Partuition_date <- as.Date(Deer$Partuition_date, "%m/%d/%y" )
Deer$FateDate <- as.Date(Deer$FateDate, "%m/%d/%y" )
Deer$Partuition_Julian <- yday(Deer$Partuition_date)
Deer$FateDate_Julian <- yday(Deer$FateDate)
str(Deer)
Deer$Year <- as.factor(Deer$Year)
Deer$GMU <- as.factor(Deer$GMU)
Deer <- subset(Deer, Deer$Species == "WTD")
Deer$Species

### DAYS OF SURVIVAL

Deer$days <- Deer$FateDate_Julian - Deer$Partuition_Julian

str(Deer)
Deer$days
Deer$Description


#### NEW AGE CLASS
Adults$AgeClassAnalysis <- ifelse(Deer$Age.Class == "15", "Fawn", 
                                ifelse(Adults$Age.Class =="16", "6 Month Fawn", 
                           ifelse(Adults$Age.Class =="17", "Yearling", 
                                  ifelse(Adults$Age.Class == "18", "Adult", NA)))
Adults$Age.Class <- as.factor(Adults$Age.Class)

###Cause-Specific Coding


## Levels: nonpredation Bear Bobcat censored Coyote Mountain Lion Unknown Predation  Wolf
Fawns$Cause_8 <- fct_collapse(Fawns$Description, Nonpredation = c ("Accident", "Disease", "Malnutrition"), Censored = c ("Censored", "Slipped Collar", "Unknown"))
Fawns$Cause_8

Cause_7Names <- c("Non-predation", "Bear", "Cat","Coyote", "Unknown-Predation","Wolf")
### Levels: nonpredation Bear cats censored Coyote Unknown Predation  Wolf
Fawns$Cause_7 <- fct_collapse(Fawns$Cause_8, Cats = c ("Mountain Lion", "Bobcat"))
Fawns$Cause_7

### Factors categorical
Fawns$Cause_code8 <- factor(Fawns$Cause_8)
Fawns$Cause_code8  <- unclass(Fawns$Cause_code8)
Fawns$Cause_code8

Fawns$Cause_code7 <- factor(Fawns$Cause_7)
Fawns$Cause_code7  <- unclass(Fawns$Cause_code7)
Fawns$Cause_code7


#### Survival Object### number of days survived and censored or not (+), lived animals and collar malfunctions were censored
survfit(Surv(days, Survival) ~ 1,
        data = Fawns)

NullFawnSurvival <- survfit(Surv(days, Survival) ~ 1,
               data = Fawns)
summary(NullFawnSurvival)
ggsurvplot(NullFawnSurvival, data = Fawns, risk.table = TRUE, conf.int = TRUE, 
           color = "sea green", xlab = " Time (Days)", 
           title = "White-Tailed Deer Neonate Survival Probability")

NullFawnSurvival_GMU <- survfit(Surv(days, Survival) ~ GMU,
                                data = Fawns)
summary(NullFawnSurvival_GMU)
ggsurvplot(NullFawnSurvival_GMU, data = Fawns, risk.table = TRUE)

NullFawnSurvival_Year <- survfit(Surv(days, Survival) ~ Year,
                                data = Fawns)
summary(NullFawnSurvival_Year)
ggsurvplot(NullFawnSurvival_Year, data = Fawns, risk.table = TRUE, pval = TRUE, conf.int = TRUE)


# Visualize with survminer
ggsurvplot(NullFawnSurvival)
ggsurvplot(NullFawnSurvival, data = Fawns, risk.table = TRUE)


##probability of survival to 43 days (6 weeks) (~ time to heel)
summary(survfit(Surv(days, Survival) ~ 1, data = Fawns), times=43 )
summary(survfit(Surv(days, Survival) ~ Sex_ID, data = Fawns), times=43 )
summary(survfit(Surv(days, Survival) ~ GMU, data = Fawns), times=43 )
summary(survfit(Surv(days, Survival) ~ Year, data = Fawns), times=43 )

## probability of survival to 120 days ~ beginning of hunting season

summary(survfit(Surv(days, Survival) ~ 1, data = Fawns), times=120 )
summary(survfit(Surv(days, Survival) ~ Sex_ID, data = Fawns), times=120 )
summary(survfit(Surv(days, Survival) ~ GMU, data = Fawns), times=120 )
summary(survfit(Surv(days, Survival) ~ Year, data = Fawns), times=120 )

#### probability of survival to 180 days ~ 6 month mark, post hunting

summary(survfit(Surv(days, Survival) ~ 1, data = Fawns), times=180 )
summary(survfit(Surv(days, Survival) ~ Sex_ID, data = Fawns), times=180 )
summary(survfit(Surv(days, Survival) ~ GMU, data = Fawns), times=180 )
summary(survfit(Surv(days, Survival) ~ Year, data = Fawns), times=180 )


####
#### Difference between Sexes####
####
Neonate_Sex <- survfit(Surv(days, Survival) ~ Sex_ID,
                data = Fawns)
survdiff(Surv(days, Survival) ~ Sex_ID,
         data = Fawns)
ggsurvplot(fit = Neonate_Sex, data = Fawns, risk.table = TRUE, pval = TRUE, conf.int = TRUE )
ggsurvplot(fit = Neonate_Sex, data = Fawns, pval = TRUE, conf.int = TRUE)

####
####GMU#######No Difference
####

Neonate_GMU <- survfit(Surv(days, Survival) ~ GMU,
                data = Fawns)
survdiff(Surv(days, Survival) ~ GMU,
         data = Fawns)
ggsurvplot(fit = Neonate_GMU, data = Fawns, risk.table = TRUE, pval = TRUE, conf.int = TRUE )
ggsurvplot(fit = Neonate_GMU, data = Fawns, pval = TRUE, conf.int = TRUE )


####
#### Difference between Years
####
Neonate_Year <- survfit(Surv(days, Survival) ~ Year,
                                 data = Fawns)
survdiff(Surv(days, Survival) ~ Year,
         data = Fawns)
ggsurvplot(fit = Neonate_Year, data = Fawns, risk.table = TRUE, pval = TRUE, conf.int = TRUE )
ggsurvplot(fit = Neonate_Year, data = Fawns, pval = TRUE, conf.int = TRUE )

####
######COX 
###
###Nullmodel
coxph(Surv(days, Survival) ~ 1, data = Fawns)


###Sex - no difference
coxphSex <- coxph(Surv(days, Survival) ~ Sex_ID, data = Fawns) %>% 
        gtsummary::tbl_regression(exp = TRUE) 
coxph(Surv(days, Survival) ~ Sex_ID, data = Fawns)

###GMU - no difference

coxphGMU <- coxph(Surv(days, Survival) ~ GMU, data = Fawns)%>% 
        gtsummary::tbl_regression(exp = TRUE) 
coxph(Surv(days, Survival) ~ GMU, data = Fawns)
coxphGMU
####Year - no difference
coxphYear <- coxph(Surv(days, Survival) ~ Year, data = Fawns)%>% 
        gtsummary::tbl_regression(exp = TRUE) 
coxphYear
####Weight - no difference
coxphWeight <- coxph(Surv(days, Survival) ~ SCLDWeight, data = Fawns)%>% 
        gtsummary::tbl_regression(exp = TRUE) 
coxph(Surv(days, Survival) ~ SCLDWeight, data = Fawns)

###Can I do birthdate?
coxphBirthdate <- coxph(Surv(days, Survival) ~ Partuition_Julian, data = Fawns)%>% 
        gtsummary::tbl_regression(exp = TRUE) 
coxphBirthdate
coxph(Surv(days, Survival) ~ Partuition_Julian, data = Fawns)

####COX GLOBAL
CoxSCLD <- coxph(Surv(days, Survival) ~ Sex_ID +
                                          GMU +
                                          Year,
                                    data = Fawns) %>% 
        gtsummary::tbl_regression(exp = TRUE) 
CoxSCLD <- coxph(Surv(days, Survival) ~ Sex_ID +
                         GMU +
                         Year,
                 data = Fawns)

ggforest(CoxSCLD, data = Fawns)



####### Cumulative Incidence Functions
print(CI_7 <- cmprsk::cuminc(Fawns$days, 
                             Fawns$Cause_code7,
                             cencode=4))

plot(CI_7,
     xlim = c(0,50),
     ylim = c(0,0.3),
     xlab = "Days",
     group = NULL,
     lty = 1, 
     color = 1:6)

ggcompetingrisks(CI_7,
                multiple_panels = FALSE,
                conf.int = TRUE)

### Package "mstate"
Fails <- c(1,2,3,5,6,7)
Mstate_CI_7 <- mstate::Cuminc(time = "days", 
                              status = "Cause_code7", 
                              data = Fawns, 
                              failcodes = Fails)
head(Mstate_CI_7)
tail(Mstate_CI_7)

print(Mstate_CI_7)

summary(Mstate_CI_7)
plot(
        Mstate_CI_7,
        use.ggplot = TRUE,
        type = "separate",
        conf.type = "log",
        conf.int = 0.95,
)

#####
#####CumIncidence (Fawns$days, )

