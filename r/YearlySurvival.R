install.packages("mstate")
library(tidyr)
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

########################################################################
#########################################################################
########################################################################
######################################################################
#########################################################################

### 6 Month Fawn 180 Day Survival



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

####WTD AND FEMALE SUBSET
######
Adults <- subset(Adults, Adults$Species == "WTD")
Adults <- subset(Adults, Adults$Sex == "F")
Adults <- subset(Adults, Adults$Sex == "M")
Adults120 <- subset(Adults, Adults$days >120)


Adults$days
str(Adults)

WrongDate <- subset(Adults, Adults$days < 0)

 ############ NEONATE

#### probability of survival to 180 days ~ 6 month mark, post hunting
summary(survfit(Surv(days, Survival) ~ 1, data = Adults), times=180 )
summary(survfit(Surv(days, Survival) ~ Age.Class, data = Adults), times=180 )

Neonate180S <- 0.393
Neonate180SE <- 0.0644


#### probability of survival to 180 days to Age 12 Months

summary(survfit(Surv(days, Survival) ~ 1, data = Adults), times=120 )
summary(survfit(Surv(days, Survival) ~ Age.Class, data = Adults), times=120 )

SixMonth180S <- 0.748
SixMonth180SE <- 0.063


#####################
####################
######### 0-12 Month Survival
FawnSurvival <-  Neonate180S * SixMonth180S  * 0.877 ## Jan/Feb survival from Lit
FawnSurvivalSE <-  sqrt( (((Neonate180SE^2) + (SixMonth180SE^2) + (0.063^2))/2) )### Jan/Feb SD from LIT
FawnSurvival
FawnSurvivalSE <- mean(c(Neonate180SE, SixMonth180SE, 0.063))
FawnSurvivalLow <- FawnSurvival - 1.96*FawnSurvivalSE
FawnSurvivalHigh <- FawnSurvival + 1.96*FawnSurvivalSE



#####################
######################
####################
#######  Yearling Survival

### Sixmonth fawn 180-365 Survival 12-18 Month


summary(survfit(Surv(days, Survival) ~ Age.Class, data = Adults120), times=300, extend = TRUE)

SixMonth365S <- 0.876
SixMonth365SE <- 0.068




#### Yearling 180 day survival 18-24 Month

summary(survfit(Surv(days, Survival) ~ Age.Class, data = Adults), times=120 )

Yearling180S <- 1
Yearling180SE <- 0

####### 12-24 Month Survival
YearlingSurvival <- SixMonth365S * Yearling180S * (0.917) ## Jan/Feb Survival of 6 month Fawn
YearlingSurvivalSE <- sqrt(( ((SixMonth365SE^2) + (Yearling180SE^2) + (0.080^2))/2))
YearlingSurvival
YearlingSurvivalSE <- mean(c(SixMonth365SE, Yearling180SE, 0.080))

YearlingSurvivalLow <- YearlingSurvival - 1.96* YearlingSurvivalSE
YearlingSurvivalHigh <- YearlingSurvival + 1.96* YearlingSurvivalSE




#############
#############
#####################
#######################
#######################
#############Adult Survival



###### ### Yearlings 180-365 Survival 24 - 30 months

summary(survfit(Surv(days, Survival) ~ Age.Class, data = Adults120), times=300, extend = TRUE )

Yearling365S <- 0.9067
Yearling365SE <- 0.0637


###### ADULTS 30-36 Months 

summary(survfit(Surv(days, Survival) ~ Age.Class, data = Adults), times=140, extend = TRUE)



AdultsS <- 0.8543
AdultsSE <- 0.0246

##############
########
######### 
###### ADULTS 24 + 
#############
##############

AdultSurvivalS <- Yearling365S * AdultsS * (1) ## Jan/Feb Yearling Survival
AdultsSurvivalSE <- sqrt((Yearling365SE^2) + (AdultsSE^2))
AdultSurvivalS
AdultsSurvivalSE <- mean(c(Yearling365SE,AdultsSE))

AdultSurvivalSLow <- AdultSurvivalS - 1.96 * AdultsSurvivalSE

AdultSurvivalSHigh <- AdultSurvivalS + 1.96 * AdultsSurvivalSE

