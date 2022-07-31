library(RColorBrewer)
library(tidyr)
library(dplyr)
library(lubridate)
library(survival)
library(survminer)
library(ggplot2)
library(ggpubr)
library(gtsummary)
library(broom)
library(forcats)
library(timereg)
library(formattable)
library(janitor)


##
###########
###################
#############


Adults <- read.csv(here::here("data","Adults2019_2021.csv"))
str(Adults)




Adults$Age.Class <- ifelse(Adults$Age.Class =="16", "6 Month Fawn", 
                           ifelse(Adults$Age.Class =="17", "Yearling", 
                                  ifelse(Adults$Age.Class == "18", "Adult", 
                                         ifelse( Adults$Age.Class == "15", "Neonate", NA)
                                  )
                           )
)

Adults$Year <- as.factor(Adults$Year)
Adults$Age.Class <- as.factor(Adults$Age.Class)

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



Adults$days
str(Adults)

WrongDate <- subset(Adults, Adults$days < 0)

Adults$Age.Class <- as.factor(Adults$Age.Class)

## Levels: [1] """Nonpredation", "Black Bear", "Censored", "Coyote", 
##"Unknown mortality", "Unknown Mortality", "Unknown Predation", "Wolf" 

Adults$Cause <- fct_collapse(Adults$Description, 
                             NonPredation = c ("Accident", "EHD", "Malnutrition", 
                                               "Car Strike", "Capture", "Non-Predation"), 
                             Censored = c ("Collar Failure", "Slipped Collar", "Unknown", "Shed", "Alive", ""))

levels(Adults$Cause)

Adults$Cause
########## RENAME AGE LEVELS BASED ON MATRIX MODELING
levels(Adults$Description)
Adults$Description <- as.factor(Adults$Description)





##### 0- 12 MONTHS TABLE "FAWNS"
SixMonthFate <- select( Adults, GMU, Year, Age.Class, Cause, days)%>%
  filter(Age.Class == "6 Month Fawn") %>%
  filter(days < 121) %>%
  group_by(Cause) %>%
  tally()



NeonateFate <- select( Adults, GMU, Year, Age.Class, Cause, days) %>%
  filter(Age.Class == "Neonate") %>%
  group_by(Cause) %>%
  tally()

FawnFate <- full_join(x=SixMonthFate, y=NeonateFate, by = "Cause")
FawnFate[is.na(FawnFate)] <- 0
FawnFate$Mortalities <- FawnFate$n.x + FawnFate$n.y


FawnFate <- FawnFate %>% select(Cause, Mortalities)


FawnFate<- FawnFate %>%  arrange(desc(Mortalities))

FawnFate




##### 12-24 Months
###############
#################
SixMonthFate365 <- select( Adults, GMU, Year, Age.Class, Cause, days) %>%
  filter(Age.Class == "6 Month Fawn") %>%
  filter(days >121) %>%
  group_by(Cause) %>%
  tally()

YearlingFate120 <- select( Adults, GMU, Year, Age.Class, Cause, days) %>%
  filter(Age.Class == "Yearling") %>%
  filter(days < 121) %>%
  group_by(Cause) %>%
  tally()

YearlingFate <- full_join(x=SixMonthFate365, y=YearlingFate120, by = "Cause")
YearlingFate[is.na(YearlingFate)] <- 0
YearlingFate$Mortalities <- YearlingFate$n.x + YearlingFate$n.y


YearlingFate <- YearlingFate %>% select(Cause, Mortalities)


YearlingFate <- YearlingFate %>%  arrange(desc(Mortalities)) 


YearlingFate

##### 24 + MONTHS
###############
#################


YearlingFate365 <- select( Adults, GMU, Year, Age.Class, Cause, days) %>%
  filter(Age.Class == "Yearling") %>%
  filter(days >120) %>%
  group_by(Cause) %>%
  tally()

AdultFates <- select( Adults, GMU, Year, Age.Class, Cause, days) %>%
  filter(Age.Class == "Adult") %>%
  group_by(Cause) %>%
  tally()


AdultFate <- full_join(x=YearlingFate365, y=AdultFates, by = "Cause")
AdultFate[is.na(AdultFate)] <- 0
AdultFate$Mortalities <- AdultFate$n.x + AdultFate$n.y


AdultFate <- AdultFate %>% select(Cause, Mortalities)


AdultFate <- AdultFate %>%  arrange(desc(Mortalities)) 

AdultFate


###########
### ALL THREE AGE CLASSES
###### JOIN ALL THREE AGE CLASSES INTO ONE TABLE WITH COUNTS AND PERCENT TOTALS OF AGE-CLASS
### SPECIFIC MORTALITY CAUSES

SummaryTable <- AdultFate %>% 
  full_join(YearlingFate, by = "Cause" ) %>%
  full_join(FawnFate, by = "Cause") %>%
  rename(Adults = "Mortalities.x",
         Yearlings = "Mortalities.y",
         Fawns = "Mortalities") %>%
  replace(is.na(.),0) 



SummaryTable <- SummaryTable %>%
  adorn_totals(c( "row", "col"),  fill = 'NA')

SummaryTable
write.csv(SummaryTable, here::here("output", "SummaryTable.csv"), row.names = TRUE)






