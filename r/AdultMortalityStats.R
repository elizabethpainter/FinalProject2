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
SixMonthFate <- select( Adults, GMU, Year, Age.Class, Cause, days) %>%
  filter(Cause != "Censored") %>%
  filter(Age.Class == "6 Month Fawn") %>%
  filter(days < 121) %>%
  group_by(Cause) %>%
  tally()



NeonateFate <- select( Adults, GMU, Year, Age.Class, Cause, days) %>%
  filter(Cause != "Censored") %>%
  filter(Age.Class == "Neonate") %>%
                    group_by(Cause) %>%
                    tally()

FawnFate <- full_join(x=SixMonthFate, y=NeonateFate, by = "Cause")
FawnFate[is.na(FawnFate)] <- 0
FawnFate$Mortalities <- FawnFate$n.x + FawnFate$n.y


FawnFate <- FawnFate %>% select(Cause, Mortalities)
  

FawnFate<- FawnFate %>%  arrange(desc(Mortalities)) %>% 
            mutate(Percent = round((Mortalities/sum(Mortalities))*100, 2)) %>%
            rename(    "Percent (%)" = "Percent")

FawnFate <- formattable(FawnFate,
                        align = c("l","c","r"),
                        list('Cause' = formatter( "span", style = ~ style(color = "black", font.weight = "bold"))))

FawnFate




##### 12-24 Months
###############
#################
SixMonthFate365 <- select( Adults, GMU, Year, Age.Class, Cause, days) %>%
  filter(Cause != "Censored") %>%
  filter(Age.Class == "6 Month Fawn") %>%
  filter(days >121) %>%
  group_by(Cause) %>%
  tally()

YearlingFate120 <- select( Adults, GMU, Year, Age.Class, Cause, days) %>%
  filter(Cause != "Censored") %>%
  filter(Age.Class == "Yearling") %>%
  filter(days < 121) %>%
  group_by(Cause) %>%
  tally()

  YearlingFate <- full_join(x=SixMonthFate365, y=YearlingFate120, by = "Cause")
  YearlingFate[is.na(YearlingFate)] <- 0
  YearlingFate$Mortalities <- YearlingFate$n.x + YearlingFate$n.y
  
  
  YearlingFate <- YearlingFate %>% select(Cause, Mortalities)
  
  
  YearlingFate <- YearlingFate %>%  arrange(desc(Mortalities)) %>% 
    mutate(Percent = round((Mortalities/sum(Mortalities))*100, 2)) %>%
    rename(    "Percent (%)" = "Percent")
  
  YearlingFate <- formattable(YearlingFate,
                          align = c("l","c","r"),
                          list('Cause' = formatter( "span", style = ~ style(color = "black", font.weight = "bold"))))
  
  YearlingFate
 
  ##### 24 + MONTHS
  ###############
  #################
  
  
  YearlingFate365 <- select( Adults, GMU, Year, Age.Class, Cause, days) %>%
    filter(Cause != "Censored") %>%
    filter(Age.Class == "Yearling") %>%
    filter(days >120) %>%
    group_by(Cause) %>%
    tally()
  
  AdultFates <- select( Adults, GMU, Year, Age.Class, Cause, days) %>%
    filter(Cause != "Censored") %>%
    filter(Age.Class == "Adult") %>%
    group_by(Cause) %>%
    tally()
  
  
  AdultFate <- full_join(x=YearlingFate365, y=AdultFates, by = "Cause")
  AdultFate[is.na(AdultFate)] <- 0
  AdultFate$Mortalities <- AdultFate$n.x + AdultFate$n.y
  
  
  AdultFate <- AdultFate %>% select(Cause, Mortalities)
  
  
  AdultFate <- AdultFate %>%  arrange(desc(Mortalities)) %>% 
    mutate(Percent = round((Mortalities/sum(Mortalities))*100, 2)) %>%
    rename(    "Percent (%)" = "Percent")
  
  AdultFate <- formattable(AdultFate,
                              align = c("l","c","r"),
                              list('Cause' = formatter( "span", style = ~ style(color = "black", font.weight = "bold"))))
  
  AdultFate
  
  
  ###########
  ### ALL THREE AGE CLASSES
  ###### JOIN ALL THREE AGE CLASSES INTO ONE TABLE WITH COUNTS AND PERCENT TOTALS OF AGE-CLASS
  ### SPECIFIC MORTALITY CAUSES
  
  MortalityTable <- AdultFate %>% 
    full_join(YearlingFate, by = "Cause" ) %>%
    full_join(FawnFate, by = "Cause") %>%
    rename("% of Adult \n Mortality" = "Percent (%).x",
           "% of Yearling \n Mortality" = "Percent (%).y",
           "% of Fawn \n Mortality" = "Percent (%)",
           "Adult Mortality" = "Mortalities.x",
           "Yearling Mortality" = "Mortalities.y",
           "Fawn Mortality" = "Mortalities") %>%
    replace(is.na(.),0) 

  
  MortalityTable <- formattable(MortalityTable,
                                align = c("l","c","c","c","c","c", "r"),
                                list('Cause' = formatter( "span", style = ~ style(color = "black", font.weight = "bold"))))
  
  MortalityTable <- MortalityTable %>%
    adorn_totals(c( "row", "col"),  fill = 'NA')
  
  write.csv(MortalityTable, here::here("output", "MortalityTable.csv"), row.names = TRUE)

levels(Adults$Cause)
 



MortalityPlot <- ggplot(MortalityTable, aes(x=Cause, y = "Adult Mortality"))




 
#######
#### FATE TABLE by Ages ######### ORIGINAL GROUPING ACCORDING TO COLLARING GUIDES
######


##### ADULT TABLE BY CAUSE, COUNT, PERCENT TOTAL ADULT MORTALITY
AdultFate <- dplyr::select( Adults, GMU, Year, Age.Class, Cause) %>%
  filter(Cause != "Censored") %>%
  filter(Cause != "") %>%
  filter(Cause !="Shed") %>%
  filter(Cause != "Unknown") %>%
  filter(Age.Class == "Adult") %>%
  
  #arrange(factor(Cause, levels = c( 'Bear', 'Mountain Lion', 'Bobcat',  'Coyote', 
  #  'Wolf', 'Unknown Predation',
  # 'Nonpredation', 'Alive'))) %>%
  group_by(Cause) %>%
  tally()

AdultFate<- AdultFate %>%  arrange(desc(n))
sum(AdultFate$n)
AdultFateTable <- AdultFate %>% 
  mutate(Percent = round((n/sum(n))*100, 2)) %>%
  rename("Adults" = "n")

formattable(AdultFateTable,
            align = c("l","c","r"),
            list('Cause' = formatter( "span", style = ~ style(color = "black", font.weight = "bold"))))



##### YEARLING TABLE BY CAUSE, COUNT, PERCENT TOTAL YEARLING MORTALITY

YearlingFate <- dplyr::select( Adults, GMU, Year, Age.Class, Cause) %>%
  filter(Cause != "Censored") %>%
  filter(Cause != "") %>%
  filter(Cause !="Shed") %>%
  filter(Cause != "Unknown") %>%
  filter(Age.Class == "Yearling") %>%
  #arrange(factor(Cause, levels = c( 'Bear', 'Mountain Lion', 'Bobcat',  'Coyote', 
                                    #  'Wolf', 'Unknown Predation',
                                     # 'Nonpredation', 'Alive'))) %>%
  group_by(Cause) %>%
  tally()

YearlingFate<- YearlingFate %>%  arrange(desc(n))
sum(YearlingFate$n)
YearlingFateTable <- YearlingFate %>% 
  mutate(Percent = round((n/sum(n))*100, 2)) %>%
  rename("Yearlings" = "n")

formattable(YearlingFateTable,
            align = c("l","c","r"),
            list('Cause' = formatter( "span", style = ~ style(color = "black", font.weight = "bold"))))



##### 6 MONTH TABLE BY CAUSE, COUNT, PERCENT TOTAL 6 MONTH MORTALITY
SixMonthFate <- dplyr::select( Adults, GMU, Year, Age.Class, Cause) %>%
  filter(Cause != "Censored") %>%
  filter(Cause != "") %>%
  filter(Cause !="Shed") %>%
  filter(Cause != "Unknown") %>%
  filter(Age.Class == "6 Month Fawn") %>%
  #arrange(factor(Cause, levels = c( 'Bear', 'Mountain Lion', 'Bobcat',  'Coyote', 
  #  'Wolf', 'Unknown Predation',
  # 'Nonpredation', 'Alive'))) %>%
  group_by(Cause) %>%
  tally()

SixMonthFate<- SixMonthFate %>%  arrange(desc(n))
sum(SixMonthFate$n)
SixMonthFateTable <- SixMonthFate %>% 
  mutate(Percent = round((n/sum(n))*100, 2)) %>%
  rename("6 Month Fawn" = "n")

formattable(SixMonthFateTable,
            align = c("l","c","r"),
            list('Cause' = formatter( "span", style = ~ style(color = "black", font.weight = "bold"))))


###### JOIN ALL THREE AGE CLASSES INTO ONE TABLE WITH COUNTS AND PERCENT TOTALS OF AGE-CLASS
### SPECIFIC MORTALITY CAUSES

MortalityTable <- AdultFateTable %>% 
                        left_join(YearlingFateTable, by = "Cause" ) %>%
                        left_join(SixMonthFateTable, by = "Cause") %>%
                        #add_row(Adults = sum(Adults), Yearlings = sum(Yearlings),"6 Month Fawn" = sum("6 Month Fawn")) %>%
                        rename("% of Adult \n Mortality" = "Percent.x",
                               "% of Yearling \n Mortality" = "Percent.y",
                               "% of 6 Mo. Fawns \n Mortality" = "Percent") %>%
                        adorn_totals("row")
                      
      


MortalityTable <- formattable(MortalityTable,
                              align = c("l","c","c","c","c","r"),
                              list('Cause' = formatter( "span", style = ~ style(
                                color = "black", font.weight = "bold"))))
                 
MortalityTable
###################

#######
#### FAWN FATES ######
#####
FawnFates <- Adults %>% 
            filter(Age.Class == "Neonate")
FawnFatePlot <- ggplot( FawnFates, aes(x=Cause, fill = Cause, color = "black"))+
  geom_bar()+
  geom_text(aes(label = ..count..),
            stat = "count",
            vjust = -0.2, colour = "black") +
  scale_x_discrete(limits = c( "Mountain Lion", "NonPredation", "Harvest", "Unknown Mortality",
                               "Unknown Predation", "Coyote", "Wolf", "Bear", "Bobcat")) +
  labs(title="Female White-Tailed Deer: All Age Classes", 
       subtitle="Mortalities from 2019-2021") +
  xlab("") +
  ylab("") +
  scale_color_viridis_d()+
  scale_fill_viridis_d() +
  theme(legend.position = "none",
        axis.text=element_text(size=14, face = "bold"),
        axis.text.x = element_text( angle = 45, hjust = 1),
        plot.title=element_text(size=20),
        plot.subtitle=element_text(size=17))

####### Adult FATES
######
AdultFatePlot <- ggplot(Adults, aes(x=Cause, fill = Cause, color = "black"))+
  geom_bar(stat = "count")+
  geom_text(aes(label = ..count..),
            stat = "count",
            vjust = -0.2, colour = "black") +
  scale_x_discrete(limits = c( "Mountain Lion", "NonPredation", "Harvest", "Unknown Mortality",
                              "Unknown Predation", "Coyote", "Wolf", "Bear", "Bobcat")) +
  labs(title="Female White-Tailed Deer: All Age Classes", 
       subtitle="Mortalities from 2019-2021") +
  xlab("") +
  ylab("") +
  scale_color_viridis_d()+
  scale_fill_viridis_d() +
  theme(legend.position = "none",
        axis.text=element_text(size=14, face = "bold"),
        axis.text.x = element_text( angle = 45, hjust = 1),
        plot.title=element_text(size=20),
        plot.subtitle=element_text(size=17))

#######
####### FATES BY AGES CLASSS
#######


AgeFatePlot <- ggplot(Adults, aes(x=Cause, fill = Age.Class))+
  geom_bar(stat = "count", color = "black", alpha = 0.9)+
  #geom_text(aes(label = ..count..),
            #stat = "count", position = position_stack(vjust = 0.5), 
            #colour = "black") +
  scale_x_discrete(limits = c( "Mountain Lion", "NonPredation", "Harvest", "Unknown Mortality",
                               "Unknown Predation", "Coyote", "Wolf", "Bear", "Bobcat")) +
  labs(title="Female White-Tailed Deer", 
       subtitle="Mortalities from 2019-2021") +
  xlab("") +
  ylab("") +
  scale_fill_brewer(palette = "Accent", 
                      name = "Age Class", 
                      limits = c("Neonate", "6 Month Fawn", "Yearling", "Adult"),
                      labels = c("Neonate", "6 Month Fawn", "Yearling", "Adult")) +
  scale_color_brewer(palette = "Accent") +
  theme(axis.text=element_text(size=16, face = "bold"),
        axis.text.x = element_text( angle = 45, hjust = 1),
        plot.title=element_text(size=24),
        plot.subtitle=element_text(size=18))
AgeFatePlot

#######
### FATES BY YEAR
########
AdultsFatesYear <- ggplot(Adults, aes(x= Cause, fill = Year))+
  geom_bar()+
  geom_text(aes(label = ..count..),
            stat = "count",
            position = position_stack(0.5)) +
  scale_x_discrete(limits = c( "Mountain Lion", "Nonpredation", "Harvest", "Unknown Mortality",
                               "Unknown Predation", "Coyote", 
                               "Wolf", "Black Bear")) +
  labs(title="Adult White-Tailed Deer", 
       subtitle="Known Fates from 2019-2021") +
  xlab("") +
  ylab("") +
  #scale_color_brewer(palette = "Dark 2") +
  scale_fill_brewer(palette =  "Accent") +
  theme_bw() +
  theme( legend.position = c(0.9, 0.5),
         axis.text=element_text(size=13),
         plot.title=element_text(size=20),
         plot.subtitle=element_text(size=17),
         legend.title = element_text(size=17),
         legend.text = element_text(size=16))
AdultsFatesYear




AdultFatePlot
#### SEX FATE
#########
AdultFatePlotSex <- ggplot(Adults, aes(x=Cause, fill = Sex))+
  geom_bar(stat = "count")+
  scale_x_discrete(limits = c( "Mountain Lion", "Nonpredation", "Harvest", "Unknown Mortality",
                               "Unknown Predation", "Coyote", "Wolf", "Black Bear")) +
  labs(title="Adult White-Tailed Deer", 
       subtitle="Known Fates from 2019-2021") +
  xlab("") +
  ylab("") +
  scale_fill_brewer (palette = "Pastel1", labels = c("Female", "Male")) +
  theme(legend.position = c(0.9, 0.5),
        legend.title = element_blank(),
        legend.text = element_text(size=16),
        axis.text=element_text(size=14, face = "bold"),
        axis.text.x = element_text( angle = 45, hjust = 1),
        plot.title=element_text(size=20),
        plot.subtitle=element_text(size=17))

AdultFatePlotSex

