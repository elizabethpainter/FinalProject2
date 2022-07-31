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



############# READ IN AND CLEAN UP #####


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
Adults$Sex <- as.factor(Adults$Sex)



##WTD AND FEMALE SUBSET

Adults <- subset(Adults, Adults$Species == "WTD")
#Adults <- subset(Adults, Adults$Sex == "F")


## Levels: [1] """Nonpredation", "Black Bear", "Censored", "Coyote", 
##"Unknown mortality", "Unknown Mortality", "Unknown Predation", "Wolf" 

Adults$Cause <- fct_collapse(Adults$Description, 
                             NonPredation = c ("Accident", "EHD", "Malnutrition", 
                                               "Car Strike", "Capture", "Non-Predation"), 
                             Censored = c ("Collar Failure", "Slipped Collar", "Unknown", "Shed", "Alive", ""))

levels(Adults$Cause)

Adults$Cause
levels(Adults$Description)
Adults$Description <- as.factor(Adults$Description)




## Cause-Specific Mortality Table with 4 Age Classes##

##### NEONATES #####



NeonateFate <- select( Adults, GMU, Year, Age.Class, Cause) %>%
  filter(Cause != "Censored") %>%
  filter(Age.Class == "Neonate") %>%
  group_by(Cause) %>%
  tally()

NeonateFate[is.na(NeonateFate)] <- 0
NeonateFate$Mortalities <- NeonateFate$n


NeonateFate <- NeonateFate %>% select(Cause, Mortalities)


NeonateFate<- NeonateFate %>%  arrange(desc(Mortalities)) %>% 
  mutate(Percent = round((Mortalities/sum(Mortalities))*100, 2)) %>%
  rename(    "Percent (%)" = "Percent")

NeonateFate <- formattable(NeonateFate,
                        align = c("l","c","r"),
                        list('Cause' = formatter( "span", style = ~ style(color = "black", font.weight = "bold"))))

NeonateFate








################# SIX MONTHS ######
SixMonthFate <- select( Adults, GMU, Year, Age.Class, Cause) %>%
  filter(Cause != "Censored") %>%
  filter(Age.Class == "6 Month Fawn") %>%
  group_by(Cause) %>%
  tally()


SixMonthFate [is.na(SixMonthFate)] <- 0
SixMonthFate$Mortalities <- SixMonthFate$n


SixMonthFate <- SixMonthFate %>% select(Cause, Mortalities)


SixMonthFate <- SixMonthFate %>%  arrange(desc(Mortalities)) %>% 
  mutate(Percent = round((Mortalities/sum(Mortalities))*100, 2)) %>%
  rename(    "Percent (%)" = "Percent")

SixMonthFate <- formattable(SixMonthFate,
                            align = c("l","c","r"),
                            list('Cause' = formatter( "span", style = ~ style(color = "black", font.weight = "bold"))))

SixMonthFate







#################Yearling ####
####

YearlingFate <- select( Adults, GMU, Year, Age.Class, Cause) %>%
  filter(Cause != "Censored") %>%
  filter(Age.Class == "Yearling") %>%
  group_by(Cause) %>%
  tally()


YearlingFate[is.na(YearlingFate)] <- 0
YearlingFate$Mortalities <- YearlingFate$n


YearlingFate <- YearlingFate %>% select(Cause, Mortalities)


YearlingFate <- YearlingFate %>%  arrange(desc(Mortalities)) %>% 
  mutate(Percent = round((Mortalities/sum(Mortalities))*100, 2)) %>%
  rename(    "Percent (%)" = "Percent")

YearlingFate <- formattable(YearlingFate,
                         align = c("l","c","r"),
                         list('Cause' = formatter( "span", style = ~ style(color = "black", font.weight = "bold"))))

YearlingFate



####ADULTS#######
#### ADULTS Adults



AdultFate <- select( Adults, GMU, Year, Age.Class, Cause) %>%
  filter(Cause != "Censored") %>%
  filter(Age.Class == "Adult") %>%
  group_by(Cause) %>%
  tally()


AdultFate[is.na(AdultFate)] <- 0
AdultFate$Mortalities <- AdultFate$n


AdultFate <- AdultFate %>% select(Cause, Mortalities)


AdultFate <- AdultFate %>%  arrange(desc(Mortalities)) %>% 
  mutate(Percent = round((Mortalities/sum(Mortalities))*100, 2)) %>%
  rename(    "Percent (%)" = "Percent")

AdultFate <- formattable(AdultFate,
                         align = c("l","c","r"),
                         list('Cause' = formatter( "span", style = ~ style(color = "black", font.weight = "bold"))))

AdultFate

### ALL FOUR AGE CLASSES####
###### JOIN ALL FOUR AGE CLASSES INTO ONE TABLE WITH COUNTS AND PERCENT TOTALS OF AGE-CLASS
### SPECIFIC MORTALITY CAUSES

MortalityTable <- AdultFate %>% 
  full_join(YearlingFate, by = "Cause" ) %>%
  full_join(SixMonthFate, by = "Cause" ) %>%
  full_join(NeonateFate, by = "Cause") %>%
  rename("% of Adult \n Mortality" = "Percent (%).x",
         "% of Yearling \n Mortality" = "Percent (%).y",
         "% of Six-Month \n Mortality" = "Percent (%).x.x",
         "% of Neonate \n Mortality" = "Percent (%).y.y",
         "Adult Mortality" = "Mortalities.x",
         "Yearling Mortality" = "Mortalities.y",
         "Six-Month Mortality" = "Mortalities.x.x" ,
        "Neonate Mortality" = "Mortalities.y.y") %>%
  replace(is.na(.),0) 


MortalityTable <- formattable(MortalityTable,
                              align = c("l","c","c","c","c","c","c","c", "r"),
                              list('Cause' = formatter( "span", style = ~ style(color = "black", font.weight = "bold"))))

MortalityTable <- MortalityTable %>%
  adorn_totals(c( "row", "col"),  fill = 'NA')

write.csv(MortalityTable, here::here("output", "MortalityTable.csv"), row.names = TRUE)

levels(Adults$Cause)









#### FATE TABLE by Ages  ORIGINAL GROUPING ACCORDING TO COLLARING GUIDES NO NEONATES#########

##### ADULT TABLE BY CAUSE, COUNT, PERCENT TOTAL ADULT MORTALITY ######
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



##### YEARLING TABLE BY CAUSE, COUNT, PERCENT TOTAL YEARLING MORTALITY ####

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



##### 6 MONTH TABLE BY CAUSE, COUNT, PERCENT TOTAL 6 MONTH MORTALITY ####
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


###### JOIN ALL THREE AGE CLASSES INTO ONE TABLE WITH COUNTS AND PERCENT TOTALS OF AGE-CLASS SPECIFIC MORTALITY CAUSES ####
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







####### Data Clean UP #######

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


Adults <- subset(Adults, Adults$Species == "WTD")
#Adults <- subset(Adults, Adults$Sex == "F")


Adults$Cause <- fct_collapse(Adults$Description, 
                             NonPredation = c ("Accident", "EHD", "Malnutrition", 
                                               "Car Strike", "Capture", "Non-Predation"), 
                             Censored = c ("Collar Failure", "Slipped Collar", "Unknown", "Shed", "Alive", ""))

levels(Adults$Cause)

Adults$Cause
levels(Adults$Description)
Adults$Description <- as.factor(Adults$Description) 

###Data Clean 



#### FAWN FATES Plots ######


FawnFates <- Adults %>% 
  filter(Age.Class == "Neonate")
FawnFatePlot <- ggplot( FawnFates, aes(x=Cause, fill = Cause, color = "black"))+
  geom_bar()+
  geom_text(aes(label = ..count..),
            stat = "count",
            vjust = -0.2, colour = "black") +
  scale_x_discrete(limits = c( "Mountain Lion", "NonPredation", "Harvest", "Unknown Mortality",
                               "Unknown Predation", "Coyote", "Wolf", "Bear", "Bobcat")) +
  labs(title="Neonate White-Tailed Deer", 
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
FawnFatePlot




####### Adult FATES Plot ####



AdultFates <- Adults %>% 
  filter(Age.Class == "Adult")

AdultFatePlot <- ggplot(AdultFates, aes(x=Cause, fill = Cause, color = "black"))+
  geom_bar(stat = "count")+
  geom_text(aes(label = ..count..),
            stat = "count",
            vjust = -0.2, colour = "black") +
  scale_x_discrete(limits = c( "Mountain Lion", "NonPredation", "Harvest", "Unknown Mortality",
                               "Unknown Predation", "Coyote", "Wolf", "Bear")) +
  labs(title="Female White-Tailed Deer: Adults (2+)", 
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
  
AdultFatePlot





####### Six Month FATES Plot ####



AdultFates <- Adults %>% 
  filter(Age.Class == "6 Month Fawn")

AdultFatePlot <- ggplot(AdultFates, aes(x=Cause, fill = Cause, color = "black"))+
  geom_bar(stat = "count")+
  geom_text(aes(label = ..count..),
            stat = "count",
            vjust = -0.2, colour = "black") +
 # scale_x_discrete(limits = c( "Mountain Lion", "NonPredation", "Harvest", "Unknown Mortality",
                              # "Unknown Predation", "Coyote")) +
  labs(title="White-Tailed Deer: Six Month Fawns", 
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

AdultFatePlot



#######  PLOTS FATES BY AGES CLASSS ####

AgeFatePlot <- ggplot(Adults, aes(x=Cause, fill = Age.Class))+
  geom_bar(stat = "count", color = "black", alpha = 0.9)+
  #geom_text(aes(label = ..count..),
  #stat = "count", position = position_stack(vjust = 0.5), 
  #colour = "black") +
  scale_x_discrete(limits = c( "Mountain Lion", "NonPredation", "Harvest", "Unknown Mortality",
                               "Unknown Predation", "Coyote", "Wolf", "Bear", "Bobcat")) +
  labs(title="White-Tailed Deer", 
       subtitle="Mortalities from 2019-2021") +
  xlab("") +
  ylab("") +
  scale_fill_brewer(palette = "Accent", 
                    name = "Age Class", 
                    limits = c("Neonate", "6 Month Fawn", "Yearling", "Adult"),
                    labels = c("Neonate", "6 Month Fawn", "Yearling", "Adult")) +
  scale_color_brewer(palette = "Accent") +
  theme(axis.text=element_text(size=14, face = "bold"),
        axis.text.x = element_text( angle = 45, hjust = 1),
        plot.title=element_text(size=24),
        plot.subtitle=element_text(size=18),
        legend.position = c(0.9, 0.7),
        legend.title = element_text(size=17),
        legend.text = element_text(size=16))
AgeFatePlot







### FATES BY YEAR ####

AdultFates <- Adults %>% 
  filter(Age.Class == "Adult", Adults$Sex == "F")

AdultsFatesYear <- ggplot(AdultFates, aes(x= Cause, fill = Year))+
  geom_bar()+
  geom_text(aes(label = ..count..),
            stat = "count",
            position = position_stack(0.5)) +
  scale_x_discrete(limits = c( "Mountain Lion", "NonPredation", "Harvest", "Unknown Mortality",
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





#### SEX FATE ####
AdultFates <- Adults %>% 
  filter(Age.Class == "Adult")

AdultFatePlotSex <- ggplot(AdultFates, aes(x=Cause, fill = Sex))+
  geom_bar(stat = "count")+
  scale_x_discrete(limits = c( "Mountain Lion", "NonPredation", "Harvest", "Unknown Mortality",
                               "Unknown Predation", "Coyote", "Wolf", "Bear")) +
  labs(title="Adult White-Tailed Deer", 
       subtitle="Known Fates from 2019-2021") +
  xlab("") +
  ylab("") +
  scale_fill_brewer (palette = "Pastel1", labels = c("Female", "Male")) +
  theme(legend.position = c(0.9, 0.7),
        legend.title = element_blank(),
        legend.text = element_text(size=16),
        axis.text=element_text(size=14, face = "bold"),
        axis.text.x = element_text( angle = 45, hjust = 1),
        plot.title=element_text(size=20),
        plot.subtitle=element_text(size=17))

AdultFatePlotSex









