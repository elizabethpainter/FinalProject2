

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

###
###
######NEONATES#####
####
####

###CURRENT DATA
Fawns <- read.csv(here::here("data","Fawns2019_2021.csv"))
## FORMAT DATE COLUMNS
Fawns$Capture_date <- as.Date(Fawns$Capture_date, "%m/%d/%y" )
Fawns$Partuition_date <- as.Date(Fawns$Partuition_date, "%m/%d/%y" )
Fawns$FateDate <- as.Date(Fawns$FateDate, "%m/%d/%y" )
Fawns$Partuition_Julian <- yday(Fawns$Partuition_date)
Fawns$FateDate_Julian <- yday(Fawns$FateDate)
str(Fawns)
Fawns$Year <- as.factor(Fawns$Year)
Fawns$GMU <- as.factor(Fawns$GMU)
### DAYS OF SURVIVAL

Fawns$days <- Fawns$FateDate_Julian - Fawns$Partuition_Julian

str(Fawns)
Fawns$days


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

#### CATS GROUPED
FawnFateCats <- dplyr::select( Fawns, GMU, Year, Cause_7) %>%
                  filter(Cause_7 != "Censored") %>%
                  group_by(Cause_7) %>%
                  tally() %>%
                  arrange(factor(Cause_7, levels = c( 'Bear', 'Cats', 'Coyote', 
                                                      'Wolf', 'Unknown Predation',
                                                      'Nonpredation', 'Alive'))) 
### CATS UPGROUPED                 
FawnFate <- dplyr::select( Fawns, GMU, Year, Cause_8) %>%
  filter(Cause_8 != "Censored",
         Cause_8 != "Alive") %>%
  arrange(factor(Cause_8, levels = c( 'Bear', 'Mountain Lion', 'Bobcat',  'Coyote', 
                                      'Wolf', 'Unknown Predation',
                                      'Nonpredation', 'Alive'))) %>%
  group_by(Cause_8) %>%
  tally()

FawnFate<- FawnFate %>%  arrange(desc(n)) %>% 
  mutate(Percent = round((n/sum(n))*100, 2)) %>%
  rename("Mortalities" = "n", "Percent (%)" = "Percent", "Cause" = "Cause_8")

FawnFate <- formattable(FawnFate,
            align = c("l","c","r"),
            list('Cause' = formatter( "span", style = ~ style(color = "black", font.weight = "bold"))))


  
### YEAR
FawnFatesYear <- ggplot(Fawns, aes(x=Cause_8, fill = Year))+
  geom_bar()+
  
  scale_x_discrete(limits = c( "Bear", "Mountain Lion", "Bobcat",  "Coyote", 
                               "Wolf", "Unknown Predation ",
                               "Nonpredation")) +
  labs(title="Neonate White-Tailed Deer", 
       subtitle="Known Fates from 2019-2021") +
  xlab("") +
  ylab("") +
  #scale_color_brewer(palette = "Dark 2") +
  scale_fill_brewer(palette =  "Dark2") +
  
  theme_bw() +
  theme( legend.position = c(0.9, 0.5),
        axis.text=element_text(size=15),
        plot.title=element_text(size=20),
        plot.subtitle=element_text(size=17),
        legend.title = element_text(size=17),
        legend.text = element_text(size=16))
FawnFatesYear



#######
####### FAWN FATES

FawnFates <- ggplot(Fawns, aes(x=Cause_8, fill = Cause_8, color = "black"))+
  geom_bar(stat = "count")+
  scale_x_discrete(limits = c( "Bear", "Mountain Lion", "Bobcat",  "Coyote", 
                                "Wolf", "Unknown Predation ",
                                "Nonpredation")) +
  labs(title="Neonate White-Tailed Deer", 
       subtitle="Known Fates from 2019-2021") +
  xlab("") +
  ylab("") +
  scale_color_viridis_d()+
  scale_fill_viridis_d() +
  theme(legend.position = "none") +
  theme(axis.text=element_text(size=15)) +
  theme(plot.title=element_text(size=20)) +
  theme(plot.subtitle=element_text(size=17))


FawnFates

