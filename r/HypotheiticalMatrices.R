###### Hypothetical Matrices


##########LION#######
######## LION 25% reduction #### 
##### SURVIVAL RATES 
MortalityTable[1,7]
MortalityTable[4,7]
sf <- FawnSurvival + (1- FawnSurvival) * 0.2292 * 0.25 + (1- FawnSurvival) * 0.2292 * 0.5 * 0.25
#### Original Survival + (1-S)*(MortalityTable[1,7] *1/100) * % reduction)
MortalityTable[1,5]
MortalityTable[4,5]
sy <- YearlingSurvival + (1 - YearlingSurvival) * 0.1667 * 0.25 +(1- YearlingSurvival) * 0.3333 * 0.5 * 0.25
#### Original Survival + (1-S)*(MortalityTable[1,7] *1/100) * % reduction)
MortalityTable[1,3]
MortalityTable[4,3]
sa <- AdultSurvivalS + (1- AdultSurvivalS) * 0.3148*0.25 + (1- AdultSurvivalS) *0.1111 * 0.5 * 0.25
#### Original Survival + (1-S)*(MortalityTable[1,7] *1/100) * % reduction)

######## MATRIX CONSTRUCTION ######


#### LOW RATES MEANS - 1.96* SE

sfLow <- sf - 1.96*FawnSurvivalSE 
syLow <- sy - 1.96* YearlingSurvivalSE
saLow <- sa - 1.96 * AdultsSurvivalSE

##### HIGH RATES MEANS + 1.96* SE
sfHigh <- sf + 1.96 *FawnSurvivalSE
syHigh <- sy + 1.96 * YearlingSurvivalSE
saHigh <- sa + 1.96 * AdultsSurvivalSE

##### BIRTHS PER FEMALE
### YEARLING
by <- 1.3

### ADULTS
ba <- 1.7


### FECUNDITITY PRODUCTION
#### PROBABILITY OF PREGNANCY
pa <- 183/212
py <- 15/27
### SURVIVAL OF PODUCING CLASS * PERCENT PREGNANT * YOUNG PRODUCED/2 (FEMALE ONLY MODEL)
ff <- 0
fy <- sy * (by/2) * py
fa<- sa * (ba/2) * pa

##### HIGH RATES VARY BY HIGHER YOUNG PRODUCED AND HIGHER SURVIVAL
ff <- 0
fyHigh <- syHigh * (by/2) * py
faHigh<- saHigh * (ba/2) * pa

##### LOWER RATES VARY BY FEWER YOUNG PRODUCED AND LOWER SURVIVAL
ff <- 0
fyLow <- syLow * (by/2) * py
faLow<- saLow * (ba/2) * pa


####### VITAL RATE MATRICES

VitalMedium <- c( ff, fy, fa, 
                  sf,  0,  0,
                  0,  sy, sa )

VitalHigh <- c(ff, fyHigh, faHigh, 
               sfHigh, 0, 0, 
               0, syHigh, saHigh)

VitalLow <- c(ff, fyLow, faLow, 
              sfLow, 0, 0,
              0, syLow, saLow)




MLow = matrix(VitalLow, 
              nrow=3, ncol=3, 
              byrow = (TRUE),
              dimnames =list(c( "Fawn", "Yearling", "Adult")) )
MHigh = matrix(VitalHigh, 
               nrow=3, ncol=3, 
               byrow = (TRUE),
               dimnames =list(c( "Fawn", "Yearling", "Adult")) )
MMedium =matrix(VitalMedium, 
                nrow=3, ncol=3, 
                byrow = (TRUE),
                dimnames =list(c( "Fawn", "Yearling", "Adult")))

MMedium


Lion25Medium <- MMedium
Lion25Low <- MLow
Lion25High <- MHigh
Lion25Reduction <- as.array(Lion25Medium,Lion25Low, Lion25High)
Lion25Eigen <- eigen.analysis(Lion25Medium)

ColumnLabels <- c( "Low", "Medium", "High")
RowLabels <- c("Fawn (S)", "Yearling (S)", "Adult (S)")


AdultS <- c(saLow, sa, saHigh)
FawnS <- c(sfLow, sf, sfHigh)
YearlingS <- c( syLow, sy, syHigh)

Lion25VitalRates <- matrix(c(FawnS, YearlingS, AdultS),
                           nrow = length(RowLabels), ncol = length(ColumnLabels), byrow = TRUE,
                           dimnames = list(RowLabels, ColumnLabels))
write.csv(Lion25VitalRates, here::here("output", "Lion25VitalRates.csv"), row.names = TRUE)


############################################


####### LION 50% Reduction ########
##### SURVIVAL RATES 
MortalityTable[1,7]
sf <- FawnSurvival + (1- FawnSurvival) * 0.2292 * 0.5 + (1- FawnSurvival) * 0.2292 * 0.5 * 0.5

MortalityTable[1,5]
sy <- YearlingSurvival + (1 - YearlingSurvival) * 0.1667 * 0.5 + (1- YearlingSurvival) * 0.3333 * 0.5 * 0.5

MortalityTable[1,3]
sa <- AdultSurvivalS + (1- AdultSurvivalS) * 0.3148 * 0.5 + (1- AdultSurvivalS) *0.1111 * 0.5 * 0.5

######## MATRIX CONSTRUCTION ######


#### LOW RATES MEANS - 1.96* SE

sfLow <- sf - 1.96*FawnSurvivalSE 
syLow <- sy - 1.96* YearlingSurvivalSE
saLow <- sa - 1.96 * AdultsSurvivalSE

##### HIGH RATES MEANS + 1.96* SE
sfHigh <- sf + 1.96 *FawnSurvivalSE
syHigh <- sy + 1.96 * YearlingSurvivalSE
saHigh <- sa + 1.96 * AdultsSurvivalSE

##### BIRTHS PER FEMALE
### YEARLING
by <- 1.3

### ADULTS
ba <- 1.7


### FECUNDITITY PRODUCTION
#### PROBABILITY OF PREGNANCY
pa <- 183/212
py <- 15/27
### SURVIVAL OF PODUCING CLASS * PERCENT PREGNANT * YOUNG PRODUCED/2 (FEMALE ONLY MODEL)
ff <- 0
fy <- sy * (by/2) * py
fa<- sa * (ba/2) * pa

##### HIGH RATES VARY BY HIGHER YOUNG PRODUCED AND HIGHER SURVIVAL
ff <- 0
fyHigh <- syHigh * (by/2) * py
faHigh<- saHigh * (ba/2) * pa

##### LOWER RATES VARY BY FEWER YOUNG PRODUCED AND LOWER SURVIVAL
ff <- 0
fyLow <- syLow * (by/2) * py
faLow<- saLow * (ba/2) * pa


####### VITAL RATE MATRICES

VitalMedium <- c( ff, fy, fa, 
                  sf,  0,  0,
                  0,  sy, sa )

VitalHigh <- c(ff, fyHigh, faHigh, 
               sfHigh, 0, 0, 
               0, syHigh, saHigh)

VitalLow <- c(ff, fyLow, faLow, 
              sfLow, 0, 0,
              0, syLow, saLow)




MLow = matrix(VitalLow, 
              nrow=3, ncol=3, 
              byrow = (TRUE),
              dimnames =list(c( "Fawn", "Yearling", "Adult")) )
MHigh = matrix(VitalHigh, 
               nrow=3, ncol=3, 
               byrow = (TRUE),
               dimnames =list(c( "Fawn", "Yearling", "Adult")) )
MMedium =matrix(VitalMedium, 
                nrow=3, ncol=3, 
                byrow = (TRUE),
                dimnames =list(c( "Fawn", "Yearling", "Adult")))

MMedium


Lion50Medium <- MMedium
Lion50Low <- MLow
Lion50High <- MHigh
Lion50Reduction <- as.array(Lion50Medium,Lion50Low, Lion50High)
Lion50Eigen <- eigen.analysis(Lion50Medium)

ColumnLabels <- c( "Low", "Medium", "High")
RowLabels <- c("Fawn (S)", "Yearling (S)", "Adult (S)")


AdultS <- c(saLow, sa, saHigh)
FawnS <- c(sfLow, sf, sfHigh)
YearlingS <- c( syLow, sy, syHigh)

Lion50VitalRates <- matrix(c(FawnS, YearlingS, AdultS),
                     nrow = length(RowLabels), ncol = length(ColumnLabels), byrow = TRUE,
                     dimnames = list(RowLabels, ColumnLabels))
write.csv(Lion50VitalRates, here::here("output", "Lion50VitalRates.csv"), row.names = TRUE)









###### BEAR #########
 ###### 25%  #####
MortalityTable[8,7]
sf <- FawnSurvival + (1- FawnSurvival) * 0.1875 * 0.25
sy <- YearlingSurvival
sa <- AdultSurvivalS


######## MATRIX CONSTRUCTION ######


#### LOW RATES MEANS - 1.96* SE

sfLow <- sf - 1.96*FawnSurvivalSE 
syLow <- sy - 1.96* YearlingSurvivalSE
saLow <- sa - 1.96 * AdultsSurvivalSE

##### HIGH RATES MEANS + 1.96* SE
sfHigh <- sf + 1.96 *FawnSurvivalSE
syHigh <- sy + 1.96 * YearlingSurvivalSE
saHigh <- sa + 1.96 * AdultsSurvivalSE

##### BIRTHS PER FEMALE
### YEARLING
by <- 1.3

### ADULTS
ba <- 1.7


### FECUNDITITY PRODUCTION
#### PROBABILITY OF PREGNANCY
pa <- 183/212
py <- 15/27
### SURVIVAL OF PODUCING CLASS * PERCENT PREGNANT * YOUNG PRODUCED/2 (FEMALE ONLY MODEL)
ff <- 0
fy <- sy * (by/2) * py
fa<- sa * (ba/2) * pa

##### HIGH RATES VARY BY HIGHER YOUNG PRODUCED AND HIGHER SURVIVAL
ff <- 0
fyHigh <- syHigh * (by/2) * py
faHigh<- saHigh * (ba/2) * pa

##### LOWER RATES VARY BY FEWER YOUNG PRODUCED AND LOWER SURVIVAL
ff <- 0
fyLow <- syLow * (by/2) * py
faLow<- saLow * (ba/2) * pa


####### VITAL RATE MATRICES

VitalMedium <- c( ff, fy, fa, 
                  sf,  0,  0,
                  0,  sy, sa )

VitalHigh <- c(ff, fyHigh, faHigh, 
               sfHigh, 0, 0, 
               0, syHigh, saHigh)

VitalLow <- c(ff, fyLow, faLow, 
              sfLow, 0, 0,
              0, syLow, saLow)




MLow = matrix(VitalLow, 
              nrow=3, ncol=3, 
              byrow = (TRUE),
              dimnames =list(c( "Fawn", "Yearling", "Adult")) )
MHigh = matrix(VitalHigh, 
               nrow=3, ncol=3, 
               byrow = (TRUE),
               dimnames =list(c( "Fawn", "Yearling", "Adult")) )
MMedium =matrix(VitalMedium, 
                nrow=3, ncol=3, 
                byrow = (TRUE),
                dimnames =list(c( "Fawn", "Yearling", "Adult")))

MMedium


Bear25Medium <- MMedium
Bear25Low <- MLow
Bear25High <- MHigh
Bear25Reduction <- as.array(Bear25Medium,Bear25Low, Bear25High)
Bear25Eigen <- eigen.analysis(Bear25Medium)


ColumnLabels <- c( "Low", "Medium", "High")
RowLabels <- c("Fawn (S)", "Yearling (S)", "Adult (S)")


AdultS <- c(saLow, sa, saHigh)
FawnS <- c(sfLow, sf, sfHigh)
YearlingS <- c( syLow, sy, syHigh)

Bear25VitalRates <- matrix(c(FawnS, YearlingS, AdultS),
                           nrow = length(RowLabels), ncol = length(ColumnLabels), byrow = TRUE,
                           dimnames = list(RowLabels, ColumnLabels))
write.csv(Bear25VitalRates, here::here("output", "Bear25VitalRates.csv"), row.names = TRUE)




##### 50% ######
MortalityTable[8,7]
sf <- FawnSurvival + (1- FawnSurvival) * 0.1875 * 0.5
sy <- YearlingSurvival
sa <- AdultSurvivalS

######## MATRIX CONSTRUCTION ######


#### LOW RATES MEANS - 1.96* SE

sfLow <- sf - 1.96*FawnSurvivalSE 
syLow <- sy - 1.96* YearlingSurvivalSE
saLow <- sa - 1.96 * AdultsSurvivalSE

##### HIGH RATES MEANS + 1.96* SE
sfHigh <- sf + 1.96 *FawnSurvivalSE
syHigh <- sy + 1.96 * YearlingSurvivalSE
saHigh <- sa + 1.96 * AdultsSurvivalSE

##### BIRTHS PER FEMALE
### YEARLING
by <- 1.3

### ADULTS
ba <- 1.7


### FECUNDITITY PRODUCTION
#### PROBABILITY OF PREGNANCY
pa <- 183/212
py <- 15/27
### SURVIVAL OF PODUCING CLASS * PERCENT PREGNANT * YOUNG PRODUCED/2 (FEMALE ONLY MODEL)
ff <- 0
fy <- sy * (by/2) * py
fa<- sa * (ba/2) * pa

##### HIGH RATES VARY BY HIGHER YOUNG PRODUCED AND HIGHER SURVIVAL
ff <- 0
fyHigh <- syHigh * (by/2) * py
faHigh<- saHigh * (ba/2) * pa

##### LOWER RATES VARY BY FEWER YOUNG PRODUCED AND LOWER SURVIVAL
ff <- 0
fyLow <- syLow * (by/2) * py
faLow<- saLow * (ba/2) * pa


####### VITAL RATE MATRICES

VitalMedium <- c( ff, fy, fa, 
                  sf,  0,  0,
                  0,  sy, sa )

VitalHigh <- c(ff, fyHigh, faHigh, 
               sfHigh, 0, 0, 
               0, syHigh, saHigh)

VitalLow <- c(ff, fyLow, faLow, 
              sfLow, 0, 0,
              0, syLow, saLow)




MLow = matrix(VitalLow, 
              nrow=3, ncol=3, 
              byrow = (TRUE),
              dimnames =list(c( "Fawn", "Yearling", "Adult")) )
MHigh = matrix(VitalHigh, 
               nrow=3, ncol=3, 
               byrow = (TRUE),
               dimnames =list(c( "Fawn", "Yearling", "Adult")) )
MMedium =matrix(VitalMedium, 
                nrow=3, ncol=3, 
                byrow = (TRUE),
                dimnames =list(c( "Fawn", "Yearling", "Adult")))

MMedium


Bear50Medium <- MMedium
Bear50Low <- MLow
Bear50High <- MHigh
Bear50Reduction <- as.array(Bear50Medium,Bear50Low, Bear50High)
Bear50Eigen <- eigen.analysis(Bear50Medium)



ColumnLabels <- c( "Low", "Medium", "High")
RowLabels <- c("Fawn (S)", "Yearling (S)", "Adult (S)")


AdultS <- c(saLow, sa, saHigh)
FawnS <- c(sfLow, sf, sfHigh)
YearlingS <- c( syLow, sy, syHigh)

Bear50VitalRates <- matrix(c(FawnS, YearlingS, AdultS),
                           nrow = length(RowLabels), ncol = length(ColumnLabels), byrow = TRUE,
                           dimnames = list(RowLabels, ColumnLabels))
write.csv(Bear50VitalRates, here::here("output", "Bear50VitalRates.csv"), row.names = TRUE)




##### ANTLERLESS HARVEST ####
#### 50% #####
MortalityTable[3,7]
sf <- FawnSurvival + (1- FawnSurvival) * 0 * 0.5

MortalityTable[3,5]
sy <- YearlingSurvival + (1 - YearlingSurvival) * 0.5 * 0.5

MortalityTable[3,3]
sa <- AdultSurvivalS + (1 - AdultSurvivalS) * 0.1111 * 0.5




######## MATRIX CONSTRUCTION ######


#### LOW RATES MEANS - 1.96* SE

sfLow <- sf - 1.96*FawnSurvivalSE 
syLow <- sy - 1.96* YearlingSurvivalSE
saLow <- sa - 1.96 * AdultsSurvivalSE

##### HIGH RATES MEANS + 1.96* SE
sfHigh <- sf + 1.96 *FawnSurvivalSE
syHigh <- sy + 1.96 * YearlingSurvivalSE
saHigh <- sa + 1.96 * AdultsSurvivalSE

##### BIRTHS PER FEMALE
### YEARLING
by <- 1.3

### ADULTS
ba <- 1.7


### FECUNDITITY PRODUCTION
#### PROBABILITY OF PREGNANCY
pa <- 183/212
py <- 15/27
### SURVIVAL OF PODUCING CLASS * PERCENT PREGNANT * YOUNG PRODUCED/2 (FEMALE ONLY MODEL)
ff <- 0
fy <- sy * (by/2) * py
fa<- sa * (ba/2) * pa

##### HIGH RATES VARY BY HIGHER YOUNG PRODUCED AND HIGHER SURVIVAL
ff <- 0
fyHigh <- syHigh * (by/2) * py
faHigh<- saHigh * (ba/2) * pa

##### LOWER RATES VARY BY FEWER YOUNG PRODUCED AND LOWER SURVIVAL
ff <- 0
fyLow <- syLow * (by/2) * py
faLow<- saLow * (ba/2) * pa


####### VITAL RATE MATRICES

VitalMedium <- c( ff, fy, fa, 
                  sf,  0,  0,
                  0,  sy, sa )

VitalHigh <- c(ff, fyHigh, faHigh, 
               sfHigh, 0, 0, 
               0, syHigh, saHigh)

VitalLow <- c(ff, fyLow, faLow, 
              sfLow, 0, 0,
              0, syLow, saLow)




MLow = matrix(VitalLow, 
              nrow=3, ncol=3, 
              byrow = (TRUE),
              dimnames =list(c( "Fawn", "Yearling", "Adult")) )
MHigh = matrix(VitalHigh, 
               nrow=3, ncol=3, 
               byrow = (TRUE),
               dimnames =list(c( "Fawn", "Yearling", "Adult")) )
MMedium =matrix(VitalMedium, 
                nrow=3, ncol=3, 
                byrow = (TRUE),
                dimnames =list(c( "Fawn", "Yearling", "Adult")))

MMedium


Antler50Medium <- MMedium
Antler50Low <- MLow
Antler50High <- MHigh
Antler50Reduction <- as.array(Antler50Medium,Antler50Low, Antler50High)
Antler50Eigen <- eigen.analysis(Antler50Medium)

ColumnLabels <- c( "Low", "Medium", "High")
RowLabels <- c("Fawn (S)", "Yearling (S)", "Adult (S)")


AdultS <- c(saLow, sa, saHigh)
FawnS <- c(sfLow, sf, sfHigh)
YearlingS <- c( syLow, sy, syHigh)

Harvest50Rates <- matrix(c(FawnS, YearlingS, AdultS),
                           nrow = length(RowLabels), ncol = length(ColumnLabels), byrow = TRUE,
                           dimnames = list(RowLabels, ColumnLabels))
write.csv(Harvest50Rates, here::here("output", "Harvest50Rates.csv"), row.names = TRUE)





##### 100% ######
MortalityTable[3,7]
sf <- FawnSurvival + (1- FawnSurvival) * 0  

MortalityTable[3,5]
sy <- YearlingSurvival + (1 - YearlingSurvival) * 0.5 

MortalityTable[3,3]
sa <- AdultSurvivalS + (1 - AdultSurvivalS) *  0.1111 

######## MATRIX CONSTRUCTION ######


#### LOW RATES MEANS - 1.96* SE

sfLow <- sf - 1.96*FawnSurvivalSE 
syLow <- sy - 1.96* YearlingSurvivalSE
saLow <- sa - 1.96 * AdultsSurvivalSE

##### HIGH RATES MEANS + 1.96* SE
sfHigh <- sf + 1.96 *FawnSurvivalSE
syHigh <- sy + 1.96 * YearlingSurvivalSE
saHigh <- sa + 1.96 * AdultsSurvivalSE

##### BIRTHS PER FEMALE
### YEARLING
by <- 1.3

### ADULTS
ba <- 1.7


### FECUNDITITY PRODUCTION
#### PROBABILITY OF PREGNANCY
pa <- 183/212
py <- 15/27
### SURVIVAL OF PODUCING CLASS * PERCENT PREGNANT * YOUNG PRODUCED/2 (FEMALE ONLY MODEL)
ff <- 0
fy <- sy * (by/2) * py
fa<- sa * (ba/2) * pa

##### HIGH RATES VARY BY HIGHER YOUNG PRODUCED AND HIGHER SURVIVAL
ff <- 0
fyHigh <- syHigh * (by/2) * py
faHigh<- saHigh * (ba/2) * pa

##### LOWER RATES VARY BY FEWER YOUNG PRODUCED AND LOWER SURVIVAL
ff <- 0
fyLow <- syLow * (by/2) * py
faLow<- saLow * (ba/2) * pa


####### VITAL RATE MATRICES

VitalMedium <- c( ff, fy, fa, 
                  sf,  0,  0,
                  0,  sy, sa )

VitalHigh <- c(ff, fyHigh, faHigh, 
               sfHigh, 0, 0, 
               0, syHigh, saHigh)

VitalLow <- c(ff, fyLow, faLow, 
              sfLow, 0, 0,
              0, syLow, saLow)




MLow = matrix(VitalLow, 
              nrow=3, ncol=3, 
              byrow = (TRUE),
              dimnames =list(c( "Fawn", "Yearling", "Adult")) )
MHigh = matrix(VitalHigh, 
               nrow=3, ncol=3, 
               byrow = (TRUE),
               dimnames =list(c( "Fawn", "Yearling", "Adult")) )
MMedium =matrix(VitalMedium, 
                nrow=3, ncol=3, 
                byrow = (TRUE),
                dimnames =list(c( "Fawn", "Yearling", "Adult")))

MMedium


Antler100Medium <- MMedium
Antler100Low <- MLow
Antler100High <- MHigh
Antler100Reduction <- as.array(Antler100Medium,Antler100Low, Antler100High)
Antler100Eigen <- eigen.analysis(Antler100Medium)


ColumnLabels <- c( "Low", "Medium", "High")
RowLabels <- c("Fawn (S)", "Yearling (S)", "Adult (S)")


AdultS <- c(saLow, sa, saHigh)
FawnS <- c(sfLow, sf, sfHigh)
YearlingS <- c( syLow, sy, syHigh)

Harvest100Rates <- matrix(c(FawnS, YearlingS, AdultS),
                              nrow = length(RowLabels), ncol = length(ColumnLabels), byrow = TRUE,
                              dimnames = list(RowLabels, ColumnLabels))
write.csv(Harvest100Rates, here::here("output", "Harvest100Rates.csv"), row.names = TRUE)





#### Conservative Approach #### 

###50% Reduction Harvest, 25% Reduction Bear, 25% in Lion####
sf <- FawnSurvival + (1- FawnSurvival)*(0.2292 * 0.25) + (1- FawnSurvival)*(0.1875 * 0.25) + (1- FawnSurvival) * 0.2292 * 0.5 * 0.25

sy <- YearlingSurvival + (1 - YearlingSurvival)*(0.1667 * 0.25) + (1 - YearlingSurvival)*(0.5 * 0.5) + (1- YearlingSurvival) * 0.3333 * 0.5 * 0.25

sa <- AdultSurvivalS + (1 - AdultSurvivalS)*(0.3148*0.25) + (1 - AdultSurvivalS)*( 0.1111 * 0.5) + (1- AdultSurvivalS) *0.1111 * 0.5 * 0.25

######## MATRIX CONSTRUCTION ######


#### LOW RATES MEANS - 1.96* SE

sfLow <- sf - 1.96*FawnSurvivalSE 
syLow <- sy - 1.96* YearlingSurvivalSE
saLow <- sa - 1.96 * AdultsSurvivalSE

##### HIGH RATES MEANS + 1.96* SE
sfHigh <- sf + 1.96 *FawnSurvivalSE
syHigh <- sy + 1.96 * YearlingSurvivalSE
saHigh <- sa + 1.96 * AdultsSurvivalSE

##### BIRTHS PER FEMALE
### YEARLING
by <- 1.3

### ADULTS
ba <- 1.7


### FECUNDITITY PRODUCTION
#### PROBABILITY OF PREGNANCY
pa <- 183/212
py <- 15/27
### SURVIVAL OF PODUCING CLASS * PERCENT PREGNANT * YOUNG PRODUCED/2 (FEMALE ONLY MODEL)
ff <- 0
fy <- sy * (by/2) * py
fa<- sa * (ba/2) * pa

##### HIGH RATES VARY BY HIGHER YOUNG PRODUCED AND HIGHER SURVIVAL
ff <- 0
fyHigh <- syHigh * (by/2) * py
faHigh<- saHigh * (ba/2) * pa

##### LOWER RATES VARY BY FEWER YOUNG PRODUCED AND LOWER SURVIVAL
ff <- 0
fyLow <- syLow * (by/2) * py
faLow<- saLow * (ba/2) * pa


####### VITAL RATE MATRICES

VitalMedium <- c( ff, fy, fa, 
                  sf,  0,  0,
                  0,  sy, sa )

VitalHigh <- c(ff, fyHigh, faHigh, 
               sfHigh, 0, 0, 
               0, syHigh, saHigh)

VitalLow <- c(ff, fyLow, faLow, 
              sfLow, 0, 0,
              0, syLow, saLow)




MLow = matrix(VitalLow, 
              nrow=3, ncol=3, 
              byrow = (TRUE),
              dimnames =list(c( "Fawn", "Yearling", "Adult")) )
MHigh = matrix(VitalHigh, 
               nrow=3, ncol=3, 
               byrow = (TRUE),
               dimnames =list(c( "Fawn", "Yearling", "Adult")) )
MMedium =matrix(VitalMedium, 
                nrow=3, ncol=3, 
                byrow = (TRUE),
                dimnames =list(c( "Fawn", "Yearling", "Adult")))

ComboMedium <- MMedium
ComboLow <- MLow
ComboHigh <- MHigh

ComboEigen <- eigen.analysis(MMedium)

ColumnLabels <- c( "Low", "Medium", "High")
RowLabels <- c("Fawn (S)", "Yearling (S)", "Adult (S)")


AdultS <- c(saLow, sa, saHigh)
FawnS <- c(sfLow, sf, sfHigh)
YearlingS <- c( syLow, sy, syHigh)

ComboRates <- matrix(c(FawnS, YearlingS, AdultS),
                              nrow = length(RowLabels), ncol = length(ColumnLabels), byrow = TRUE,
                              dimnames = list(RowLabels, ColumnLabels))
write.csv(ComboRates, here::here("output", "ComboRates.csv"), row.names = TRUE)







#### Not Conservative Approach ####
##### 50% Reduction Harvest, 50% Reduction Bear, 50% in Lion #####
sf <- FawnSurvival + (1- FawnSurvival)*(0.2292 * 0.5) + (1- FawnSurvival)*(0.1875 * 0.5) + (1- FawnSurvival) * 0.2292 * 0.5 * 0.55

sy <- YearlingSurvival + (1 - YearlingSurvival)*(0.1667 * 0.5) + (1 - YearlingSurvival)*(0.5 * 0.5) + (1- YearlingSurvival) * 0.3333 * 0.5 * 0.5

sa <- AdultSurvivalS + (1 - AdultSurvivalS)*(0.3148*0.5) + (1 - AdultSurvivalS)*( 0.1111 * 0.5) + (1 - AdultSurvivalS)*( 0.1111 * 0.5) + (1- AdultSurvivalS) *(0.1111 * 0.5 * 0.5)

#### MATRIX CONSTRUCTION ###############

sfLow <- sf - 1.96*FawnSurvivalSE 
syLow <- sy - 1.96* YearlingSurvivalSE
saLow <- sa - 1.96 * AdultsSurvivalSE

##### HIGH RATES MEANS + 1.96* SE
sfHigh <- sf + 1.96 *FawnSurvivalSE
syHigh <- sy + 1.96 * YearlingSurvivalSE
saHigh <- sa + 1.96 * AdultsSurvivalSE

##### BIRTHS PER FEMALE
### YEARLING
by <- 1.3

### ADULTS
ba <- 1.7


### FECUNDITITY PRODUCTION
#### PROBABILITY OF PREGNANCY
pa <- 183/212
py <- 15/27
### SURVIVAL OF PODUCING CLASS * PERCENT PREGNANT * YOUNG PRODUCED/2 (FEMALE ONLY MODEL)
ff <- 0
fy <- sy * (by/2) * py
fa<- sa * (ba/2) * pa

##### HIGH RATES VARY BY HIGHER YOUNG PRODUCED AND HIGHER SURVIVAL
ff <- 0
fyHigh <- syHigh * (by/2) * py
faHigh<- saHigh * (ba/2) * pa

##### LOWER RATES VARY BY FEWER YOUNG PRODUCED AND LOWER SURVIVAL
ff <- 0
fyLow <- syLow * (by/2) * py
faLow<- saLow * (ba/2) * pa


####### VITAL RATE MATRICES

VitalMedium <- c( ff, fy, fa, 
                  sf,  0,  0,
                  0,  sy, sa )

VitalHigh <- c(ff, fyHigh, faHigh, 
               sfHigh, 0, 0, 
               0, syHigh, saHigh)

VitalLow <- c(ff, fyLow, faLow, 
              sfLow, 0, 0,
              0, syLow, saLow)




MLow = matrix(VitalLow, 
              nrow=3, ncol=3, 
              byrow = (TRUE),
              dimnames =list(c( "Fawn", "Yearling", "Adult")) )
MHigh = matrix(VitalHigh, 
               nrow=3, ncol=3, 
               byrow = (TRUE),
               dimnames =list(c( "Fawn", "Yearling", "Adult")) )
MMedium =matrix(VitalMedium, 
                nrow=3, ncol=3, 
                byrow = (TRUE),
                dimnames =list(c( "Fawn", "Yearling", "Adult")))

XTRAMedium <- MMedium
XTRALow <- MLow
XTRAHigh <- MHigh

XTRAEigen <- eigen.analysis(MMedium)
XTRAVital <- c(sf, sy, sa)
XTRASensitivity <- sensitivity(XTRAMedium, zero = TRUE)
XTRASensitivity


ColumnLabels <- c( "Low", "Medium", "High")
RowLabels <- c("Fawn (S)", "Yearling (S)", "Adult (S)")


AdultS <- c(saLow, sa, saHigh)
FawnS <- c(sfLow, sf, sfHigh)
YearlingS <- c( syLow, sy, syHigh)

Combo50Rates <- matrix(c(FawnS, YearlingS, AdultS),
                          nrow = length(RowLabels), ncol = length(ColumnLabels), byrow = TRUE,
                          dimnames = list(RowLabels, ColumnLabels))
write.csv(Combo50Rates, here::here("output", "Combo50Rates.csv"), row.names = TRUE)

##################

lambda(Lion25Medium)
lambda(Lion50Medium)
lambda(Bear25Medium)
lambda(Bear50Medium)
lambda(Antler50Medium)
lambda(Antler100Medium)
lambda(ComboMedium)
lambda(XTRAMedium)

lambda(Lion25Low)
lambda(Lion50Low)
lambda(Bear25Low)
lambda(Bear50Low)
lambda(Antler50Low)
lambda(Antler100Low)
lambda(ComboLow)
lambda(XTRALow)

lambda(Lion25High)
lambda(Lion50High)
lambda(Bear25High)
lambda(Bear50High)
lambda(Antler50High)
lambda(Antler100High)
lambda(ComboHigh)
lambda(XTRAHigh)

eigen.analysis(Lion25Medium, zero = TRUE)
eigen.analysis(Lion50Medium, zero = TRUE)
eigen.analysis(Bear25Medium, zero = TRUE)
eigen.analysis(Bear50Medium, zero = TRUE)
eigen.analysis(Antler50Medium, zero = TRUE)
eigen.analysis(Antler100Medium, zero = TRUE)
eigen.analysis(ComboMedium, zero = TRUE)
eigen.analysis(XTRAMedium, zero = TRUE)

