install.packages("popbio")
library(popbio)

############# START WITH STEP 1 OF YEARLY SURVIVAL




 ##### SURVIVAL RATES 
sf <- FawnSurvival
sy <- YearlingSurvival
sa <- AdultSurvivalS

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
byLow <- 1.1
byHigh <- 1.5

### ADULTS
ba <- 1.7
baLow <- 1.5
baHigh <- 1.9

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

N <- matrix(NA, 2, T)

####PopBio package
### eigen.analysis
####Right Eigenvector - Stable stage distribution

###M*x = Lambda*x
MMedium =matrix(c( ff, fy, fa, 
              sf,  0,  0,
               0,  sy, sa ), 
             nrow=3, ncol=3, 
             byrow = (TRUE),
             dimnames =list(c( "Fawn", "Yearling", "Adult")))
                  # add row and column names

MMedium



fawn <- 1000
yearling <- 1000
adult <- 1000

T = 50
N0 = matrix(c(fawn,yearling,adult), ncol=1)        # Initial population
rownames(N0) <- c("Fawn", "Yearling", "Adult")                 # add row and column names
colnames(N0) <- c("Abundance")
N0


M=matrix(c(ff, fy, fa, 
          sf,  0,  0,
          0,  sy, sa ), 
           nrow=3, ncol=3, 
           byrow = (TRUE),
           dimnames =list(c( "Fawn", "Yearling", "Adult")))

M

N = matrix(NA,
           nrow=nrow(N0),
           ncol = T,
           dimnames =list(c( "Fawn", "Yearling", "Adult")))
N
N[,1] <- N0                     ## year 0 abundance



for (t in 2:T) { 
  N[,t] <- M %*% N[,t-1]
  
  
  
}
N


matplot(1:T, t(N), type = "l", lwd = 3, col = c( "dark red", "dark blue", "dark magenta"),
        ylab = "population size", xlab = "Time (t)", 
        main= "Age-Stuctured Population Model")
legend("topright", legend = c("Fawn", "Yearling", "Adult"),lwd = 3, 
       col = c("dark red", "dark blue", "dark magenta"), bty = "n")





eigen.analysis(MMedium, zero = TRUE)
eigen.analysis(MHigh)
eigen.analysis(MLow)





#### CONSTRUCTION OF TABLE
### TABLE LABELS
ColumnLabels <- c( "Low", "Medium", "High")
RowLabels <- c("Yearling (Birthrate)", "Adult (Birthrate)", "Fawn (S)", "Yearling (S)", "Adult (S)")

YearlingB <- c(byLow, by, byHigh)
AdultB <- c(baLow, ba, baHigh)
AdultS <- c(saLow, sa, saHigh)
FawnS <- c(sfLow, sf, sfHigh)
YearlingS <- c( syLow, sy, syHigh)

VitalRates <- matrix(c(YearlingB *0.5, AdultB * 0.5, FawnS, YearlingS, AdultS),
                        nrow = length(RowLabels), ncol = length(ColumnLabels), byrow = TRUE,
                        dimnames = list(RowLabels, ColumnLabels))
write.csv(VitalRates, here::here("output", "VitalRates.csv"), row.names = TRUE)
