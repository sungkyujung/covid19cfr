setwd("/Users/BWKIM/Dropbox/ByungWonKim/Posdoc_Research_DrJang/Corona-19/CFR_Stratification_Web_updates/Global")
source("/Users/BWKIM/Dropbox/ByungWonKim/Posdoc_Research_DrJang/Corona-19/CFR_Stratification_Web_updates/Code_functions_SCFR.r")
Global.N.raw <- read.csv("/Users/BWKIM/Dropbox/ByungWonKim/Posdoc_Research_DrJang/Corona-19/CFR_Stratification_Web_updates/Raw/time_series_covid19_confirmed_global.csv", header = T)
Global.D.raw <- read.csv("/Users/BWKIM/Dropbox/ByungWonKim/Posdoc_Research_DrJang/Corona-19/CFR_Stratification_Web_updates/Raw/time_series_covid19_deaths_global.csv", header = T)
Global.C.raw <- read.csv("/Users/BWKIM/Dropbox/ByungWonKim/Posdoc_Research_DrJang/Corona-19/CFR_Stratification_Web_updates/Raw/time_series_covid19_recovered_global.csv", header = T)

# Basically, the number of columns have to be the same

n.columns <- min(ncol(Global.C.raw), ncol(Global.D.raw), ncol(Global.N.raw))
Global.C.raw <- Global.C.raw[,1:n.columns]
Global.D.raw <- Global.D.raw[,1:n.columns]
Global.N.raw <- Global.N.raw[,1:n.columns]

days <- as.Date(colnames(Global.C.raw)[5:ncol(Global.C.raw)], "X%m.%d.%Y") # days will be used as row names
library(lubridate)
year(days) <- 2020

# Data will be transformed to have (time) x (country) form
# Provinces or states will be combined
Global.C.raw$Country.Region <- as.character(Global.C.raw$Country.Region)
Global.D.raw$Country.Region <- as.character(Global.D.raw$Country.Region)
Global.N.raw$Country.Region <- as.character(Global.N.raw$Country.Region)

# Numbers of countries in C, D and N are different => will use countries appear in all C, D, and N
Country <- read.csv("/Users/BWKIM/Dropbox/ByungWonKim/Posdoc_Research_DrJang/Corona-19/CFR_Stratification_Web_updates/Raw/Country_name.csv", header = TRUE)
Country.name <- as.character(Country$Country)

Global.C <- matrix(0, nrow = length(days), ncol = length(Country.name))
Global.D <- matrix(0, nrow = length(days), ncol = length(Country.name))
Global.N <- matrix(0, nrow = length(days), ncol = length(Country.name))
for (i in 1:length(Country.name)){
  temp.C <- Global.C.raw[Global.C.raw$Country.Region == Country.name[i], 5:n.columns]
  if (nrow(temp.C) == 1){
    Global.C[,i] <- t(temp.C)
  }else{
    Global.C[,i] <- apply(t(temp.C), 1, sum)
  }
  temp.D <- Global.D.raw[Global.D.raw$Country.Region == Country.name[i], 5:n.columns]
  if (nrow(temp.D) == 1){
    Global.D[,i] <- t(temp.D)
  }else{
    Global.D[,i] <- apply(t(temp.D), 1, sum)
  }
  temp.N <- Global.N.raw[Global.N.raw$Country.Region == Country.name[i], 5:n.columns]
  if (nrow(temp.N) == 1){
    Global.N[,i] <- t(temp.N)
  }else{
    Global.N[,i] <- apply(t(temp.N), 1, sum)
  }
}
Global.U <- Global.N - Global.C - Global.D
Global.C <- as.data.frame(Global.C)
colnames(Global.C) <- Country.name
rownames(Global.C) <- days
Global.D <- as.data.frame(Global.D)
colnames(Global.D) <- Country.name
rownames(Global.D) <- days
Global.N <- as.data.frame(Global.N)
colnames(Global.N) <- Country.name
rownames(Global.N) <- days
Global.U <- as.data.frame(Global.U)
colnames(Global.U) <- Country.name
rownames(Global.U) <- days

write.csv(Global.C, "Global_Cured.csv")
write.csv(Global.D, "Global_Dead.csv")
write.csv(Global.U, "Global_Unknown.csv")
write.csv(Global.N, "Global_Confirmed.csv")

