setwd("/Users/BWKIM/Dropbox/ByungWonKim/Posdoc_Research_DrJang/Corona-19/CFR_Stratification_Web_updates/China")
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

China.C.raw <- Global.C.raw[Global.C.raw$Country.Region == "China",]
China.D.raw <- Global.D.raw[Global.D.raw$Country.Region == "China",]
China.N.raw <- Global.N.raw[Global.N.raw$Country.Region == "China",]

province.name <- as.character(China.C.raw$Province.State)

China.C <- matrix(0, nrow = length(days), ncol = length(province.name))
China.D <- matrix(0, nrow = length(days), ncol = length(province.name))
China.N <- matrix(0, nrow = length(days), ncol = length(province.name))


for (i in 1:length(province.name)){
  temp.C <- China.C.raw[China.C.raw$Province.State == province.name[i], 5:n.columns]
  if (nrow(temp.C) == 1){
    China.C[,i] <- t(temp.C)
  }else{
    China.C[,i] <- apply(t(temp.C), 1, sum)
  }
  temp.D <- China.D.raw[China.D.raw$Province.State == province.name[i], 5:n.columns]
  if (nrow(temp.D) == 1){
    China.D[,i] <- t(temp.D)
  }else{
    China.D[,i] <- apply(t(temp.D), 1, sum)
  }
  temp.N <- China.N.raw[China.N.raw$Province.State == province.name[i], 5:n.columns]
  if (nrow(temp.N) == 1){
    China.N[,i] <- t(temp.N)
  }else{
    China.N[,i] <- apply(t(temp.N), 1, sum)
  }
}
China.U <- China.N - China.C - China.D
China.C <- as.data.frame(China.C)
colnames(China.C) <- province.name
rownames(China.C) <- days
China.D <- as.data.frame(China.D)
colnames(China.D) <- province.name
rownames(China.D) <- days
China.N <- as.data.frame(China.N)
colnames(China.N) <- province.name
rownames(China.N) <- days
China.U <- as.data.frame(China.U)
colnames(China.U) <- province.name
rownames(China.U) <- days

write.csv(China.C, "China_Cured.csv")
write.csv(China.D, "China_Dead.csv")
write.csv(China.U, "China_Unknown.csv")
write.csv(China.N, "China_Confirmed.csv")
