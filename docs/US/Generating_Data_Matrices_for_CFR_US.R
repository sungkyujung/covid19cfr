setwd("/Users/BWKIM/Dropbox/ByungWonKim/Posdoc_Research_DrJang/Corona-19/CFR_Stratification_Web_updates/US")
source("/Users/BWKIM/Dropbox/ByungWonKim/Posdoc_Research_DrJang/Corona-19/CFR_Stratification_Web_updates/Code_functions_SCFR.r")
#------------------------------------------------------------------------------------------------------------------
US_State_Time <- read.csv("/Users/BWKIM/Dropbox/ByungWonKim/Posdoc_Research_DrJang/Corona-19/CFR_Stratification_Web_updates/Raw/us-states.csv", header = TRUE)
US_State_Time <- US_State_Time[,c("date", "state", "cases", "deaths")]
### US_State_Time starts from 2020-01-21 ###
colnames(US_State_Time) <- c("date", "state", "confirmed", "deceased")

Global.C.raw <- read.csv("/Users/BWKIM/Dropbox/ByungWonKim/Posdoc_Research_DrJang/Corona-19/CFR_Stratification_Web_updates/Raw/time_series_covid19_recovered_global.csv", header = T)
days = as.Date(colnames(Global.C.raw)[5:ncol(Global.C.raw)], "X%m.%d.%Y")
### Global.C.raw starts from 2020-01-22 ###
library(lubridate)
year(days) <- 2020
US_State_Time$date <- as.Date(US_State_Time$date, "%Y-%m-%d")
if (length(days) < length(unique(US_State_Time$date))){
  length_of_days <- length(days)
}else{
  length_of_days <- length(days[1:(length(unique(US_State_Time$date))-1)])
}

US_State_Time_data <- SCFR_data(raw_data = US_State_Time, time.var = "date", group.var = "state")
US_State_Time_N <- US_State_Time_data[[1]]
US_State_Time_D <- US_State_Time_data[[2]]
US_State_Time_N[is.na(US_State_Time_N)] = 0
US_State_Time_D[is.na(US_State_Time_D)] = 0
US_State_Time_N <- US_State_Time_N[2:(length_of_days+1),]
US_State_Time_D <- US_State_Time_D[2:(length_of_days+1),]

cured_count <- as.numeric(Global.C.raw[Global.C.raw["Country.Region"]=="US", 5:(length_of_days+4)])
Total_count <- apply(US_State_Time_N, 1, sum)
for (i in 1:length_of_days){
  if (cured_count[i] > Total_count[i]){
    cured_count[i] <- Total_count[i]
  }
}
US_State_Time_C <- SCFR_Cured(N = US_State_Time_N - US_State_Time_D, cured_count = cured_count, Matrix = TRUE)
US_State_Time_U <- US_State_Time_N - US_State_Time_D - US_State_Time_C

##---
write.csv(US_State_Time_C, "US_State_Cured.csv")
write.csv(US_State_Time_D, "US_State_Dead.csv")
write.csv(US_State_Time_U, "US_State_Unknown.csv")
write.csv(US_State_Time_N, "US_State_Confirmed.csv")
##---


