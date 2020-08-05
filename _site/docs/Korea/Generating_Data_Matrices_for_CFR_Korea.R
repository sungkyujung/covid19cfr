setwd("/Users/BWKIM/Dropbox/ByungWonKim/Posdoc_Research_DrJang/Corona-19/CFR_Stratification_Web_updates/Korea")
source("/Users/BWKIM/Dropbox/ByungWonKim/Posdoc_Research_DrJang/Corona-19/CFR_Stratification_Web_updates/Code_functions_SCFR.r")
#------------------------------------------------------------------------------------------------------------------

Korea_Age_Time <- read.csv("/Users/BWKIM/Dropbox/ByungWonKim/Posdoc_Research_DrJang/Corona-19/CFR_Stratification_Web_updates/Raw/TimeAge.csv", header = TRUE)
Korea_Age_Time$date <- as.Date(Korea_Age_Time$date, "%Y.%m.%d")
Korea_Age_Time <- Korea_Age_Time[ ,c("date", "age", "confirmed", "deceased")]

Korea_Age_Time_data <- SCFR_data(raw_data = Korea_Age_Time, time.var = "date", group.var = "age")
Korea_Age_Time_N <- Korea_Age_Time_data[[1]]
Korea_Age_Time_D <- Korea_Age_Time_data[[2]]

Korea_All <- read.csv("/Users/BWKIM/Dropbox/ByungWonKim/Posdoc_Research_DrJang/Corona-19/CFR_Stratification_Web_updates/Raw/Time.csv", header = TRUE)
cured_count <- Korea_All$released[43:nrow(Korea_All)] # Need counts from Mar 2, 2020

Korea_Age_Time_C <- SCFR_Cured(N = Korea_Age_Time_N - Korea_Age_Time_D, cured_count = cured_count, Matrix = TRUE)
Korea_Age_Time_U <- Korea_Age_Time_N - Korea_Age_Time_D - Korea_Age_Time_C

##---
write.csv(Korea_Age_Time_C, "Korea_Agegroup_Cured.csv")
write.csv(Korea_Age_Time_D, "Korea_Agegroup_Dead.csv")
write.csv(Korea_Age_Time_U, "Korea_Agegroup_Unknown.csv")
write.csv(Korea_Age_Time_N, "Korea_Agegroup_Confirmed.csv")
##---
