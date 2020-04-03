setwd("~/Downloads/covid19cfr-master/COVID19_CFR_webpage")
source("Code_functions_SCFR.r")
#------------------------------------------------------------------------------------------------------------------
# Example 1: Age group stratified CFR

TimeAge <- read.csv("Raw/us-states.csv", header = TRUE)
TimeAge$date <- as.Date(TimeAge$date, "%Y.%m.%d")
TimeAge <- TimeAge[ ,c("date", "state", "confirmed", "deceased")]

TimeAge_data <- SCFR_data(raw_data = TimeAge, time.var = "date", group.var = "state")
TimeAge_N <- TimeAge_data[[1]]
TimeAge_D <- TimeAge_data[[2]]
TimeAge_N[is.na(TimeAge_N)] = 0
TimeAge_D[is.na(TimeAge_D)] = 0

######
#TimeAge.combined_N <- SCFR_partial_group(data.table = TimeAge_N,
#                                         group.ind = list(young = c("0s", "10s", "20s"),
#                                                          middle = c("30s", "40s", "50s"),
#                                                          old = c("60s", "70s", "80s")))
#
#TimeAge.partial_N <- SCFR_partial_group(data.table = TimeAge_N,
#                                        group.ind = list(young = c("0s", "10s", "20s")))
#####


Global.C.raw <- read.csv("Raw/time_series_covid19_recovered_global.csv", header = T)
cured_count <- as.numeric(Global.C.raw[Global.C.raw["Country.Region"]=="US",5:ncol(Global.C.raw)]) # Need counts from Mar 2, 2020
TimeAge_C <- SCFR_Cured(N = TimeAge_N, cured_count = cured_count, Matrix = TRUE)
TimeAge_U <- TimeAge_N - TimeAge_D - TimeAge_C

##---
write.csv(TimeAge_C, "US_State_Cured.csv")
write.csv(TimeAge_D, "US_State_Dead.csv")
write.csv(TimeAge_U, "US_State_Unknown.csv")
write.csv(TimeAge_N, "US_State_Confirmed.csv")
##---
####################################################################################################################

