setwd("~/Downloads/covid19cfr-master/COVID19_CFR_webpage")
source("Code_functions_SCFR.r")
#------------------------------------------------------------------------------------------------------------------
# Example 1: Age group stratified CFR

TimeAge <- read.csv("Raw/TimeAge.csv", header = TRUE)
TimeAge$date <- as.Date(TimeAge$date, "%Y.%m.%d")
TimeAge <- TimeAge[ ,c("date", "age", "confirmed", "deceased")]

TimeAge_data <- SCFR_data(raw_data = TimeAge, time.var = "date", group.var = "age")
TimeAge_N <- TimeAge_data[[1]]
TimeAge_D <- TimeAge_data[[2]]

######
#TimeAge.combined_N <- SCFR_partial_group(data.table = TimeAge_N,
#                                         group.ind = list(young = c("0s", "10s", "20s"),
#                                                          middle = c("30s", "40s", "50s"),
#                                                          old = c("60s", "70s", "80s")))
#
#TimeAge.partial_N <- SCFR_partial_group(data.table = TimeAge_N,
#                                        group.ind = list(young = c("0s", "10s", "20s")))
#####
TimeAll <- read.csv("Raw/TimeAll.csv", header = TRUE)
cured_count <- TimeAll$released[43:nrow(TimeAll)] # Need counts from Mar 2, 2020

TimeAge_C <- SCFR_Cured(N = TimeAge_N, cured_count = cured_count, Matrix = TRUE)
TimeAge_U <- TimeAge_N - TimeAge_D - TimeAge_C

##---
write.csv(TimeAge_C, "Korea_Agegroup_Cured.csv")
write.csv(TimeAge_D, "Korea_Agegroup_Dead.csv")
write.csv(TimeAge_U, "Korea_Agegroup_Unknown.csv")
write.csv(TimeAge_N, "Korea_Agegroup_Confirmed.csv")
##---
####################################################################################################################



FR.ours <- SCFR_estimation(D = TimeAge_D, U = TimeAge_U, C = TimeAge_C, N = TimeAge_N, model.option = 1)
FR.WHO1 <- SCFR_estimation(D = TimeAge_D, U = TimeAge_U, C = TimeAge_C, N = TimeAge_N, model.option = 2)
FR.WHO2 <- SCFR_estimation(D = TimeAge_D, U = TimeAge_U, C = TimeAge_C, N = TimeAge_N, model.option = 3)
FR.WHO1.group <- SCFR_estimation(D = TimeAge_D, U = TimeAge_U, C = TimeAge_C, N = TimeAge_N, model.option = 4)
FR.WHO2.group <- SCFR_estimation(D = TimeAge_D, U = TimeAge_U, C = TimeAge_C, N = TimeAge_N, model.option = 5)

FR.pred.op1 <- SCFR_prediction(D = TimeAge_D, U = TimeAge_U, C = TimeAge_C, N = TimeAge_N, option = 1)
FR.pred.op2 <- SCFR_prediction(D = TimeAge_D, U = TimeAge_U, C = TimeAge_C, N = TimeAge_N, option = 2)

# Figure 1: Gross CFRs - model 1
y.limit <- c(min(FR.ours$gross, 
                 FR.WHO1$gross, 
                 FR.WHO2$gross) - 0.01, 
             max(FR.ours$gross, 
                 FR.WHO1$gross, 
                 FR.WHO2$gross) + 0.01)
nt <- nrow(TimeAge_D)
days <- rownames(TimeAge_D)
par(mar = c(8,5,5,3))
plot(1:nt, FR.ours$gross, 
     type = "n", ylim = y.limit, xlim = c(0, nt + 1),
     ylab = "case fatality rate", xlab = "", xaxt = "n")
polygon(c(1:nt, rev(1:nt)), c(FR.ours$gross.lower, rev(FR.ours$gross.upper)), 
        border = NA, col = "gray")
lines(1:nt, FR.WHO1$gross, type = "l", lty = 2, lwd = 2, col = "black",
      ylab = "", xlab = "")
lines(1:nt, FR.WHO2$gross, type = "l", lty = 3, lwd = 2, col = "black",
      ylab = "", xlab = "")
lines(1:nt, FR.ours$gross, type = "l", lty = 1, lwd = 2, col = "red",
      ylab = "", xlab = "")
legend("topleft", 
       c("WHO 1st = # deaths / # confirmed", 
         "WHO 2nd = # deaths / (# deaths + # cured)", 
         "SCFR model 1 - gross"), 
       col = c("black", "black", "red"), 
       lty = c(2,3,1),
       lwd = c(2,2,2),
       cex = 0.7)
axis(side = 1, at = c(1, seq(5, length(days)-1, by = 5), length(days)),
     labels = days[c(1, seq(5, length(days)-1, by = 5), length(days))],
     las = 2)

# Figure 2: Gross CFRs - model 2
y.limit <- c(min(FR.pred.op1$gross,
#                 FR.pred.op2$gross,
                 FR.WHO1$gross, 
                 FR.WHO2$gross) - 0.01, 
             max(FR.pred.op1$gross,
#                 FR.pred.op2$gross, 
                 FR.WHO1$gross, 
                 FR.WHO2$gross) + 0.01)
nt <- nrow(TimeAge_D)
days <- rownames(TimeAge_D)
par(mar = c(8,5,5,3))
plot(1:nt, FR.pred.op1$gross, 
     type = "n", ylim = y.limit, xlim = c(0, nt + 1),
     ylab = "case fatality rate", xlab = "", xaxt = "n")
polygon(c(1:nt, rev(1:nt)), c(FR.pred.op1$gross.lower, rev(FR.pred.op1$gross.upper)), 
        border = NA, col = "gray")
#polygon(c(1:nt, rev(1:nt)), c(FR.pred.op2$gross.lower, rev(FR.pred.op2$gross.upper)), 
#        border = NA, col = "yellow")
lines(1:nt, FR.WHO1$gross, type = "l", lty = 2, lwd = 2, col = "black",
      ylab = "", xlab = "")
lines(1:nt, FR.WHO2$gross, type = "l", lty = 3, lwd = 2, col = "black",
      ylab = "", xlab = "")
lines(1:nt, FR.pred.op1$gross, type = "l", lty = 1, lwd = 2, col = "red",
      ylab = "", xlab = "")
#lines(1:nt, FR.pred.op2$gross, type = "l", lty = 1, lwd = 2, col = "blue",
#      ylab = "", xlab = "")
legend("topleft", 
       c("WHO 1st = # deaths / # confirmed", 
         "WHO 2nd = # deaths / (# deaths + # cured)", 
         "SCFR model 2 - gross (option 1 - WHO 1 type)"),
       col = c("black", "black", "red"), 
       lty = c(2,3,1),
       lwd = c(2,2,2),
       cex = 0.7)
axis(side = 1, at = c(1, seq(5, length(days)-1, by = 5), length(days)),
     labels = days[c(1, seq(5, length(days)-1, by = 5), length(days))],
     las = 2)

# Figure 3: Age group-wise confidence intervals
par(mar = c(3, 10, 5, 3))
plot(seq(0, max(FR.ours$group.upper)+0.05, length.out = 10),
     seq(0:9), type = "n", ylim = c(0,9.5), xlim = c(0, max(FR.ours$group.upper)+0.05),
     ylab = "", xlab = "",
     xaxt = "n", yaxt = "n")
for(i in 1:9){
  points(FR.ours$group[nrow(FR.ours$group),i], i, pch = 1, lwd = 10, col = "dodgerblue")
  lines(c(FR.ours$group.lower[nrow(FR.ours$group),i], 
          FR.ours$group.upper[nrow(FR.ours$group),i]),
        rep(i, 2), 
        type = "b", lty = 1, pch = 4, lwd = 5, col = "seagreen")
}
axis(side = 3, at = seq(0, 0.35, by = 0.05),
     labels = paste0(seq(0, 35, by = 5), "%"),
     las = 1)
axis(side = 2, at = 1:9,
     labels = c("0 - 9", "10 - 19", "20 - 29", "30 - 39", "40 - 49",
                "50 - 59", "60 - 69", "70 - 79", "over 80"),
     las = 2)


#------------------------------------------------------------------------------------------------------------------
# Example 2: Country stratified CFR

TimeCountry <- read.csv("TimeCountry_20200325.csv", header = TRUE)
TimeCountry$date <- as.Date(TimeCountry$date, "%Y.%m.%d")
TimeCountry_data <- SCFR_data(raw_data = TimeCountry, time.var = "date", group.var = "country")
TimeCountry_N <- TimeCountry_data[[1]]
TimeCountry_D <- TimeCountry_data[[2]]
TimeCountry_C <- TimeCountry_data[[3]]
TimeCountry_U <- TimeCountry_N - TimeCountry_D - TimeCountry_C

FR.ours <- SCFR_estimation(D = TimeCountry_D, U = TimeCountry_U, C = TimeCountry_C, N = TimeCountry_N, model.option = 1)
FR.WHO1 <- SCFR_estimation(D = TimeCountry_D, U = TimeCountry_U, C = TimeCountry_C, N = TimeCountry_N, model.option = 2)
FR.WHO2 <- SCFR_estimation(D = TimeCountry_D, U = TimeCountry_U, C = TimeCountry_C, N = TimeCountry_N, model.option = 3)
FR.WHO1.group <- SCFR_estimation(D = TimeCountry_D, U = TimeCountry_U, C = TimeCountry_C, N = TimeCountry_N, model.option = 4)
FR.WHO2.group <- SCFR_estimation(D = TimeCountry_D, U = TimeCountry_U, C = TimeCountry_C, N = TimeCountry_N, model.option = 5)

FR.pred.op1 <- SCFR_prediction(D = TimeCountry_D, U = TimeCountry_U, C = TimeCountry_C, N = TimeCountry_N, option = 1)
FR.pred.op2 <- SCFR_prediction(D = TimeCountry_D, U = TimeCountry_U, C = TimeCountry_C, N = TimeCountry_N, option = 2)

# Figure 1: Gross CFRs - model 1
nt <- nrow(TimeCountry_D)
days <- rownames(TimeCountry_D)
par(mar = c(8,5,5,3))
plot(1:nt, FR.ours$gross, 
     type = "n", ylim = c(0, 0.20), xlim = c(0, nt + 1),
     ylab = "case fatality rate", xlab = "", xaxt = "n", yaxt = "n")
polygon(c(1:nt, rev(1:nt)), c(FR.ours$gross.lower, rev(FR.ours$gross.upper)), 
        border = NA, col = "gray")
polygon(c(1:nt, rev(1:nt)), c(FR.ours$group.lower[,"Korea"], rev(FR.ours$group.upper[,"Korea"])), 
        border = NA, col = "tan")
polygon(c(1:nt, rev(1:nt)), c(FR.ours$group.lower[,"Italy"], rev(FR.ours$group.upper[,"Italy"])), 
        border = NA, col = "wheat")
lines(1:nt, FR.WHO1$gross, type = "l", lty = 2, lwd = 2, col = "black",
      ylab = "", xlab = "")
lines(1:nt, FR.WHO2$gross, type = "l", lty = 3, lwd = 2, col = "black",
      ylab = "", xlab = "")
lines(1:nt, FR.ours$gross, type = "l", lty = 1, lwd = 2, col = "red",
      ylab = "", xlab = "")
lines(1:nt, FR.ours$group[,"Korea"], type = "l", lty = 1, lwd = 2, col = "blue",
      ylab = "", xlab = "")
lines(1:nt, FR.ours$group[,"Italy"], type = "l", lty = 1, lwd = 2, col = "seagreen",
      ylab = "", xlab = "")
legend("topleft", 
       c("WHO 1st = # deaths / # confirmed", 
         "WHO 2nd = # deaths / (# deaths + # cured)", 
         "SCFR model 1 - gross",
         "SCFR model 1 - Korea",
         "SCFR model 1 - Italy"), 
       col = c("black", "black", "red", "blue", "seagreen"), 
       lty = c(2,3,1,1,1),
       lwd = c(2,2,2,2,2),
       cex = 0.7)
axis(side = 1, at = c(1, seq(5, length(days)-1, by = 5), length(days)),
     labels = days[c(1, seq(5, length(days)-1, by = 5), length(days))],
     las = 2)
axis(side = 2, at = seq(0, 0.20, by = 0.05),
     labels = paste0(seq(0, 20, by = 5), "%"),
     las = 2)

# Figure 2: Gross CFRs - model 2
y.limit <- c(min(FR.pred.op1$gross,
                 #                 FR.pred.op2$gross,
                 FR.WHO1$gross, 
                 FR.WHO2$gross) - 0.01, 
             max(FR.pred.op1$gross,
                 #                 FR.pred.op2$gross, 
                 FR.WHO1$gross, 
                 FR.WHO2$gross) + 0.01)
par(mar = c(8,5,5,3))
plot(1:nt, FR.pred.op1$gross, 
     type = "n", ylim = y.limit, xlim = c(0, nt + 1),
     ylab = "case fatality rate", xlab = "", xaxt = "n")
polygon(c(1:nt, rev(1:nt)), c(FR.pred.op1$gross.lower, rev(FR.pred.op1$gross.upper)), 
        border = NA, col = "gray")
polygon(c(1:nt, rev(1:nt)), c(FR.pred.op1$group.lower[,1], rev(FR.pred.op1$group.upper[,1])), 
        border = NA, col = "tan")
lines(1:nt, FR.WHO1$gross, type = "l", lty = 2, lwd = 2, col = "black",
      ylab = "", xlab = "")
lines(1:nt, FR.WHO2$gross, type = "l", lty = 3, lwd = 2, col = "black",
      ylab = "", xlab = "")
lines(1:nt, FR.pred.op1$gross, type = "l", lty = 1, lwd = 2, col = "red",
      ylab = "", xlab = "")
lines(1:nt, FR.pred.op1$group[,1], type = "l", lty = 1, lwd = 2, col = "blue",
      ylab = "", xlab = "")
legend("topleft", 
       c("WHO 1st = # deaths / # confirmed", 
         "WHO 2nd = # deaths / (# deaths + # cured)", 
         "SCFR model 2 - gross",
         "SCFR model 2 - Korea"),
       col = c("black", "black", "red", "blue"), 
       lty = c(2,3,1,1),
       lwd = c(2,2,2,2),
       cex = 0.7)
axis(side = 1, at = c(1, seq(5, length(days)-1, by = 5), length(days)),
     labels = days[c(1, seq(5, length(days)-1, by = 5), length(days))],
     las = 2)

# Figure 3: Country-wise confidence intervals
par(mar = c(3, 10, 5, 3))
plot(seq(0, max(FR.ours$group.upper)+0.05, length.out = 11),
     seq(0:10), type = "n", ylim = c(0,10.5), xlim = c(0, 0.20),
     ylab = "", xlab = "",
     xaxt = "n", yaxt = "n")
for(i in 1:10){
  points(FR.ours$group[nrow(FR.ours$group),i], i, pch = 1, lwd = 10, col = "dodgerblue")
  lines(c(FR.ours$group.lower[nrow(FR.ours$group),i], 
          FR.ours$group.upper[nrow(FR.ours$group),i]),
        rep(i, 2), 
        type = "b", lty = 1, pch = 4, lwd = 5, col = "seagreen")
}
axis(side = 3, at = seq(0, 0.20, by = 0.05),
     labels = paste0(seq(0, 20, by = 5), "%"),
     las = 1)
axis(side = 2, at = 1:10,
     labels = colnames(FR.ours$group),
     las = 2)
