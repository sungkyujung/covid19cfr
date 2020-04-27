setwd("~/Downloads/covid19cfr-master/COVID19_CFR_webpage")
Global.N.raw <- read.csv("Raw/time_series_covid19_confirmed_global.csv", header = T)
Global.D.raw <- read.csv("Raw/time_series_covid19_deaths_global.csv", header = T)
Global.C.raw <- read.csv("Raw/time_series_covid19_recovered_global.csv", header = T)

# Basically, the number of columns have to be the same
# but now, Global.C does not have the column of 2020-03-25
# So, the column of 2020-03-25 for Global.N and Global.D will be dropped

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
Country <- read.csv("Country_name.csv", header = TRUE)
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

#-------------------------------------------------------------------------------------------
# Calculate CFRs

Global.FR.ours <- SCFR_estimation(D = Global.D, U = Global.U, C = Global.C, N = Global.N, model.option = 1)
Global.FR.WHO1 <- SCFR_estimation(D = Global.D, U = Global.U, C = Global.C, N = Global.N, model.option = 2)
Global.FR.WHO2 <- SCFR_estimation(D = Global.D, U = Global.U, C = Global.C, N = Global.N, model.option = 3)
Global.FR.WHO1.group <- SCFR_estimation(D = Global.D, U = Global.U, C = Global.C, N = Global.N, model.option = 4)
Global.FR.WHO2.group <- SCFR_estimation(D = Global.D, U = Global.U, C = Global.C, N = Global.N, model.option = 5)

Global.FR.pred.op1 <- SCFR_prediction(D = Global.D, U = Global.U, C = Global.C, N = Global.N, option = 1)
Global.FR.pred.op2 <- SCFR_prediction(D = Global.D, U = Global.U, C = Global.C, N = Global.N, option = 2)

# Figure 1: Gross CFRs - model 1
nt <- nrow(Global.C)
days <- rownames(Global.C)
par(mar = c(8,5,5,3))
plot(1:nt, Global.FR.ours$gross, 
     type = "n", ylim = c(0, 0.20), xlim = c(0, nt + 1),
     ylab = "case fatality rate", xlab = "", xaxt = "n", yaxt = "n")
polygon(c(1:nt, rev(1:nt)), c(Global.FR.ours$gross.lower, rev(Global.FR.ours$gross.upper)), 
        border = NA, col = "gray")
polygon(c(1:nt, rev(1:nt)), c(Global.FR.ours$group.lower[,"Korea, South"], rev(Global.FR.ours$group.upper[,"Korea, South"])), 
        border = NA, col = "tan")
polygon(c(1:nt, rev(1:nt)), c(Global.FR.ours$group.lower[,"Italy"], rev(Global.FR.ours$group.upper[,"Italy"])), 
        border = NA, col = "wheat")
lines(1:nt, Global.FR.WHO1$gross, type = "l", lty = 2, lwd = 2, col = "black",
      ylab = "", xlab = "")
lines(1:nt, Global.FR.WHO2$gross, type = "l", lty = 3, lwd = 2, col = "black",
      ylab = "", xlab = "")
lines(1:nt, Global.FR.ours$gross, type = "l", lty = 1, lwd = 2, col = "red",
      ylab = "", xlab = "")
lines(1:nt, Global.FR.ours$group[,"Korea, South"], type = "l", lty = 1, lwd = 2, col = "blue",
      ylab = "", xlab = "")
lines(1:nt, Global.FR.ours$group[,"Italy"], type = "l", lty = 1, lwd = 2, col = "seagreen",
      ylab = "", xlab = "")
legend("topleft", 
       c("WHO 1st = # deaths / # confirmed", 
         "WHO 2nd = # deaths / (# deaths + # cured)", 
         "SCFR model 1 - gross",
         "SCFR model 1 - Korea, South",
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

# Figure 2: Country-wise confidence intervals - TOP 10 in # of confirmed patients
TOP10.index <- order(Global.N[nrow(Global.N),], decreasing = TRUE)[1:10]
TOP10.CFR <- Global.FR.ours$group[nrow(Global.N), TOP10.index]
TOP10.CFR.upper <- Global.FR.ours$group.upper[nrow(Global.N), TOP10.index]
TOP10.CFR.lower <- Global.FR.ours$group.lower[nrow(Global.N), TOP10.index]
CFR.order <- order(TOP10.CFR, decreasing = TRUE)

par(mar = c(3, 10, 5, 3))
plot(seq(0, 0.20, length.out = 11),
     seq(0:10), type = "n", ylim = c(0,10.5), xlim = c(0, 0.20),
     ylab = "", xlab = "",
     xaxt = "n", yaxt = "n")
for(i in 1:10){
  points(TOP10.CFR[CFR.order[i]], 11 - i, pch = 1, lwd = 10, col = "dodgerblue")
  lines(c(TOP10.CFR.lower[CFR.order[i]], 
          TOP10.CFR.upper[CFR.order[i]]),
        rep(11 - i, 2), 
        type = "b", lty = 1, pch = 4, lwd = 5, col = "seagreen")
}
axis(side = 3, at = seq(0, 0.20, by = 0.05),
     labels = paste0(seq(0, 20, by = 5), "%"),
     las = 1)
axis(side = 2, at = 10:1,
     labels = names(TOP10.CFR)[CFR.order],
     las = 2)





