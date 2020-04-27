#------------------------------------------------------------------------------------
# Functions for calculation of stratification-based case fatality rate
# Byungwon Kim (Mar 25, 2020)
#------------------------------------------------------------------------------------

## 1. Data manipulation

SCFR_data <- function(raw_data, time.var, group.var){
  # This function translates the raw data into an appropriate form for our proposed CFR calculation
  # Input: raw_data - data frame that contains time variable, group variable, several count variables
  #        time.var - the name of time variable in the raw data
  #        group.var - the name of group variable in the raw data
  #
  # Output: data.out - a list of data frames transformed from the raw data
  #                    each data frame has the form of (time) x (group) matrix
  #                    whose elements correspond to their counts
  
  Time.index <- sort(unique(raw_data[,time.var]))
  Group.index <- sort(unique(raw_data[,group.var]))
  Num.count.var <- ncol(raw_data) - 2           # number of variables except the time and group variables
  data.out.array <- array(0, dim = c(Num.count.var, length(Time.index), length(Group.index)))
  for (j in 1:length(Time.index)){
    temp <- data.frame(raw_data[raw_data[,time.var] == Time.index[j], colnames(raw_data) != time.var])
    for (k in 1:length(Group.index)){
      temp2 <- data.frame(temp[temp[,group.var] == Group.index[k], colnames(temp) != group.var])
      for (i in 1:Num.count.var){
        data.out.array[i, j, k] <- as.numeric(temp2[i])
      }
    }
  }
  
  data.out <- list()
  for (i in 1:Num.count.var){
    data.out[[i]] <- data.frame(data.out.array[i,,])
    rownames(data.out[[i]]) <- Time.index
    colnames(data.out[[i]]) <- Group.index
  }
  return(data.out)
}


## 1-(1). Partial grouping

SCFR_partial_group <- function(data.table, group.ind){
  # This function can be used to define partial groups from existing groups if required
  # Input: data.table - a table (count matrix) produced by SCFR_data()
  #                     need to be a form of (time) x (group) <- each column of matrix indicates each group
  #        group.ind - a list of sub-group indicator 
  #                    e.g.) if existing age groups are ("0s", "10s", ..., "80s")
  #                          and someone wants to make different groups of age such as ("young", "middle", "old")
  #                          then group.ind can be stated as
  #                          group.ind = list(young = c("0s", "10s", "20s"), 
  #                                           middle = c("30s", "40s", "50s"), 
  #                                           old = c("60s", "70s", "80s"))
  #                    No need to cover all existing groups
  #                    But names or locations of columns need to be clearly stated
  # Output: data.out - a table (count matrix) that has a form of (time) x (sub group)
  Name.subgroup <- names(group.ind)
  if (length(Name.subgroup) == 0){
    # if names of subgroups are not given,
    Name.subgroup <- 1:length(group.ind)
  }
  data.out <- data.frame(matrix(0, nrow = nrow(data.table), ncol = length(Name.subgroup)))
  for (i in 1:length(Name.subgroup)){
    temp <- data.table[, group.ind[[Name.subgroup[i]]]]
    if (ncol(temp) > 1){
      data.out[,i] <- apply(temp, 1, sum)
    }else{
      data.out[,i] <- temp
    }
  }
  rownames(data.out) <- rownames(data.table)
  colnames(data.out) <- Name.subgroup
  
  return(data.out)
}

## 1-(2). Estimation of cured counts in each group

SCFR_Cured <- function(N, cured_count, Matrix = TRUE){
  # This function estimates the number of cured patients in each group
  # based on the proportion of each group among total confirmed patients
  # Input: N - a vector or matrix of confirmed patients
  #            need to be a form of (time) x (group)
  #        cured_count - total count of cured patients
  #                      if N is a matrix cured_count is a vector, if N is a vector cured_count is a scalar
  #        Matrix - indicator variable, TRUE (default) = N is a matrix
  #                                     FALSE = N is a vector
  # Output: C - a vector or matrix of estimated counts of cured patients
  #
  # ** This function is required for "SCFR calculation - stratified by age group - for COVID_19 outbreak in South Korea"
  # ** because KCDC (Korea Center for Disease Control and prevention) does not report the age-grouped number of cured patients
  
  if (Matrix){
    Total <- apply(N, 1, sum)
    Prop_group <- N / matrix(rep(Total, ncol(N)), ncol = ncol(N), byrow = FALSE)
    C <- round(Prop_group * matrix(rep(cured_count, ncol(Prop_group)), ncol = ncol(Prop_group), byrow = FALSE), 0)
  }else{
    Total <- sum(N)
    Prop_group <- N / Total
    C <- round(Prop_group * cured_count, 0)
  }
}

## 2. Calculation of SCFR
SCFR_estimation <- function(D, U, C, N, model.option = 1){
  # This function calculates our proposed (stratification-based) case fatality rate (CFR)
  # Input: D - (time) x (group) formed count matrix of deaths
  #        U - (time) x (group) formed count matrix of quarantined patients (confirmed but not recovered nor dead)
  #        C - (time) x (group) formed count matrix of cured patients
  #        N - (time) x (group) formed count matrix of total confirmed patients
  #        model.option - 1 (default): our proposed CFR
  #                       2: 1st model of WHO (number of deaths / number of total confirmed patients)
  #                       3: 2nd model of WHO (number of deaths / (number of cured patients + number of deaths))
  #                       4: group-wise 1st model of WHO + weighted mean for gross CFR
  #                       5: group-wise 2nd model of WHO + weighted mean for gross CFR
  # Output: CFR - a list of CFRs
  #               CFR[["info"]] - information of used option
  #               CFR[["gross"]] - a (time) x 1 vector of gross CFRs
  #                                provided for all options
  #               CFR[["group"]] - a (time) x (group) matrix of group-wise CFRs
  #                                provided for options 1, 4, and 5
  #               Additionally for option 1 only,
  #               CFR[["gross.variance"]] - estimated variances of gross CFRs
  #               CFR[["gross.lower"]] - lower bounds of 95% confidence band for gross CFRs
  #               CFR[["gross.upper"]] - upper bounds of 95% confidence band for gross CFRs
  #               CFR[["group.variance"]] - estimated variances of group-wise CFRs
  #               CFR[["group.lower"]] - lower bounds of 95% confidence band for group-wise CFRs
  #               CFR[["group.upper"]] - upper bounds of 95% confidence band for group-wise CFRs
  if (model.option == 1){
    CFR <- list(info = "stratification-based CFR",
                gross = matrix(0, nrow = nrow(D), ncol = 1),
                group = matrix(0, nrow = nrow(D), ncol = ncol(D)),
                gross_new = matrix(0, nrow = nrow(D), ncol = 1),
                group_new = matrix(0, nrow = nrow(D), ncol = ncol(D)),
                gross.variance = matrix(0, nrow = nrow(D), ncol = 1),
                gross.lower = matrix(0, nrow = nrow(D), ncol = 1),
                gross.upper = matrix(0, nrow = nrow(D), ncol = 1),
                group.variance = matrix(0, nrow = nrow(D), ncol = ncol(D)),
                group.lower = matrix(0, nrow = nrow(D), ncol = ncol(D)),
                group.upper = matrix(0, nrow = nrow(D), ncol = ncol(D)))
    CFR[["group"]] <- (D + U * D / N) / N
    CFR[["group_new"]] <- (D + U * D / (C+D)) / N
    p.d <- D / N
    p.u <- U / N
    eta <- p.d * (1 - p.d) + p.d * p.u * (2 - 3 * p.d + p.u - 4 * p.d * p.u) * (N - 1) / N
    zeta <- p.d * p.u * (1 - 2 * p.d + 6 * p.d * p.u - 2 * p.u) * (N - 1) / N
    CFR[["group.variance"]] <- eta / N + zeta / N^2
    for (i in 1:nrow(D)){
      CFR$group[i, is.na(CFR$group[i,])] <- 0
      CFR$group.variance[i, is.na(CFR$group.variance[i,])] <- 0
    }
    CFR[["gross"]] <- (apply(CFR[["group"]] * N, 1, sum)) / apply(N, 1, sum)
    CFR[["gross_new"]] <- (apply(CFR[["group_new"]] * N, 1, sum)) / apply(N, 1, sum)
    CFR[["gross.variance"]] <- apply(CFR[["group.variance"]] * N^2, 1, sum) / (apply(N, 1, sum)^2)
    CFR[["gross.lower"]] <- CFR[["gross"]] - 1.96 * sqrt(CFR[["gross.variance"]])
    CFR[["gross.upper"]] <- CFR[["gross"]] + 1.96 * sqrt(CFR[["gross.variance"]])
    CFR[["group.lower"]] <- CFR[["group"]] - 1.96 * sqrt(CFR[["group.variance"]])
    CFR[["group.upper"]] <- CFR[["group"]] + 1.96 * sqrt(CFR[["group.variance"]])
  }else if (model.option == 2){
    CFR <- list(info = "WHO: D / N",
                gross = apply(D, 1, sum) / apply(N, 1, sum),
                group = matrix(0, nrow = nrow(D), ncol = ncol(D)))
    CFR[["group"]] = D/N
  }else if (model.option == 3){
    CFR <- list(info = "WHO: D / (C + D)",
                gross = apply(D, 1, sum) / (apply(C, 1, sum) + apply(D, 1, sum)))
    CFR$gross[is.na(CFR$gross)] <- 0
  }else if (model.option == 4){
    CFR <- list(info = "group-wise WHO: D / N",
                gross = apply(D, 1, sum) / apply(N, 1, sum),            # Same as option 2
                group = D / N)
  }else{
    CFR <- list(info = "group-wise WHO: D / (C + D)",
                gross = apply(D / (C + D) * N, 1, sum) / apply(N, 1, sum),
                group = D / (C + D))
    CFR$gross[is.na(CFR$gross)] <- 0
    for (i in 1:nrow(D)){
      CFR$group[i, is.na(CFR$group[i,])] <- 0
    }
  }
  
  return(CFR)
}

# 3. Prediction of case fatality rate (Model 2)
SCFR_prediction <- function(D, U, C, N, option = 1){
  # This function calculates the predicted values of CFRs based on our 2nd model
  # Input: D - (time) x (group) formed count matrix of deaths
  #        U - (time) x (group) formed count matrix of quarantined patients (confirmed but not recovered nor dead)
  #        C - (time) x (group) formed count matrix of cured patients
  #        N - (time) x (group) formed count matrix of total confirmed patients
  #        option - two options for q hat (estimated fatality rate for uncertain patients) 
  #                 1 (default): number of deaths / number of confirmed patients
  #                 2: number of death / (number of cured patients + number of deaths)
  # Output: CFR.pred - a list of predicted CFRs
  #               CFR.pred[["gross"]] - a (time) x 1 vector of gross predicted CFRs
  #               CFR.pred[["group"]] - a (time) x (group) matrix of group-wise predicted CFRs
  #               CFR.pred[["gross.variance"]] - estimated variances of gross predicted CFRs
  #               CFR.pred[["gross.lower"]] - lower bounds of 95% prediction band for gross predicted CFRs
  #               CFR.pred[["gross.upper"]] - upper bounds of 95% prediction band for gross predicted CFRs
  #               CFR.pred[["group.variance"]] - estimated variances of group-wise CFRs
  #               CFR.pred[["group.lower"]] - lower bounds of 95% prediction band for group-wise predicted CFRs
  #               CFR.pred[["group.upper"]] - upper bounds of 95% prediction band for group-wise predicted CFRs
  
  if (option == 1){
    q.hat <- D / N
  }else if (option == 2){
    q.hat <- D / (C + D)
    q.hat[is.na(q.hat)] <- 0
  }
  CFR.pred <- list(gross = matrix(0, nrow = nrow(D), ncol = 1),
                   group = matrix(0, nrow = nrow(D), ncol = ncol(D)),
                   gross.variance = matrix(0, nrow = nrow(D), ncol = 1),
                   gross.lower = matrix(0, nrow = nrow(D), ncol = 1),
                   gross.upper = matrix(0, nrow = nrow(D), ncol = 1),
                   group.variance = matrix(0, nrow = nrow(D), ncol = ncol(D)),
                   group.lower = matrix(0, nrow = nrow(D), ncol = ncol(D)),
                   group.upper = matrix(0, nrow = nrow(D), ncol = ncol(D)))
  CFR.pred[["group"]] <- (D + U * q.hat) / N
  CFR.pred[["gross"]] <- (apply(D, 1, sum) + apply((U * q.hat), 1, sum)) / apply(N, 1, sum)

  CFR.pred[["group.variance"]] <- U * q.hat * (1 - q.hat) / N
  CFR.pred[["gross.variance"]] <- apply(CFR.pred[["group.variance"]] * N, 1, sum) / apply(N, 1, sum)
  CFR.pred[["gross.lower"]] <- CFR.pred[["gross"]] - 1.96 * sqrt(CFR.pred[["gross.variance"]])
  CFR.pred[["gross.upper"]] <- CFR.pred[["gross"]] + 1.96 * sqrt(CFR.pred[["gross.variance"]])
  CFR.pred[["group.lower"]] <- CFR.pred[["group"]] - 1.96 * sqrt(CFR.pred[["group.variance"]])
  CFR.pred[["group.upper"]] <- CFR.pred[["group"]] + 1.96 * sqrt(CFR.pred[["group.variance"]])
  
  return(CFR.pred)
}


t_col <- function(color, percent = 50, name = NULL) {
  #      color = color name
  #    percent = % transparency
  #       name = an optional name for the color
  
  ## Get RGB values for named color
  rgb.val <- col2rgb(color)
  
  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100 - percent) * 255 / 100)
  
  ## Save the color
  invisible(t.col)
}
