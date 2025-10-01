#-------------------#
# stem-based bias correction method
# This R file contains all functions necessary 
# (i) to run the stem-based bias correction method; and
# (ii) to generate related figures
# The structure of folder is as follows:
# 
##0. assign default technical parameters for estimation
#    (param)
##1. outer algorithm
#    - stem
#    - stem_converge
##2. inner algorithm
#    - stem_compute
#    - variance_b
#    - variance_0
#    - weighted_mean
#    - weighted_mean_squared
##3. figures
#    (install package)
#    - se_rescale
#    - stem_funnel
#    - stem_MSE
##4. auxiliary function
#    - data_median
# author: Chishio Furukawa
# contact: cfurukawa@mit.edu
#-------------------#

#### ADDED EXTERNALLY, JUST FOR CONFLICTLESS PACKAGE HANDLING ####
load_packages <- function(package_list) {
  # Install packages not yet installed
  installed_packages <- package_list %in% rownames(installed.packages())
  if (any(installed_packages == FALSE)) {
    print(paste("Installing package ", package_list[!installed_packages],"...", sep = ""))
    install.packages(package_list[!installed_packages])
  }
  # Package loading
  invisible(lapply(package_list, library, character.only = TRUE))
}

load_packages(c('ggplot2', 'data.table'))

#### END OF PACKAGE HANDLING ####

##0 set stem parameters
# param <- c(
#  10^(-4), # Tolerance #set level of sufficiently small stem to determine convergence
#  10^3 # max_N_count #set maximum number of iteration before termination
#  )

##1 outer algorithm
stem <- function(beta, se, param) {
  #Initial Values
  N_study <- length(beta)
  
  # sending sigma0->infinity implies equal weights to all studies
  beta_equal <- mean(beta)
  max_sigma_squared <- variance_0(N_study, beta, se, beta_equal)
  max_sigma = sqrt(max_sigma_squared)
  min_sigma = 0
  tolerance = param[1]
  
  #Sorting data by ascending order of standard error
  data1 <- cbind(beta,se)
  data_sorted <- data1[order(data1[,2]),]
  beta_sorted <- data_sorted[,1]
  se_sorted <- data_sorted[,2]
  
  #Compute stem based estimates until convergence from max and min of sigma
  output_max <- stem_converge(max_sigma, beta_sorted, se_sorted, param)
  output_min <- stem_converge(min_sigma, beta_sorted, se_sorted, param)
  Y_max <- output_max$estimates
  Y_min <- output_min$estimates
  
  
  #Check whether max and min agree
  diff_sigma <- abs(Y_max[3] -  Y_min[3])
  if (diff_sigma > (2*tolerance)) {
    multiple = 1
  }
  else{
    multiple = 0
  }
  
  #information in sample
  n_stem <- Y_max[4]
  sigma0 <- Y_max[3]
  inv_var <- 1/(se_sorted^2+sigma0^2)
  info_in_sample = sum(inv_var[1:n_stem])/sum(inv_var)
  
  #Return
  Y1 = c(Y_max, multiple, info_in_sample)
  Y2 <- t(Y1)
  Z1 <- output_max$MSE
  Z2 <- t(Z1)
  
  colnames(Y2) <- c("estimate","se", "sd of total heterogeneity", "n_stem", "n_iteration", "multiple", "% info used")
  colnames(Z2) <- c("MSE", "variance", "bias_squared")
  output <- list("estimates" = Y2, "MSE" = Z2)
  return(output)
}

stem_converge <- function(initial_sigma, beta_sorted, se_sorted, param) {
  converged = 0
  N_count = 0
  tolerance = param[1]
  max_N_count = param[2]
  sigma0 = initial_sigma
  
  while (converged == 0) {
    output <- stem_compute(beta_sorted, se_sorted, sigma0)
    Y_stem <- output$estimates
    sigma = Y_stem[3]
    evolution = abs(sigma0 - sigma)
    N_count = N_count + 1
    
    if (evolution<tolerance) {
      converged = 1
    }
    else if (N_count > max_N_count) {
      converged = 1
    }
    else{
      sigma0 = sigma
    }
  }
  Y <- c(Y_stem, N_count)
  Z <- output$MSE
  output <- list("estimates" = Y, "MSE" = Z)
  return(output)
}


##2 inner algorithm
stem_compute <- function(beta, se, sigma0) {
  
  N_study = length(beta)
  
  # relevant bias squared
  Eb_all = weighted_mean(beta, se, sigma0)
  Eb_leave_top_out = weighted_mean(beta[2:N_study], se[2:N_study], sigma0)
  Eb_squared = weighted_mean_squared(beta[2:N_study], se[2:N_study], sigma0)
  MSE_original = Eb_squared - 2*beta[1]*Eb_leave_top_out
  # since Eb_squared[1] cannot be computed without bias
  
  # variance
  Var_all = variance_b(se, sigma0)
  
  # minimize MSE
  n_stem_min = 3
  MSE = MSE_original[(n_stem_min-1):(N_study-1)]
  Bias = MSE - Var_all[n_stem_min:N_study]
  index <- which.min(MSE)
  n_stem = index+(n_stem_min-1)
  
  # assign values
  beta_stem = Eb_all[n_stem]
  se_stem = Var_all[n_stem]^(0.5)
  var_stem = variance_0(N_study, beta, se, beta_stem)
  sigma_stem = sqrt(var_stem)
  
  # stack outputs
  Y = cbind(beta_stem,se_stem,sigma_stem,n_stem)
  Z = rbind(MSE, Var_all[n_stem_min:N_study], Bias[(n_stem_min-1):(N_study-1)])
  output <- list("estimates" = Y, "MSE" = Z)
  return(output)
}

variance_b <- function(se, sigma) {
  N_study <- length(se)
  Y <- vector(mode = 'numeric', length = N_study)
  proportional_weights = 1/(se^2 + sigma^2)
  
  for (i in 1:N_study) {
    Y[i] <- 1/sum(proportional_weights[1:i])
  }
  return(Y)
}


variance_0 <- function(n_stem, beta, se, beta_mean) {
  # formula adopted from DerSimonian and Laird (1996)
  weights <- 1/(se[1:n_stem]^2)
  total_weight = sum(weights)
  
  Y1 <- (t(weights) %*% (beta[1:n_stem] - beta_mean)^2) - (n_stem - 1)
  Y2 <- total_weight - (t(weights) %*% weights)/total_weight
  var = pmax(0, Y1/Y2)
  
  Y <- var
  return(Y)
}

weighted_mean <- function(beta, se, sigma) {
  N_study <- length(beta)
  Y <- vector(mode = 'numeric', length = N_study)
  
  proportional_weights <- 1/(se^2 + sigma^2)
  
  for (i in 1:N_study) {
    Y[i] <- beta[1:i] %*% proportional_weights[1:i]/sum(proportional_weights[1:i])
  }
  return(Y)
}

# New function for calculating partial matrix sums
compute_submat_sums <- function(mat) {
  # calculate cumulative sums over rows and columns
  f <- function(x) {cumsum(as.numeric(x))}
  sum1 <- apply(mat, 2, f) # Sum over cols
  sum2 <- apply(sum1, 1, f) # Sum over rows
  out <- diag(sum2)
  return(out)
}

# New function for weighted mean of squared matrixes
weighted_mean_squared <- function(beta, se, sigma) {
  N <- length(beta)
  Y <- vector(mode = 'numeric', length = N)
  
  weights <- 1/(se^2 + sigma^2)
  weights_beta <- weights*beta
  
  W <- weights %o% weights
  WB <- weights_beta %o% weights_beta
  
  W_csum <- compute_submat_sums(W)
  WB_csum <- compute_submat_sums(WB)
  
  # Create cumulative sum of weights_beta squared - works fine
  weights_beta_csum <- cumsum(weights_beta^2)
  weights_csum <- cumsum(weights^2)
  
  # Preallocate vector
  Y1 <- numeric(N - 1)
  Y2 <- numeric(N - 1)
  
  # Perform the operation
  Y1 <- WB_csum - weights_beta_csum
  Y2 <- W_csum - weights_csum
  Y <- Y1 / Y2
  Y[1] <- 0
  
  return(Y)
}

# Original function for weighted mean of squared matrixes
weighted_mean_squared_orig <- function(beta, se, sigma) {
  N <- length(beta)
  Y <- vector(mode = 'numeric', length = N)
  
  weights <- 1/(se^2 + sigma^2)
  weights_beta <- weights*beta
  
  W <- weights %o% weights
  WB <- weights_beta %o% weights_beta
  
  for (i in 2:N) {
    Y1 <- sum(WB[1:i,1:i]) - sum(weights_beta[1:i]^2)
    Y2 <- sum(W[1:i,1:i]) - sum(weights[1:i]^2)
    Y[i] <- Y1/Y2
  }
  return(Y)
}

##3. figures
#install.packages("ggplot2")
#library(ggplot2)

se_rescale <- function(se) {
  Y <- -log10(se)
  return(Y)
}

stem_funnel <- function(beta_input, se_input, stem_estimates, theme, legend_pos = "topleft") {
  #take stem estimates
  b_stem <- stem_estimates[1]
  SE_b_stem <- stem_estimates[2]
  sigma0 <- stem_estimates[3]
  n_stem <- stem_estimates[4]
  
  #cumulative estimates
  data_input <- cbind(beta_input,se_input)
  data_sorted <- data_input[order(data_input[,2]),]
  beta_sorted <- data_sorted[,1]
  se_sorted <- data_sorted[,2]
  cumulative_estimates = weighted_mean(beta_sorted, se_sorted, sigma0)
  
  #set values for figures
  t_stat <- 1.96
  lineswidth<-2.5
  filled_diamond <- 18
  points_size <-2
  se_axis_min <- 0
  beta_axis_min <- min(beta_input) - min(beta_input) / 15
  beta_axis_max <- max(beta_input) + max(beta_input) / 15
  labNames <- c('Coefficient ','Precision ')
  
  # rescale SE for ease of visual interpretation
  se_axis <- se_rescale(se_sorted)
  
  #--------------
  # plot
  #--------------
  # Study color, stem based color, augment color
  color_spectrum <- switch(theme,
    blue = c("#005CAB", "#e8e813", "#4a19bd"),
    yellow = c("#AB9800", "#009B0F", "#4dacfa"),
    green = c("#009B0F", "#AB0000", "#423091"),
    red = c("#AB0000", "#6A0DAB", "#8cb1fa"),
    purple = c("#6A0DAB", "#005CAB", "#fa8ce6"),
    stop("Invalid theme type.")
  )
  
  plot.new()
  # adjust margin
  par(mar=c(4.1,4.1,1,1), bg = rgb(255, 255, 255, maxColorValue = 255))
  # plot studies
  study_color <- color_spectrum[1]
  plot(beta_sorted, se_axis, 
       col=study_color, pch = 1, lwd = 2.5,
       xlim= c(beta_axis_min, beta_axis_max),
       xlab=substitute(paste(name, beta), list(name=labNames[1])),
       ylab=substitute(paste(name, -log(SE)), list(name=labNames[2]))) #@@@@@@ modify this line if rescaling with a different function)     
  #stem (cumulative estimates)
  lines(cumulative_estimates, se_axis, col=rgb(96, 96, 96, maxColorValue = 255), lwd=lineswidth)
  #augment stem
  augment_color <- color_spectrum[3]
  points(b_stem, se_axis[n_stem], pch=filled_diamond ,col=augment_color, cex = points_size)
  segments(b_stem, se_axis[1], b_stem, se_axis_min,col=augment_color, lwd=lineswidth)
  #stem-based estimate
  stem_est_color <- color_spectrum[2] # External color
  points(b_stem, se_axis[1], pch=filled_diamond ,col=stem_est_color, cex = points_size)
  segments(b_stem-t_stat*SE_b_stem, se_axis[1], b_stem+t_stat*SE_b_stem, se_axis[1],col=stem_est_color, lwd=lineswidth)
  #zero line
  abline(v=0, col=rgb(192, 192, 192, maxColorValue = 255), lty=2, lwd=lineswidth)
  
  #legend
  legend(legend_pos, #@@@@@@ modify this line if want to put legend in a different location
         legend = c("stem-based estimate","95 confidence interval","cumulative estimate", "minimal precision", "study") , 
         col = c(stem_est_color, 
                 stem_est_color,
                 rgb(96, 96, 96, maxColorValue = 255) ,
                 augment_color,  
                 study_color) , 
         bty = "n", 
         lty = c(NA, 1, 1, NA, NA), lwd = c(NA, 2, 2, NA, 2.5),
         pch= c(18, NA, NA, 18, 1), 
         pt.cex = 1.8, cex = 0.8, horiz = FALSE, inset = c(0, 0),
         y.intersp=1.5)
  
}

stem_MSE <- function(V) {
  MSE = V[,1]
  bias_squared = V[,3]
  variance = V[,2]
  N_study = dim(V)[1]
  
  #figure input
  N_min = 2
  lineset <- 2.5
  num_study <- (N_min+1):(N_study+1)
  
  layout(matrix(c(1,2,3,3), 2, 2, byrow = TRUE))
  plot(num_study,bias_squared[N_min:N_study],type='l',  col="blue", lwd = lineset, xlab = 'Num of included studies i', ylab='', main= expression(Bias^2 - b[0]^2))
  plot(num_study,variance[N_min:N_study],type='l',  col="blue", lwd = lineset, xlab = 'Num of included studies i', ylab='', main = expression(Variance))
  plot(num_study,MSE[N_min:N_study],type='l',  col="blue", lwd = lineset, xlab = 'Num of included studies i', ylab='', main = expression(MSE - b[0]^2))
  
}



#4. auxiliary function
#install.packages("data.table")
#library("data.table")

data_median <- function(data, id_var, main_var, additional_var) {
  
  #1. drop rows with any NA
  complete_data <- na.omit(data)
  
  #2. rename columns
  column_id <- eval((substitute(complete_data[a], list(a = id_var))))
  colnames(column_id)[1] <- "id"
  column_main <- eval((substitute(complete_data[a], list(a = main_var))))
  colnames(column_main)[1] <- "main"
  column_additional <- eval((substitute(complete_data[a], list(a = additional_var))))
  colnames(column_additional)[1] <- "additional"
  
  columns_main_merged <- merge(column_id, column_main, by=0, all=TRUE) 
  columns_additional_merged <- merge(column_id, column_additional, by=0, all=TRUE) 
  
  #3. choose median of main_var along with id
  median_only <- aggregate(main~id,columns_main_merged,median)
  
  #4. merge with complete data
  median_together <- merge(median_only, columns_main_merged, by.x="id", by.y="id")
  median_all <- merge(median_together, columns_additional_merged, by.x="Row.names", by.y="Row.names")
  
  #5. choose data closest to the computed median
  median_all$diff_squared <- (median_all$main.x - median_all$main.y)^2
  table_form <- data.table(median_all)
  median_combined <- table_form[ , .SD[which.min(diff_squared)], by = id.x]
  
  #6. clean before output
  median_combined2 <- median_combined[order(median_combined$id.x),]
  median_combined3 <- median_combined2[, c("id.x", "main.x", "additional")]
  colnames(median_combined3)[1] <- "ID"
  colnames(median_combined3)[2] <- "coefficient"
  colnames(median_combined3)[3] <- "standard_error"
  Y <- median_combined3
  
  return(Y)
}