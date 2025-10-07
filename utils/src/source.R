######################### P-HACKING TESTS #########################

###### PUBLICATION BIAS - Caliper test (Gerber & Malhotra, 2008) ######

#' Run a Caliper Test
#'
#' This function performs a Caliper test on a data set to detect selective reporting of statistically significant results.
#'
#' @param input_data [data.frame] A data.frame containing the data set to be tested. The data.frame must have at least two columns named
#'  "t_stat" and "study_id", and these columns must be numeric.
#' @param threshold [numeric] The t-statistic threshold used to define statistically significant results. Default is 1.96.
#' @param width [numeric] The width of the Caliper interval used to define the sub-sample of observations used in the test. Default is 0.05.
#' @param add_significance_marks [logical] If TRUE, calculate significance levels and mark these in the tables.
#'  Defaults to T.
#' @return A numeric vector with four elements: the estimate of the proportion of results reported, the standard error of the estimate,
#' the number of observations with t-statistics above the threshold, and the number of observations with t-statistics below the threshold.
runCaliperTest <- function(input_data, threshold = 1.96, width = 0.05, add_significance_marks = T) {
  # Validate input
  required_cols <- getDefaultColumns()
  stopifnot(
    is.data.frame(input_data),
    is.numeric(threshold),
    is.numeric(width),
    is.logical(add_significance_marks),
    all(required_cols %in% colnames(input_data))
  )
  # Add a column indicating which observations have t-stats above (below) threshold
  if (threshold >= 0) { # Explicit because ifelse does not work, due to some dark spells probably
    significant_obs <- input_data$t_stat > threshold
  } else {
    significant_obs <- input_data$t_stat < threshold
  }
  input_data$significant_t <- ifelse(significant_obs, 1, 0) # Col of 0/1
  # Initialize variables for output storage
  caliper_output <- list()
  # Run the test
  lower_bound <- input_data$t_stat > (threshold - width) # Bool vector
  upper_bound <- input_data$t_stat < (threshold + width) # Bool vector
  subsetted_data <- input_data[lower_bound & upper_bound, ] # Only desired rows
  if (nrow(subsetted_data) == 0) {
    return(c(0, 0, 0, 0)) # No observations in the interval
  }
  cal_res <- lm(formula = significant_t ~ t_stat - 1, data = subsetted_data)
  cal_res_coefs <- coeftest(cal_res, vcov = vcovHC(cal_res, type = "const", cluster = c(input_data$study_id)))
  cal_est <- round(cal_res_coefs["t_stat", "Estimate"], 3) # Estimate
  cal_se <- round(cal_res_coefs["t_stat", "Std. Error"], 3) # Standard Error
  cal_above <- nrow(subsetted_data[subsetted_data$t_stat > threshold, ]) # N. obs above the threshold
  cal_below <- nrow(subsetted_data[subsetted_data$t_stat < threshold, ]) # N. obs below the threshold
  # Add significance marks if desired
  if (add_significance_marks) {
    cal_est <- add_asterisks(cal_est, cal_se)
  }
  # Return the output
  res <- c(
    cal_est,
    cal_se,
    cal_above,
    cal_below
  )
  invisible(res)
}

#' Run Caliper tests across all thresholds and widths and store the output in a data frame
#'
#' @param input_data [data.frame] A data frame containing the input data.
#' @param thresholds [vector] A numeric vector containing the thresholds at which the caliper tests are to be run.
#'                   Defaults to c(0, 1.96, 2.58).
#' @param widths [vector] A numeric vector containing the caliper widths at which the tests are to be run.
#'               Defaults to c(0.05, 0.1, 0.2).
#' @param display_ratios [bool] A logical value indicating whether to display ratios of the number of estimates
#'  found on each side of the interval. The alternative is to display the sum of numbers (occurances) within
#'  the whole interval. Defaults to FALSE.
#' @param verbose [bool] A logical value indicating whether the results should be printed to the console.
#'                Defaults to TRUE.
#' @param add_significance_marks [logical] If TRUE, calculate significance levels and mark these in the tables.
#'  Defaults to T.
#'
#' @return A data frame with dimensions nrow = length(widths) * 3 and ncol = length(thresholds),
#'         where the rows are named with the caliper width and its estimate, standard error, and n1/n2 ratio,
#'         and the columns are named with the corresponding thresholds.
getCaliperResults <- function(
    input_data,
    thresholds = c(0, 1.96, 2.58),
    widths = c(0.05, 0.1, 0.2),
    display_ratios = F,
    verbose = T,
    add_significance_marks = T) {
  # Validate the input
  stopifnot(
    is.data.frame(input_data),
    is.vector(thresholds),
    is.vector(widths),
    is.numeric(thresholds),
    is.numeric(widths),
    is.logical(display_ratios),
    is.logical(verbose),
    is.logical(add_significance_marks)
  )
  # Initialize the output data frame
  num_thresholds <- length(thresholds)
  num_widths <- length(widths)
  result_df <- data.frame(matrix(ncol = num_thresholds, nrow = num_widths * 3))
  colnames(result_df) <- paste0("Threshold ", thresholds)
  rownames_vec <- c()
  for (width in widths) {
    rows <- c(
      paste0("Caliper width ", width, " - Estimate"),
      paste0("Caliper width ", width, " - SE"),
      paste0("Caliper width ", width, ifelse(display_ratios, " - n1/n2", " - n total"))
    )
    rownames_vec <- append(rownames_vec, rows)
  }
  rownames(result_df) <- rownames_vec
  # Run caliper tests for all thresholds and widths
  for (i in 1:num_thresholds) {
    for (j in 1:num_widths) {
      caliper_res <- runCaliperTest(input_data,
        threshold = thresholds[i], width = widths[j],
        add_significance_marks = add_significance_marks
      )
      lcount <- as.numeric(caliper_res[3]) # Left interval
      rcount <- as.numeric(caliper_res[4]) # Right interval
      ncount <- ifelse(display_ratios, paste0(lcount, "/", rcount), as.character(sum(lcount, rcount))) # Count in intervals
      result_df[j * 3 - 2, i] <- caliper_res[1] # Estimate
      result_df[j * 3 - 1, i] <- paste0("(", caliper_res[2], ")") # Standard Error
      result_df[j * 3, i] <- ncount # n1/n2 or  n1+n2
    }
  }
  # Verbose output
  if (verbose) {
    getCaliperResultsVerbose(result_df, verbose = verbose)
  }
  # Return the data frame
  return(result_df)
}

#' Verbose output for the getCaliperResults function
getCaliperResultsVerbose <- function(res, ...) {
  args <- list(...)
  verbose_on <- args$verbose
  # Verbose output
  if (verbose_on) {
    print("Results of the Caliper tests:")
    print(res)
    cat("\n\n")
  }
}

###### PUBLICATION BIAS - p-hacking test (Elliott et al., 2022) ######

#' getElliottResults - Calculate Elliott's five tests and other statistics for a given dataset
#'  - Source: https://onlinelibrary.wiley.com/doi/abs/10.3982/ECTA18583
#'
#' @param input_data A data frame containing at least the "t_stat" column.
#' @param script_path Full path to the source script.
#' @param temp_data_path Store temporary output here.
#' @param data_subsets A character vector with the names of the subsets of data to test. By default, only "All data" is tested.
#' @param p_min The minimum p-value threshold for the tests. Default is 0.
#' @param p_max The maximum p-value threshold for the tests. Default is 1.
#' @param d_point The discontinuity cutoff point for the discontinuity test. Default is 0.15.
#' @param CS_bins The number of bins for the Cox-Shi test. Default is 10.
#' @param verbose A logical indicating whether to print the results to console. Default is TRUE.
#'
#' @return A data frame with the results of the Elliott tests and other statistics.
getElliottResults <- function(input_data, script_path, temp_data_path, data_subsets = c("All data"),
                              p_min = 0, p_max = 1, d_point = 0.15, CS_bins = 10, verbose = T) {
  # Validate input
  stopifnot(
    is.data.frame(input_data),
    is.character(script_path),
    is.character(temp_data_path),
    is.vector(data_subsets),
    is.numeric(p_min),
    is.numeric(p_max),
    is.numeric(d_point),
    is.numeric(CS_bins),
    is.logical(verbose),
    all("t_stat" %in% colnames(input_data))
  )
  # Static values, not necessary to adjust (probably)
  id <- 1 # No dependence
  h <- 0.01
  lcm_norm <- 8
  # Create the data frame with the appropriate dimensions and labels
  # Rownames
  threshold1_verbose <- paste0("Observations in [", p_min, ", ", p_max, "]")
  threshold2_verbose <- paste0("Observations <= ", d_point)
  data_rownames <- c(
    "Binomial:",
    "s Test:",
    "Discontinuity:",
    "CS1:",
    "CS2B:",
    "LCM:",
    threshold1_verbose,
    threshold2_verbose
  )
  # Colnames
  data_colnames <- data_subsets
  # DF
  elliott_df <- data.frame(matrix(NA, nrow = length(data_rownames), ncol = length(data_colnames)))
  rownames(elliott_df) <- data_rownames
  colnames(elliott_df) <- data_colnames
  # Load the source script
  source(script_path)
  # Load the file with CDFs (if it does not exist, create one)
  validateFolderExistence(temp_data_path) # Validate cache folder existence
  elliott_source_file <- paste0(temp_data_path, "elliott_data_temp.csv")
  # On the first run, create a cached file of CDFs (large in memory)
  if (!file.exists(elliott_source_file)) {
    print(paste0("Creating a temporary file in the '", temp_data_path, "' folder for the Elliott et al. (2022) method..."))
    cdfs <- getCDFs() # Generate the file from scratch (takes time)
    write.table(cdfs, elliott_source_file, col.names = "cdfs", row.names = F, sep = ";", dec = ".")
  }
  cdfs <- read.csv(elliott_source_file, col.names = "cdfs", sep = ";", dec = ".") # Read the cached file
  cdfs <- as.numeric(cdfs[, 1]) # To a numeric vector
  # Run the estimation for all data subsets
  for (data_col in data_colnames) {
    # Get the data subset
    data <- input_data # Adjust later if desired

    # Convert t-statistics to p-values
    t_stat <- data$t_stat
    df <- ifelse("reg_df" %in% colnames(data), data$reg_df, data$n_obs) # Nobs if DoF not available
    P <- 2 * pt(abs(t_stat), df = df, lower.tail = FALSE) # p-values

    # Tests (each test returns the corresponding p-value)
    Bin_test <- Binomial(P, p_min, p_max, "c")
    Discontinuity <- Discontinuity_test(P, d_point, h)
    LCM_sup <- LCM(P, p_min, p_max, lcm_norm, cdfs)
    CS_1 <- CoxShi(P, id, p_min, p_max, CS_bins, 1, 0) # Test for 1-non-increasingness
    CS_2B <- CoxShi(P, id, p_min, p_max, CS_bins, 2, 1) # Test for 2-monotonicity and bounds
    FM <- Fisher(P, p_min, p_max)

    # Save the results
    elliott_res <- c(Bin_test, Discontinuity, LCM_sup, CS_1, CS_2B, FM)
    elliott_res <- sapply(elliott_res, function(x) {
      round(x, 3)
    })

    # Thresholds
    n_obs_between <- length(P[P >= p_min & P <= p_max])
    n_obs_below <- length(P[P <= d_point & P >= 0])

    # Fill in the data frame with values from elliott_res
    elliott_df["Binomial:", data_col] <- elliott_res[1]
    elliott_df["Discontinuity:", data_col] <- elliott_res[2]
    elliott_df["LCM:", data_col] <- elliott_res[3]
    elliott_df["CS1:", data_col] <- elliott_res[4]
    elliott_df["CS2B:", data_col] <- elliott_res[5]
    elliott_df["s Test:", data_col] <- elliott_res[6]
    elliott_df[threshold1_verbose, data_col] <- n_obs_between # In between min, max
    elliott_df[threshold2_verbose, data_col] <- n_obs_below # Below disc cutoff
  }

  # Verbose output
  if (verbose) {
    getElliottResultsVerbose(elliott_df, verbose = verbose)
  }
  # Return the data frame
  return(elliott_df)
}

#' Verbose output for the getElliottResults function
getElliottResultsVerbose <- function(res, ...) {
  args <- list(...)
  verbose_on <- args$verbose
  # Print out the output
  if (verbose_on) {
    print(paste0("Results of the Elliott tests:"))
    print(res)
    cat("\n\n")
  }
}


###### MAIVE Estimator (Irsova et al., 2023) ######

#' Run the MAIVE estimation using a modified source script
#'  - Source: http://meta-analysis.cz/maive/
#'
#' @param method [int] Method. Options - PET:1, PEESE:2, PET-PEESE:3, EK:4 (default 3)
#' @param script_path [character] Full to the source script.
#' @param weight [int] Weighting. Options - no weight: 0 ; weights: 1, adjusted weights: 2 (default 0)
#' @param instrument [int] Instrumenting. Options - 0;1(default 1)
#' @param studylevel[int] Correlation at study level. Options -  none: 0 (default), fixed effects: 1, cluster: 2
#'  (default 0)
#' @param verbose [bool] Print out the results into the console in a nice format.
#' @param add_significance_marks [logical] If TRUE, calculate significance levels and mark these in the tables.
#'  Defaults to T.
#' @inheritDotParams Parameters for the extractExoCoefs function.
#'
#' @import maive_master_thesis_cala.R
getMaiveResults <- function(input_data, script_path,
                            method = 3, weight = 0, instrument = 1, studylevel = 2,
                            verbose = T, add_significance_marks = T, ...) {
  # Read the source file
  source(script_path)
  # Validate that the necessary columns are present
  required_cols <- getDefaultColumns()
  stopifnot(
    all(required_cols %in% names(input_data)),
    is.character(script_path),
    method %in% c(1, 2, 3, 4),
    weight %in% c(0, 1, 2),
    instrument %in% c(0, 1),
    studylevel %in% c(0, 1, 2),
    is.logical(verbose),
    is.logical(add_significance_marks)
  )
  # Subset data and rename columns
  input_data <- input_data[, c("effect", "se", "n_obs", "study_id")]
  colnames(input_data) <- c("bs", "sebs", "Ns", "studyid")
  # Run the estimation
  MAIVE <- maive(dat = input_data, method = method, weight = weight, instrument = instrument, studylevel = studylevel)
  # Add significance marks if desired
  if (add_significance_marks) {
    MAIVE$beta <- add_asterisks(MAIVE$beta, MAIVE$SE)
  }
  # Extract (and print) the output
  object <- c(
    "MAIVE coefficient", "MAIVE standard error", "F-test of first step in IV",
    "Hausman-type test (use with caution)", "Critical Value of Chi2(1)"
  )
  maive_coefs_all <- c(MAIVE$beta, MAIVE$SE, MAIVE$`F-test`, MAIVE$Hausman, MAIVE$Chi2)
  MAIVEresults <- data.frame(object, maive_coefs_all)
  colnames(MAIVEresults) <- c("Object", "Coefficient")
  # Verbose output
  if (verbose) {
    getMaiveResultsVerbose(MAIVEresults, verbose = verbose)
  }
  # Return the data frame
  return(MAIVEresults)
}

#' Verbose output for the getMaiveResults function
getMaiveResultsVerbose <- function(res, ...) {
  args <- list(...)
  verbose_on <- args$verbose
  # Print out the output
  if (verbose_on) {
    print(paste0("Results of the MAIVE estimator:"))
    print(res)
    cat("\n\n")
  }
}
