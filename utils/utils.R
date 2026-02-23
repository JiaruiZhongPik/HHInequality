#collection of utility functions
#Jiarui Zhong, 2025 - 


cons.perCapAvg.lognorm <- function(I, cbar, sigma.logn){
  # Computes average per capita consumption of I quantiles given a lognormal income distribution
  
  # Compute lognormal mean
  mu.logn <- log(cbar) - sigma.logn^2/2
  
  # Compute quantile bounds
  quantile.bounds <- qlnorm(p = seq(0,1-1e-8,length.out = I+1), meanlog = mu.logn, sdlog = sigma.logn)
  
  #helper function for numerical integration
  int.fx.logn <- function(x) x * dlnorm(x, meanlog = mu.logn, sdlog = sigma.logn)
  
  #preallocate output vector
  cons.quant.lognorm <- numeric(I)
  
  for (i in seq_len(I)){
    cons.quant.lognorm[i]<- integrate(int.fx.logn, quantile.bounds[i], quantile.bounds[i+1])$value * I
  }
  return(cons.quant.lognorm)
}

#utility functions for scenario selection
scenario_combiner <- function(runscen, budget, prestring, addstring) {
  # Ensures all arguments are treated as character strings
  glue("{as.character(prestring)}{as.character(runscen)}-{as.character(budget)}{as.character(addstring)}")
}

findReporting <- function(run,path,pattern){
  #TODO hacked this for standalone runs, see if this can be done in a cleaner way
  x <- list.files(path)
  x <- x[grepl(run,x)]
  if (length(x) > 1) stop("more than one run matches scenario. please clean up or hard code path")
  x <- list.files(file.path(path,x), pattern = glob2rx(pattern), full.names = TRUE)
  if (length(x) == 0){
    x <- NA
  } else {
    # as readRemind() computes Policy Costs separately, I don't need the adjusted policy costs mif
    x <- x[!grepl("withoutPlus|adjustedPolicyCosts",x)]
  }
  return(x)
}

#utility function to capitalize the first letter
capitalize_first <- function(s) {
  paste0(toupper(substring(s, 1, 1)), substring(s, 2))
}


#utility function to compute un/weighted Theil's T and L index
compute_theil.wtd <- function(x, weights = NULL, type = c("T", "L")) { 
  type <- match.arg(type)  # Ensure valid type
  
  if (is.null(weights)) {
    weights <- rep(1, length(x))
  }
  
  # Remove NAs
  valid <- !(is.na(x) | is.na(weights))
  x <- x[valid]
  weights <- weights[valid]
  
  if (!all(weights >= 0)) stop("At least one weight is negative.")
  if (all(weights == 0)) stop("All weights are zero.")
  
  # Select positive x only
  positive <- x > 0
  x_sel <- x[positive]
  weights_sel <- weights[positive]
  
  # Normalize weights
  weights_sel <- weights_sel / sum(weights_sel)
  
  # Compute weighted mean
  mean_x <- stats::weighted.mean(x_sel, weights_sel)
  
  if (type == "T") {
    # Theil T: emphasizes top inequality
    x_norm <- x_sel / mean_x
    theil <- sum(weights_sel * x_norm * log(x_norm))
  } else if (type == "L") {
    # Theil L: emphasizes bottom inequality
    x_norm <- mean_x / x_sel
    theil <- sum(weights_sel * log(x_norm))
  }
  
  return(theil)
}



logit <- function(p, eps = 1e-9) {
  p <- pmin(pmax(p, eps), 1 - eps)
  log(p/(1-p))
}

invLogit <- function(x) 1/(1 + exp(-x))


#Helper : region mapping helper
load_regionMapping <- function(regressRegGrouping) {
  if (regressRegGrouping == "H12") {
    
    readr::read_delim(
      "input/regionmappingH12.csv",
      delim = ";",
      escape_double = FALSE,
      col_types = readr::cols(X = readr::col_skip()),
      trim_ws = TRUE,
      show_col_types = FALSE
    ) %>%
      dplyr::rename(geo = CountryCode, region = RegionCode)
    
  } else if (regressRegGrouping == "H21") {
    readr::read_delim(
      "input/regionmapping_21_EU11.csv",
      delim = ";",
      escape_double = FALSE,
      col_types = readr::cols(X = readr::col_skip(), missingH12 = readr::col_skip()),
      trim_ws = TRUE,
      show_col_types = FALSE
    ) %>%
      dplyr::rename(geo = CountryCode, region = RegionCode)
    
  } else {
    NULL
  }
}

  
  add_regionTag <- function(df, regressRegGrouping, regionMapping = NULL) {
    if (regressRegGrouping == "pool") {
      df %>% dplyr::mutate(region = "pool")
    } else {
      if (is.null(regionMapping)) stop("regionMapping is NULL but regressRegGrouping requires it.")
      df %>% dplyr::left_join(regionMapping, by = "geo")
    }
  }