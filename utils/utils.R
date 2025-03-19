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
