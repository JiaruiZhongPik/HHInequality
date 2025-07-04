#This scripts prepares Gini data
#There are 3 available sources, for SSP - Rao et al and IIASA, for SDP Min et al.
#This script should generate the SSP and SDP gini data and convert them to
#consumption shares for 10 deciles in each region


prepare_GiniSDP <- function(){
  
  x <- read.csv("input/SDP Gini_GDP pathways v1.0.csv") %>%
    select(-Country, -GDP.pathway...bil., -Year.when.SDG1.achieved) %>%
    rename(region = iso3c,
           scenario = Scenario,
           year = Year,
           value = Gini.pathway) %>%
    filter(!scenario %in% c('SSP2')) %>%
    as.magpie(spatial = 1, temporal = 3) %>%
    time_interpolate(interpolated_year =c(seq(2000, 2019, by = 1), seq(2105, 2150, by = 5)),
                     integrate_interpolated_years = TRUE, extrapolation_type = "constant"
    ) 
  
  getItems(x, dim = 1) <- countrycode::countrycode(getRegions(x),
                                                   origin = "wb",
                                                   destination = "iso3c",
                                                   custom_match = c("SOM" = "SOM"))
  getItems(x, dim = 3)<-gsub("^SSP1$", "SDP", getItems(x, dim = 3))
  getItems(x, dim = 3) <- gsub("-", "_", getItems(x, dim = 3))
  
  getSets(x)[1] <- "iso3c"
  #----------Convert Gini-----------
  
  # fill in missing country 
  
  x <- toolCountryFill(x, fill = 1e-8, verbosity = 2)
  
  x <- x[, seq(2020, 2150, by = 5), ]
  
  # convert to 0..1 range
  x <- x / 100
  
  
  #------------CalcTheil--------------
  
  ##  helper functions.
  TheilT.from.sigma <- function(sigma) {
    # Theil T coefficient for lognormal distribution
    TheilT <- sigma^2 / 2.
    return(TheilT)
  }
  
  sigma.from.Gini <- function(G) {
    # assuming lognormal distribution: convert Gini to sigmas
    sigma <- sqrt(2) * qnorm((G + 1) / 2)
    return(sigma)
  }
  
  years <- getYears(x)
  TheilT <- TheilT.from.sigma(sigma.from.Gini(x))
  
  y <- getYears(TheilT)
  gdp      <- calcOutput("GDP", scenario = 'SDPs', extension2150 = "constant", years = y, aggregate = FALSE)
  getSets(gdp)[3] <- "scenario"
  gdpReg   <- calcOutput("GDP", scenario = 'SDPs', extension2150 = "constant", years = y)
  getSets(gdpReg)[3] <- "scenario"
  gdppc    <- calcOutput("GDPpc", scenario = 'SDPs', extension2150 = "constant", years = y, aggregate = FALSE)
  getSets(gdppc)[3] <- "scenario"
  gdppcReg <- calcOutput("GDPpc", scenario = 'SDPs', extension2150 = "constant", years = y)
  getSets(gdppcReg)[3] <- "scenario"
  
  # Allocate empty objects for storing Theil contribution and weights
  contribTheilT <- TheilT * NA
  weight <- TheilT * NA
  
  # Compute Theil contribution and weights
  regionmapping <- toolGetMapping(getConfig("regionmapping"), type = "regional", where = "mappingfolder")
  regionmapping_aligned <- regionmapping[regionmapping$CountryCode %in% getCells(gdp),]
  
  for (rr in getRegions(gdppcReg)) {
    rrCountries <-   regionmapping_aligned$CountryCode[regionmapping_aligned$RegionCode == rr]
    # Contribution to Theil index (unweighted)
    contribTheilT[rrCountries, , ] <- TheilT[rrCountries, , ] + log(gdppc[rrCountries, , ] / gdppcReg[rr, , ])
    # Weights = country shares of regional GDP
    weight[rrCountries, , ] <- gdp[rrCountries, , ] / gdpReg[rr, , ]
    # Sanity check: ensure that weights for a region sum to one (within floating point precision)
    stopifnot(max(abs(dimSums(weight[rrCountries, , ], dim = 1) - 1)) < 1e-10)
  }
  
  
  #--------------------calcConsShare-----------------------------------
  
  TheilTReg = toolAggregate(contribTheilT, rel = regionmapping_aligned, weight = weight)
  
  # Calculate regional sigma from Theil
  sigma <- sqrt(2 * TheilTReg)
  
  # helper function compute 10 income group share given sigma
  decShareFromSigma <- function(sigma) {
    # assume any value of mu, which doesn't affect the result
    mu <- 1
    
    # decile boundaries
    deciles <- stats::qlnorm(seq(0, 1, by = 0.1), meanlog = mu, sdlog = sigma)
    
    # function for the integrand x * f(x)
    integrand <- function(x) {
      x * stats::dlnorm(x, meanlog = mu, sdlog = sigma)
    }
    
    # Compute shares by integrating over each decile range
    incomeShares <- numeric(length(deciles) - 1)
    for (i in 2:length(deciles)) {
      incomeShares[i - 1] <- stats::integrate(integrand, lower = deciles[i - 1], upper = deciles[i])$value
    }
    
    # Normalize
    normalizedShares <- incomeShares / sum(incomeShares)
    
    return(normalizedShares)
  }
  
  consShare <- array(NA, dim = c(dim(sigma), 10),
                     dimnames = c(dimnames(sigma), list("decile" = 1:10)))
  
  # Applying function across each Theil
  for (i in seq_len(dim(sigma)[1])) {
    for (j in seq_len(dim(sigma)[2])) {
      for (k in seq_len(dim(sigma)[3])) {
        consShare[i, j, k, ] <- decShareFromSigma(sigma[i, j, k])
      }
    }
  }
  
  # Check whether shares equal 1
  sum <- apply(consShare, c(1, 2, 3), sum)
  
  if (any(abs(sum - 1) > 1e-6)) {
    warning("Some entries do not sum to 1 over the 4th dimension.")
  }
  
  df <- as.data.frame.table(consShare, responseName = "value") %>%
    rename(region = iso3c,
           period = year,
           decileGroup = decile,
           gdp_scenario = scenario,
           consShare = value)%>%
    mutate(period = as.integer(sub("y", "", period)),
           decileGroup = as.integer(decileGroup))
  
  return(df)
  
  
}

