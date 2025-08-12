

read_magpiePolicy <- function(magpierun, magpiepath, magpie_base, magpiepath_base,
                              magpie_includeCO2rev = FALSE){
  #This function computes the price changes of policies scenarios and expendtures
  #of the policy and baseline scenarios.
 
  priceRun <- read_magpiePrice(magpiepath) 
  priceBase <-  read_magpiePrice(magpiepath_base) 
  
  relaPrice <- priceRun / priceBase - 1 
  
  #compute the price change
  dfRun <- as.data.frame(relaPrice) %>%
    as_tibble()%>%
    rename(
      scenario = Cell,
      region = Region,
      year = Year,
      sector = Data1,
      value = Value
    ) %>%
    mutate( sector = case_when(
      sector == "livestock" ~ "Animal products",
      sector == "staples" ~ "Staples",
      sector == "veg" ~ "Fruits vegetables nuts",
      sector == "processed" ~ "Empty calories",
      TRUE ~ sector),
      scenario = magpierun,
            variable = paste0('deltPrice|',sector),
      baseline = magpie_base,
      unit = '%')
  
  #get the absolute price
  dfRun <-  priceRun %>% 
    as.data.frame() %>%
    as_tibble() %>%
    rename(
      scenario = Cell,
      region = Region,
      year = Year,
      sector = Data1,
      value = Value
    )%>%
    mutate( sector = case_when(
      sector == "livestock" ~ "Animal products",
      sector == "staples" ~ "Staples",
      sector == "veg" ~ "Fruits vegetables nuts",
      sector == "processed" ~ "Empty calories",
      TRUE ~ sector),
      scenario = magpierun,
      variable = paste0('Price|',sector),
      baseline = 'NA',
      unit = 'USD17PPP per kcal') %>%
    bind_rows(dfRun)

  #get the expenditure
  dfRun <- read_magpieExpShare(magpiepath) %>%
    as.data.frame() %>%
    as_tibble() %>%
    rename(
      scenario = Cell,
      region = Region,
      year = Year,
      sector = Data1,
      value = Value
    )%>%
    mutate( sector = case_when(
      sector == "livestock" ~ "Animal products",
      sector == "staples" ~ "Staples",
      sector == "veg" ~ "Fruits vegetables nuts",
      sector == "processed" ~ "Empty calories",
      TRUE ~ sector),
      scenario = magpierun,
      variable = paste0('share|',sector),
      baseline = 'NA',
      unit = 'unitless') %>%
    bind_rows(dfRun) 
  
  
  #get tax revenue
  priceGHG <- PriceGHG(magpiepath)
  getNames(priceGHG) <- 
    gsub("co2_c","co2",getNames(priceGHG)) %>% 
    gsub("n2o_n_direct","n2o",.)
  
  emi <- Emissions(magpiepath, type = c('co2_c','n2o_n','ch4'), unit = 'gas', subcategories = TRUE, inorg_fert_split = FALSE)
  emi <- dimOrder(emi, perm = c(2,1))
  
  assert_that(all(getNames(emi) == getNames(priceGHG)))
  
  # tax revenue in million $ MER (only CH4 and N2O, see above)
  if (!magpie_includeCO2rev){
    print("only using CH4 + N2O revenues from MAgPIE (default)")
    taxrev_magpie <- dimSums((emi*priceGHG)[,,c("n2o","ch4")],dim=3)
  } else {
    print("including CO2 LUC revenues from MAgPIE (non-default)")
    taxrev_magpie <- dimSums((emi*priceGHG)[,,c("co2","n2o","ch4")],dim=3)
  }  
   
  
dfRun <-    taxrev_magpie %>% 
  as.data.frame() %>%
  as_tibble() %>%
  select(Region, Year, Value) %>%
  rename(
    region = Region,
    year = Year,
    value = Value
  ) %>%
  mutate( value = value / 1e3,
          sector = 'total',
          scenario = magpierun,
          variable = 'Taxes|GHGenergy|MAGPIE',
          baseline = 'NA',
          unit = 'billion $ MER') %>%
  bind_rows(dfRun) %>% 
  select(-sector)

    
    
  return(dfRun)

}
