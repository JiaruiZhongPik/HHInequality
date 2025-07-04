

read_magpiePolicy <- function(magpierun, magpiepath, magpie_base, magpiepath_base){
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
    )%>%
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
    bind_rows(dfRun) %>% 
    select(-sector)
  
  return(dfRun)

}
