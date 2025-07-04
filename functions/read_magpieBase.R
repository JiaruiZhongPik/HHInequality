#This function prepares the price and expenditure from baseline scenarios.

read_magpieBase <- function(magpie_base, magpiepath_base){

  priceBase <-  read_magpiePrice(magpiepath_base) 
  
  #get the absolute prpice
  dfRun <- priceBase %>% 
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
      scenario = magpie_base,
      variable = paste0('Price|',sector),
      baseline = 'NA',
      unit = 'USD17PPP per kcal' )

  #get the expenditure
  dfRun <- 
  read_magpieExpShare(magpiepath_base) %>%
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
      scenario = magpie_base,
      variable = paste0('share|',sector),
      baseline = 'NA',
      unit = 'unitless') %>%
    bind_rows(dfRun) %>% 
    select(-sector)
  
  return(dfRun)

}
