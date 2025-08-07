# This function gets the MCC estimate for the enegel curve and fix missing values
# with internal estimates
# Jiarui Zhong

get_estimateMcc <- function(regions = 'H12'){
 
   if(regions == 'H12'){
    
     region <- read_delim("input/regionmappingH12.csv", 
                         delim = ";", escape_double = FALSE, col_types = cols(X = col_skip()), 
                         trim_ws = TRUE,
                         show_col_types = FALSE) %>%
       select(RegionCode) %>%
       distinct() %>%
       pull(RegionCode)
   
     
     coef_mcc <- read_excel("input/Engel_Curve_12Regions.xlsx") %>%
       rename(region = Region,
              value = estimate,
              regressor = term,
              sector = Type) %>%
       select(region, sector, regressor, value) %>%
       mutate(
         label = 'mcc',
         regressor = str_replace_all(regressor,c(
           "log_hh_expenditures_USD_2017_square" = "I(log(exp)^2)",
           "log_hh_expenditures_USD_2017" = "log(exp)"
         )),
         sector = str_replace_all(sector,c(
           'Electricity' = 'Building electricity',
           'Other fuels' = 'Building other fuels',
           'Gas' = 'Building gases',
           'Fruits & Vegetables' = 'Fruits vegetables nuts',
           'Staple food' = 'Staples',
           'Other food' = 'Empty calories',
           'Goods' = 'Other commodities'
          )  
         )
        ) 
     
     print('JPN data missing and CHN estimate invalid, they are 
           replaced with GCD global estiamtes')
     
     coef_gcd <- analyze_regression(regression_model = regression_model, ConsData = ConsData, regionmapping = 'pool',
                                    isDisplay = TRUE, isExport = T) %>%
       mutate(label = 'gcd')
      
     
     coef = coef_gcd %>% mutate(region = 'JPN') %>%
       bind_rows(coef_gcd %>% mutate(region = 'CHA')) %>%
       bind_rows(coef_mcc %>% filter(region != 'CHA') )
     
    
    }else if(regions == 'H21') {
    region <- 
     read_delim("input/regionmapping_21_EU11.csv", 
                delim = ";", escape_double = FALSE, col_types = cols(X = col_skip(), 
                                                                     missingH12 = col_skip()), 
                trim_ws = TRUE,
                show_col_types = FALSE) %>%
      select(RegionCode) %>%
      distinct() %>%
      pull(RegionCode)
    
    print('input not available yet')
  }

  

  
  return(coef)
  
  
}