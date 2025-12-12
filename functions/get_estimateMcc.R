# This function gets the MCC estimate for the enegel curve and fix missing values
# with internal estimates
# Jiarui Zhong

get_estimateMcc <- function(regressRegGrouping = 'H12'){
 
   if(regressRegGrouping == 'H12'){
    
     region <- read_delim("input/regionmappingH12.csv", 
                         delim = ";", escape_double = FALSE, col_types = cols(X = col_skip()), 
                         trim_ws = TRUE,
                         show_col_types = FALSE) %>%
       select(RegionCode) %>%
       distinct() %>%
       pull(RegionCode)
   
     
     coef_mcc <- read_excel("input/Engel_Curve_12Regions_Deciles.xlsx") %>%
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
     
     print('JPN/CHA data missing and CHN estimate invalid, they are 
           replaced with GCD global estiamtes')
     
     coef_gcd <- analyze_regression(regressModel = regressModel, consData = 'gcd', regressRegGrouping = 'pool',
                                    isDisplay = TRUE, isExport = T) %>%
       mutate(label = 'gcd')
      
     
     coef = coef_gcd %>% mutate(region = 'JPN') %>%
       bind_rows(coef_gcd %>% mutate(region = 'CHA')) %>%
       bind_rows(coef_mcc %>% filter(region != 'CHA') )
     
    
    }else if(regressRegGrouping == 'H21') {
    
    region <- 
     read_delim("input/Engel_Curve_12Regions_Deciles.csv", 
                delim = ";", escape_double = FALSE, col_types = cols(X = col_skip(), 
                                                                     missingH12 = col_skip()), 
                trim_ws = TRUE,
                show_col_types = FALSE) %>%
      select(RegionCode) %>%
      distinct() %>%
      pull(RegionCode)
    
    print('input not available yet and using regional-specific Engel curve is abandoned as being unrealiable for future projection')
    
    } else if(regressRegGrouping == 'pool'){
    

      coef <- read_excel("input/Engel_Curve_Joint_Estimation_Alternative_20251210.xlsx") %>%
        rename(value = estimate,
               regressor = term,
               sector = Type) %>%
        select(sector, regressor, value) %>%
        mutate(
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
          ))) %>%
        mutate(region = 'pool')
      
      
  }

  

  
  return(coef)
  
  
}