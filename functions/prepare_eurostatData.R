#This functions reads the Eurostat consumption data and prepares it for the 
#regression anlaysis


prepare_eurostatData <- function(){
  
  #--1.prepare the national total consumption data(Euro)--
  
  expUnit = 'Current prices, million euro' #this can be set to other unit by demand
  
  exp <-read_csv("input/estat_nama_10_co3_p3_en.csv", 
                 col_types = cols(
                   DATAFLOW = col_skip(), 
                   `LAST UPDATE` = col_skip(), 
                   freq = col_skip())) %>% 
    filter(coicop == 'Total',unit == expUnit)
  
  #--2.prepare quintile consumption data (PPS)--
  
  expQuiUnit = 'Purchasing power standard (PPS) per adult equivalent'
  
  expQui <- read_csv("input/estat_hbs_exp_t133_en.csv", 
                     col_types = cols(DATAFLOW = col_skip(), 
                                      `LAST UPDATE` = col_skip(), freq = col_skip()))%>%
    filter( unit == expQuiUnit)
  
  
  #--3.prepare the quintile ocnsumption strucutre data (COIPOI)
  
  sector <- c('Food','Electricity, gas and other fuels')
  #There is almost no data for the detailed energy sectors
  
  expStr <- read_delim("input/estat_hbs_str_t223_en.csv", 
                       delim = ";", escape_double = FALSE, col_types = cols(DATAFLOW = col_skip(), 
                                                                            `LAST UPDATE` = col_skip(), freq = col_skip()), 
                       trim_ws = TRUE) %>%
    filter (coicop %in% sector) %>%
    mutate(OBS_VALUE = OBS_VALUE/1000) %>%
    pivot_wider(
      names_from = coicop,  # The column to expand into new columns
      values_from =OBS_VALUE  
    )
  
  colnames(expStr)[colnames(expStr) == "Food"] <- "wFood"
  colnames(expStr)[colnames(expStr ) == "Electricity, gas and other fuels"] <- "wEnergy"
  
  #commute the share of other commodities by hand
  expStr$wCommodity = 1 - expStr$wFood - expStr$wEnergy
  
  
  #--4. get the CPI for corresponding sectors---
  priceUnit <- 'Annual average index'
  
  
  hicp <- read_csv("input/estat_prc_hicp_aind_en.csv", 
                   col_types = cols(DATAFLOW = col_skip(), 
                                    `LAST UPDATE` = col_skip(), freq = col_skip(),
                                    OBS_FLAG = col_skip())) %>%
    filter(coicop %in% c(sector,'All-items HICP'), unit == priceUnit)%>%
    mutate(coicop = gsub("All-items HICP", "Total", coicop)) %>%
    rename( hicp = OBS_VALUE)
  
  #HICP is indexed for 2015=100 for each country, and captures annual price changes
  #of a same country. Cross cross-country price variation is reflected in PPP
  #the two should be mixed to get the correct price measure
  
  pppUnit = "Price level indices (EU28=100)"
  
  ppp <- read_csv("input/estat_prc_ppp_ind_en.csv", 
                  col_types = cols(DATAFLOW = col_skip(), 
                                   `LAST UPDATE` = col_skip(), freq = col_skip(), 
                                   OBS_FLAG = col_skip())) %>%
    filter( ppp_cat %in% c(sector,'Actual individual consumption'), na_item == pppUnit)%>%
    mutate(ppp_cat = gsub("Actual individual consumption", "Total", ppp_cat)) %>%
    rename( ppp = OBS_VALUE,
            coicop = ppp_cat)
  
  
  price <- inner_join(ppp,hicp, by = c('geo','TIME_PERIOD','coicop'))%>%
    mutate(price = ppp/100 * hicp/100 ) %>%
    select(-ppp,-hicp,-na_item, -unit) %>%
    pivot_wider(
      names_from = coicop,  # The column to expand into new columns
      values_from = price,
    ) %>%
    rename( pFood = Food,
            pEnergy ="Electricity, gas and other fuels",
            pTotal = Total)
  
  # Read from estat_nama, seems to be wrong
  # priceUnit = 'Chain linked volumes, index 2015=100' #CPI by sector
  # 
  # price <-read_csv("input/estat_nama_10_co3_p3_en.csv", 
  #                col_types = cols(
  #                  DATAFLOW = col_skip(), 
  #                  `LAST UPDATE` = col_skip(), 
  #                  freq = col_skip())) %>% 
  #   filter(coicop %in% c(sector,'Total'), unit == priceUnit) %>%
  #   pivot_wider(
  #     names_from = coicop,  # The column to expand into new columns
  #     values_from =OBS_VALUE  
  #   ) %>%
  #   rename( pFood = Food,
  #           pEnergy ="Electricity, gas and other fuels",
  #           pTotal = Total)
  
  #Compute the price index of the rest commodity, satisfying for 
  #each country-year combination, the weightedprice index should equal the total
  #price index from the database, for this purpose, read in the average weight 
  #of national consumption
  
  weight <- read_csv("input/estat_hbs_exp_t121_en.csv", 
                     col_types = cols(DATAFLOW = col_skip(), 
                                      `LAST UPDATE` = col_skip(), freq = col_skip()))%>%
    filter (coicop %in% sector) %>%
    mutate(OBS_VALUE = OBS_VALUE/1000) %>%
    pivot_wider(
      names_from = coicop,  # The column to expand into new columns
      values_from =OBS_VALUE  
    ) %>%
    rename( wFood = Food,
            wEnergy = 'Electricity, gas and other fuels') %>%
    mutate( wCommodity = 1-wFood-wEnergy)
  
  pCommodity = merge(weight, price, by = c('TIME_PERIOD','geo'),all.x = T)%>%
    mutate( pCommodity =  (pTotal - wFood*pFood - wEnergy*pEnergy)/wCommodity  )
  
  price <- merge(price,pCommodity[,c('TIME_PERIOD','geo','pCommodity')],by = c('TIME_PERIOD','geo'), all.x = T )
  
  
  
  #-----Merge data together-----
  
  df <- expQui %>%
    rename(exp = OBS_VALUE)%>%
    inner_join(expStr[,!names(expStr) %in% c("unit", "OBS_FLAG")], 
               by= c('quantile','geo','TIME_PERIOD'))%>%
    inner_join( price[,!names(price) %in% c("unit", "OBS_FLAG")],
                by= c('geo','TIME_PERIOD'))%>%
    select (-OBS_FLAG) %>%
    mutate(TIME_PERIOD = factor(TIME_PERIOD),
           geo = factor(geo))
  
  return(df)
  
}


