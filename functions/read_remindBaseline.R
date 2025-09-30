#This function reads baseline results

read_remindBaseline <- function( remind_path_base, do_plots = TRUE){
  
  remind_data <- 
    read.quitte(c(remind_path_base)) %>%
    as_tibble()

    FE_price <-
    remind_data %>% 
    filter(grepl("Price|Final Energy",variable,fixed = TRUE)) %>% 
    # remove "Stationary" prices (not a real FE sector like industry/buildings/transport/CDR)
    filter(!grepl("Stationary",variable),
           !region == 'World') %>% 
    separate(variable, into = c(NA,NA,"sector","carrier","further"), sep = "\\|", extra = "merge", fill = "right") %>% 
    filter(is.na(further)) %>% 
    select(-further) %>% 
    rename(price = value, price_unit = unit)

    FE_quantity <- 
      remind_data %>% 
      filter(grepl("FE|Buildings|+|",variable, fixed = TRUE) | 
               grepl("FE|Industry|+|",variable, fixed = TRUE) | 
               grepl("FE|Transport|+|",variable, fixed = TRUE) |
               grepl("FE|CDR|+|",variable, fixed = TRUE)
             
      ) %>% 
      separate(variable, into = c(NA,"sector",NA,"carrier","further"), sep = "\\|", fill = "right") %>%
      filter(is.na(further)) %>%
      select(-further) %>%
      #JZ: There is no price information in CDR Liquids, so remove the quantities
      filter( !(sector == 'CDR' & carrier =='Liquids'),
              !region == 'World') %>%
      rename(quantity = value, quantity_unit = unit)
  
  assert_that(nrow(FE_price) == nrow(FE_quantity))
  
  #JZ: household data doesn't have distinguished energy types in private transport, we assume HH use a aggregated
  #Also only use FE energy in passenger transportation.
  assert_that( nrow(FE_price[FE_price$sector =='Transport',]) == nrow(FE_quantity[FE_quantity$sector=='Transport',]))
  
  consumerPriceTrans <-
    inner_join(FE_price[FE_price$sector =='Transport',],FE_quantity[FE_quantity$sector=='Transport',], 
               by = c("model", "scenario", "region", "sector", "carrier", "period")) %>%
    group_by(model,scenario,region,sector,period, price_unit) %>%
    summarise( price = sum(price*quantity)/sum(quantity) ) %>%
    mutate(carrier = 'FE') 
  
  #JZ:Mapping building FE carriers to HH consumption sectors, Electricity and Hydrogen are combined and called electricity,
  #as Hydrogen quantity is extremely small, and they are both considered as green energy
  
  other_fuels <- inner_join(
    FE_price %>% filter(sector == 'Buildings', carrier %in% c('Heat', 'Liquids', 'Solids')),
    FE_quantity %>% filter(sector == 'Buildings', carrier %in% c('Heat', 'Liquids', 'Solids')),
    by = c("model", "scenario", "region", "sector", "carrier", "period")
  ) %>%
    group_by(model, scenario, region, sector, period) %>%
    summarise(price = sum(price * quantity) / sum(quantity), .groups = "drop") %>%
    mutate(carrier = "Other fuels",
           price_unit = "US$2017/GJ")
  
  # Join and aggregate for Electricity
  electricity <- inner_join(
    FE_price %>% filter(sector == 'Buildings', carrier %in% c('Electricity', 'Hydrogen')),
    FE_quantity %>% filter(sector == 'Buildings', carrier %in% c('Electricity', 'Hydrogen')),
    by = c("model", "scenario", "region", "sector", "carrier", "period")
  ) %>%
    group_by(model, scenario, region, sector, period) %>%
    summarise(price = sum(price * quantity) / sum(quantity), .groups = "drop") %>%
    mutate(carrier = "Electricity",
           price_unit = "US$2017/GJ")
  
  # Combine all FE prices
  FE_consumerPrice <- other_fuels %>%
    bind_rows(electricity)%>%
    bind_rows(consumerPriceTrans)%>%
    rbind(FE_price[FE_price$sector =='Buildings'& FE_price$carrier%in%c('Gases'),])%>%
    ungroup()%>%
    mutate(variable = paste0('Price|',sector,'|',carrier)) %>%
    select(-sector, -carrier) %>%
    rename(unit = price_unit, value = price)
  
  
  FE_consumerQuantityBuilding <- 
    remind_data %>% 
    filter(grepl("FE|Buildings|+|",variable, fixed = TRUE) 
    ) %>%
    separate(variable, into = c(NA,"sector",NA,"carrier","further"), sep = "\\|", fill = "right") %>%
    filter(is.na(further)) %>%
    select(-further) %>%
    rename(quantity = value, quantity_unit = unit)%>%
    mutate(carrier = ifelse(carrier %in% c('Electricity', 'Gases','Hydrogen'), carrier, 'Other fuels')) %>%
    group_by(model,scenario,region,sector,carrier,period, quantity_unit) %>%
    summarise (quantity = sum(quantity)) %>%
    ungroup() %>%
    mutate(carrier = ifelse(carrier %in% c('Gases','Other fuels'), carrier, 'Electricity')) %>%
    group_by(model,scenario,region,sector,carrier,period, quantity_unit) %>%
    summarise (quantity = sum(quantity)) %>%
    ungroup()%>%
    mutate( variable = paste0( "FE|",sector,'|',carrier ) )%>%
    select(-sector, -carrier) %>%
    rename(unit = quantity_unit, value = quantity)
  
  
  # #used to compute final energy service price 
  # FE_sectorprice <- 
  #   inner_join(FE_price,FE_quantity, by = c("model", "scenario", "region", "sector", "carrier", "period")) %>% 
  #   group_by(model,scenario,region,sector,period, price_unit) %>% 
  #   summarise(price = sum(price*quantity)/sum(quantity)) %>% 
  #   ungroup() %>% 
  #   mutate(variable = paste0("Price|Final Energy|",sector)) %>% 
  #   select(-sector) %>% 
  #   rename(unit = price_unit, value = price)
  
  remind_data <- bind_rows(remind_data, FE_consumerPrice,  
                           FE_consumerQuantityBuilding)
  
  remind_data <- 
    filter(remind_data,
           variable %in% c('GDP|MER','GDP|PPP','Population',
                           'Consumption',
                           "FE|++|Transport",
                           "FE|Buildings|Other fuels","FE|Buildings|Gases","FE|Buildings|Electricity",
                           "Price|Transport|FE",
                           "Price|Buildings|Other fuels","Price|Buildings|Electricity","Price|Buildings|Gases",
                           "deltPrice|Transport|FE",
                           "deltPrice|Buildings|Electricity","deltPrice|Buildings|Gases",
                           "deltPrice|Buildings|Other fuels")
    ) %>% 
    mutate_at(vars(c(scenario,variable,unit,model)), ~as.character(.))
  
  return(remind_data)
}
