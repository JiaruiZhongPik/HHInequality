#This funciton reads REMIND data


read_remindPolicy <- function(remind_run,remind_path, remind_path_base, isDisplay = TRUE, isExport = TRUE){
  
  #for testing purpose
  # remind_run = all_paths$remind_run[1]
  # remind_path = all_paths$remind_path[1]
  # remind_path_base = all_paths$remind_path_base[1]
  
  remind_data <- 
    read.quitte(c(remind_path, remind_path_base)) %>%
    as_tibble()
  

  scens <- getScenarios(remind_data)
  
  #BS: the NPi-CCimp should be treated as a policy scenario (comparing it to the no-impact NPi)
  #BS: TODO added "CP" to list of identifiers of reference/base scenario for tests on ELEVATE scenarios. keep this?
  scen_base <- scens[grepl("Base|NPi|npi-base|CP",scens) & !grepl("NPi-CCimp",scens)] #added npi-base and npi-policy to align with the above
  scen_policy <- scens[grepl("1150|1000|900|650|500|Budg|NDC|npi-policy|NPi-CCimp",scens)] #added budget numbers here for use with hybrid runs in SDP review
  
  Good_price <- 
    remind_data %>% 
    filter(grepl("PVP2|Good",variable,fixed = TRUE)) %>% 
    mutate(variable = "Price|Other commodities") %>%
    filter(!region == 'World') 
  
  
  Good_price <-   Good_price %>% 
    mutate( scenario = case_when(scenario == scen_base ~ "base",
                           scenario == scen_policy ~ "policy")) %>%
    pivot_wider(names_from = scenario, values_from = value) %>% 
    mutate(deltPrice = policy/base - 1 ) %>%
    select(-policy, -base) %>%
    mutate(variable = "deltPrice|Other commodities",
           baseline = scen_base,
           scenario = scen_policy) %>%
    rename( value = deltPrice ) %>%
    bind_rows(Good_price)

    
  
  
  FE_price <-
    remind_data %>% 
    filter(grepl("Price|Final Energy",variable,fixed = TRUE)) %>% 
    # remove "Stationary" prices (not a real FE sector like industry/buildings/transport/CDR)
    filter(!grepl("Stationary",variable),
           !region == 'World' ) %>% 
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
  #as Hydrogen quantity is extremely small

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
  
  FE_consumerPriceDelt<- FE_consumerPrice%>%
    mutate(
      scenario = case_when(scenario == scen_base ~ "base",
                           scenario == scen_policy ~ "policy"))%>%
    spread(scenario,value)%>%
    mutate(priceRel = policy/base-1)%>%
    select(-policy,-base)%>%
    mutate(variable = paste0('delt',variable),
           scenario = scen_policy,
           baseline = scen_base)%>%
    rename(value = priceRel)
  
  
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
  #   summarize(price = sum(price*quantity)/sum(quantity)) %>% 
  #   ungroup() %>% 
  #   mutate(variable = paste0("Price|Final Energy|",sector)) %>% 
  #   select(-sector) %>% 
  #   rename(unit = price_unit, value = price)
  
  remind_data <- bind_rows(remind_data, FE_consumerPrice,  
                           FE_consumerQuantityBuilding,
                           FE_consumerPriceDelt,
                           Good_price)
  
  
  remind_data <- remind_data %>% 
    filter(variable %in% c('GDP|MER','GDP|PPP','Population',
                           'Consumption',
                           "FE|++|Transport",
                           "FE|Buildings|Other fuels","FE|Buildings|Gases","FE|Buildings|Electricity",
                           "Price|Transport|FE",
                           "Price|Buildings|Other fuels","Price|Buildings|Electricity","Price|Buildings|Gases",
                           "Price|Other commodities",
                           "deltPrice|Transport|FE",
                           "deltPrice|Buildings|Electricity","deltPrice|Buildings|Gases",
                           "deltPrice|Buildings|Other fuels",
                           "deltPrice|Other commodities"
                           ),
           scenario == scen_policy
    ) %>% 
    mutate_at(vars(c(scenario,variable,unit,model)), ~as.character(.))
  
  p0<- ggplot(FE_consumerPriceDelt, aes( x =period, y=value * 100, color = variable ))+
    geom_point(size = 0.6, alpha = 0.6 )+
    geom_line()+
    facet_wrap(~region)+
    labs(title = paste0(str_extract(remind_run, "PkBudg[^-]+")," Energy price changes to Baseline(NPi)"), x = "Year", y = "percentage(%)") +
    theme_minimal()+
    coord_cartesian(ylim = c(-10, 400))
  
  if(isDisplay) {

    print(p0)
  }
  if(isExport){
    ggsave(paste0(outputPath,'/Energy price changes to Baseline (NPi)',str_extract(remind_run, "PkBudg[^-]+"),'.tiff'),p0 , width = 6, height =5, units = "in", dpi = 300 )
  }
  
 
  return(remind_data)
}
