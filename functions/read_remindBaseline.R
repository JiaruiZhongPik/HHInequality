#This function reads baseline results

read_remindBaseline <- function( remind_path_base, do_plots = TRUE){
  
  remind_data <- 
    read.quitte(c(remind_path_base)) %>%
    as_tibble()
  
  
  # BS:2023-01-17 remove MAgPIE population in new joint reporting
  remind_data <- filter(remind_data,!(model == "MAgPIE" & variable == "Population"))
  
  
  #BS: 2023-05-31 compute average final energy prices
  # this now uses FE prices after correction of problems and moving average, same as reported to project DBs, https://github.com/pik-piam/remind2/pull/402
  # (these are called "Price|Final Energy|SECTOR|CARRIER without further suffixes)
  # Note: non-SSP2 scenarios have 2025 value replaced with 2020 & 2030 average
  
  #TODO
  warning("FE expenditure calculation currently removes Buildings|Hydrogen (set to zero in current default) and CDR|Liquids (only non-zero if Enhanced Weathering or Ocean Alkalinisation switched on).")
  
  FE_price <-
    remind_data %>% 
    filter(grepl("Price|Final Energy",variable,fixed = TRUE)) %>% 
    # remove "Stationary" prices (not a real FE sector like industry/buildings/transport/CDR)
    filter(!grepl("Stationary",variable)) %>% 
    # remove this as quantities are zero anyway, and no or incomplete price information available
    filter(variable != "Price|Final Energy|Buildings|Hydrogen",
           variable != "Price|Final Energy|CDR|Liquids") %>%
    # naming: Price|Final Energy|SECTOR|CARRIER|[SOURCE / FURTHER INFO]
    # I only want the SECTOR|CARRIER category without further differentiation
    #fifth piece is to remove subcategories such as Moving Avg, LDV/HDV
    separate(variable, into = c(NA,NA,"sector","carrier","further"), sep = "\\|", extra = "merge", fill = "right") %>% 
    filter(is.na(further)) %>% 
    select(-further) %>% 
    rename(price = value, price_unit = unit)
  
  #here to continue
  #JZ:The price is for Dollar per GJ, the same price should be used in the leontief coeffcient estimation.
  
  
  FE_quantity <- 
    remind_data %>% 
    filter(grepl("FE|Buildings|+|",variable, fixed = TRUE) | 
             grepl("FE|Industry|+|",variable, fixed = TRUE) | 
             grepl("FE|Transport|+|",variable, fixed = TRUE) |
             grepl("FE|CDR|+|",variable, fixed = TRUE)
    ) %>% 
    # remove this as quantities are zero anyway, and no or incomplete price information available
    filter(variable != "FE|CDR|+|Liquids", 
           variable != "FE|Buildings|+|Hydrogen") %>%
    # naming: FE|SECTOR|+|CARRIER|[SOURCE / FURTHER INFO]
    # I only want the SECTOR|CARRIER category without further differentiation
    #fith piece is to remove subcategories such as Moving Avg, LDV/HDV
    # this is technically not needed here but keep if in case further layers are added
    separate(variable, into = c(NA,"sector",NA,"carrier","further"), sep = "\\|", fill = "right") %>%
    filter(is.na(further)) %>%
    select(-further) %>%
    rename(quantity = value, quantity_unit = unit)
  
  assert_that(nrow(FE_price) == nrow(FE_quantity))
  
  
  # BS: 2023-01-18 catch (near-)zero or negative prices for non-negligible quantities
  price_problems <-   
    inner_join(FE_price,FE_quantity, by = c("model", "scenario", "region", "sector", "carrier", "period")) %>% 
    filter(period > 2020, period <= 2050,
           price < 1., # $/GJ
           quantity > 1. # EJ
    ) %>% 
    group_by(scenario, region, sector, carrier) %>%
    summarize(how_many_periods = n()) %>% 
    ungroup() %>% 
    arrange(desc(how_many_periods))
  
  if (nrow(price_problems) > 0){
    warning("found FE|sector|carriers with (near-)zero or negative price and non-negligible quantity. check carefully! \n")
    print(price_problems)  
  }
  
  #In case the household data doesn't have distinguished energy types in private transport, we assume HH use a aggregated
  #FE energy in Passenger freight.
  #Todo: The mapping in the futures should be done in an automatic way!!
  assert_that( nrow(FE_price[FE_price$sector =='Transport',]) == nrow(FE_quantity[FE_quantity$sector=='Transport',]))
  
  consumerPriceTrans <-
    inner_join(FE_price[FE_price$sector =='Transport',],FE_quantity[FE_quantity$sector=='Transport',], 
               by = c("model", "scenario", "region", "sector", "carrier", "period")) %>%
    group_by(model,scenario,region,sector,period, price_unit) %>%
    summarize( price = sum(price*quantity)/sum(quantity) ) %>%
    mutate(carrier = 'FE') 
  
  #Mapping building FE carriers to HH consumption sectors
  FE_consumerPrice <-  inner_join(FE_price[FE_price$sector =='Buildings'& FE_price$carrier%in%c('Heat','Liquids','Solids'),], 
                                  FE_quantity[FE_quantity$sector =='Buildings'& FE_quantity$carrier%in%c('Heat','Liquids','Solids'),],
                                  by = c("model", "scenario", "region", "sector", "carrier", "period"))%>%
    group_by(model,scenario,region,sector,period, price_unit) %>%
    summarize( price = sum(price*quantity)/sum(quantity) ) %>%
    mutate(carrier = 'Other fuels') %>%
    rbind(consumerPriceTrans)%>%
    rbind(FE_price[FE_price$sector =='Buildings'& !FE_price$carrier%in%c('Heat','Liquids','Solids'),])%>%
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
    mutate(carrier = ifelse(carrier %in% c('Electricity', 'Gases'), carrier, 'Other fuels')) %>%
    group_by(model,scenario,region,sector,carrier,period, quantity_unit) %>%
    summarize (quantity = sum(quantity)) %>%
    ungroup() %>%
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
  
  
  
  #   # calculate GDP Loss by hand because we need it w.r.t. our base scenario, not w.r.t. to the reference scenario used in REMIND config
  #   remind_gdploss <-
  #     remind_data %>%
  #     filter(variable == "GDP|MER") %>%
  #     spread(scenario,value) %>%
  #     mutate(`Policy Cost|GDP Loss` = base-policy) %>%
  #     select(-c("base","policy")) %>%
  #     mutate(variable = as.character("Policy Cost|GDP Loss")) %>%
  #     mutate(scenario = "policy") %>%
  #     rename(value = `Policy Cost|GDP Loss`) %>%
  #     as.quitte() %>% 
  #     mutate_at(vars(c(scenario,variable,unit,model)), ~as.character(.))
  #   
  #   
  #   # replace reporting GDP loss with by-hand version (see above)
  #   remind_data <- 
  #     remind_data %>%
  #     filter(variable != "Policy Cost|GDP Loss") %>%
  #     mutate_at(vars(c(scenario,variable,unit,model)), ~as.character(.)) %>% 
  #     bind_rows(remind_gdploss)
  #   
  #   # hardcoded these numbers below for easier functionality with calc_AddVariable()
  #   # GWP_CH4_AR5 <- 28
  #   # GWP_N2O_AR5 <- 265
  #   
  #   # all costs are calculated as share of base GDP = policy GDP + GDP loss
  #   remind_rel <- 
  #     remind_data %>%
  #     # completeMissing flag sets Policy cost in baseline run to zero
  #     calc_addVariable("`GDP|MER|Base`" = "`GDP|MER` + `Policy Cost|GDP Loss`",
  #                      units = "billion US$2017/yr", na.rm = FALSE, completeMissing = TRUE) %>%
  #     # GDP loss (proxy for macroeconomic policy cost), GDP/capita
  #     calc_addVariable("`Policy Cost|GDP Loss|Rel`" = "`Policy Cost|GDP Loss`/`GDP|MER|Base`",
  #                      "`GDPpcap|MER`" = "`GDP|MER`*1e9/(Population*1e6)",
  #                      "`GDPpcap|PPP`" = "`GDP|PPP`*1e9/(Population*1e6)",
  #                      units = c(NA,"US$2017/yr","US$2017/yr")) %>%
  #     # Tax revenue
  #     calc_addVariable(
  #       "`Taxes|EmiC02FFI`" = "`Emi|CO2|Energy and Industrial Processes`*1e6 * `Price|Carbon`/ 1e9",
  #       "`Taxes|EmiCO2FFI|Rel`" = "`Taxes|EmiC02FFI` / `GDP|MER|Base`",
  #       # taxes from non-LU CH4 (in Mt CH4) and N2O (in kt N2O) emissions
  #       "`Taxes|EmiCH4Energy`" = "(`Emi|CH4|+|Energy Supply` + `Emi|CH4|+|Extraction`)*1e6 * 28 * `Price|Carbon`/1e9",
  #       "`Taxes|EmiN2OEnergy`" = "(`Emi|N2O|+|Energy Supply` + `Emi|N2O|+|Industry` + `Emi|N2O|+|Transport`) *1e3 * 265 * `Price|Carbon`/1e9",
  #       "`Taxes|GHGenergy|REMIND`" = "`Taxes|EmiC02FFI` + `Taxes|EmiCH4Energy` + `Taxes|EmiN2OEnergy`",
  #       "`Taxes|GHGenergy|REMIND|Rel`" = "`Taxes|GHGenergy|REMIND`/`GDP|MER|Base`",
  #       # LUC revenues are not used for redistribution, so don't need these
  #       # "`Taxes|EmiCO2LUC`" = "`Emi|CO2|Land-Use Change`*1e6 * `Price|Carbon`/ 1e9",
  #       # "`Taxes|EmiCO2LUC|Rel`" = "`Taxes|EmiCO2LUC`/`GDP|MER|Base`",
  #       units = c(
  #         "billion US$2017/yr",NA,"billion US$2017/yr","billion US$2017/yr","billion US$2017/yr",NA
  #       )) %>%
  #     # FE expenditures
  #     calc_addVariable("`FE expenditures`" = "`FE|++|Industry` * `Price|Final Energy|Industry` + `FE|++|Transport` * `Price|Final Energy|Transport` + `FE|++|Buildings` * `Price|Final Energy|Buildings`",
  #                      "`FE expenditures|Rel`" = "`FE expenditures`/`GDP|MER|Base`", 
  #                      units = c("billion US$2017/yr",NA)) %>% 
  #     # FE taxes and subsidies
  #     calc_addVariable("`FE taxes|Rel`" = "`Taxes|Final Energy`/`GDP|MER|Base`",
  #                      "`FE subsidies|Rel`" = "`Subsidies|Final Energy`/`GDP|MER|Base`",
  #                      "`FE net tax/sub|Rel`" = "`FE taxes|Rel` - `FE subsidies|Rel`")
  #   
  # 
  
  #   if (do_plots){
  #     p0 <- 
  #       remind_rel %>%
  #       filter(grepl("Taxes",variable),
  #              !grepl("Rel",variable)) %>%
  #       filter(region != "World", period <= 2100) %>%
  #       filter(scenario == "policy") %>%
  #       ggplot() +
  #       geom_line(aes(period,value,color = variable, linetype = variable), size = 1) +
  #       facet_wrap(~region, scales = "free_y") +
  #       theme(legend.position = "bottom") +
  #       labs(y = "tax revenue [billion $]",
  #            title = "Tax revenue policy run")
  #     print(p0)
  #   }
  #   
  #   
  #   if (do_plots){
  #     p1 <- 
  #       remind_rel %>%
  #       filter(grepl("|Rel",variable,fixed = TRUE),
  #              region != 'World',
  #              period <= 2100) %>%
  #       ggplot() + 
  #       geom_line(aes(period,value, color = variable, linetype = scenario)) + 
  #       facet_wrap(~region, scales = "free_y") + 
  #       theme(legend.position = "bottom", legend.box="vertical", legend.margin=margin()) +
  #       labs(title = "Expenditures or revenue relative to baseline GDP")
  #     print(p1)
  #   }
  #   
  #   # calculate FE expenditure difference in expenditures (policy - baseline)
  #   remind_rel2 <-
  #     remind_rel %>% 
  #     filter(variable %in% c("FE expenditures|Rel","FE taxes|Rel","FE subsidies|Rel","FE net tax/sub|Rel")) %>% 
  #     spread(scenario,value) %>% 
  #     mutate(diff = policy - base) %>% 
  #     select(-c(base,policy)) %>% 
  #     mutate(variable = gsub("Rel","Additional",variable),
  #            base = 0) %>% 
  #     rename(policy = diff) %>% 
  #     gather(policy,base, key = "scenario", value = "value") %>% 
  #     bind_rows(remind_rel)
  #   
  #   
  #   if (do_plots){
  #     p2 <-  
  #       remind_rel2 %>%
  #       filter(scenario == "policy") %>%
  #       filter(variable %in% c("FE expenditures|Additional", "FE taxes|Additional", "FE subsidies|Additional", 
  #                              "Policy Cost|GDP Loss|Rel", 
  #                              "Taxes|EmiCO2FFI|Rel",
  #                              # "Taxes|EmiCO2LUC|Rel",
  #                              "Taxes|GHG emissions|GAMS calculated|Rel","Taxes|GHGenergy|REMIND|Rel"),
  #              region != 'World',
  #              period <= 2100) %>%
  #       ggplot() +
  #       geom_line(aes(period,value, color = variable), size = 0.7) +
  #       facet_wrap(~region, scales = "free_y") + 
  #       theme(legend.position = "bottom")
  #     print(p2)
  #   }
  #   
  #   # final policy costs
  #   remind_policycost <- 
  #     remind_rel2 %>%
  #     filter(scenario == "policy") %>%
  #     filter(variable %in% c("FE expenditures|Additional", "FE taxes|Additional", "FE subsidies|Additional", "FE net tax/sub|Additional",
  #                            "Taxes|EmiCO2FFI|Rel",
  #                            "Taxes|GHGenergy|REMIND|Rel",
  #                            # "Taxes|EmiCO2LUC|Rel",
  #                            "Policy Cost|GDP Loss|Rel","GDP|MER","GDP|MER|Base","GDP|PPP","GDPpcap|MER","GDPpcap|PPP","Population")) %>%
  #     select(-c(model,scenario,unit)) %>%
  #     spread(variable,value) %>% 
  #     filter(period <= 2100)
  #   
  #   if (do_plots){
  #     p3 <- 
  #       remind_policycost %>% 
  #       gather(`FE expenditures|Additional`:`Taxes|GHGenergy|REMIND|Rel`, key = "variable",value="value") %>% 
  #       filter(grepl("Taxes",variable),grepl("Rel",variable),
  #              region != "World") %>% 
  #       filter(period >= 2015, period <= 2100) %>% 
  #       ggplot() + 
  #       geom_line(aes(period,value, color = variable, linetype = variable), size = 1) +
  #       facet_wrap(~region, scales = "free_y") +
  #       theme(legend.position = "bottom") +
  #       labs(title = "Tax revenue as a fraction of baseline GDP") +
  #       geom_hline(yintercept = 0, color = "grey")
  #     print(p3)
  #   }
  #   
  return(remind_data)
}
