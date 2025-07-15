#---This function simulates the welfare effect of price changes----------------



#-------------1. First-order welfare effect-------------------------

predict_decileWelfChange <- function(data1 = data, data2 = decileConsShare, micro_model = 'FOwelfare', fixed_point = 'midpoint'){
  
  #get sectors for later use
  sectors <- data2 %>%
    select(starts_with("share|")) %>%
    names() %>%
    stringr::str_remove("^share\\|")  
  
  if( length(sectors) == 3 ){
    
    deltPriceEne <- data1 %>%
      select(-unit,-baseline)%>%
      filter(
        str_starts(variable, "deltPrice") | str_starts(variable, "FE"),
        str_detect(scenario, str_c(all_budgets, collapse = "|"))
      ) %>%
      pivot_wider(
        names_from = variable, values_from = value
      ) %>%
      mutate( deltPriceEne = (`deltPrice|Transport|FE` * `FE|++|Transport` + 
                                `deltPrice|Buildings|Electricity` * `FE|Buildings|Electricity` + 
                                `deltPrice|Buildings|Gases` * `FE|Buildings|Gases` +
                                `deltPrice|Buildings|Other fuels` * `FE|Buildings|Other fuels`)/
                (`FE|++|Transport`+`FE|Buildings|Electricity`+`FE|Buildings|Gases` + `FE|Buildings|Other fuels`),
              variable = 'deltPrice|Ene'
      )%>%
      select( scenario, model, region, period, variable, deltPriceEne )%>%
      rename( value = deltPriceEne)
    
    
    #By definitionm commodity price doesn't change as it it numeraire
    deltPriceComm <- deltPriceEne %>% 
      mutate( value = 0,
              variable = 'deltPrice|Other commodities')
    
    #todo: introduce real food price change from MAgPIE, now is temporarily assumed to be 0
    deltPriceFood <- deltPriceEne %>% 
      mutate( value = 0,
              variable = 'deltPrice|Food')
    
    deltPrice <- bind_rows(deltPriceFood,deltPriceEne,deltPriceComm) %>%
      separate(col = variable,into = c("variable", "category"), sep = "\\|") %>%
      rename(deltPrice = value) %>%
      select(-variable)
    
  } else if ( length(sectors) == 9 ){
    
    deltPrice <- data1 %>%
      select(-unit,-baseline)%>%
      filter(
        str_starts(variable, "deltPrice"),
        str_detect(scenario, str_c(all_budgets, collapse = "|"))
      ) %>%
      mutate(      
        variable = case_when(
        variable ==  "deltPrice|Transport|FE" ~ "deltPrice|Transport energy",
        variable ==  "deltPrice|Buildings|Electricity" ~ "deltPrice|Building electricity" ,
        variable == "deltPrice|Buildings|Other fuels" ~ "deltPrice|Building other fuels",
        variable == "deltPrice|Buildings|Gases" ~ "deltPrice|Building gases",
        TRUE ~ variable  # keep unchanged otherwise
      )) %>%
      #Note: there are INF for minor cases, when the base year price is zero. For
      #these, I currently replace with the value from the closest year
      #same is done for food price
      group_by(region, scenario, variable) %>%
      mutate(value = ifelse(
        is.infinite(value),
        max(value[is.finite(value)], na.rm = TRUE),
        value
      )) %>%
      ungroup() 
    
    
    deltPrice <- deltPrice %>%
      bind_rows(  deltPrice %>% 
                    filter( variable == 'deltPrice|Building electricity' ) %>%
                    mutate( value = 0,
                            variable = 'deltPrice|Other commodities')
                      )  %>%
      separate( col= variable,into = c("variable", "category"), sep = "\\|"  ) %>%
      select( -variable ) %>%
      rename( deltPrice = value )

  }
  

  
  if(micro_model == 'FOwelfare'){
    
    if(fixed_point =='policy'){
      
      decileWelfChange<-
        data2 %>%
        select(-consumptionCa)%>%
        filter(scenario  %in% paste0( 'C_',all_runscens,'-',all_budgets  )) %>%
        pivot_longer( cols = starts_with('share'),
                      names_to = 'variable',
                      values_to = 'share') %>%
        separate( col= variable,into = c("variable", "category"), sep = "\\|"  ) %>%
        select(-variable ) %>%
        left_join(deltPrice %>% select(-model), by = c('scenario','region','period','category')) %>%
        group_by(scenario, region, period, decileGroup, category) %>%
        summarise(
          decilWelfChange = -  sum(deltPrice * share) * 100,
          .groups = "drop"
        ) 
      
    }else if (fixed_point == 'base'){
      
      mapping <- all_paths %>%
        select( remind_run, remind_base ) %>%
      mutate(
        remind_base = str_remove(remind_base, "-(rem|mag)-\\d+$"),
        remind_run  = str_remove(remind_run, "-(rem|mag)-\\d+$")
      )
      
      baseConsShare <- data2 %>%
        filter(
          scenario %in% 
            (all_paths$remind_base %>% 
               str_remove("-(rem|mag)-\\d+$") %>% 
               unique())
               ) %>%
        select(-consumptionCa) %>%
        rename(remind_base = scenario)
      
      decileWelfChange <- data2 %>%
        filter(scenario  %in% paste0( 'C_',all_runscens,'-',all_budgets)) %>%
        select(scenario,region,period, decileGroup) %>%
        left_join( mapping, by= c('scenario'= 'remind_run') ) %>%
        merge(baseConsShare, by = c( "region", "period", "decileGroup", "remind_base")) %>%
        pivot_longer( cols = starts_with('share'),
                      names_to = 'variable',
                      values_to = 'share') %>%
        separate( col= variable,into = c("variable", "category"), sep = "\\|") %>%
        select(-variable ) %>%
        left_join(deltPrice, by = c('scenario','region','period','category')) %>%
        group_by(scenario, region, period, decileGroup,category) %>%
        summarise(
          decilWelfChange = -  sum(deltPrice * share) * 100,
          .groups = "drop"
        ) 
      
    } else if (fixed_point == 'midpoint') {
      
      mapping <- all_paths %>%
        select( remind_run, remind_base ) %>%
        mutate(
          remind_base = str_remove(remind_base, "-(rem|mag)-\\d+$"),
          remind_run  = str_remove(remind_run, "-(rem|mag)-\\d+$")
        )
      
      baseConsShare <- data2 %>%
        filter(
          scenario %in% 
            (all_paths$remind_base %>% 
               str_remove("-(rem|mag)-\\d+$") %>% 
               unique())
        ) %>%
        select(-consumptionCa) %>%
        rename(remind_base = scenario)  %>%
        rename_with(
          .fn = ~ paste0(., "|Base"),
          .cols = starts_with("share|")
        )
      
      consShare <- data2 %>%
        select( -consumptionCa) %>%
        filter(scenario  %in% paste0( 'C_',all_runscens,'-',all_budgets)) %>%
        left_join( mapping, by= c('scenario'= 'remind_run')) %>%
        merge(baseConsShare, by = c( "region", "period", "decileGroup", "remind_base"))
      
      
      for (s in sectors) {
        orig_col <- paste0("share|", s)
        base_col <- paste0("share|", s, "|Base")
        avg_col  <- paste0("share|", s, "|Avg")
        
        consShare[[avg_col]] <- (consShare[[orig_col]] + consShare[[base_col]]) / 2
      }
      
      decileWelfChange <- consShare %>%
        select(
          -matches("^share\\|[^|]+$"),    # removes original share|X
          -matches("\\|Base$")            # removes any column ending with |Base
        ) %>%
        rename_with(
          ~ stringr::str_remove(., "\\|Avg$"),  # remove |Avg suffix
          .cols = matches("\\|Avg$")
        ) %>%
        pivot_longer( cols = starts_with('share'),
                      names_to = 'variable',
                      values_to = 'share')%>%
        separate( col= variable,into = c("variable", "category"), sep = "\\|"  )%>%
        select(-variable ) %>%
        left_join(deltPrice, by = c('scenario','region','period','category')) %>%
        group_by(scenario, region, period, decileGroup, category)%>%
        summarise(
          decilWelfChange = -  sum(deltPrice * share) * 100,
          .groups = "drop"
        ) 
      
    }

    
    
    
  }else(print ('Other approach not yet implemented'))
  
  
  #add real consumption change
  decileWelfChange <-   decileConsShare %>%
    select(scenario, region, period, decileGroup, consumptionCa) %>%
    pivot_wider(names_from = scenario, values_from = consumptionCa) %>%
    mutate(`C_SSP2-PkBudg650` = (`C_SSP2-PkBudg650` /  `C_SSP2-NPi2025` - 1) * 100,
           `C_SSP2-PkBudg1000` = (`C_SSP2-PkBudg1000` /  `C_SSP2-NPi2025` - 1) * 100  ) %>%
    select( -`C_SSP2-NPi2025` ) %>%
    mutate(category = 'consumptionCa') %>%
    pivot_longer(cols = starts_with('C_SSP2'), names_to = 'scenario', values_to = 'decilWelfChange') %>%
    bind_rows(decileWelfChange)
  
  
  
  return(decileWelfChange)
  
}
