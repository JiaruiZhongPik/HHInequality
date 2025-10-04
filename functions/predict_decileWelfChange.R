#---This function simulates the welfare effect of price changes----------------



#-------------1. First-order welfare effect-------------------------

predict_decileWelfChange <- function(data1 = data, data2 = decileConsShare, 
                                     micro_model = 'FOwelfare', fixed_point = 'midpoint'){
  
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
    
    relaPrice <- data1 %>%
      select(-unit,-baseline)%>%
      filter(
        str_starts(variable, "relaPrice"),
        str_detect(scenario, str_c(all_budgets, collapse = "|"))
      ) %>%
      mutate(      
        variable = case_when(
        variable ==  "relaPrice|Transport|FE" ~ "relaPrice|Transport energy",
        variable ==  "relaPrice|Buildings|Electricity" ~ "relaPrice|Building electricity" ,
        variable == "relaPrice|Buildings|Other fuels" ~ "relaPrice|Building other fuels",
        variable == "relaPrice|Buildings|Gases" ~ "relaPrice|Building gases",
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
      ungroup() %>%
      separate( col= variable,into = c("variable", "category"), sep = "\\|"  ) %>%
      select( -variable ) %>%
      rename( relaPrice = value )


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
        left_join(relaPrice %>% select(-model), by = c('scenario','region','period','category')) %>%
        mutate(decilWelfChange = - log(relaPrice) * share *100) %>%
        select(scenario, region, period, decileGroup, category, decilWelfChange)

      
      
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
        left_join(relaPrice, by = c('scenario','region','period','category')) %>%
        mutate(decilWelfChange = - log(relaPrice) * share *100) %>%
        select(scenario, region, period, decileGroup, category, decilWelfChange)

      
      
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
        left_join(relaPrice, by = c('scenario','region','period','category')) %>%
        mutate(decilWelfChange = - log(relaPrice) * share *100) %>%
        select(scenario, region, period, decileGroup, category, decilWelfChange)
      
    }

    
    
    
  }else(print ('Other approach not yet implemented'))
  

  #Add total consumption budget effect (inequality neutral)
  decileWelfChange <-
    decileConsShare %>%
    select(scenario, region, period, decileGroup, consumptionCa) %>%
    pivot_wider(names_from = scenario, values_from = consumptionCa) %>%
    {
      wide <- .
      # identify scenario columns (everything that isn’t an id column)
      id_cols <- c("region", "period", "decileGroup")
      scen_cols <- setdiff(names(wide), id_cols)
      
      # baseline column = any column whose name contains reference_run_name (first match if multiple)
      baseline_col <- scen_cols[str_detect(scen_cols, fixed(reference_run_name))]
      
      # policy columns = any columns whose names contain any of the all_budgets tokens
      policy_regex <- paste(all_budgets, collapse = "|")
      policy_cols <- scen_cols[str_detect(scen_cols, policy_regex)]
      
      wide %>%
        mutate(.base = .data[[baseline_col]]) %>%
        mutate(
          across(
            all_of(policy_cols),
            ~ log(.x / .base) * 100
          )
        ) %>%
        select(all_of(id_cols), all_of(policy_cols)) %>%
        pivot_longer(
          cols = all_of(policy_cols),
          names_to = "scenario",
          values_to = "decilWelfChange"
        ) %>%
        mutate(category = "Consumption With NeutTransf")
    } %>%
    bind_rows(decileWelfChange)
  
  
  
  #Tax revenue recycling

  
  # Progressive: equal per capita transfer
  transferEpc <-   data1 %>%
    filter(variable %in% c('Taxes|GHG|MAGPIE','Taxes|GHG|REMIND','Population'),
           scenario  %in% paste0( 'C_',all_runscens,'-',all_budgets ),
           region != 'World') %>%
    select(-model, -baseline) %>%
    calc_addVariable(
      "`Taxes|GHG`" = "`Taxes|GHG|MAGPIE` + `Taxes|GHG|REMIND`",
      "transferEpc" = "`Taxes|GHG` * 1e3 / `Population`" ,
      units = c('billion US$2017/yr', 'US$2017/person/yr')
    ) %>%
    filter(variable == 'transferEpc') %>%
    select(-variable, -unit) %>%
    rename(transferEpc = value) %>%
    crossing(decileGroup = 1:10) %>%
    arrange(scenario, region, period, decileGroup)
  
  transferNeut <- data1 %>%
    filter(variable %in% c('Taxes|GHG|MAGPIE','Taxes|GHG|REMIND','Population'),
           scenario  %in% paste0( 'C_',all_runscens,'-',all_budgets ),
           region != 'World') %>%
    select(-model, -baseline) %>%
    calc_addVariable(
      "`Taxes|GHG`" = "`Taxes|GHG|MAGPIE` + `Taxes|GHG|REMIND`",
      "`TaxesCa|GHG`" = "`Taxes|GHG` * 1e3 / `Population`" ,
      units = c('billion US$2017/yr','US$2017/ca/yr')
    ) %>%
    filter(variable == "TaxesCa|GHG") %>%
    select(-variable, -unit) %>%
    crossing(decileGroup = 1:10) %>%
    arrange(scenario, region, period, decileGroup) %>%
    left_join(weight, by = c('scenario', 'region','period','decileGroup')) %>%
    mutate(transferNeut = value * consShare * 10) %>%
    select(-value, -consShare)
  
  decileWelfChange <- decileConsShare %>%
    select(scenario, region, period, decileGroup, consumptionCa) %>% 
    left_join(transferEpc,by = c('scenario', 'region', 'period','decileGroup')) %>%
    left_join(transferNeut,by = c('scenario', 'region', 'period','decileGroup')) %>%
    mutate(transferEpc = if_else(scenario == "C_SSP2-NPi2025", 0, transferEpc),
           transferNeut = if_else(scenario == "C_SSP2-NPi2025", 0, transferNeut),
           consumptionCa  = consumptionCa - transferNeut + transferEpc) %>%
    select(-transferNeut, -transferEpc) %>%
    pivot_wider(names_from = scenario, values_from = consumptionCa) %>%
    {
      wide <- .
      # identify scenario columns (everything that isn’t an id column)
      id_cols <- c("region", "period", "decileGroup")
      scen_cols <- setdiff(names(wide), id_cols)
      
      # baseline column = any column whose name contains reference_run_name (first match if multiple)
      baseline_col <- scen_cols[str_detect(scen_cols, fixed(reference_run_name))]
      
      # policy columns = any columns whose names contain any of the all_budgets tokens
      policy_regex <- paste(all_budgets, collapse = "|")
      policy_cols <- scen_cols[str_detect(scen_cols, policy_regex)]
      
      wide %>%
        mutate(.base = .data[[baseline_col]]) %>%
        mutate(
          across(
            all_of(policy_cols),
            ~ log(.x / .base) * 100
          )
        ) %>%
        select(all_of(id_cols), all_of(policy_cols)) %>%
        pivot_longer(
          cols = all_of(policy_cols),
          names_to = "scenario",
          values_to = "decilWelfChange"
        ) %>%
        mutate(category = "Consumption With EpcTransf")
    } %>%
    bind_rows(decileWelfChange)
  
  
  # # Neutral: proportional
  # weight <- decileConsShare %>%
  #   filter(scenario == 'C_SSP2-NPi2025') %>%
  #   select(scenario, region, period, decileGroup, consumptionCa) %>%
  #   mutate(
  #     total = sum(consumptionCa, na.rm = TRUE),
  #     consShare = if_else(total > 0, consumptionCa / total, NA_real_),
  #     .by = c(scenario, region, period)
  #   ) %>%
  #   select(-total, -consumptionCa,-scenario) %>%
  #   tidyr::crossing(scenario = paste0( 'C_',all_runscens,'-',all_budgets )) 
    


    
  return(decileWelfChange)
  
}
