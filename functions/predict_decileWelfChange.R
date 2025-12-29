#---This function simulates the welfare effect of price changes----------------



#-------------1. First-order welfare effect-------------------------

predict_decileWelfChange <- function(data1 = data, data2 = decileConsShare, 
                                     climaFund = 0,
                                     fund_return_scale = 1,
                                     payg = 1,
                                     micro_model = 'FOwelfare', fixed_point = 'midpoint'){
  
  #get sectors for later use
  sectors <- data2 %>%
    select(starts_with("share|")) %>%
    names() %>%
    stringr::str_remove("^share\\|")  
  
  mapping <- all_paths %>%
    select( remind_run, remind_base ) %>%
    mutate(
      remind_base = str_remove(remind_base, "-(rem|mag)-\\d+$"),
      remind_run  = str_remove(remind_run, "-(rem|mag)-\\d+$")
    )
  
  if (any(grepl("loOS|hiOS", mapping$remind_run))) {
    mapping <- mapping %>%
      mutate(remind_run = str_replace_all(remind_run, regex("RESCUE-T(?:ier|hier)2"), "SSP2"))
  }
  
  
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
      #these, I currently replace with the value from the closest yea
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
  
  
  #Compute transfer

  transferEpc <- compute_transfer(data1 = data1, data2 = data2, 
                                  climaFund = climaFund, fund_return_scale = fund_return_scale, 
                                  payg = payg,
                                  recycle ='epc')
  transferNeut <- compute_transfer(data1 = data1, data2 = data2, 
                                   climaFund = climaFund, fund_return_scale = fund_return_scale,
                                   payg = payg,
                                   recycle ='neut')
  
  # baseline + policy scenario set
  baselineScenario <- paste0('C_',all_runscens,'-',reference_run_name)
  policyRegex <- paste(all_budgets, collapse = "|")
  
  computeLogChangeVsBaseline <- function(df, value_col, baselineScenario, policyRegex, label) {
    id_cols <- c("region", "period", "decileGroup")
    
    base <- df %>%
      dplyr::filter(scenario == baselineScenario) %>%
      dplyr::select(dplyr::all_of(id_cols), base = {{ value_col }})
    
    out <- df %>%
      dplyr::filter(stringr::str_detect(scenario, policyRegex)) %>%
      dplyr::left_join(base, by = id_cols) %>%
      dplyr::mutate(
        decilWelfChange = log({{ value_col }} / base) * 100,
        category = label
      ) %>%
      dplyr::select(scenario, region, period, decileGroup, category, decilWelfChange)
    
    out
  }
  
  # consumption with Neut (your current assumption)
  consNeut <- data2 %>%
    dplyr::select(scenario, region, period, decileGroup, consumptionCa) %>%
    dplyr::left_join(transferNeut, by = c("scenario","region","period","decileGroup")) %>%
    dplyr::mutate(
      transferNeut = dplyr::if_else(scenario == baselineScenario, 0, transferNeut),
      consumptionPre = consumptionCa - transferNeut
    )
  
  incomeEffect <- computeLogChangeVsBaseline(
    df = consNeut,
    value_col = consumptionPre,
    baselineScenario = baselineScenario,
    policyRegex = policyRegex,
    label = "Consumption pre-transfer (income effect)"
  )
  
  
  neutTransferEffect <- consNeut %>%
    dplyr::filter(stringr::str_detect(scenario, policyRegex)) %>%
    dplyr::mutate(
      decilWelfChange = log(consumptionCa / consumptionPre) * 100,
      category = "Neut transfer effect"
    ) %>%
    dplyr::select(scenario, region, period, decileGroup, category, decilWelfChange)
  
  
  neutTotalEffect <- computeLogChangeVsBaseline(
    df = consNeut,
    value_col = consumptionCa,
    baselineScenario = baselineScenario,
    policyRegex = policyRegex,
    label = "Consumption With NeutTransf"
  )
  
  
  consEpc <- consNeut %>%
    dplyr::left_join(transferEpc, by = c("scenario","region","period","decileGroup")) %>%
    dplyr::mutate(
      transferEpc = dplyr::if_else(scenario == baselineScenario, 0, transferEpc),
      consumptionEpc = consumptionPre + transferEpc
    )
  
  
  epcTransferEffect <- consEpc %>%
    dplyr::filter(stringr::str_detect(scenario, policyRegex)) %>%
    dplyr::mutate(
      decilWelfChange = log(consumptionEpc / consumptionPre) * 100,
      category = "Epc transfer effect"
    ) %>%
    dplyr::select(scenario, region, period, decileGroup, category, decilWelfChange)
  
  
  epcTotalEffect <- computeLogChangeVsBaseline(
    df = consEpc,
    value_col = consumptionEpc,
    baselineScenario = baselineScenario,
    policyRegex = policyRegex,
    label = "Consumption With EpcTransf"
  )
  
  
  decileWelfChange <- bind_rows(decileWelfChange, incomeEffect, 
                                neutTransferEffect, neutTotalEffect,
                                epcTransferEffect,epcTotalEffect)

    
  return(decileWelfChange)
  
}
