#---This function simulates the welfare effect of price changes----------------



#-------------1. First-order welfare effect-------------------------

predict_decileWelfChange <- function(data1 = data, data2 = decileConsShare, micro_model = 'FOwelfare', fixed_point = 'base'){
  
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
        str_detect(scenario, "PkBudg650|PkBudg1000")
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
    
  } else if ( length(sectors) == 9 ){
    
    deltPriceEne <- data1 %>%
      select(-unit,-baseline)%>%
      filter(
        str_starts(variable, "deltPrice"),
        str_detect(scenario, "PkBudg650|PkBudg1000")
      ) %>%
      mutate(      
        variable = case_when(
        variable ==  "deltPrice|Transport|FE" ~ "deltPrice|Transport energy",
        variable ==  "deltPrice|Buildings|Electricity" ~ "deltPrice|Building electricity" ,
        variable == "deltPrice|Buildings|Other fuels" ~ "deltPrice|Building other fuels",
        variable == "deltPrice|Buildings|Gases" ~ "deltPrice|Building gases",
        TRUE ~ variable  # keep unchanged otherwise
      ))
    
    deltPriceFood <- deltPriceEne %>%
      mutate(
        value = 0,
        variable = case_when(
          variable ==  "deltPrice|Transport energy" ~ "deltPrice|Staple",
          variable ==  "deltPrice|Building electricity" ~ "deltPrice|Animal products" ,
          variable == "deltPrice|Building other fuels" ~ "deltPrice|Fruits vegetables nuts",
          variable == "deltPrice|Building gases" ~ "deltPrice|Empty calories",
          TRUE ~ variable  # keep unchanged otherwise
        )
      )
    
    deltPriceComm <- deltPriceEne %>% 
      filter( variable == "deltPrice|Transport energy") %>%
      mutate( value = 0,
              variable = 'deltPrice|Other commodities')
  }
  
  
  deltPrice <- bind_rows(deltPriceFood,deltPriceEne,deltPriceComm) %>%
    separate(col = variable,into = c("variable", "category"), sep = "\\|") %>%
    rename(deltPrice = value) %>%
    select(-variable)
  
  
  if(micro_model == 'FOwelfare'){
    
    if(fixed_point =='policy'){
      
      decileWelfChange<-
        data2 %>%
        select(-consumptionCa)%>%
        filter(scenario  %in% paste0( 'C_',all_runscens,'-',all_budgets,'-rem-5'  ))%>%
        pivot_longer( cols = starts_with('share'),
                      names_to = 'variable',
                      values_to = 'share')%>%
        separate( col= variable,into = c("variable", "category"), sep = "\\|"  )%>%
        select(-variable ) %>%
        left_join(deltPrice %>% select(-model), by = c('scenario','region','period','category')) %>%
        group_by(scenario, region, period, decileGroup, category)%>%
        summarise(
          decilWelfChange = -  sum(deltPrice * share) * 100,
          .groups = "drop"
        ) 
      
    }else if (fixed_point == 'base'){
      
      baseConsShare <- data2 %>%
        filter(scenario == 'C_SSP2-NPi-rem-5') %>%
        select( -consumptionCa, -scenario)
      
      decileWelfChange <- data2 %>%
        filter(scenario  %in% paste0( 'C_',all_runscens,'-',all_budgets,'-rem-5')) %>%
        select(scenario,region,period, decileGroup) %>%
        merge(baseConsShare, by = c( "region", "period", "decileGroup")) %>%
        pivot_longer( cols = starts_with('share'),
                      names_to = 'variable',
                      values_to = 'share')%>%
        separate( col= variable,into = c("variable", "category"), sep = "\\|"  )%>%
        select(-variable ) %>%
        left_join(deltPrice, by = c('scenario','region','period','category')) %>%
        group_by(scenario, region, period, decileGroup,category)%>%
        summarise(
          decilWelfChange = -  sum(deltPrice * share) * 100,
          .groups = "drop"
        ) 
      
    } else if (fixed_point == 'midpoint') {
      
      baseConsShare <- data2 %>%
        filter(scenario == 'C_SSP2-NPi-rem-5') %>%
        select( -consumptionCa, -scenario) %>%
        rename_with(
          .fn = ~ paste0(., "|Base"),
          .cols = starts_with("share|")
        )
      
      df<-data2 %>%
        select( -consumptionCa)%>%
        filter(scenario  %in% paste0( 'C_',all_runscens,'-',all_budgets,'-rem-5')) %>%
        merge(baseConsShare, by = c( "region", "period", "decileGroup")) 
      
      
      for (s in sectors) {
        orig_col <- paste0("share|", s)
        base_col <- paste0("share|", s, "|Base")
        avg_col  <- paste0("share|", s, "|Avg")
        
        df[[avg_col]] <- (df[[orig_col]] + df[[base_col]]) / 2
      }
      
      decileWelfChange <- df %>%
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
  
  
  
  
  return(decileWelfChange)
  
}
