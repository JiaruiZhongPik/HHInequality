

aggregate_decileWelfChange <- function( data1 = decileWelfChange, data2 = decileConsShare, data3 = data, 
                                        level = c("full", "grouped", "total"), region = 'region') {
  welf = data1
  
  pop = data3 %>%
    filter(variable == 'Population') %>%
    select(-model, -unit, -baseline, -variable) %>%
    rename(pop = value)
  
  # exp = data2 %>%
  #   filter(region != 'World') %>%
  #   merge(pop, by = c('scenario', 'region', 'period')) %>%
  #   mutate(pop = pop/10,                              #compute decile population,mil 
  #          consumption = consumptionCa * pop / 1e3,   #unit of this is billion USD
  #          
  #          `exp|Building gases` = `share|Building gases` * `consumption`,
  #          `exp|Building electricity` = `share|Building electricity` * `consumption`,
  #          `exp|Building other fuels` = `share|Building other fuels` * `consumption`,
  #          `exp|Transport energy` = `share|Transport energy` * `consumption`,
  #          
  #          `exp|Staples` = `share|Staples` * `consumption`,
  #          `exp|Animal products` = `share|Animal products` * `consumption`,
  #          `exp|Fruits vegetables nuts` = `share|Fruits vegetables nuts` * `consumption`,
  #          `exp|Empty calories` = `share|Empty calories` * `consumption`,
  #          
  #          `exp|Other commodities` = `share|Other commodities` * `consumption`,
  #   ) %>%
  #   select(scenario, region, period, decileGroup | starts_with("exp|")) %>%
  #   pivot_longer( cols = starts_with('exp|'), names_to = "variable", values_to = "exp")%>%
  #   separate(variable, into = c("variable","category"), sep = "\\|", extra = "merge", fill = "right") %>%
  #   select( -variable )
  
  exp <- data2 %>%
    select( -starts_with("share|") )
  
  aggregate_welfare <- function(welf, exp, level = c("full", "grouped", "total"), region) {
    level <- match.arg(level)
    
    # Define category groupings
    food_cats <- c("Animal products", "Empty calories", "Fruits vegetables nuts", "Staples")
    energy_cats <- c("Building electricity", "Building gases", "Building other fuels", "Transport energy")
    

    # Re-group categories if needed
    df <-  welf %>%
      mutate(group = case_when(
        level == "full" ~ category,
        level == "grouped" & category %in% food_cats ~ "Food",
        level == "grouped" & category %in% energy_cats ~ "Energy",
        level == "grouped" ~ "Other",
        level == "total" ~ "Total"
      )) %>%
      group_by(scenario, region, decileGroup , period, group) %>%
      summarise( welfChangeGroup = sum(decilWelfChange, na.rm = T),
                 , .groups = "drop") %>%
      left_join( exp, by = c("scenario", "region", "decileGroup",  "period"))

    
    
    if(region == 'region'){
 
      welfAgg <-  df %>%
        group_by(scenario, region, period, group) %>%
        summarise(
          weighted_welfare = sum( welfChangeGroup * consumptionCa, na.rm = TRUE) / sum(consumptionCa, na.rm = TRUE),
          .groups = "drop"
        )%>%
        rename(welfChange = weighted_welfare,
               category = group)
      
    } else if(region =='global') {

      welfAgg <-  df %>%
        group_by(scenario, period, group) %>%
        summarise(
          weighted_welfare = sum( welfChangeGroup * consumptionCa, na.rm = TRUE) / sum(consumptionCa, na.rm = TRUE),
          .groups = "drop"
        )%>%
        rename(welfChange = weighted_welfare,
               category = group)
      
    } else if ( region == 'decile' ) {
      
      welfAgg <-  df %>%
        group_by(scenario, region, period, decileGroup, group) %>%
        summarise(
          weighted_welfare = sum( welfChangeGroup * consumptionCa, na.rm = TRUE) / sum(consumptionCa, na.rm = TRUE),
          .groups = "drop"
        )%>%
        rename(welfChange = weighted_welfare,
               category = group)
      
    }
    
    

    
    return(welfAgg)
  }
  
  welfAgg = aggregate_welfare(welf, exp, level = level, region = region)
  
  return(welfAgg)
}





  


