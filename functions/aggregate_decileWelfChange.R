

aggregate_decileWelfChange <- function( data1 = decileWelfChange, data2 = decileConsShare,
                                        level = c("full", "grouped", "total", "totalWithTransf"), region = 'region') {
  
  if(level != 'totalWithTransf'){
    
    welf <- data1 %>%
      filter(category != 'Transfer')
    
  } else {
    welf <- data1
  }

  
  exp <- data2 %>%
    select( -starts_with("share|") )
  
  
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
      level == "grouped" & category == "Other commodities" ~ "Other commodities",
      level == "grouped" & category == "Consumption" ~ "Consumption",
      level == "total" ~ "Total",
      level == "totalWithTransf" ~ "TotalWithTransf"
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





  


