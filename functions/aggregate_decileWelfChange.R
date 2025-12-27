

aggregate_decileWelfChange <- function( data1 = decileWelfChange, 
                                        data2 = decileConsShare,
                                        level = c("fullSec", "groupedSec", 
                                                  "totalSec", "totalWithTransfEpc", 
                                                  "totalWithTransfNeut","all"), 
                                        region = 'region') {
  
  if(level == 'all'){
    
    welf <- data1
  
  } else if( level %in% c("fullSec", "groupedSec", 
                          "totalSec") ){
    welf <- data1 %>%
      filter(!str_starts(category, "Consumption") &
               !stringr::str_detect(category, "transfer effect"))
    
  } else if(level == "totalWithTransfEpc" ) {
    
    welf <- data1 %>%
      filter(!(str_starts(category, "Consumption") & category != "Consumption With EpcTransf") &
               !stringr::str_detect(category, "transfer effect"))
    
  } else if (level == "totalWithTransfNeut") {
    
    welf <- data1 %>%
      filter(!(str_starts(category, "Consumption") & category != "Consumption With NeutTransf")&
               !stringr::str_detect(category, "transfer effect"))
  
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
      level == "fullSec" ~ category,
      level == "groupedSec" & category %in% food_cats ~ "Food",
      level == "groupedSec" & category %in% energy_cats ~ "Energy",
      level == "groupedSec" & category == "Other commodities" ~ "Other commodities",
      level == "groupedSec" & category == "Consumption" ~ "Consumption",
      level == "totalSec" ~ "TotalSec",
      level == "totalWithTransfEpc" ~ "TotalWithTransfEpc",
      level == "totalWithTransfNeut" ~ "TotalWithTransfNeut",
      level == "all" ~ category
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
    
  }else if ( region == 'globalDecile' ) {
    
    welfAgg <-  df %>%
      group_by(scenario, period, decileGroup, group) %>%
      summarise(
        weighted_welfare = sum( welfChangeGroup * consumptionCa, na.rm = TRUE) / sum(consumptionCa, na.rm = TRUE),
        .groups = "drop"
      )%>%
      rename(welfChange = weighted_welfare,
             category = group)
    
  }
  
  return(welfAgg)
}





  


