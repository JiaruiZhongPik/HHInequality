
write_results <- function (data, decileConsShare,
                           decileWelfChange, anchRealCons,
                           ineqAll, ineqChannel){
  
  # Validate required globals are in scope
  required_globals <- c("outputPath")
  missing <- required_globals[!sapply(required_globals, exists, where = parent.frame())]
  
  if (length(missing) > 0) {
    stop("Function requires these variables in calling environment: ", 
         paste(missing, collapse = ", "),
         "\nMake sure they are defined in the script before calling this function.")
  }
  
  # Dynamically extract sectors from decileConsShare share| columns
  sectors <- decileConsShare %>%
    dplyr::select(dplyr::starts_with("share|")) %>%
    names() %>%
    stringr::str_remove("^share\\|")
  
  out <- data %>%
    filter(
      variable %in% c(
        "GDP|MER", "Population", "Consumption",
        "Taxes|CO2|REMIND", "Taxes|GHG|MAGPIE", "Taxes|GHG|REMIND"
      ) |
        startsWith(variable, "Price") |
        startsWith(variable, "share")
    ) %>% 
    select(-baseline) %>%
    mutate(decileGroup = 'all')
  
  
  out <-   decileConsShare %>%
    pivot_longer(
      cols = -c(scenario, region, period, decileGroup),
      names_to  = "variable",
      values_to = "value",
      values_drop_na = TRUE
    ) %>%
    transmute(
      region      = as.character(region),
      period      = as.numeric(period),
      decileGroup = as.character(decileGroup),
      variable    = as.character(variable),
      value       = as.numeric(value),
      scenario    = as.character(scenario),
      unit        = dplyr::case_when(
        startsWith(variable, "share|") ~ "unitless",
        variable == "consumptionCa"    ~ "US$2017/yr",
        TRUE ~ NA_character_
      ),
      model       = "micro"
    ) %>%
    rbind(out)
  
  
  out <- decileWelfChange %>%
    transmute(
      region      = as.character(region),
      period      = as.numeric(period),
      decileGroup = as.character(decileGroup),
      variable    = case_when(
        category %in% sectors ~ paste0("dlnCons|goods|", category),
        
        str_detect(category, "^Consumption pre-transfer")    ~ "dlnCons|account|Consumption pre-transfer (income effect)",
        str_detect(category, "^Neut transfer effect")        ~ "dlnCons|account|Neut transfer effect",
        str_detect(category, "^Consumption With NeutTransf") ~ "dlnCons|account|Consumption With NeutTransf",
        str_detect(category, "^Epc transfer effect")         ~ "dlnCons|account|Epc transfer effect",
        str_detect(category, "^Consumption With EpcTransf")  ~ "dlnCons|account|Consumption With EpcTransf",
        
        TRUE ~ paste0("dlnCons|other|", category)
      ),
      value    = as.numeric(decilWelfChange),  
      scenario = as.character(scenario),
      unit     = "log points",
      model    = "micro"
    ) %>%
    rbind(out)
  
  out <- anchRealCons %>%
    transmute(
      region      = as.character(region),
      period      = as.numeric(period),
      decileGroup = as.character(decileGroup),
      variable = paste0("Consumption|rc=", scheme),
      value       = as.numeric(consDecile_pc),
      scenario    = as.character(scenario),
      unit        = "US$2017/yr",
      model       = "micro"
    )  %>%
    rbind(out)
  
  out <- ineqAll$ineqAll%>%
    transmute(
      region   = as.character(region),
      period   = as.numeric(period),
      variable = paste0(
        as.character(variable),
        "|",
        dplyr::case_when(
          category == "Reference"           ~ "ref",
          category == "TotalWithTransfEpc"  ~ "rc=Epc",
          category == "TotalWithTransfNeut" ~ "rc=Neut",
          TRUE                              ~ category
        )
      ),
      value    = as.numeric(value),
      scenario = as.character(scenario),
      unit     = "unitless",
      model    = "micro",
      decileGroup = 'all'
    ) %>%
    rbind(out)
  
  
  out <- ineqChannel %>%
    transmute(
      region   = as.character(region),
      period   = as.numeric(period),
      variable = paste0(variable, "|", category),
      value    = as.numeric(value),
      scenario = as.character(scenario),
      unit     = "unitless",
      model    = "micro",
      decileGroup = 'all'
    ) %>%
    rbind(out)
  
  
  out_wide <- out %>%
    pivot_wider(
      names_from  = period,
      values_from = value
    )
  
  dir.create(file.path(outputPath, "data"), recursive = TRUE, showWarnings = FALSE)
  write_csv(out, paste0(outputPath,"/data/out_all.csv"))
  
  return(out)
  
}

