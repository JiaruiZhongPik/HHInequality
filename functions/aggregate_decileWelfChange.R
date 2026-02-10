aggregate_decileWelfChange <- function(
    data1 = decileWelfChange,
    data2 = decileConsShare,
    secLevel = c("fullSec", "groupedSec", "totalSec",
                 "totalWithTransfEpc", "totalWithTransfNeut", "all"),
    scope = c("region", "global", "decile", "globalDecile"),
    weightScenario = "C_SSP2-NPi2025",          
    weightCol = "consumptionCa"    
    ) {
  secLevel <- match.arg(secLevel)
  scope    <- match.arg(scope)
  
  # --- 1) Filter components based on secLevel ---
  if (secLevel == "all") {
    welf <- data1
  } else if (secLevel %in% c("fullSec", "groupedSec", "totalSec")) {
    welf <- data1 %>%
      dplyr::filter(
        !stringr::str_starts(category, "Exp"),
        !stringr::str_detect(category, "transfer effect")
      )
  } else if (secLevel == "totalWithTransfEpc") {
    welf <- data1 %>%
      dplyr::filter(
        !(stringr::str_starts(category, "Exp") &
            category != "Exp With EpcTransf"),
        !stringr::str_detect(category, "transfer effect")
      )
  } else if (secLevel == "totalWithTransfNeut") {
    welf <- data1 %>%
      dplyr::filter(
        !(stringr::str_starts(category, "Exp") &
            category != "Exp With NeutTransf"),
        !stringr::str_detect(category, "transfer effect")
      )
  }
  
  # --- 2) Build weights table ---
  if (!weightCol %in% names(data2)) {
    stop("weightCol = '", weightCol, "' not found in data2.")
  }
  
  weight_sym <- rlang::sym(weightCol)
  
  if (!is.null(weightScenario)) {
    weights_tbl <- data2 %>%
      dplyr::filter(scenario == weightScenario) %>%
      dplyr::select(region, period, decileGroup, weight = !!weight_sym)
  } else {
    # scenario-specific weights (generally NOT what you want for consistency)
    weights_tbl <- data2 %>%
      dplyr::select(scenario, region, period, decileGroup, weight = !!weight_sym)
  }
  
  # --- 3) Regroup categories ---
  food_cats   <- c("Animal products", "Empty calories", "Fruits vegetables nuts", "Staples")
  energy_cats <- c("Building electricity", "Building gases", "Building other fuels", "Transport energy")
  
  df <- welf %>%
    dplyr::mutate(group = dplyr::case_when(
      secLevel == "fullSec" ~ category,
      
      secLevel == "groupedSec" & category %in% food_cats ~ "Food",
      secLevel == "groupedSec" & category %in% energy_cats ~ "Energy",
      secLevel == "groupedSec" & category == "Other commodities" ~ "Other commodities",
      secLevel == "groupedSec" & category == "Consumption" ~ "Consumption",
      
      secLevel == "totalSec" ~ "TotalSec",
      secLevel == "totalWithTransfEpc" ~ "TotalWithTransfEpc",
      secLevel == "totalWithTransfNeut" ~ "TotalWithTransfNeut",
      
      secLevel == "all" ~ category,
      TRUE ~ category
    )) %>%
    dplyr::group_by(scenario, region, decileGroup, period, group) %>%
    dplyr::summarise(
      welfChangeGroup = sum(decilWelfChange, na.rm = TRUE),
      .groups = "drop"
    )
  
  # --- 4) Join weights ---
  if (is.null(weightScenario)) {
    df <- df %>%
      dplyr::left_join(
        weights_tbl,
        by = c("scenario", "region", "period", "decileGroup")
      )
  } else {
    df <- df %>%
      dplyr::left_join(
        weights_tbl,
        by = c("region", "period", "decileGroup")
      )
  }
  
  if (!"weight" %in% names(df)) {
    stop("Internal error: weight column missing after join.")
  }
  
  # Optional sanity check: warn if weights missing
  if (any(is.na(df$weight))) {
    warning("Some weights are NA after the join (likely missing baseline weights for some region/decile/period).")
  }
  
  # --- 5) True aggregated log change: log( sum(w * exp(delta)) / sum(w) ) ---
  trueLogAgg <- function(d) {
    d %>%
      dplyr::summarise(
        welfChange = 100 * log(
          sum(.data$weight * exp(.data$welfChangeGroup / 100), na.rm = TRUE) /
            sum(.data$weight, na.rm = TRUE)
        ),
        .groups = "drop"
      )
  }
  
  # --- 6) Aggregate by scope ---
  if (scope == "region") {
    out <- df %>%
      dplyr::group_by(scenario, region, period, group) %>%
      trueLogAgg() %>%
      dplyr::rename(category = group)
    
  } else if (scope == "global") {
    out <- df %>%
      dplyr::group_by(scenario, period, group) %>%
      trueLogAgg() %>%
      dplyr::rename(category = group)
    
  } else if (scope == "decile") {
    out <- df %>%
      dplyr::group_by(scenario, region, period, decileGroup, group) %>%
      trueLogAgg() %>%
      dplyr::rename(category = group)
    
  } else if (scope == "globalDecile") {
    out <- df %>%
      dplyr::group_by(scenario, period, decileGroup, group) %>%
      trueLogAgg() %>%
      dplyr::rename(category = group)
  }
  
  out
}






  


