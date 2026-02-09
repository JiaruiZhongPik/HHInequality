# This function computes all inequality metrics, TheilL, TheilT and Gini.
# It returns a list of 3 component:
#
# $ineqAll: stores the overall inequality change of each country-year-scenario and in 
# all 3 inequaliyt metrics
#
# $theilLDecomp: stores the between and within country decomposation of TheilL,
# 
# $theilTDecomp: stores the between adn within country decomposation of TheilT


compute_inequalityOutcomes <- function(
    decileWelfChange,
    decileConsShare,
    anchRealCons,
    data,
    doChecks = TRUE
) {
  
  # Validate required globals are in scope
  required_globals <- c("all_runscens", "reference_run_name")
  missing <- required_globals[!sapply(required_globals, exists, where = parent.frame())]
  
  if (length(missing) > 0) {
    stop("Function requires these variables in calling environment: ", 
         paste(missing, collapse = ", "),
         "\nMake sure they are defined in the script before calling this function.")
  }
  
  result <- list()
  
  # Dynamically extract sectors from decileConsShare share| columns
  sectors <- decileConsShare %>%
    dplyr::select(dplyr::starts_with("share|")) %>%
    names() %>%
    stringr::str_remove("^share\\|")
  
  baselineScenario <- paste0('C_',all_runscens,'-',reference_run_name)
  
  anchRealCons <- anchRealCons %>% mutate(decilePop_mil = Pop_mil/10 )
  
  # -----------------------------
  # 1) Inequality indices (Region + World) from anchored per-capita levels
  #    - Region: equal weight across 10 deciles (population deciles)
  #    - World : population-weighted across all region-deciles
  # -----------------------------
  ineqReg <- anchRealCons %>%
    dplyr::group_by(scheme, scenario, region, period) %>%
    dplyr::summarise(
      `ineq|Gini`   = Gini(consDecile_pc),
      `ineq|TheilT` = compute_theil.wtd(consDecile_pc, type = "T"),
      `ineq|TheilL` = compute_theil.wtd(consDecile_pc, type = "L"),
      .groups = "drop"
    ) %>%
    dplyr::mutate(category = "Anchored")
  
  ineqWorld <- anchRealCons %>%
    dplyr::group_by(scheme, scenario, period) %>%
    dplyr::summarise(
      `ineq|Gini`   = weighted.gini(consDecile_pc, decilePop_mil)[["Gini"]],
      `ineq|TheilT` = compute_theil.wtd(consDecile_pc, decilePop_mil, type = "T"),
      `ineq|TheilL` = compute_theil.wtd(consDecile_pc, decilePop_mil, type = "L"),
      .groups = "drop"
    ) %>%
    dplyr::mutate(region = "World", category = "Anchored")
  
  # (optional) explicitly tag reference rows
  ineqRegRef <- decileConsShare %>%
    dplyr::filter(scenario == baselineScenario, region != "World") %>%
    dplyr::group_by(region, period) %>%
    dplyr::summarise(
      `ineq|Gini`   = Gini(consumptionCa),
      `ineq|TheilT` = compute_theil.wtd(consumptionCa, type = "T"),
      `ineq|TheilL` = compute_theil.wtd(consumptionCa, type = "L"),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      scheme   = NA_character_,
      scenario = baselineScenario,
      category = "Reference"
    )
  
  macroPop_base <- data %>%
    dplyr::filter(variable == "Population", region != "World", scenario == baselineScenario) %>%
    dplyr::select(region, period, Pop_mil = value)
  
  ineqWorldRef <- decileConsShare %>%
    dplyr::filter(scenario == baselineScenario, region != "World") %>%
    dplyr::left_join(macroPop_base, by = c("region","period")) %>%
    dplyr::mutate(decilePop_mil = Pop_mil / 10) %>%
    dplyr::group_by(period) %>%
    dplyr::summarise(
      `ineq|Gini`   = weighted.gini(consumptionCa, decilePop_mil)[["Gini"]],
      `ineq|TheilT` = compute_theil.wtd(consumptionCa, decilePop_mil, type = "T"),
      `ineq|TheilL` = compute_theil.wtd(consumptionCa, decilePop_mil, type = "L"),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      scheme   = NA_character_,
      scenario = baselineScenario,
      region   = "World",
      category = "Reference"
    )
  
  # -----------------------------
  # 2) Theil within/between decomposition at World level (for each scheme)
  #    using anchored levels + decile population weights
  # -----------------------------
  theil_decomp <- function(df_scheme, theil_fun = c("T", "L"),
                                           value_col = "consDecile_pc",
                                           base_col  = "c_base_pc",
                                           w_col     = NULL) {
    theil_fun <- match.arg(theil_fun)
    
    
    df_scheme %>%
      dplyr::group_by(scheme, scenario, period) %>%
      dplyr::group_modify(function(.x, .y) {
        
        df <- .x
        
        if (is.null(w_col)) {
          w <- df$Pop_mil / 10
        } else {
          w <- df[[w_col]]
        }
        
        grp <- as.integer(as.factor(df$region))
        
        if (theil_fun == "T") {
          theil_result      <- as.data.frame(iTheilT(df[[value_col]], grp, w = w))
          theil_result_base <- as.data.frame(iTheilT(df[[base_col]],  grp, w = w))
        } else {
          theil_result      <- as.data.frame(iTheilL(df[[value_col]], grp, w = w))
          theil_result_base <- as.data.frame(iTheilL(df[[base_col]],  grp, w = w))
        }
        
        # rename base columns to avoid collisions
        names(theil_result_base) <- paste0(names(theil_result_base), "|base")
        
        # IMPORTANT: bind back onto df, preserving region/decile rows
        out <- dplyr::bind_cols(df, theil_result, theil_result_base) 
        

      }) %>%
      dplyr::ungroup() %>%
      dplyr::select(
        scheme,
        scenario,
        period,
        region,
        decileGroup,
        dplyr::matches("^(Tt|Tl)\\.")
      ) 
    

  }
  
  

  
  result[["theilTDecomp"]] <- theil_decomp(anchRealCons, "T") 
  result[["theilLDecomp"]] <- theil_decomp(anchRealCons, "L") %>%
    rename(Tl.b = Tl.ib,
           `Tl.b|base` = `Tl.ib|base`)
  
  # -----------------------------
  # 4) Assemble tidy output: ineq table 
  # -----------------------------
  result[["ineqAll"]] <- dplyr::bind_rows(
    ineqRegRef, ineqWorldRef,
    ineqReg %>% dplyr::filter(scenario != baselineScenario),
    ineqWorld %>% dplyr::filter(scenario != baselineScenario)
  ) %>%
    tidyr::pivot_longer(
      cols = dplyr::starts_with("ineq|"),
      names_to = "variable",
      values_to = "value"
    ) %>% 
    mutate(
      category = dplyr::case_when(
        scheme == "EPC"  ~ "TotalWithTransfEpc",
        scheme == "Neut" ~ "TotalWithTransfNeut",
        TRUE             ~ category
      )
    ) %>%
    select(-scheme)
  
  return(result)
}



