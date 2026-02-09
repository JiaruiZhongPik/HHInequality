#This function Constructs micro-anchored, decile-level real consumption outcomes 
#under neutral and equal-per-capita (EPC) recycling schemes, fully consistent 
#with REMIND’s regional aggregate consumption.


compute_anchoredRealCons <- function(
    decileWelfChange,
    decileConsShare,
    data
) {
  
  # Validate required globals are in scope
  required_globals <- c("all_runscens", "reference_run_name")
  missing <- required_globals[!sapply(required_globals, exists, where = parent.frame())]
  
  if (length(missing) > 0) {
    stop("Function requires these variables in calling environment: ", 
         paste(missing, collapse = ", "),
         "\nMake sure they are defined in the script before calling this function.")
  }
  
  baselineScenario <- paste0('C_',all_runscens,'-',reference_run_name)
  weightCol = "consumptionCa"
  
  stopifnot(all(c("scenario","region","period","decileGroup") %in% names(decileWelfChange)))
  stopifnot(all(c("scenario","region","period","decileGroup", weightCol) %in% names(decileConsShare)))
  stopifnot(all(c("scenario","region","period","variable","value") %in% names(data)))


  # --- baseline decile per-capita consumption (US$2017/person/yr) ---
  base_decile <- decileConsShare %>%
    dplyr::filter(scenario == baselineScenario) %>%
    dplyr::transmute(region, period, decileGroup, c_base_pc = .data[[weightCol]])
  
  # --- macro totals (billion US$2017/yr) & population (million) ---
  macro <- data %>%
    dplyr::filter(region != "World", variable %in% c("Consumption","Population")) %>%
    dplyr::select(scenario, region, period, variable, value) %>%
    tidyr::pivot_wider(names_from = variable, values_from = value) %>%
    dplyr::rename(C_total_bil = Consumption, Pop_mil = Population) %>%
    dplyr::mutate(C_pc = (C_total_bil * 1e9) / (Pop_mil * 1e6))
  
  # --- compute decile-level dln on real consumption ---
  dln_neut <- aggregate_decileWelfChange(
    data1 = decileWelfChange,
    data2 = decileConsShare,
    secLevel = "totalWithTransfNeut",
    scope = "decile",
    weightScenario = baselineScenario,
    weightCol = weightCol
  ) %>%
    dplyr::filter(category == "TotalWithTransfNeut") %>%
    dplyr::transmute(
      scenario, region, period, decileGroup,
      dln_neut = welfChange
    )
  
  dln_epc <- aggregate_decileWelfChange(
    data1 = decileWelfChange,
    data2 = decileConsShare,
    secLevel = "totalWithTransfEpc",
    scope = "decile",
    weightScenario = baselineScenario,
    weightCol = weightCol
  ) %>%
    dplyr::filter(category == "TotalWithTransfEpc") %>%
    dplyr::transmute(
      scenario, region, period, decileGroup,
      dln_epc = welfChange
    )
  
  # helper function for anchoring
  anchor_one <- function(dln_tbl, dln_col, scheme_label) {

    out <- dln_tbl %>%
      dplyr::left_join(base_decile, by = c("region","period","decileGroup")) %>%
      dplyr::mutate(
        consMultiplier = exp(.data[[dln_col]] / 100),
        c_pol_tilde_pc = c_base_pc * consMultiplier
      ) %>%
      dplyr::group_by(scenario, region, period) %>%
      dplyr::mutate(
        relConsIndex = c_pol_tilde_pc / mean(c_pol_tilde_pc, na.rm = TRUE)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::left_join(macro, by = c("scenario","region","period")) %>%
      dplyr::mutate(
        consDecile_total_bil = C_total_bil * (relConsIndex / 10),
        consDecile_pc        = C_pc * relConsIndex,
        # Log-change: better for decomposition/elasticity (symmetric)
        dln_pc_vs_base       = 100 * log(consDecile_pc / c_base_pc),
        # Linear percentage change: more intuitive for interpretation
        relChange_pc_vs_base = (consDecile_pc / c_base_pc - 1) * 100,
        scheme               = scheme_label
      )
    
    
    out %>%
      dplyr::select(
        scenario, region, period, decileGroup,
        relConsIndex, C_total_bil, Pop_mil, c_base_pc,
        consDecile_total_bil, consDecile_pc,
        relChange_pc_vs_base, scheme
      )
  }
  
  dplyr::bind_rows(
    anchor_one(dln_neut, "dln_neut", "Neut"),
    anchor_one(dln_epc,  "dln_epc",  "EPC")
  )
}


