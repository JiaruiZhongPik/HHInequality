#This function Constructs micro-anchored, decile-level real consumption outcomes 
#under neutral and equal-per-capita (EPC) recycling schemes, fully consistent 
#with REMIND’s regional aggregate consumption.


compute_anchoredRealCons <- function(
    decileWelfChange,
    decileConsShare,
    data,
    aggregate_decileWelfChange_fn = aggregate_decileWelfChange,
    doChecks = TRUE
) {
  
  baselineScenario <- paste0('C_',all_runscens,'-',reference_run_name)
  weightCol = "consumptionCa"
  
  # --- basic checks ---
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
  
  # --- helper: produce dln (log points) from your aggregation function ---
  get_dln <- function(secLevel, category_keep, out_col) {
    aggregate_decileWelfChange_fn(
      data1 = decileWelfChange,
      data2 = decileConsShare,
      secLevel = secLevel,
      scope = "decile",
      weightScenario = baselineScenario,
      weightCol = weightCol
    ) %>%
      dplyr::filter(category == category_keep) %>%
      dplyr::select(scenario, region, period, decileGroup, !!out_col := welfChange)
  }
  
  dln_neut <- get_dln(
    secLevel = "totalWithTransfNeut",
    category_keep = "TotalWithTransfNeut",
    out_col = rlang::sym("dln_neut")
  )
  
  dln_epc <- get_dln(
    secLevel = "totalWithTransfEpc",
    category_keep = "TotalWithTransfEpc",
    out_col = rlang::sym("dln_epc")
  )
  
  # --- core anchoring (shared) ---
  anchor_one <- function(dln_tbl, dln_col, scheme_label) {
    dln_sym <- rlang::sym(dln_col)
    
    out <- dln_tbl %>%
      dplyr::left_join(base_decile, by = c("region","period","decileGroup")) %>%
      dplyr::mutate(
        m = exp(!!dln_sym / 100),
        c_pol_tilde_pc = c_base_pc * m
      ) %>%
      dplyr::group_by(scenario, region, period) %>%
      dplyr::mutate(
        phi = c_pol_tilde_pc / mean(c_pol_tilde_pc, na.rm = TRUE)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::left_join(macro, by = c("scenario","region","period")) %>%
      dplyr::mutate(
        consDecile_total_bil = C_total_bil * (phi / 10),
        consDecile_pc        = C_pc * phi,
        dln_pc_vs_base       = 100 * log(consDecile_pc / c_base_pc),
        relChange_pc_vs_base = (consDecile_pc / c_base_pc - 1) * 100,
        scheme               = scheme_label
      )
    
    if (doChecks) {
      chk <- out %>%
        dplyr::group_by(scenario, region, period) %>%
        dplyr::summarise(
          phiMean = mean(phi, na.rm = TRUE),
          totalSum_bil = sum(consDecile_total_bil, na.rm = TRUE),
          C_total_bil = dplyr::first(C_total_bil),
          pcMean = mean(consDecile_pc, na.rm = TRUE),
          C_pc = dplyr::first(C_pc),
          .groups = "drop"
        )
      
      if (any(abs(chk$phiMean - 1) > 1e-8, na.rm = TRUE)) warning(scheme_label, ": mean(phi)!=1 somewhere.")
      if (any(abs(chk$totalSum_bil - chk$C_total_bil) > 1e-6, na.rm = TRUE)) warning(scheme_label, ": totals don't sum to macro somewhere.")
      if (any(abs(chk$pcMean - chk$C_pc) > 1e-6, na.rm = TRUE)) warning(scheme_label, ": mean(pc) != macro pc somewhere.")
    }
    
    out %>%
      dplyr::select(
        scenario, region, period, decileGroup,
        !!dln_sym, m, phi,
        C_total_bil, Pop_mil, C_pc, c_base_pc,
        consDecile_total_bil, consDecile_pc,
        dln_pc_vs_base, relChange_pc_vs_base,
        scheme
      )
  }
  
  dplyr::bind_rows(
    anchor_one(dln_neut, "dln_neut", "Neut"),
    anchor_one(dln_epc,  "dln_epc",  "EPC")
  )
}


