compute_priceChannelShapley <- function(
    decileWelfChange,
    decileConsShare,
    data,
    sectors = c("Building electricity", "Building other fuels",
                "Empty calories","Fruits vegetables nuts", "Transport energy",
                "Other commodities", "Building gases", "Staples" ,"Animal products"),
    montecarlo = TRUE,
    n_perms = 300,
    doChecks = TRUE,
    returnDebug = FALSE   # NEW: if TRUE, also return priceOnlyLevels + shockTable
) {
  
  baselineScenario = paste0('C_',all_runscens,'-',reference_run_name)
  
  # -----------------------------
  # 0) Basic checks
  # -----------------------------
  stopifnot(all(c("scenario","region","period","decileGroup","category","decilWelfChange") %in% names(decileWelfChange)))
  stopifnot(all(c("scenario","region","period","decileGroup","consumptionCa") %in% names(decileConsShare)))
  stopifnot(all(c("scenario","region","period","variable","value") %in% names(data)))
  
  # -----------------------------
  # 1) Baseline decile per-capita levels (C0) from decileConsShare
  # -----------------------------
  base_pc_tbl <- decileConsShare %>%
    dplyr::filter(scenario == baselineScenario, region != "World") %>%
    dplyr::select(region, period, decileGroup, C0 = consumptionCa)
  
  if (nrow(base_pc_tbl) == 0) {
    stop("No baseline decile data found in decileConsShare for baselineScenario = ", baselineScenario)
  }
  
  # -----------------------------
  # 2) Price-channel log points table (ensure all sectors exist per group)
  # -----------------------------
  df_price <- decileWelfChange %>%
    dplyr::filter(category %in% sectors) %>%
    dplyr::select(scenario, region, period, decileGroup, category, d_k = decilWelfChange) %>%
    tidyr::complete(
      scenario, region, period, decileGroup,
      category = sectors,
      fill = list(d_k = 0)
    ) %>%
    dplyr::left_join(base_pc_tbl, by = c("region","period","decileGroup")) %>%
    dplyr::filter(!is.na(C0), is.finite(C0), C0 > 0)
  
  if (nrow(df_price) == 0) {
    stop("After filtering/joins, df_price is empty. Check sectors/category names and baseline coverage.")
  }
  
  # -----------------------------
  # 3) Total price-only counterfactual level change per decile
  #    delta_Ctot = C0*(exp(sum d_k)-1)
  # -----------------------------
  price_tot <- df_price %>%
    dplyr::group_by(scenario, region, period, decileGroup) %>%
    dplyr::summarise(
      C0 = dplyr::first(C0),
      d_price_tot = sum(d_k, na.rm = TRUE),   # percent log points
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      C1_priceOnly = C0 * exp(d_price_tot / 100),
      delta_Ctot   = C1_priceOnly - C0
    )
  
  # -----------------------------
  # 4) Allocate level change across goods categories (for Shapley "players")
  #    shock_k = delta_Ctot * w_k, where w_k ∝ r_k = exp(d_k)-1
  # -----------------------------
  df_shock <- df_price %>%
    dplyr::left_join(
      price_tot %>% dplyr::select(scenario, region, period, decileGroup, delta_Ctot),
      by = c("scenario","region","period","decileGroup")
    ) %>%
    dplyr::mutate(
      d_k_dec = d_k / 100,
      r_k = exp(d_k_dec) - 1
    ) %>%
    dplyr::group_by(scenario, region, period, decileGroup) %>%
    dplyr::mutate(
      r_sum = sum(r_k, na.rm = TRUE),
      w_k   = dplyr::if_else(r_sum == 0, 1 / dplyr::n(), r_k / r_sum),
      shock = delta_Ctot * w_k
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(scenario, region, period, decileGroup, goodsCategory = category, C0, shock)
  
  # -----------------------------
  # 5) Shapley computation (Region-level; deciles unweighted)
  # -----------------------------
  compute_shapley <- function(df_group,
                              index = c("Theil", "Gini"),
                              theil_type = "L",
                              montecarlo = TRUE,
                              n_perms = 300) {
    
    index <- match.arg(index)
    all_cats <- sort(unique(df_group$goodsCategory))
    
    # baseline vector per decile
    base_tbl <- df_group %>%
      dplyr::distinct(decileGroup, C0) %>%
      dplyr::arrange(decileGroup)
    base <- base_tbl$C0
    
    # shocks per category aligned by decileGroup
    shocks <- lapply(all_cats, function(ct) {
      df_group %>%
        dplyr::filter(goodsCategory == ct) %>%
        dplyr::arrange(decileGroup) %>%
        dplyr::pull(shock)
    })
    names(shocks) <- all_cats
    
    # mask invalid baseline
    mask <- is.finite(base) & base > 0
    base <- base[mask]
    shocks <- lapply(shocks, function(x) x[mask])
    
    # inequality function
    ineq_local <- switch(
      index,
      "Theil" = function(x) compute_theil.wtd(x, type = theil_type),
      "Gini"  = function(x) Gini(x)
    )
    
    # permutations
    if (montecarlo) {
      perms_list <- replicate(n_perms, sample(all_cats), simplify = FALSE)
    } else {
      perms_matrix <- gtools::permutations(length(all_cats), length(all_cats), v = all_cats)
      perms_list <- lapply(seq_len(nrow(perms_matrix)), function(i) perms_matrix[i, ])
    }
    
    marginal <- matrix(0, nrow = length(perms_list), ncol = length(all_cats))
    colnames(marginal) <- all_cats
    
    for (i in seq_along(perms_list)) {
      ord <- perms_list[[i]]
      curr <- base
      prev <- ineq_local(curr)
      
      for (ct in ord) {
        curr <- curr + shocks[[ct]]
        now  <- ineq_local(curr)
        marginal[i, ct] <- now - prev
        prev <- now
      }
    }
    
    out <- colMeans(marginal, na.rm = TRUE)
    tibble::tibble(goodsCategory = names(out), value = as.numeric(out))
  }
  
  shapleyTheilL <- df_shock %>%
    dplyr::group_by(scenario, region, period) %>%
    dplyr::group_modify(~ compute_shapley(.x, index = "Theil", theil_type = "L",
                                          montecarlo = montecarlo, n_perms = n_perms)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(variable = "ineq|shapleyTheilL")
  
  shapleyTheilT <- df_shock %>%
    dplyr::group_by(scenario, region, period) %>%
    dplyr::group_modify(~ compute_shapley(.x, index = "Theil", theil_type = "T",
                                          montecarlo = montecarlo, n_perms = n_perms)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(variable = "ineq|shapleyTheilT")
  
  shapleyGini <- df_shock %>%
    dplyr::group_by(scenario, region, period) %>%
    dplyr::group_modify(~ compute_shapley(.x, index = "Gini",
                                          montecarlo = montecarlo, n_perms = n_perms)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(variable = "ineq|shapleyGini")
  
  shapley_out <- dplyr::bind_rows(shapleyTheilL, shapleyTheilT, shapleyGini) %>%
    rename(category = goodsCategory)
  
  # -----------------------------
  # 6) Optional checks
  # -----------------------------
  if (doChecks) {
    chk <- df_shock %>%
      dplyr::group_by(scenario, region, period, decileGroup) %>%
      dplyr::summarise(shock_sum = sum(shock, na.rm = TRUE), .groups = "drop") %>%
      dplyr::left_join(
        price_tot %>% dplyr::select(scenario, region, period, decileGroup, delta_Ctot),
        by = c("scenario","region","period","decileGroup")
      ) %>%
      dplyr::mutate(err = shock_sum - delta_Ctot)
    
    bad <- chk %>% dplyr::filter(is.finite(err) & abs(err) > 1e-6)
    if (nrow(bad) > 0) {
      warning("Shapley shock allocation check failed for some rows (sum(shock) != delta_Ctot).")
    }
  }
  
  # -----------------------------
  # 7) Return
  # -----------------------------
  if (returnDebug) {
    return(list(
      ineq = shapley_out,
      priceOnlyLevels = price_tot,
      shockTable = df_shock
    ))
  } else {
    return(
      ineq = shapley_out
    )
  }
}


