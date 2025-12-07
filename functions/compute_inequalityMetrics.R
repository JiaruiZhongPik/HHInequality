


compute_inequalityMetrics <- function(data1 = decileWelfChange, 
                                      data2 = decileConsShare, data3 = data, 
                                      montecarlo = TRUE, n_perms = 300){
  
  
  result <- list()
  
  mapping <- all_paths %>%
    select( remind_run, remind_base ) %>%
    mutate(
      remind_base = str_remove(remind_base, "-(rem|mag)-\\d+$"),
      remind_run  = str_remove(remind_run, "-(rem|mag)-\\d+$")
    )
  
  
  if (any(grepl("loOS|hiOS", mapping$remind_run))) {
    mapping <- mapping %>%
      mutate(remind_run = str_replace_all(remind_run, regex("RESCUE-T(?:ier|hier)2"), "SSP2"))
  }
  
  consBase <- data2 %>%
    filter(
      scenario %in% 
        (all_paths$remind_base %>% 
           str_remove("-(rem|mag)-\\d+$") %>% 
           unique())
    ) %>%
    select(-starts_with("share|")) %>%
    rename( remind_base = scenario)
  
  consBase <- data2 %>%
    select(-starts_with("share|"), -consumptionCa) %>%
    filter(scenario  %in% paste0( 'C_',all_runscens,'-',all_budgets)) %>%
    left_join( mapping, by= c('scenario'= 'remind_run')) %>%
    merge(consBase, by = c("region", "period", "decileGroup", "remind_base")) %>%
    select(-remind_base)
  
  pop <- data3 %>%filter(variable == 'Population') %>% select(-unit,-variable,-baseline, -model) %>%
    rename(population = value)
  
  
  #-------Compute the reference inequality --------------------------------------------
  #baseline inequality
  
  ineqRegRef <- consBase %>%
    filter(scenario == paste0( 'C_',all_runscens,'-',all_budgets)[1]) %>% # consBase contains the baseline consumption for policy scenario
    mutate(scenario = "C_SSP2-NPi2025") %>%
    group_by(scenario, region, period) %>%
    summarise(
      `ineq|Gini` = Gini(consumptionCa),
      `ineq|TheilT` = compute_theil.wtd(consumptionCa, type = 'T'),
      `ineq|TheilL` = compute_theil.wtd(consumptionCa, type = 'L'),
      .groups = "drop"
    ) %>% mutate (category = 'Reference')
  
  
  #Reference 
  ineqGlobalRef<- consBase %>%
    filter(scenario == paste0( 'C_',all_runscens,'-',all_budgets)[1]) %>% # consBase contains the baseline consumption for policy scenario
    mutate(scenario = "C_SSP2-NPi2025") %>%
    merge(pop, by = c('scenario','region','period')) %>%
    mutate(population = population/10) %>% 
    group_by(scenario, period) %>%
    summarise(
      `ineq|Gini` = weighted.gini(consumptionCa, population)[['Gini']],
      `ineq|TheilT` = compute_theil.wtd(consumptionCa, population,type = 'T'),
      `ineq|TheilL` = compute_theil.wtd(consumptionCa, population,type = 'L'),
      .groups = "drop"
    ) %>% 
    mutate (category = 'Reference',
            region = 'World')
  
  #-----------------------------------End----------------------------------------
  
  #--------Compute the post inequality metrics (all channels included With neutral tranfer)-----------
  #get the total log points change
  welfByDecileTransfNeut <- aggregate_decileWelfChange(data1 = decileWelfChange, 
                                                       data2 = decileConsShare, level = c("totalWithTransfNeut"), 
                                                       region = 'decile')
  
  
  
  
  #Compute inequality in policy scenario, total with Neut transfer
  ineqRegPolAllNeut <- consBase %>%
    merge( welfByDecileTransfNeut, by = c('scenario', 'region', 'period', 'decileGroup')) %>%
    mutate(consumptionCaPost = consumptionCa *  exp(welfChange/100) ) %>%
    group_by(scenario, region, period) %>%
    summarise(
      `ineq|Gini` = Gini(consumptionCaPost),
      `ineq|TheilT` = compute_theil.wtd(consumptionCaPost, type = 'T'),
      `ineq|TheilL` = compute_theil.wtd(consumptionCaPost,  type = 'L'),
      .groups = "drop"
    ) %>%
    mutate (category = 'TotalWithTransfNeut')
  
  
  
  #Compute the inequality metric for world (use pop weights)
  
  
  #policy
  ineqGlobalPolAllNeut <- consBase %>%
    merge( welfByDecileTransfNeut, by = c('scenario', 'region', 'period', 'decileGroup')) %>%
    mutate(consumptionCaPost = consumptionCa * exp(welfChange/100)) %>%
    merge(pop, by = c('scenario','region','period')) %>%
    mutate(population = population/10) %>%
    group_by(scenario, period) %>%
    summarise(
      `ineq|Gini` = weighted.gini(consumptionCaPost, population)[['Gini']],
      `ineq|TheilT` = compute_theil.wtd(consumptionCaPost, population, type = 'T'),
      `ineq|TheilL` = compute_theil.wtd(consumptionCaPost, population, type = 'L'),
      .groups = "drop"
    ) %>%
    mutate (category = 'TotalWithTransfNeut',
            region = 'World')
  #-----------------------------------End----------------------------------------------
  
  
  #--------Compute the post inequality metrics (all channels included and Epc Transfer)-----------
  
  welfByDecileTransfEpc <- aggregate_decileWelfChange(data1 = decileWelfChange, 
                                                      data2 = decileConsShare, 
                                                      level = c("totalWithTransfEpc"), 
                                                      region = 'decile')
  
  ineqRegPolAllEpc <- consBase %>%
    merge( welfByDecileTransfEpc, by = c('scenario', 'region', 'period', 'decileGroup')) %>%
    mutate(consumptionCaPost = consumptionCa *  exp(welfChange/100) ) %>%
    group_by(scenario, region, period) %>%
    summarise(
      `ineq|Gini` = Gini(consumptionCaPost),
      `ineq|TheilT` = compute_theil.wtd(consumptionCaPost, type = 'T'),
      `ineq|TheilL` = compute_theil.wtd(consumptionCaPost,  type = 'L'),
      .groups = "drop"
    ) %>%
    mutate (category = 'TotalWithTransfEpc')
  
  
  #policy
  ineqGlobalPolAllEpc <- consBase %>%
    merge( welfByDecileTransfEpc, by = c('scenario', 'region', 'period', 'decileGroup')) %>%
    mutate(consumptionCaPost = consumptionCa * exp(welfChange/100)) %>%
    merge(pop, by = c('scenario','region','period')) %>%
    mutate(population = population/10) %>%
    group_by(scenario, period) %>%
    summarise(
      `ineq|Gini` = weighted.gini(consumptionCaPost, population)[['Gini']],
      `ineq|TheilT` = compute_theil.wtd(consumptionCaPost, population, type = 'T'),
      `ineq|TheilL` = compute_theil.wtd(consumptionCaPost, population, type = 'L'),
      .groups = "drop"
    ) %>%
    mutate (category = 'TotalWithTransfEpc',
            region = 'World')
  
  #-----------------------------------End----------------------------------------
  
  
  #--------Compute categorical impacts for regions-------------------------------
  
  # welfByDecileSec <- aggregate_decileWelfChange(data1 = decileWelfChange, 
  #                                               data2 = decileConsShare, 
  #                                               level = c("fullSec"), 
  #                                               region = 'decile')
  
  #Step1:channel-wise shock
  df <- consBase %>%
    merge(
      data1 %>% filter(!str_starts(category, "Consumption")),
      by = c("scenario", "region", "period", "decileGroup"),
      suffixes = c("", "_welf")
    ) %>%
    group_by(scenario, region, period, decileGroup) %>%
    mutate(
      # Baseline total consumption (same for all categories in this group)
      C0 = consumptionCa,
      
      # Log contributions per category (in decimal, not percent)
      d_k = decilWelfChange / 100,
      
      # Relative “raw” contribution from each log shock
      r_k = exp(d_k) - 1,
      r_sum = sum(r_k, na.rm = TRUE),
      
      # Total log change and total level change
      d_tot     = sum(d_k, na.rm = TRUE),
      delta_Ctot = C0 * (exp(d_tot) - 1),
      
      # Weights for allocating total level change
      w_k = ifelse(r_sum == 0, 1 / n(), r_k / r_sum),
      
      # Final sector shock in levels that *sum exactly* to total change
      shock = delta_Ctot * w_k
    ) %>%
    ungroup() %>%
    select(
      scenario, region, period, decileGroup,
      category,
      consumptionCa,  # baseline total
      shock           # sector-wise ΔC_k
      # drop decilWelfChange unless you still need it
    )
  
  
  #helper function for monte carlo simulaiton of sharpley value
  compute_shapley <- function(df_group,
                              index      = c("Theil", "Gini"),
                              theil_type = "L",
                              montecarlo = TRUE,
                              n_perms    = 300) {
    stopifnot(all(c("category", "consumptionCa", "shock") %in% names(df_group)))
    
    index <- match.arg(index)
    all_cats <- sort(unique(df_group$category))
    
    # Baseline: total consumption (once per decile),
    # taken from "Staples" rows as a dedup trick
    base <- df_group$consumptionCa[df_group$category == "Staples"]
    
    # Shocks per category, ordered consistently by decileGroup
    shocks <- lapply(all_cats, function(ct) df_group$shock[df_group$category == ct])
    names(shocks) <- all_cats
    
    # Mask zero baseline
    mask_once <- base > 0
    base      <- base[mask_once]
    shocks    <- lapply(shocks, function(x) x[mask_once])
    
    # Inequality function
    ineq_fun <- switch(
      index,
      "Theil" = function(x) compute_theil.wtd(x, type = theil_type),
      "Gini"  = function(x) Gini(x)
    )
    
    # Permutations
    if (montecarlo) {
      perms_list <- replicate(n_perms, sample(all_cats), simplify = FALSE)
    } else {
      perms_matrix <- gtools::permutations(
        n = length(all_cats),
        r = length(all_cats),
        v = all_cats
      )
      perms_list <- lapply(seq_len(nrow(perms_matrix)), function(r) perms_matrix[r, ])
    }
    
    # Shapley loop
    marginal_matrix <- matrix(0, nrow = length(perms_list), ncol = length(all_cats))
    colnames(marginal_matrix) <- all_cats
    
    for (i in seq_along(perms_list)) {
      ord   <- perms_list[[i]]
      curr  <- base
      T_prev <- ineq_fun(curr)
      
      for (ct in ord) {
        curr   <- curr + shocks[[ct]]
        T_curr <- ineq_fun(curr)
        marginal_matrix[i, ct] <- T_curr - T_prev
        T_prev <- T_curr
      }
    }
    
    shapley_values <- colMeans(marginal_matrix, na.rm = TRUE)
    tibble::tibble(category = names(shapley_values), shapley = shapley_values)
  }
  
  #compute shapley 
  shapleyTheilL <- df %>%
    group_by(scenario, region, period) %>%
    group_modify(~ compute_shapley(.x, index = 'Theil', theil_type = 'L', 
                                   montecarlo = montecarlo, n_perms = n_perms)) %>%
    ungroup() %>%
    mutate(variable = "ineq|shapleyTheilL") %>%
    rename( value = shapley )
  
  shapleyTheilT <- df %>%
    group_by(scenario, region, period) %>%
    group_modify(~ compute_shapley(.x, index = 'Theil', theil_type = 'T', 
                                   montecarlo = montecarlo, n_perms = n_perms)) %>%
    ungroup() %>%
    mutate(variable = "ineq|shapleyTheilT") %>%
    rename( value = shapley )
  
  shapleyGini <- df %>%
    group_by(scenario, region, period) %>%
    group_modify(~ compute_shapley(.x, index = 'Gini', 
                                   montecarlo = montecarlo, n_perms = n_perms)) %>%
    ungroup() %>%
    mutate(variable = "ineq|shapleyGini") %>%
    rename( value = shapley )
  
  #-----------------------------------End-----------------------------------------
  
  
  result[['ineq']] <- bind_rows(ineqRegRef, ineqGlobalRef,
                                ineqRegPolAllNeut, ineqGlobalPolAllNeut,
                                ineqRegPolAllEpc, ineqGlobalPolAllEpc) %>%
    pivot_longer(
      cols = starts_with("ineq|"),
      names_to = "variable",
      values_to = "value"
    ) %>%
    bind_rows(shapleyTheilT, shapleyTheilL, shapleyGini)
  
  
  # Decomposation of between- and within-region inequality
  result[['theilTDecompEpc']] <- consBase %>%
    merge(welfByDecileTransfEpc, by = c("scenario", "region", "period", "decileGroup")) %>%
    mutate(consumptionCaPost = consumptionCa * exp(welfChange/100)) %>%
    merge(pop, by = c("scenario", "region", "period")) %>%
    mutate(population = population / 10) %>%
    group_by(scenario, period) %>%
    group_modify(~{
      df <- .x
      
      theil_result <- as.data.frame(iTheilT(df$consumptionCaPost, 
                                            as.integer(as.factor(df$region)), 
                                            w = df$population))
      
      theil_result_base <- as.data.frame(iTheilT(df$consumptionCa, 
                                                 as.integer(as.factor(df$region)), 
                                                 w = df$population))
      
      # Rename base columns to avoid conflicts
      names(theil_result_base) <- paste0(names(theil_result_base), "|base")
      
      df <- cbind(df, theil_result, theil_result_base)
    })
  
  result[['theilLDecompEpc']] <- consBase %>%
    merge(welfByDecileTransfEpc, by = c("scenario", "region", "period", "decileGroup")) %>%
    mutate(consumptionCaPost = consumptionCa * exp(welfChange/100)) %>%
    merge(pop, by = c("scenario", "region", "period")) %>%
    mutate(population = population / 10) %>%
    group_by(scenario, period) %>%
    group_modify(~{
      df <- .x
      
      theil_result <- as.data.frame(iTheilL(df$consumptionCaPost, 
                                            as.integer(as.factor(df$region)), 
                                            w = df$population))
      
      theil_result_base <- as.data.frame(iTheilL(df$consumptionCa, 
                                                 as.integer(as.factor(df$region)), 
                                                 w = df$population))
      
      # Rename base columns to avoid conflicts
      names(theil_result_base) <- paste0(names(theil_result_base), "|base")
      
      df <- cbind(df, theil_result, theil_result_base)
    })
  
  
  
  result[['theilTDecompNeut']] <- consBase %>%
    merge(welfByDecileTransfNeut, by = c("scenario", "region", "period", "decileGroup")) %>%
    mutate(consumptionCaPost = consumptionCa * exp(welfChange/100)) %>%
    merge(pop, by = c("scenario", "region", "period")) %>%
    mutate(population = population / 10) %>%
    group_by(scenario, period) %>%
    group_modify(~{
      df <- .x
      
      theil_result <- as.data.frame(iTheilT(df$consumptionCaPost, 
                                            as.integer(as.factor(df$region)), 
                                            w = df$population))
      
      theil_result_base <- as.data.frame(iTheilT(df$consumptionCa, 
                                                 as.integer(as.factor(df$region)), 
                                                 w = df$population))
      
      # Rename base columns to avoid conflicts
      names(theil_result_base) <- paste0(names(theil_result_base), "|base")
      
      df <- cbind(df, theil_result, theil_result_base)
    })
  
  
  result[['theilLDecompNeut']] <- consBase %>%
    merge(welfByDecileTransfNeut, by = c("scenario", "region", "period", "decileGroup")) %>%
    mutate(consumptionCaPost = consumptionCa * exp(welfChange/100)) %>%
    merge(pop, by = c("scenario", "region", "period")) %>%
    mutate(population = population / 10) %>%
    group_by(scenario, period) %>%
    group_modify(~{
      df <- .x
      
      theil_result <- as.data.frame(iTheilL(df$consumptionCaPost, 
                                            as.integer(as.factor(df$region)), 
                                            w = df$population))
      
      theil_result_base <- as.data.frame(iTheilL(df$consumptionCa, 
                                                 as.integer(as.factor(df$region)), 
                                                 w = df$population))
      
      # Rename base columns to avoid conflicts
      names(theil_result_base) <- paste0(names(theil_result_base), "|base")
      
      df <- cbind(df, theil_result, theil_result_base)
    })
  
  
  
  
  
  return(result)
  
  
  
  
}

