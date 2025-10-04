# This function computes the inequality metrics for welfare change

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

  

  #--------Compute the post inequality metrics (all channels included With neutral tranfer)-----------
  #get the total log points change
  welfByDecile <- aggregate_decileWelfChange(data1 = decileWelfChange, 
                                             data2 = decileConsShare, level = c("totalWithTransfNeut"), 
                                             region = 'decile')
  
  pop <- data3 %>%filter(variable == 'Population') %>% select(-unit,-variable,-baseline, -model) %>%
    rename(population = value)
  

  #Compute the prior and post inequality metrics over all categories
  dfIneq <- consBase %>%
    merge( welfByDecile, by = c('scenario', 'region', 'period', 'decileGroup')) %>%
    mutate(consumptionCaPost = consumptionCa *  exp(welfChange/100) ) %>%
    group_by(scenario, region, period) %>%
    summarise(
      `ineq|Gini` = Gini(consumptionCa),
      `ineq|TheilT` = compute_theil.wtd(consumptionCa, type = 'T'),
      `ineq|TheilL` = compute_theil.wtd(consumptionCa, type = 'L'),
      `ineq|GiniPost` = Gini(consumptionCaPost),
      `ineq|TheilTPost` = compute_theil.wtd(consumptionCaPost, type = 'T'),
      `ineq|TheilLPost` = compute_theil.wtd(consumptionCaPost,  type = 'L'),
      `ineq|deltGini` = `ineq|GiniPost` - `ineq|Gini`,
      `ineq|deltTheilT` = `ineq|TheilTPost` - `ineq|TheilT`,
      `ineq|deltTheilL` = `ineq|TheilLPost` - `ineq|TheilL`,
      
      .groups = "drop"
    ) %>%
    mutate (category = 'TotalWithTransfNeut')%>%
    pivot_longer(cols = starts_with("ineq|"), names_to = "variable", values_to = "value")
  
  
  #Compute the world inequality metrics
  dfIneq <- consBase %>%
    merge( welfByDecile, by = c('scenario', 'region', 'period', 'decileGroup')) %>%
    mutate(consumptionCaPost = consumptionCa * exp(welfChange/100)) %>%
    merge(pop, by = c('scenario','region','period')) %>%
    mutate(population = population/10) %>%
    group_by(scenario, period) %>%
    summarise(
      `ineq|Gini` = weighted.gini(consumptionCa, population)[['Gini']],
      `ineq|TheilT` = compute_theil.wtd(consumptionCa, population,type = 'T'),
      `ineq|TheilL` = compute_theil.wtd(consumptionCa, population,type = 'L'),
      `ineq|GiniPost` = weighted.gini(consumptionCaPost, population)[['Gini']],
      `ineq|TheilTPost` = compute_theil.wtd(consumptionCaPost, population, type = 'T'),
      `ineq|TheilLPost` = compute_theil.wtd(consumptionCaPost, population, type = 'L'),
      `ineq|deltGini` = `ineq|GiniPost` - `ineq|Gini`,
      `ineq|deltTheilT` = `ineq|TheilTPost` - `ineq|TheilT`,
      `ineq|deltTheilL` = `ineq|TheilLPost` - `ineq|TheilL`,
      .groups = "drop"
    ) %>%
    mutate (category = 'TotalWithTransfNeut',
            region = 'World')%>%
    pivot_longer(cols = starts_with("ineq|"), names_to = "variable", values_to = "value") %>%
    bind_rows(dfIneq)
  
  
  
  
  #--------Compute the post inequality metrics (all channels included and Epc Transfer)-----------
  
  # Epc transfer
  welfByDecileTransfEpc <- aggregate_decileWelfChange(data1 = decileWelfChange, 
                                                   data2 = decileConsShare, 
                                                   level = c("totalWithTransfEpc"), 
                                                   region = 'decile')
  
  #Compute the prior and post inequality metrics over all categories for each region
  dfIneq <- consBase %>%
    merge(   welfByDecileTransf, by = c('scenario', 'region', 'period', 'decileGroup')) %>%
    mutate(consumptionCaPost = consumptionCa * exp(welfChange/100)) %>%
    group_by(scenario, region, period) %>%
    summarise(
      `ineq|Gini` = Gini(consumptionCa),
      `ineq|TheilT` = compute_theil.wtd(consumptionCa, type = 'T'),
      `ineq|TheilL` = compute_theil.wtd(consumptionCa, type = 'L'),
      `ineq|GiniPost` = Gini(consumptionCaPost),
      `ineq|TheilTPost` = compute_theil.wtd(consumptionCaPost, type = 'T'),
      `ineq|TheilLPost` = compute_theil.wtd(consumptionCaPost,  type = 'L'),
      `ineq|deltGini` = `ineq|GiniPost` - `ineq|Gini`,
      `ineq|deltTheilT` = `ineq|TheilTPost` - `ineq|TheilT`,
      `ineq|deltTheilL` = `ineq|TheilLPost` - `ineq|TheilL`,
      
      .groups = "drop"
    ) %>%
    mutate (category = 'TotalWithTransfEpc')%>%
    pivot_longer(cols = starts_with("ineq|"), names_to = "variable", values_to = "value") %>%
    bind_rows(dfIneq)
  
  
  #For world
  dfIneq <- consBase %>%
    merge( welfByDecileTransfEpc, by = c('scenario', 'region', 'period', 'decileGroup')) %>%
    mutate(consumptionCaPost = consumptionCa * exp(welfChange/100)) %>%
    merge(pop, by = c('scenario','region','period')) %>%
    mutate(population = population/10) %>%
    group_by(scenario, period) %>%
    summarise(
      `ineq|Gini` = weighted.gini(consumptionCa, population)[['Gini']],
      `ineq|TheilT` = compute_theil.wtd(consumptionCa, population,type = 'T'),
      `ineq|TheilL` = compute_theil.wtd(consumptionCa, population,type = 'L'),
      `ineq|GiniPost` = weighted.gini(consumptionCaPost, population)[['Gini']],
      `ineq|TheilTPost` = compute_theil.wtd(consumptionCaPost, population, type = 'T'),
      `ineq|TheilLPost` = compute_theil.wtd(consumptionCaPost, population, type = 'L'),
      `ineq|deltGini` = `ineq|GiniPost` - `ineq|Gini`,
      `ineq|deltTheilT` = `ineq|TheilTPost` - `ineq|TheilT`,
      `ineq|deltTheilL` = `ineq|TheilLPost` - `ineq|TheilL`,
      .groups = "drop"
    ) %>%
    mutate (category = 'TotalWithTransfEpc',
            region = 'World')%>%
    pivot_longer(cols = starts_with("ineq|"), names_to = "variable", values_to = "value") %>%
    bind_rows(dfIneq)
  
  
  
  
  #-----------Compute post inequality metrics for categorical impacts-----------
  welfByDecileSec <- aggregate_decileWelfChange(data1 = decileWelfChange, 
                                                data2 = decileConsShare, 
                                                level = c("fullSec"), 
                                                region = 'decile')
  
  
  dfIneq <- consBase %>%
    merge( welfByDecileSec, by = c('scenario', 'region', 'period', 'decileGroup')) %>%
    mutate(consumptionCaPost = consumptionCa * exp(welfChange/100)) %>%
    group_by(scenario, region, period, category) %>%
    summarise(
      `ineq|Gini` = Gini(consumptionCa),
      `ineq|TheilT` = compute_theil.wtd(consumptionCa, type = 'T'),
      `ineq|TheilL` = compute_theil.wtd(consumptionCa, type = 'L'),
      `ineq|GiniPost` = Gini(consumptionCaPost),
      `ineq|TheilTPost` = compute_theil.wtd(consumptionCaPost, type = 'T'),
      `ineq|TheilLPost` = compute_theil.wtd(consumptionCaPost, type = 'L'),
      `ineq|deltGini` = `ineq|GiniPost` - `ineq|Gini`,
      `ineq|deltTheilT` = `ineq|TheilTPost` - `ineq|TheilT`,
      `ineq|deltTheilL` = `ineq|TheilLPost` - `ineq|TheilL`,
      .groups = "drop"
    ) %>%
    pivot_longer(cols = starts_with("ineq|"), names_to = "variable", values_to = "value") %>%
    bind_rows(dfIneq)
  
  dfIneq <-  consBase %>%
    merge( welfByDecileSec, by = c('scenario', 'region', 'period', 'decileGroup')) %>%
    mutate(consumptionCaPost = consumptionCa * exp(welfChange/100)) %>%
    merge(pop, by = c('scenario','region','period')) %>%
    mutate(population = population/10) %>%
    group_by(scenario, period, category) %>%
    summarise(
      `ineq|Gini` = Gini(consumptionCa),
      `ineq|TheilT` = compute_theil.wtd(consumptionCa, population, type = 'T'),
      `ineq|TheilL` = compute_theil.wtd(consumptionCa, population, type = 'L'),
      `ineq|GiniPost` = Gini(consumptionCaPost),
      `ineq|TheilTPost` = compute_theil.wtd(consumptionCaPost, population, type = 'T'),
      `ineq|TheilLPost` = compute_theil.wtd(consumptionCaPost, population, type = 'L'),
      `ineq|deltGini` = `ineq|GiniPost` - `ineq|Gini`,
      `ineq|deltTheilT` = `ineq|TheilTPost` - `ineq|TheilT`,
      `ineq|deltTheilL` = `ineq|TheilLPost` - `ineq|TheilL`,
      .groups = "drop"
    ) %>%
    mutate(region = 'World') %>%
    pivot_longer(cols = starts_with("ineq|"), names_to = "variable", values_to = "value") %>%
    bind_rows(dfIneq)

  
  
  #Shapley docomposition 
  #Step1:channel-wise shock
  dfShock <- consBase %>%
    merge( data1 %>% filter(!str_starts(category, "Consumption")), by = c('scenario', 'region', 'period', 'decileGroup')) %>%
    mutate(shock = consumptionCa * (exp(decilWelfChange/100)-1) ) %>%
    select( -consumptionCa, -decilWelfChange ) 

  df <- dfShock %>%
    left_join(consBase, by = c("scenario", "region", "period", "decileGroup"))
  
  
  # Step 2: Shapley with zero-income exclusion
  compute_shapley<- function(df_group,
                             theil_type = "L",
                             montecarlo = TRUE,
                             n_perms = 300) {
    # 1) Prep
    stopifnot(all(c("category","consumptionCa","shock") %in% names(df_group)))
    all_cats <- sort(unique(df_group$category))
    base <- df_group[df_group$category=='Staples',]$consumptionCa
    
    
    # 2) shocks list aligned by category
    shocks <- lapply(all_cats, function(ct) df_group$shock[df_group$category == ct])
    names(shocks) <- all_cats
    
    # 3) permutations
    if (montecarlo) {
      perms_list <- replicate(n_perms, sample(all_cats), simplify = FALSE)
    } else {
      # requires gtools
      perms_matrix <- gtools::permutations(n = length(all_cats), r = length(all_cats), v = all_cats)
      perms_list <- lapply(seq_len(nrow(perms_matrix)), function(r) perms_matrix[r, ])
    }
    
    # 4) zero-income exclusion mask (apply at each step or once; here we do once based on baseline)
    mask_once <- base > 0
    # If you want stepwise exclusion, move masking inside the j-loop after updating `curr`.
    
    # 5) iterate permutations and accumulate marginals
    marginal_matrix <- matrix(0, nrow = length(perms_list), ncol = length(all_cats))
    colnames(marginal_matrix) <- all_cats
    
    for (i in seq_along(perms_list)) {
      ord <- perms_list[[i]]
      curr <- base
      T_prev <- compute_theil.wtd(curr,type = theil_type)
      
      for (j in seq_along(ord)) {
        ct <- ord[j]
        curr <- curr + shocks[[ct]]
        
        # optional: enforce non-negativity to avoid pathological Theil values
        # curr <- pmax(curr, 0)
        
        T_curr <- compute_theil.wtd(curr,type = theil_type)
        marginal_matrix[i, ct] <- T_curr - T_prev
        T_prev <- T_curr
      }
    }
    
    shapley_values <- colMeans(marginal_matrix, na.rm = TRUE)
    tibble::tibble(category = names(shapley_values), shapley = shapley_values)
  }  
  
  
  # Use Monte Carlo, n_perms = 300 already gives good approximation
  shapleyL <- df %>%
    group_by(scenario, region, period) %>%
    group_modify(~ compute_shapley(.x, theil_type = 'L', montecarlo = montecarlo, n_perms = n_perms)) %>%
    ungroup() %>%
    mutate(variable = "ineq|TheilShapley") %>%
    rename( value = shapley )
  
  shapleyT <- df %>%
    group_by(scenario, region, period) %>%
    group_modify(~ compute_shapley(.x, theil_type = 'T', montecarlo = montecarlo, n_perms = n_perms)) %>%
    ungroup() %>%
    mutate(variable = "ineq|TheilShapley") %>%
    rename( value = shapley )
  
  # Compute the relative shapley contribution
  shapleyRelaL <-   shapleyL %>%
    group_by(scenario, region, period) %>%
    mutate(shapleyRela = abs(value) / sum(abs(value), na.rm = TRUE)) %>%
    ungroup() %>%
    select(-value, -variable) %>%
    mutate(shapleyRela = ifelse(is.nan(shapleyRela), 0, shapleyRela))
  
  shapleyRelaT <-   shapleyT %>%
    group_by(scenario, region, period) %>%
    mutate(shapleyRela = abs(value) / sum(abs(value), na.rm = TRUE)) %>%
    ungroup() %>%
    select(-value, -variable) %>%
    mutate(shapleyRela = ifelse(is.nan(shapleyRela), 0, shapleyRela))

  # Use full permutation
  # shapley_full <- df %>%
  #   filter(period == 2050) %>%
  #   group_by(scenario, region, period) %>%
  #   group_modify(~ compute_shapley_zero_flexible(.x, montecarlo = FALSE)) %>%
  #   ungroup() %>%
  #   rename( shapleyFull = shapley)
  
  #Compute for whole world
  # Use Monte Carlo, n_perms = 300 already gives good approximation
  shapleyWorldL <- df %>%
    group_by(scenario, period) %>%
    group_modify(~ compute_shapley(.x,theil_type = 'L', montecarlo = montecarlo, n_perms = n_perms)) %>%
    ungroup() %>%
    mutate(variable = "ineq|TheilShapley",
           region = 'World') %>%
    rename( value = shapley ) 
  
  shapleyWorldT <- df %>%
    group_by(scenario, period) %>%
    group_modify(~ compute_shapley(.x,theil_type = 'T', montecarlo = montecarlo, n_perms = n_perms)) %>%
    ungroup() %>%
    mutate(variable = "ineq|TheilShapley",
           region = 'World') %>%
    rename( value = shapley ) 
  
  # Compute the relative shapley value
  shapleyWorldRelaL <-   shapleyWorldL %>%
    group_by(scenario, region, period) %>%
    mutate(shapleyRela = abs(value) / sum(abs(value), na.rm = TRUE)) %>%
    ungroup() %>%
    select(-value, -variable) %>%
    mutate(shapleyRela = ifelse(is.nan(shapleyRela), 0, shapleyRela))
  
  shapleyWorldRelaT <-   shapleyWorldT %>%
    group_by(scenario, region, period) %>%
    mutate(shapleyRela = abs(value) / sum(abs(value), na.rm = TRUE)) %>%
    ungroup() %>%
    select(-value, -variable) %>%
    mutate(shapleyRela = ifelse(is.nan(shapleyRela), 0, shapleyRela))
  
  shapleyRelaL <- shapleyWorldRelaL %>%
    bind_rows(shapleyRelaL)
  
  shapleyRelaT <- shapleyWorldRelaT %>%
    bind_rows(shapleyRelaT)
  

  dfIneq <- dfIneq %>%
    filter (category =='TotalWithTransfNeut',variable =='ineq|deltTheilT') %>%
    select(-variable, -category) %>%
    rename(deltTheil = value) %>%
    merge(shapleyRelaT, by = c('scenario','region','period')) %>%
    group_by(scenario,region,period) %>%
    mutate(deltTheilShapley = deltTheil * shapleyRela)%>%
    select( - deltTheil, -shapleyRela) %>%
    mutate(variable = 'ineq|deltTheilTShapley') %>%
    rename(value = deltTheilShapley) %>%
    bind_rows(dfIneq)
  
  result[['ineq']] <- dfIneq %>%
    filter (category =='TotalWithTransfNeut',variable =='ineq|deltTheilL') %>%
    select(-variable, -category) %>%
    rename(deltTheil = value) %>%
    merge(shapleyRelaL, by = c('scenario','region','period')) %>%
    group_by(scenario,region,period) %>%
    mutate(deltTheilShapley = deltTheil * shapleyRela)%>%
    select( - deltTheil, -shapleyRela) %>%
    mutate(variable = 'ineq|deltTheilLShapley') %>%
    rename(value = deltTheilShapley) %>%
    bind_rows(dfIneq) 
  

    
  result[['theilTDecomp']] <- consBase %>%
    merge(welfByDecile, by = c("scenario", "region", "period", "decileGroup")) %>%
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
  
  result[['theilLDecomp']] <- consBase %>%
    merge(welfByDecile, by = c("scenario", "region", "period", "decileGroup")) %>%
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



 
