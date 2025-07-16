# This function computes the inequality metrics for welfare change

compute_inequalityMetrics <- function(data1 = decileWelfChange, 
                                      data2 = decileConsShare, data3 = data, 
                                      montecarlo = TRUE, n_perms = 300){
  
  result <- list()
  
  #Compute the post inequality metrics (all channels included)
  welfByDecile <- aggregate_decileWelfChange(data1 = decileWelfChange, data2 = decileConsShare, data3 = data, level = c("total"), region = 'decile')
  
  pop <- data3 %>%filter(variable == 'Population') %>% select(-unit,-variable,-baseline, -model) %>%
    rename(population = value)
  
  #Compute the prior and post inequality metrics over all categories
  dfIneq <- data2 %>% 
    select(-starts_with("share|")) %>%
    merge( welfByDecile, by = c('scenario', 'region', 'period', 'decileGroup')) %>%
    mutate(consumptionCaPost = consumptionCa * (1 + welfChange/100)) %>%
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
    mutate (category = 'Total')%>%
    pivot_longer(cols = starts_with("ineq|"), names_to = "variable", values_to = "value")
  
  
  #Compute the world inequality metrics
  dfIneq <- data2 %>% 
    select(-starts_with("share|")) %>%
    merge( welfByDecile, by = c('scenario', 'region', 'period', 'decileGroup')) %>%
    mutate(consumptionCaPost = consumptionCa * (1 + welfChange/100)) %>%
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
      `ineq|deltTheilT` = `ineq|TheilTPost` - `ineq|TheilT`,
      `ineq|deltTheilL` = `ineq|TheilLPost` - `ineq|TheilL`,
      .groups = "drop"
    ) %>%
    mutate (category = 'Total',
            region = 'World')%>%
    pivot_longer(cols = starts_with("ineq|"), names_to = "variable", values_to = "value") %>%
    bind_rows(dfIneq)
  
  
  
  
  #Compute post inequality metrics for categorical impacts
  welfByDecileSec <- aggregate_decileWelfChange(data1 = decileWelfChange, data2 = decileConsShare, data3 = data, level = c("full"), region = 'decile')
  
  
  dfIneq <- data2 %>% 
    select(-starts_with("share|")) %>%
    merge( welfByDecileSec, by = c('scenario', 'region', 'period', 'decileGroup')) %>%
    mutate(consumptionCaPost = consumptionCa * (1 + welfChange/100)) %>%
    group_by(scenario, region, period, category) %>%
    summarise(
      `ineq|Gini` = Gini(consumptionCa),
      `ineq|TheilT` = compute_theil.wtd(consumptionCa, type = 'T'),
      `ineq|TheilL` = compute_theil.wtd(consumptionCa, type = 'L'),
      `ineq|GiniPost` = Gini(consumptionCaPost),
      `ineq|TheilTPost` = compute_theil.wtd(consumptionCaPost, type = 'T'),
      `ineq|TheilLPost` = compute_theil.wtd(consumptionCaPost, type = 'L'),
      `ineq|deltTheilT` = `ineq|TheilTPost` - `ineq|TheilT`,
      `ineq|deltTheilL` = `ineq|TheilLPost` - `ineq|TheilL`,
      .groups = "drop"
    ) %>%
    pivot_longer(cols = starts_with("ineq|"), names_to = "variable", values_to = "value") %>%
    bind_rows(dfIneq)
  
  dfIneq <-  data2 %>% 
    select(-starts_with("share|")) %>%
    merge( welfByDecileSec, by = c('scenario', 'region', 'period', 'decileGroup')) %>%
    mutate(consumptionCaPost = consumptionCa * (1 + welfChange/100)) %>%
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
      `ineq|deltTheilT` = `ineq|TheilTPost` - `ineq|TheilT`,
      `ineq|deltTheilL` = `ineq|TheilLPost` - `ineq|TheilL`,
      .groups = "drop"
    ) %>%
    mutate(region = 'World') %>%
    pivot_longer(cols = starts_with("ineq|"), names_to = "variable", values_to = "value") %>%
    bind_rows(dfIneq)

  
  
  #Compute channel-wise shock
  
  dfShock <- data2 %>% 
    select(-starts_with("share|")) %>%
    merge( data1, by = c('scenario', 'region', 'period', 'decileGroup')) %>%
    mutate(shock = consumptionCa * decilWelfChange / 100 ) %>%
    select( -consumptionCa, -decilWelfChange ) 
  
  dfBase <- data2 %>% 
    select(-starts_with("share|")) 
  
  
  
  df <- dfShock %>%
    left_join(dfBase, by = c("scenario", "region", "period", "decileGroup"))
  
  
  # Step 2: Shapley with zero-income exclusion
  compute_shapley_zero_flexible <- function(df_group, montecarlo = TRUE, n_perms = 300) {
    all_cats <- unique(df_group$category)
    base <- df_group[df_group$category=='Staples',]$consumptionCa
    shocks <- split(df_group$shock, df_group$category)
    
    if (montecarlo) {
      # Sampled permutations
      perms_list <- replicate(n_perms, sample(all_cats), simplify = FALSE)
    } else {
      # Full factorial permutations
      perms_matrix <- permutations(n = length(all_cats), r = length(all_cats), v = all_cats)
      perms_list <- split(perms_matrix, seq(nrow(perms_matrix)))
    }
    
    marginal_matrix <- matrix(0, nrow = length(perms_list), ncol = length(all_cats))
    colnames(marginal_matrix) <- all_cats
    
    for (i in seq_along(perms_list)) {
      seq <- perms_list[[i]]
      curr <- base
      T_prev <- Theil(curr)
      
      for (j in seq_along(seq)) {
        curr <- curr + shocks[[seq[j]]]
        T_curr <- Theil(curr)
        marginal_matrix[i, seq[j]] <- T_curr - T_prev
        T_prev <- T_curr
      }
    }
    
    shapley_values <- colMeans(marginal_matrix)
    tibble(category = names(shapley_values), shapley = shapley_values)
  }
  
  
  
  # Use Monte Carlo, n_perms = 300 already gives good approximation
  shapley <- df %>%
    group_by(scenario, region, period) %>%
    group_modify(~ compute_shapley_zero_flexible(.x, montecarlo = montecarlo, n_perms = n_perms)) %>%
    ungroup() %>%
    mutate(variable = "ineq|TheilShapley") %>%
    rename( value = shapley )
  
  # Compute the relative shapley value
  shapleyRela <-   shapley %>%
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
  shapleyWorld <- df %>%
    group_by(scenario, period) %>%
    group_modify(~ compute_shapley_zero_flexible(.x, montecarlo = montecarlo, n_perms = n_perms)) %>%
    ungroup() %>%
    mutate(variable = "ineq|TheilShapley",
           region = 'World') %>%
    rename( value = shapley ) 
  
  # Compute the relative shapley value
  shapleyWorldRela <-   shapleyWorld %>%
    group_by(scenario, region, period) %>%
    mutate(shapleyRela = abs(value) / sum(abs(value), na.rm = TRUE)) %>%
    ungroup() %>%
    select(-value, -variable) %>%
    mutate(shapleyRela = ifelse(is.nan(shapleyRela), 0, shapleyRela))
  
  shapleyRela <- shapleyWorldRela %>%
    bind_rows(shapleyRela)
  

    
  dfIneq <- dfIneq %>%
    filter (category =='Total',variable =='ineq|deltTheilT') %>%
    select(-variable, -category) %>%
    rename(deltTheil = value) %>%
    merge(shapleyRela, by = c('scenario','region','period')) %>%
    group_by(scenario,region,period) %>%
    mutate(deltTheilShapley = deltTheil * shapleyRela)%>%
    select( - deltTheil, -shapleyRela) %>%
    mutate(variable = 'ineq|deltTheilTShapley') %>%
    rename(value = deltTheilShapley) %>%
    bind_rows(dfIneq)
  
  result[['ineq']] <- dfIneq %>%
    filter (category =='Total',variable =='ineq|deltTheilL') %>%
    select(-variable, -category) %>%
    rename(deltTheil = value) %>%
    merge(shapleyRela, by = c('scenario','region','period')) %>%
    group_by(scenario,region,period) %>%
    mutate(deltTheilShapley = deltTheil * shapleyRela)%>%
    select( - deltTheil, -shapleyRela) %>%
    mutate(variable = 'ineq|deltTheilLShapley') %>%
    rename(value = deltTheilShapley) %>%
    bind_rows(dfIneq) 
  

    
  result[['theilTDecomp']] <- data2 %>%
    select(-starts_with("share|")) %>%
    merge(welfByDecile, by = c("scenario", "region", "period", "decileGroup")) %>%
    mutate(consumptionCaPost = consumptionCa * (1 + welfChange / 100)) %>%
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
  
  result[['theilLDecomp']] <- data2 %>%
    select(-starts_with("share|")) %>%
    merge(welfByDecile, by = c("scenario", "region", "period", "decileGroup")) %>%
    mutate(consumptionCaPost = consumptionCa * (1 + welfChange / 100)) %>%
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



 
