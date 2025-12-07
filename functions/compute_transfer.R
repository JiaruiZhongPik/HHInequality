
compute_transfer <- function (data1 = data, data2 = decileConsShare, 
                              climaFund = 1, fund_return_scale = 1,
                              recycle = "neut", revenueRep = 0){
  
  ir2130 <- data1 %>%
    filter(variable == "Interest Rate (t+1)/(t-1)|Real",
           scenario %in% paste0( 'C_',all_runscens,'-',all_budgets ),
           region != "World",
           period == 2130) %>%
    select(scenario, region, value_2130 = value)
  
  df <- data1 %>%
    filter(
      variable %in% c("Taxes|GHG|MAGPIE", "Taxes|GHG|REMIND", "Interest Rate (t+1)/(t-1)|Real"),
      scenario %in% paste0( 'C_',all_runscens,'-',all_budgets ),
      region != "World"
    ) %>%
    select(-model, -baseline) %>%
    left_join(ir2130, by = c("scenario", "region")) %>%
    mutate(
      value = if_else(
        variable == "Interest Rate (t+1)/(t-1)|Real" & period == 2150 & is.na(value),
        value_2130,
        value
      )
    ) %>%
    select(-value_2130) %>%
    calc_addVariable(
      "`Taxes|GHG`" = "`Taxes|GHG|MAGPIE` + `Taxes|GHG|REMIND`",
      units         = c("billion US$2017/yr")
    ) %>% filter(variable %in% c('Taxes|GHG','Interest Rate (t+1)/(t-1)|Real')) 
  
  ir  <- df %>% filter(variable == "Interest Rate (t+1)/(t-1)|Real")  %>% select(scenario, region, period, r = value)
  rev <- df %>% filter(variable == "Taxes|GHG") %>% select(scenario, region, period, revenue = value)
  
  
  
  joined <- rev %>%
    left_join(ir, by = c("scenario", "region", "period")) %>%
    arrange(scenario, region, period) %>%
    group_by(scenario, region) %>%
    mutate(
      year_gap = period - dplyr::lag(period, default = first(period)),
      r_lag    = dplyr::lag(r,      default = first(r)),
      r_fund_lag  = fund_return_scale * r_lag,
      # PV weights relative to first row
      df = {
        acc <- numeric(dplyr::n()); acc[1] <- 1
        if (dplyr::n() > 1) {
          for (i in 2:dplyr::n()) {
            acc[i] <- acc[i-1] / ((1 + r_fund_lag[i])^(year_gap[i]))
          }
        }
        acc
      }
    ) %>% ungroup()
  
  # alpha per scenario/region (PV_neg / PV_pos, clipped to [0,1])
  alphas <- joined %>%
    group_by(scenario, region) %>%
    summarise(
      PV_pos   = sum(pmax(revenue, 0) * df, na.rm = TRUE),
      PV_neg   = sum(pmax(-revenue, 0) * df, na.rm = TRUE),
      alpha    = dplyr::if_else(PV_pos > 0, pmin(pmax(PV_neg / PV_pos, 0), 1), 0),
      .groups = "drop"
    ) 
  
  joined_alpha <- joined %>% left_join(alphas, by = c("scenario","region"))
  
  # simulate fund; read alpha from the data (not from .y)
  simulate_fund <- function(df_grp) {
    df_grp <- df_grp %>% arrange(period)
    n <- nrow(df_grp)
    out_saved <- out_payout <- out_recycle <- numeric(n)
    
    # safe alpha
    a <- df_grp$alpha[1]
    if (is.na(a) || length(a) == 0) a <- 0
    
    fund <- 0
    fb <- numeric(n)
    
    for (i in seq_len(n)) {
      if (i > 1) {
        fund <- fund * (1 + df_grp$r_fund_lag[i])^(df_grp$year_gap[i])
      }
      rev_i <- df_grp$revenue[i]
      
      if (is.na(rev_i)) {
        out_saved[i]   <- NA_real_
        out_payout[i]  <- NA_real_
        out_recycle[i] <- NA_real_
        fb[i]          <- if (i == 1) fund else fund
        next
      }
      
      if (rev_i >= 0) {
        out_saved[i]   <- a * rev_i
        out_recycle[i] <- (1 - a) * rev_i
        out_payout[i]  <- 0
        fund <- fund + out_saved[i]
      } else {
        need          <- -rev_i
        pay           <- min(fund, need)
        out_payout[i] <- pay
        out_recycle[i] <- 0
        fund <- fund - pay
      }
      fb[i] <- fund
    }
    
    df_grp$saved        <- out_saved
    df_grp$payout       <- out_payout
    df_grp$recycled_now <- out_recycle
    df_grp$fund_balance <- fb
    df_grp
  }
  
  revenue_smoothed <- joined_alpha %>%
    group_by(scenario, region) %>%
    group_modify(~ simulate_fund(.x)) %>%
    ungroup() %>%
    select(scenario, region, period, interest = r, revenue_raw = revenue,
           alpha, saved, payout, recycled_now, fund_balance) %>%
    rename(revenue = recycled_now)%>%
    select(scenario, region, period, revenue) 
  
  
  if(climaFund == 1) {
    revenue = revenue_smoothed
  } else {
    revenue = rev
  }
  
  
  if(recycle == 'epc'){
    
    transfer <- data1 %>%
      filter(variable %in% c('Population'),
             scenario %in% paste0( 'C_',all_runscens,'-',all_budgets ),
             region != 'World') %>%
      rename(population = value) %>%
      select(scenario,region,period, population) %>%
      left_join(revenue, by = c("scenario", "region", "period")) %>% 
      mutate(transferEpc = (revenue * 1e9) / (population * 1e6)) %>%
      select(scenario,region,period,transferEpc) %>% 
      crossing(decileGroup = 1:10) %>%
      arrange(scenario, region, period, decileGroup) 
    
  } else if(recycle == 'neut'){
    
    weight <- data2 %>%
      filter(scenario == 'C_SSP2-NPi2025') %>%
      select(scenario, region, period, decileGroup, consumptionCa) %>%
      mutate(
        total = sum(consumptionCa, na.rm = TRUE),
        consShare = if_else(total > 0, consumptionCa / total, NA_real_),
        .by = c(scenario, region, period)
      ) %>%
      select(-total, -consumptionCa,-scenario) %>%
      tidyr::crossing(scenario = paste0( 'C_',all_runscens,'-',all_budgets ))
    
    transfer <- data1 %>%
      filter(variable %in% c('Population'),
             scenario %in% paste0( 'C_',all_runscens,'-',all_budgets ),
             region != 'World') %>%
      rename(population = value) %>%
      select(scenario,region,period, population) %>%
      left_join(revenue, by = c("scenario", "region", "period")) %>%
      mutate(revenue_ca =revenue * 1e3 / population ) %>%
      right_join(weight,by = c("scenario", "region", "period") ) %>%
      mutate( transferNeut =  revenue_ca * consShare * 10) %>%
      select(scenario, region, period,decileGroup,transferNeut)

  }
  
  if(revenueRep == 1){
    return(revenue)
  } else {
    return(transfer)
  }
  

}






