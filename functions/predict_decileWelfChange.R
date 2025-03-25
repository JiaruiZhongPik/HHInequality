#---This function simulates the welfare effect of price changes----------------



#-------------1. First-order welfare effect-------------------------

predict_decileWelfChange <- function(micro_model = 'FOwelfare',data1 = data, data2 = decileConsShare, isDisplay = FALSE, isExport = FALSE ){
  deltPriceEne <- data1 %>%
    select(-unit,-baseline)%>%
    filter(
      str_starts(variable, "deltPrice") | str_starts(variable, "FE"),
      str_detect(scenario, "PkBudg650|PkBudg1000")
    ) %>%
    pivot_wider(
      names_from = variable, values_from = value
    ) %>%
    mutate( deltPriceEne = (`deltPrice|Transport|FE` * `FE|++|Transport` + 
                              `deltPrice|Buildings|Electricity` * `FE|Buildings|Electricity` + 
                              `deltPrice|Buildings|Gases` * `FE|Buildings|Gases` +
                              `deltPrice|Buildings|Other fuels` * `FE|Buildings|Other fuels`)/
              (`FE|++|Transport`+`FE|Buildings|Electricity`+`FE|Buildings|Gases` + `FE|Buildings|Other fuels`),
            variable = 'deltPrice|Ene'
    )%>%
    select( scenario, model, region, period, variable, deltPriceEne )%>%
    rename( value = deltPriceEne)
  
  #By definitionm commodity price doesn't change as it it numeraire
  deltPriceComm <- deltPriceEne %>% 
    mutate( value = 0,
            variable = 'deltPrice|Comm')
  
  #todo: introduce real food price change from MAgPIE, now is temporarily assumed to be 0
  deltPriceFood <- deltPriceEne %>% 
    mutate( value = 0,
            variable = 'deltPrice|Food')
  
  deltPrice <- bind_rows(deltPriceFood,deltPriceEne,deltPriceComm) %>%
    separate(col = variable,into = c("variable", "category"), sep = "\\|") %>%
    rename(deltPrice = value) %>%
    select(-variable)
  
  decileWelfChange<-
    data2 %>%
    select(-consumptionCa)%>%
    pivot_longer( cols = starts_with('share'),
                  names_to = 'variable',
                  values_to = 'share')%>%
    separate( col= variable,into = c("variable", "category"), sep = "\\|"  )%>%
    select(-variable ) %>%
    left_join(deltPrice, by = c('scenario','model','region','period','category')) %>%
    group_by(scenario, region, period, decileGroup)%>%
    summarise(
      decilWelfChange = -  sum(deltPrice * share) * 100,
      .groups = "drop"
    ) 
  

  
  #todo: is using weighted average better?
  avg_df_period <- decileWelfChange %>%
    filter(period <= 2100) %>%
    group_by(scenario, period) %>%
    summarise(mean_welfare = mean(decilWelfChange, na.rm = TRUE), .groups = "drop")
  
  #optional, not exported or displayed
  p1 <- ggplot(decileWelfChange %>% filter(period <= 2100), 
               aes(x = period, y = decilWelfChange)) +
    geom_boxplot(aes(group = period), outlier.shape = NA, width = 5, fill = "#d9e6f4", color = "gray40") +
    stat_summary(fun = mean, geom = "point", shape = 20, size = 2.5, color = "gray30") +
    geom_jitter(width = 2, alpha = 0.4, size = 0.8, color = "gray40") +
    geom_line(data = avg_df_period %>% filter(period >= 2025), 
              aes(x = period, y = mean_welfare), 
              color = "#687482", size = 0.9) +
    facet_wrap(~ scenario) +
    labs(x = "Period", y = "Welfare Change (%)") +
    theme_minimal() +
    coord_cartesian(ylim = c(-20, 1)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  p2 <- ggplot(decileWelfChange %>% filter(period <= 2100), 
              aes(x = period, y = decilWelfChange, fill = scenario)) +
    
    # Side-by-side boxplots per scenario and period
    geom_boxplot(aes(group = interaction(period, scenario)), 
                 position = position_dodge(width = 3), width = 3,
                 outlier.shape = NA, color = "gray40") +
    
    # Mean points
    stat_summary(fun = mean, geom = "point", 
                 aes(group = scenario), 
                 position = position_dodge(width = 5), 
                 shape = 20, size = 2.5, color = "gray30") +
    geom_jitter(aes(color = scenario),
                position = position_jitterdodge(jitter.width = 1.5, dodge.width = 5), 
                alpha = 0.3, size = 0.2) +
    scale_color_manual(values = c(
      "C_SSP2-PkBudg1000-rem-5" = "#e8ab67",
      "C_SSP2-PkBudg650-rem-5"  = "#264f78"
    ))+
    
    # One-by-one geom_line for each scenario
    geom_line(data = avg_df_period %>% filter(scenario == "C_SSP2-PkBudg1000-rem-5", period >= 2025),
              aes(x = period - 1, y = mean_welfare),
              color = "#ba7a31", size = 0.7) +
    
    geom_line(data = avg_df_period %>% filter(scenario == "C_SSP2-PkBudg650-rem-5", period >= 2025),
              aes(x = period + 1, y = mean_welfare),
              color = "#264f78", size = 0.7)+
    
    #  Custom color mapping for boxes and lines
    scale_fill_manual(values = c(
      "C_SSP2-PkBudg1000-rem-5" = "#e8ab67",  # light blue
      "C_SSP2-PkBudg650-rem-5"  = "#779ec6"   # mid blue
    )) +
    scale_x_continuous(breaks = unique(decileWelfChange$period),
                       labels = unique(decileWelfChange$period))+
    
    # Labels and styling
    labs(x = "Period", y = "Welfare Change (%)", fill = "Scenario", color = "Scenario") +
    coord_cartesian(ylim = c(-20, 1)) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      legend.direction = "horizontal"
    )

  
  avg_df_decile <- decileWelfChange %>%
    filter(period <= 2100) %>%
    group_by(scenario, decileGroup) %>%
    summarise(mean_welfare = mean(decilWelfChange, na.rm = TRUE), .groups = "drop") %>%
    mutate(decileGroup = as.factor(decileGroup)) 
  
  #optional way of ploting,not exported
  p3 <- ggplot(decileWelfChange %>% filter(period <= 2100), 
               aes(x = factor(decileGroup), y = decilWelfChange)) +
    geom_boxplot(aes(group = decileGroup), outlier.shape = NA, width = 0.7, 
                 fill = "#ecdef4", color = "gray40") +
    stat_summary(fun = mean, geom = "point", shape = 20, size = 2.5, color = "gray30") +
    geom_jitter(width = 0.2, alpha = 0.4, size = 0.8, color = "gray40") +
    geom_line(data = avg_df_decile, 
              aes(x = decileGroup, y = mean_welfare,group = scenario), 
              color = "#7c5c91", size = 0.9)  +
    facet_wrap(~ scenario) +
    labs(x = "Income Decile Group", y = "Welfare Change (%)") +
    theme_minimal() +
    coord_cartesian(ylim = c(-10, 0.5)) + #preserves outliers but doesn't show them
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  p4 <- ggplot(decileWelfChange %>% filter(period <= 2100), 
         aes(x = factor(decileGroup), y = decilWelfChange, fill = scenario)) +
    geom_boxplot(outlier.shape = NA, width = 0.4, 
                 position = position_dodge(width = 0.5), color = "grey20") +
    geom_jitter(aes(color = scenario),
                position = position_jitterdodge(jitter.width = 0.5, dodge.width = 0.5), 
                alpha = 0.3, size = 0.2) +

    # 🔹 Add mean lines by scenario
    geom_line(data = avg_df_decile, 
              aes(x = decileGroup, y = mean_welfare, group = scenario, color = scenario), 
              size = 0.7) +
    scale_fill_manual(values = c(
      "C_SSP2-PkBudg1000-rem-5" = "#e8ab67",  # light orange
      "C_SSP2-PkBudg650-rem-5"  = "#779ec6"   # soft purple
    )) +
    scale_color_manual(values = c(
      "C_SSP2-PkBudg1000-rem-5" = "#ba7a31",
      "C_SSP2-PkBudg650-rem-5"  = "#264f78"
    ))+
    stat_summary(fun = mean, geom = "point", 
                 aes(group = scenario), 
                 position = position_dodge(width = 0.7), 
                 shape = 20, size = 2.5, color = "gray30") +
    
    labs(x = "Income Decile Group", y = "Welfare Change (%)", fill = "Scenario", color = "Scenario") +
    theme_minimal() +
    coord_cartesian(ylim = c(-10, 0.5)) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      legend.direction = "horizontal"
    )
  
  if(isDisplay) {
    print(p2)
  }
  
  if(isExport){
    ggsave(paste0("figure/welfChange_period_1_",micro_model,'.tiff'), p1, width = 10, height =4, units = "in", dpi = 300)
    ggsave(paste0("figure/welfChange_period_2_",micro_model,'.tiff'), p2, width = 7, height =4, units = "in", dpi = 300)
    ggsave(paste0("figure/welfChange_decileGroup_1_",micro_model,'.tiff'), p3, width = 10, height =4, units = "in", dpi = 300)
    ggsave(paste0("figure/welfChange_decileGroup_2_",micro_model,'.tiff'), p4, width = 7, height =4, units = "in", dpi = 300)
  }
  
  return(decileWelfChange)
  
}
