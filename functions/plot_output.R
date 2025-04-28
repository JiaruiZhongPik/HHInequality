#This is the function to plot welfare chagne



plot_output <- function(outputPath, data,  plotlist='NA' , micro_model, fixed_point, exampleReg = 'EUR', isDisplay = T, isExport = FALSE, allExport=FALSE ) {
  
  numSector <- length(unique(data$category))
  
  p=list()
  
  if(any(plotlist == "welfByDecile") | allExport){
    #todo: is using weighted average better?
    
    dataDecile <- data %>%
      filter(period <= 2100) %>%
      group_by(scenario, period, region, decileGroup) %>%
      summarise(sumWelfChange = sum(decilWelfChange, na.rm = TRUE), .groups = "drop")

    avg_df_period <- dataDecile %>%
      group_by(scenario, period) %>%
      summarise(meanWelfChange = mean(sumWelfChange, na.rm = TRUE), .groups = "drop")
      
    
    
    p[['welfByDecile']] <- list(
      plot = ggplot(dataDecile %>% filter(period <= 2100), 
                   aes(x = period, y = sumWelfChange, fill = scenario)) +
        
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
                  aes(x = period - 1, y = meanWelfChange),
                  color = "#ba7a31", size = 0.7) +
        
        geom_line(data = avg_df_period %>% filter(scenario == "C_SSP2-PkBudg650-rem-5", period >= 2025),
                  aes(x = period + 1, y = meanWelfChange),
                  color = "#264f78", size = 0.7)+
        
        #  Custom color mapping for boxes and lines
        scale_fill_manual(values = c(
          "C_SSP2-PkBudg1000-rem-5" = "#e8ab67",  # light blue
          "C_SSP2-PkBudg650-rem-5"  = "#779ec6"   # mid blue
        )) +
        scale_x_continuous(breaks = unique(data$period),
                           labels = unique(data$period))+
        
        # Labels and styling
        labs(x = "Period", y = "Welfare Change (%)", fill = "Scenario", color = "Scenario") +
        coord_cartesian(ylim = c(-20, 1)) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom",
          legend.direction = "horizontal"
        ),
      
      width = 5,
      height = 3
      
    )
  
    
    
    }
  
  
  if(any(plotlist == "welfByPeriod") | allExport){
    
    
    avg_df_decile <- dataDecile %>%
      group_by(scenario, decileGroup) %>%
      summarise(meanWelfChange = mean(sumWelfChange, na.rm = TRUE), .groups = "drop") %>%
      mutate(decileGroup = as.factor(decileGroup)) 
    
    
    p[['welfByPeriod']] <- list (
      plot = ggplot(dataDecile %>% filter(period <= 2100), 
                    aes(x = factor(decileGroup), y = sumWelfChange, fill = scenario)) +
        
        geom_boxplot(outlier.shape = NA, width = 0.4, 
                     position = position_dodge(width = 0.5), color = "grey20") +
        
        geom_jitter(aes(color = scenario),
                    position = position_jitterdodge(jitter.width = 0.5, dodge.width = 0.5), 
                    alpha = 0.3, size = 0.2) +
        
        # 🔹 Add mean lines by scenario
        geom_line(data = avg_df_decile, 
                  aes(x = decileGroup, y = meanWelfChange, group = scenario, color = scenario), 
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
        ),
      
      width = 5,
      height = 3 
      
    )
  }
  
  
  if(any(plotlist == "welfByDecileReg") | allExport){
    
    avg_df_decile <- dataDecile %>%
      filter(period <= 2100) %>%
      group_by(scenario, decileGroup,region) %>%
      summarise(mean_welfare = mean(sumWelfChange, na.rm = TRUE), .groups = "drop") %>%
      mutate(decileGroup = as.factor(decileGroup)) 
    
    
    p[["welfByDecileReg"]] <- list(
      plot = ggplot(dataDecile %>% filter(period <= 2100), 
                    aes(x = factor(decileGroup), y = sumWelfChange, fill = scenario)) +
        geom_boxplot(outlier.shape = NA, width = 0.5, 
                     position = position_dodge(width = 0.6), color = "grey20") +
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
        facet_wrap(~ region)+
        theme_minimal() +
        coord_cartesian(ylim = c(-15, 0.5)) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom",
          legend.direction = "horizontal"
        ),
      
      width = 10,
      height = 8
    )
      
      
      
      
      
  }
  
  
  if(any(plotlist=='welfByPeriodReg')| allExport ){
    #todo: is using weighted average better?
    avg_df_period <- dataDecile %>%
      filter(period <= 2100) %>%
      group_by(scenario, period, region) %>%
      summarise(meanWelfChange = mean(sumWelfChange, na.rm = TRUE), .groups = "drop")
    
    p[["welfByPeriodReg"]] <- list(
      plot = ggplot(dataDecile %>% filter(period <= 2100), 
                    aes(x = period, y = sumWelfChange, fill = scenario)) +
        
        # Side-by-side boxplots per scenario and period
        geom_boxplot(aes(group = interaction(period, scenario)), 
                     position = position_dodge(width = 4), width = 4,
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
                  aes(x = period - 1, y = meanWelfChange),
                  color = "#ba7a31", size = 0.7) +
        
        geom_line(data = avg_df_period %>% filter(scenario == "C_SSP2-PkBudg650-rem-5", period >= 2025),
                  aes(x = period + 1, y = meanWelfChange),
                  color = "#264f78", size = 0.7)+
        
        #  Custom color mapping for boxes and lines
        scale_fill_manual(values = c(
          "C_SSP2-PkBudg1000-rem-5" = "#e8ab67",  # light blue
          "C_SSP2-PkBudg650-rem-5"  = "#779ec6"   # mid blue
        )) +
        scale_x_continuous(breaks = unique(data$period),
                           labels = unique(data$period))+
        
        # Labels and styling
        labs(x = "Period", y = "Welfare Change (%)", fill = "Scenario", color = "Scenario") +
        coord_cartesian(ylim = c(-20, 1)) +
        facet_wrap(~ region)+
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom",
          legend.direction = "horizontal"
        ),
      
      width = 10,
      height = 8
      
    )
    
  }
  
  
  if(any(plotlist=='welfByDecileSecEne')| allExport ){
    
    eneSec = c("Building electricity", "Building gases", "Building other fuels", "Transport energy")

    
    # Automate over all scenarios
    p[['welfByDecileSecEne']] <- list(
      plot = ggplot(data %>% filter(period <= 2100, 
                                    category %in% eneSec
                                   ), 
                    aes(x = factor(decileGroup), y = decilWelfChange, fill = category)) +
        geom_boxplot(aes(group = interaction(factor(decileGroup), category)),
                     outlier.shape = NA, color = "gray40") +
        
        # geom_jitter(aes(color = category),
        #             position = position_jitterdodge(jitter.width = 1, dodge.width =0.75), 
        #             alpha = 0.3, size = 0.2) +
        scale_fill_viridis_d(option = "D") +
        facet_wrap(~scenario, ncol = 2) +
        # Labels and styling
        labs(x = "Decile", y = "Welfare Change (%)", fill = "FE category") +
        coord_cartesian(ylim = c(-5, 1)) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom",
          legend.direction = "horizontal"
        ),
      
      width = 8,
      height = 3
      
    )
    
  
    
    
  }
  
  if(any(plotlist == 'welfByDecileSecEneRegion' | allExport  )){
    
    eneSec = c("Building electricity", "Building gases", "Building other fuels", "Transport energy")
    
    # Automate over all scenarios
    p[[paste0('welfByDecileSecEne',exampleReg)]] <- list(
      plot = ggplot(data %>% filter(period <= 2100, 
                                    category %in% eneSec,
                                    region == exampleReg
      ), 
      aes(x = factor(decileGroup), y = decilWelfChange, fill = category)) +
        geom_boxplot(aes(group = interaction(factor(decileGroup), category)),
                     outlier.shape = NA, color = "gray40") +
        
        # geom_jitter(aes(color = category),
        #             position = position_jitterdodge(jitter.width = 1, dodge.width =0.75), 
        #             alpha = 0.3, size = 0.2) +
        scale_fill_viridis_d(option = "D") +
        facet_wrap(~scenario, ncol = 2) +
        # Labels and styling
        labs(x = "Decile", y = "Welfare Change (%)", fill = "FE category") +
        coord_cartesian() +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom",
          legend.direction = "horizontal"
        ),
      
      width = 8,
      height = 3
      
    )
    
  }
  
  
  
  if(isDisplay){
    print(p)
  }
  
  
  
  if(isExport|allExport){
    for (name in names(p)) {
      ggsave(
        filename = paste0(outputPath, micro_model, capitalize_first(fixed_point),"/", name, ".tiff"),
        plot = p[[name]]$plot,
        width = p[[name]]$width,
        height = p[[name]]$height,
        dpi = 300
      )
    }
  }
  
}


