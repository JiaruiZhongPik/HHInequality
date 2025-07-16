#This is the function to plot welfare chagne



plot_output <- function(outputPath, plotdataWelf, data2, data3, plotdataIneq,  plotlist='NA' , micro_model, fixed_point, exampleReg = 'EUR', isDisplay = T, isExport = FALSE, allExport=FALSE ) {
  
  plotdataWelf <- plotdataWelf %>%
    filter( period %notin% c(2015,2010,2020,2025))

  
  numSector <- length(unique(plotdataWelf$category))
  foodSec <- c("Animal products","Empty calories","Fruits vegetables nuts","Staples")
  eneSec <- c("Building electricity", "Building gases", "Building other fuels","Transport energy")
  allSec <- c(foodSec, eneSec, 'Other commodities','Consumption')
  p=list()

#-------------------------1.Plots all aggregated welfare change------------------------
  
  if(any(plotlist == "welfByPeriod") | allExport){
    #todo: is using weighted average better?
    
    dataDecile <- plotdataWelf %>%
      filter(period <= 2100) %>%
      group_by(scenario, period, region, decileGroup) %>%
      summarise(sumWelfChange = sum(decilWelfChange, na.rm = TRUE), .groups = "drop")

    avg_df_period <- dataDecile %>%
      group_by(scenario, period) %>%
      summarise(meanWelfChange = mean(sumWelfChange, na.rm = TRUE), .groups = "drop")
      
    
    
    p[['welfByPeriod']] <- list(
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
          "C_SSP2-PkBudg1000" = "#e8ab67",
          "C_SSP2-PkBudg650"  = "#264f78"
        )) +
        
        # One-by-one geom_line for each scenario
        geom_line(data = avg_df_period %>% filter(scenario == "C_SSP2-PkBudg1000", period >= 2025),
                  aes(x = period - 1, y = meanWelfChange),
                  color = "#ba7a31", linewidth = 0.7) +
        
        geom_line(data = avg_df_period %>% filter(scenario == "C_SSP2-PkBudg650", period >= 2025),
                  aes(x = period + 1, y = meanWelfChange),
                  color = "#264f78", linewidth = 0.7)+
        
        #  Custom color mapping for boxes and lines
        scale_fill_manual(values = c(
          "C_SSP2-PkBudg1000" = "#e8ab67",  # light blue
          "C_SSP2-PkBudg650"  = "#779ec6"   # mid blue
        )) +
        scale_x_continuous(breaks = unique(plotdataWelf$period),
                           labels = unique(plotdataWelf$period)) +
        
        # Labels and styling
        labs(x = "Period", y = "Welfare Change (%)", fill = "Scenario", color = "Scenario") +
        coord_cartesian(ylim = quantile(dataDecile$sumWelfChange, probs = c(0.02, 0.99), na.rm = TRUE)) +
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
  
  
  if(any(plotlist == "welfByDecile") | allExport){
    
    dataDecile <- plotdataWelf %>%
      filter(period <= 2100) %>%
      group_by(scenario, period, region, decileGroup) %>%
      summarise(sumWelfChange = sum(decilWelfChange, na.rm = TRUE), .groups = "drop")
    
    avg_df_decile <- dataDecile %>%
      group_by(scenario, decileGroup) %>%
      summarise(meanWelfChange = mean(sumWelfChange, na.rm = TRUE), .groups = "drop") %>%
      mutate(decileGroup = as.factor(decileGroup)) 
    
    
    p[['welfByDecile']] <- list (
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
                  linewidth = 0.7) +
        scale_fill_manual(values = c(
          "C_SSP2-PkBudg1000" = "#e8ab67",  # light orange
          "C_SSP2-PkBudg650"  = "#779ec6"   # soft purple
        )) +
        scale_color_manual(values = c(
          "C_SSP2-PkBudg1000" = "#ba7a31",
          "C_SSP2-PkBudg650"  = "#264f78"
        ))+
        stat_summary(fun = mean, geom = "point", 
                     aes(group = scenario), 
                     position = position_dodge(width = 0.7), 
                     shape = 20, size = 2.5, color = "gray30") +
        
        labs(x = "Income Decile Group", y = "Welfare Change (%)", fill = "Scenario", color = "Scenario") +
        theme_minimal() +
        coord_cartesian(ylim = quantile(dataDecile$sumWelfChange, probs = c(0.02, 0.99), na.rm = TRUE)) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom",
          legend.direction = "horizontal"
        ),
      
      width = 5,
      height = 3 
      
    )
  }
#---------------------End:Plots all aggregated----------------------------------  

  
  
  
  
#-----------------------2.1. Plots Ene aggregated(welfare change)---------------
  
  if(any(plotlist == "welfByPeriodEne") | allExport){
    #todo: is using weighted average better?
    
    dataDecile <- plotdataWelf %>%
      filter(period <= 2100,
             category %in% eneSec) %>%
      group_by(scenario, period, region, decileGroup) %>%
      summarise(sumWelfChange = sum(decilWelfChange, na.rm = TRUE), .groups = "drop")
    
    avg_df_period <- dataDecile %>%
      group_by(scenario, period) %>%
      summarise(meanWelfChange = mean(sumWelfChange, na.rm = TRUE), .groups = "drop")
    
    
    p[['welfByPeriodEne']] <- list(
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
          "C_SSP2-PkBudg1000" = "#e8ab67",
          "C_SSP2-PkBudg650"  = "#264f78"
        )) +
        
        # One-by-one geom_line for each scenario
        geom_line(data = avg_df_period %>% filter(scenario == "C_SSP2-PkBudg1000", period >= 2025),
                  aes(x = period - 1, y = meanWelfChange),
                  color = "#ba7a31", linewidth = 0.7) +
        
        geom_line(data = avg_df_period %>% filter(scenario == "C_SSP2-PkBudg650", period >= 2025),
                  aes(x = period + 1, y = meanWelfChange),
                  color = "#264f78", linewidth = 0.7)+
        
        #  Custom color mapping for boxes and lines
        scale_fill_manual(values = c(
          "C_SSP2-PkBudg1000" = "#e8ab67",  # light blue
          "C_SSP2-PkBudg650"  = "#779ec6"   # mid blue
        )) +
        scale_x_continuous(breaks = unique(plotdataWelf$period),
                           labels = unique(plotdataWelf$period))+
        
        # Labels and styling
        labs(x = "Period", y = "Welfare Change (%)", fill = "Scenario", color = "Scenario") +
        coord_cartesian(ylim = quantile(dataDecile$sumWelfChange, probs = c(0.02, 0.99), na.rm = TRUE)) +
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
  
  
  if(any(plotlist == "welfByDecileEne") | allExport){
    
    dataDecile <- plotdataWelf %>%
      filter(period <= 2100,
             category %in% eneSec) %>%
      group_by(scenario, period, region, decileGroup) %>%
      summarise(sumWelfChange = sum(decilWelfChange, na.rm = TRUE), .groups = "drop")
    
    avg_df_decile <- dataDecile %>%
      group_by(scenario, decileGroup) %>%
      summarise(meanWelfChange = mean(sumWelfChange, na.rm = TRUE), .groups = "drop") %>%
      mutate(decileGroup = as.factor(decileGroup)) 
    
    
    p[['welfByDecile']] <- list (
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
                  linewidth = 0.7) +
        scale_fill_manual(values = c(
          "C_SSP2-PkBudg1000" = "#e8ab67",  # light orange
          "C_SSP2-PkBudg650"  = "#779ec6"   # soft purple
        )) +
        scale_color_manual(values = c(
          "C_SSP2-PkBudg1000" = "#ba7a31",
          "C_SSP2-PkBudg650"  = "#264f78"
        ))+
        stat_summary(fun = mean, geom = "point", 
                     aes(group = scenario), 
                     position = position_dodge(width = 0.7), 
                     shape = 20, size = 2.5, color = "gray30") +
        
        labs(x = "Income Decile Group", y = "Welfare Change (%)", fill = "Scenario", color = "Scenario") +
        theme_minimal() +
        coord_cartesian(ylim = quantile(dataDecile$sumWelfChange, probs = c(0.02, 0.99), na.rm = TRUE)) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom",
          legend.direction = "horizontal"
        ),
      
      width = 5,
      height = 3 
      
    )
  }
#---------------------End:Plots Ene aggregated----------------------------------  
  
  
  
  
  
  
#-----------------------2.2. Plots food aggregated(welfare change)--------------
  
  if(any(plotlist == "welfByPeriodFood") | allExport){
    #todo: is using weighted average better?
    
    dataDecile <- plotdataWelf %>%
      filter(period <= 2100,
             category %in% foodSec) %>%
      group_by(scenario, period, region, decileGroup) %>%
      summarise(sumWelfChange = sum(decilWelfChange, na.rm = TRUE), .groups = "drop")
    
    avg_df_period <- dataDecile %>%
      group_by(scenario, period) %>%
      summarise(meanWelfChange = mean(sumWelfChange, na.rm = TRUE), .groups = "drop")
    
    
    p[['welfByPeriodFood']] <- list(
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
          "C_SSP2-PkBudg1000" = "#e8ab67",
          "C_SSP2-PkBudg650"  = "#264f78"
        )) +
        
        # One-by-one geom_line for each scenario
        geom_line(data = avg_df_period %>% filter(scenario == "C_SSP2-PkBudg1000", period >= 2025),
                  aes(x = period - 1, y = meanWelfChange),
                  color = "#ba7a31", linewidth = 0.7) +
        
        geom_line(data = avg_df_period %>% filter(scenario == "C_SSP2-PkBudg650", period >= 2025),
                  aes(x = period + 1, y = meanWelfChange),
                  color = "#264f78", linewidth = 0.7)+
        
        #  Custom color mapping for boxes and lines
        scale_fill_manual(values = c(
          "C_SSP2-PkBudg1000" = "#e8ab67",  # light blue
          "C_SSP2-PkBudg650"  = "#779ec6"   # mid blue
        )) +
        scale_x_continuous(breaks = unique(plotdataWelf$period),
                           labels = unique(plotdataWelf$period))+
        
        # Labels and styling
        labs(x = "Period", y = "Welfare Change (%)", fill = "Scenario", color = "Scenario") +
        coord_cartesian(ylim = quantile(dataDecile$sumWelfChange, probs = c(0.02, 0.99), na.rm = TRUE)) +
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
  
  
  if(any(plotlist == "welfByDecileFood") | allExport){
    
    dataDecile <- plotdataWelf %>%
      filter(period <= 2100,
             category %in% foodSec) %>%
      group_by(scenario, period, region, decileGroup) %>%
      summarise(sumWelfChange = sum(decilWelfChange, na.rm = TRUE), .groups = "drop")
    
    avg_df_decile <- dataDecile %>%
      group_by(scenario, decileGroup) %>%
      summarise(meanWelfChange = mean(sumWelfChange, na.rm = TRUE), .groups = "drop") %>%
      mutate(decileGroup = as.factor(decileGroup)) 
    
    
    p[['welfByDecileFood']] <- list (
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
          "C_SSP2-PkBudg1000" = "#e8ab67",  # light orange
          "C_SSP2-PkBudg650"  = "#779ec6"   # soft purple
        )) +
        scale_color_manual(values = c(
          "C_SSP2-PkBudg1000" = "#ba7a31",
          "C_SSP2-PkBudg650"  = "#264f78"
        ))+
        stat_summary(fun = mean, geom = "point", 
                     aes(group = scenario), 
                     position = position_dodge(width = 0.7), 
                     shape = 20, size = 2.5, color = "gray30") +
        
        labs(x = "Income Decile Group", y = "Welfare Change (%)", fill = "Scenario", color = "Scenario") +
        theme_minimal() +
        coord_cartesian(ylim = quantile(dataDecile$sumWelfChange, probs = c(0.02, 0.99), na.rm = TRUE)) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom",
          legend.direction = "horizontal"
        ),
      
      width = 5,
      height = 3 
      
    )
  }
  
#---------------------End:Plots Ene aggregated----------------------------------  
  
  
  
  
  
#-------------------------3.reg plots all aggregated(welfare change)------------  
  if(any(plotlist == "welfByDecileReg") | allExport){
    
    dataDecile <- plotdataWelf %>%
      filter(period <= 2100) %>%
      group_by(scenario, period, region, decileGroup) %>%
      summarise(sumWelfChange = sum(decilWelfChange, na.rm = TRUE), .groups = "drop")
    
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
          "C_SSP2-PkBudg1000" = "#e8ab67",  # light orange
          "C_SSP2-PkBudg650"  = "#779ec6"   # soft purple
        )) +
        scale_color_manual(values = c(
          "C_SSP2-PkBudg1000" = "#ba7a31",
          "C_SSP2-PkBudg650"  = "#264f78"
        )) +
        stat_summary(fun = mean, geom = "point", 
                     aes(group = scenario), 
                     position = position_dodge(width = 0.7), 
                     shape = 20, size = 2.5, color = "gray30") +
        
        labs(x = "Income Decile Group", y = "Welfare Change (%)", fill = "Scenario", color = "Scenario") +
        facet_wrap(~ region)+
        theme_minimal() +
        coord_cartesian(ylim = quantile(dataDecile$sumWelfChange, probs = c(0.02, 0.99), na.rm = TRUE)) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom",
          legend.direction = "horizontal"
        )
      ,
      
      width = 10,
      height = 8
    )
  }
  
  
  if(any(plotlist=='welfByPeriodReg')| allExport ){
    
    dataDecile <- plotdataWelf %>%
      filter(period <= 2100) %>%
      group_by(scenario, period, region, decileGroup) %>%
      summarise(sumWelfChange = sum(decilWelfChange, na.rm = TRUE), .groups = "drop")
    
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
          "C_SSP2-PkBudg1000" = "#e8ab67",
          "C_SSP2-PkBudg650"  = "#264f78"
        ))+
        
        # One-by-one geom_line for each scenario
        geom_line(data = avg_df_period %>% filter(scenario == "C_SSP2-PkBudg1000", period >= 2025),
                  aes(x = period - 1, y = meanWelfChange),
                  color = "#ba7a31", size = 0.7) +
        
        geom_line(data = avg_df_period %>% filter(scenario == "C_SSP2-PkBudg650", period >= 2025),
                  aes(x = period + 1, y = meanWelfChange),
                  color = "#264f78", size = 0.7)+
        
        #  Custom color mapping for boxes and lines
        scale_fill_manual(values = c(
          "C_SSP2-PkBudg1000" = "#e8ab67",  # light blue
          "C_SSP2-PkBudg650"  = "#779ec6"   # mid blue
        )) +
        scale_x_continuous(breaks = unique(plotdataWelf$period),
                           labels = unique(plotdataWelf$period))+
        
        # Labels and styling
        labs(x = "Period", y = "Welfare Change (%)", fill = "Scenario", color = "Scenario") +
        coord_cartesian(ylim = quantile(dataDecile$sumWelfChange, probs = c(0.002, 0.99), na.rm = TRUE)) +
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
  
#-----------------------------End. 3.reg plots all aggregated----------------- 
  
  
  
  
  
  
#-------------------------3.1 reg plots ene aggregated(welfare change)----------
  if(any(plotlist == "welfByDecileRegEne") | allExport){
    
    
    dataDecile <- plotdataWelf %>%
      filter(period <= 2100,
             category %in% eneSec) %>%
      group_by(scenario, period, region, decileGroup) %>%
      summarise(sumWelfChange = sum(decilWelfChange, na.rm = TRUE), .groups = "drop")
    
    avg_df_decile <- dataDecile %>%
      filter(period <= 2100) %>%
      group_by(scenario, decileGroup,region) %>%
      summarise(mean_welfare = mean(sumWelfChange, na.rm = TRUE), .groups = "drop") %>%
      mutate(decileGroup = as.factor(decileGroup)) 
    
    
    p[["welfByDecileRegEne"]] <- list(
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
          "C_SSP2-PkBudg1000" = "#e8ab67",  # light orange
          "C_SSP2-PkBudg650"  = "#779ec6"   # soft purple
        )) +
        scale_color_manual(values = c(
          "C_SSP2-PkBudg1000" = "#ba7a31",
          "C_SSP2-PkBudg650"  = "#264f78"
        )) +
        stat_summary(fun = mean, geom = "point", 
                     aes(group = scenario), 
                     position = position_dodge(width = 0.7), 
                     shape = 20, size = 2.5, color = "gray30") +
        
        labs(x = "Income Decile Group", y = "Welfare Change (%)", fill = "Scenario", color = "Scenario") +
        facet_wrap(~ region)+
        theme_minimal() +
        coord_cartesian(ylim = quantile(dataDecile$sumWelfChange, probs = c(0.005, 0.99), na.rm = TRUE)) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom",
          legend.direction = "horizontal"
        ),
      
      width = 10,
      height = 8
    )
  }
  
  
  if(any(plotlist=='welfByPeriodRegEne')| allExport ){
    #todo: is using weighted average better?
    dataDecile <- plotdataWelf %>%
      filter(period <= 2100,
             category %in% eneSec) %>%
      group_by(scenario, period, region, decileGroup) %>%
      summarise(sumWelfChange = sum(decilWelfChange, na.rm = TRUE), .groups = "drop")
    
    
    avg_df_period <- dataDecile %>%
      filter(period <= 2100) %>%
      group_by(scenario, period, region) %>%
      summarise(meanWelfChange = mean(sumWelfChange, na.rm = TRUE), .groups = "drop")
    
    p[["welfByPeriodRegEne"]] <- list(
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
          "C_SSP2-PkBudg1000" = "#e8ab67",
          "C_SSP2-PkBudg650"  = "#264f78"
        ))+
        
        # One-by-one geom_line for each scenario
        geom_line(data = avg_df_period %>% filter(scenario == "C_SSP2-PkBudg1000", period >= 2025),
                  aes(x = period - 1, y = meanWelfChange),
                  color = "#ba7a31", size = 0.7) +
        
        geom_line(data = avg_df_period %>% filter(scenario == "C_SSP2-PkBudg650", period >= 2025),
                  aes(x = period + 1, y = meanWelfChange),
                  color = "#264f78", size = 0.7)+
        
        #  Custom color mapping for boxes and lines
        scale_fill_manual(values = c(
          "C_SSP2-PkBudg1000" = "#e8ab67",  # light blue
          "C_SSP2-PkBudg650"  = "#779ec6"   # mid blue
        )) +
        scale_x_continuous(breaks = unique(plotdataWelf$period),
                           labels = unique(plotdataWelf$period))+
        
        # Labels and styling
        labs(x = "Period", y = "Welfare Change (%)", fill = "Scenario", color = "Scenario") +
        coord_cartesian(ylim = quantile(dataDecile$sumWelfChange, probs = c(0.002, 0.99), na.rm = TRUE)) +
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
  
#---------------------End 3.1 reg plots ene aggregated------------------------
  
  
  
  
  
  
  
#-------------------------3.2 reg plots ene aggregated(welfChange)---------------
  if(any(plotlist == "welfByDecileRegFood") | allExport){
    
    dataDecile <- plotdataWelf %>%
      filter(period <= 2100,
             category %in% foodSec) %>%
      group_by(scenario, period, region, decileGroup) %>%
      summarise(sumWelfChange = sum(decilWelfChange, na.rm = TRUE), .groups = "drop")
    
    avg_df_decile <- dataDecile %>%
      filter(period <= 2100) %>%
      group_by(scenario, decileGroup,region) %>%
      summarise(mean_welfare = mean(sumWelfChange, na.rm = TRUE), .groups = "drop") %>%
      mutate(decileGroup = as.factor(decileGroup)) 
    
    
    p[["welfByDecileRegFood"]] <- list(
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
          "C_SSP2-PkBudg1000" = "#e8ab67",  # light orange
          "C_SSP2-PkBudg650"  = "#779ec6"   # soft purple
        )) +
        scale_color_manual(values = c(
          "C_SSP2-PkBudg1000" = "#ba7a31",
          "C_SSP2-PkBudg650"  = "#264f78"
        )) +
        stat_summary(fun = mean, geom = "point", 
                     aes(group = scenario), 
                     position = position_dodge(width = 0.7), 
                     shape = 20, size = 2.5, color = "gray30") +
        
        labs(x = "Income Decile Group", y = "Welfare Change (%)", fill = "Scenario", color = "Scenario") +
        facet_wrap(~ region)+
        theme_minimal() +
        coord_cartesian(ylim = quantile(dataDecile$sumWelfChange, probs = c(0.005, 0.99), na.rm = TRUE)) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom",
          legend.direction = "horizontal"
        ),
      
      width = 10,
      height = 8
    )
  }
  
  
  if(any(plotlist=='welfByPeriodRegFood')| allExport ){
    #todo: is using weighted average better?
    dataDecile <- plotdataWelf %>%
      filter(period <= 2100,
             category %in% foodSec) %>%
      group_by(scenario, period, region, decileGroup) %>%
      summarise(sumWelfChange = sum(decilWelfChange, na.rm = TRUE), .groups = "drop")
    
    
    avg_df_period <- dataDecile %>%
      filter(period <= 2100) %>%
      group_by(scenario, period, region) %>%
      summarise(meanWelfChange = mean(sumWelfChange, na.rm = TRUE), .groups = "drop")
    
    p[["welfByPeriodRegFood"]] <- list(
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
          "C_SSP2-PkBudg1000" = "#e8ab67",
          "C_SSP2-PkBudg650"  = "#264f78"
        ))+
        
        # One-by-one geom_line for each scenario
        geom_line(data = avg_df_period %>% filter(scenario == "C_SSP2-PkBudg1000", period >= 2025),
                  aes(x = period - 1, y = meanWelfChange),
                  color = "#ba7a31", linewidth = 0.7) +
        
        geom_line(data = avg_df_period %>% filter(scenario == "C_SSP2-PkBudg650", period >= 2025),
                  aes(x = period + 1, y = meanWelfChange),
                  color = "#264f78", size = 0.7)+
        
        #  Custom color mapping for boxes and lines
        scale_fill_manual(values = c(
          "C_SSP2-PkBudg1000" = "#e8ab67",  # light blue
          "C_SSP2-PkBudg650"  = "#779ec6"   # mid blue
        )) +
        scale_x_continuous(breaks = unique(plotdataWelf$period),
                           labels = unique(plotdataWelf$period))+
        
        # Labels and styling
        labs(x = "Period", y = "Welfare Change (%)", fill = "Scenario", color = "Scenario") +
        coord_cartesian(ylim = quantile(dataDecile$sumWelfChange, probs = c(0.002, 0.99), na.rm = TRUE)) +
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
  
#---------------------End 3.2 reg plots ene aggregated------------------------
  
  
  
  
  
  
  
  
  
#------------------4. Plot by individual channel:Global (welfare change)--------
  
  if(any(plotlist=='welfByDecileSec')| allExport ){
    
    plotdf <- plotdataWelf %>%
      filter( period <= 2100) %>%
      mutate( category = factor(category, levels = allSec ))

    
    # Automate over all scenarios
    p[['welfByDecileSec']] <- list(
      plot = ggplot(plotdf, 
                    aes(x = factor(decileGroup), y = decilWelfChange, fill = category)) +
        geom_boxplot(aes(group = interaction(factor(decileGroup), category)),
                     outlier.shape = NA, color = "gray40") +
        
        # geom_jitter(aes(color = category),
        #             position = position_jitterdodge(jitter.width = 1, dodge.width =0.75), 
        #             alpha = 0.3, size = 0.2) +
        #scale_fill_viridis_d(option = "A") +
        scale_fill_paletteer_d("dutchmasters::view_of_Delft") +
        facet_wrap(~scenario, ncol = 2) +
        # Labels and styling
        labs(x = "Decile", y = "Welfare Change (%)", fill = "FE category") +
        coord_cartesian(ylim = quantile(plotdataWelf$decilWelfChange, probs = c(0.01, 0.99), na.rm = TRUE)) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom",
          legend.direction = "horizontal"
        ),
      
      width = 10,
      height = 5
      
    )
  }
  
  if(any(plotlist=='globalWelfBySec')| allExport ){
    
    plotdf <- aggregate_decileWelfChange(data1 = plotdataWelf, data2 = data2, data3 = data3, 
                                         level = c("full"), region = 'global') %>%
      mutate(category = factor(category, levels = allSec))
    
    
    # Automate over all scenarios
    p[['globalWelfBySec']] <- list(
      plot = ggplot(plotdf, aes(x = factor(period), y = welfChange, fill = category)) +
        geom_col(position = "stack") +
        geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
        facet_wrap(~ scenario) +
        labs(
          x = "Year",
          y = "Welfare Change (%)",
          fill = "Category"
        ) +
        theme_minimal() +
        #scale_fill_paletteer_d("ggprism::floral2") +
        #scale_fill_paletteer_d("beyonce::X82") +
        scale_fill_paletteer_d("dutchmasters::view_of_Delft") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))+
        guides(fill = guide_legend(reverse = TRUE))
      
      ,
      
      width = 8,
      height = 3
      
    )
  }

  
#--------------------END. 4. Plot by individual channel:Global-----------------

  
  
  
  
  
      
#---------------4.1 Plot by individual channel:regional (welfare Change)--------
  

  if(any(plotlist == 'welfByDecileSecReg' | allExport  )){
    
    plotdf <- plotdataWelf %>%
      filter( period <= 2100,
              region == exampleReg) %>%
      mutate( category = factor(category, levels = allSec))
    
    
    # Automate over all scenarios
    p[[paste0('welfByDecileSec',exampleReg)]] <- list(
      plot = ggplot(plotdf, 
      aes(x = factor(decileGroup), y = decilWelfChange, fill = category)) +
        geom_boxplot(aes(group = interaction(factor(decileGroup), category)),
                     outlier.shape = NA, color = "gray40") +
        
        # geom_jitter(aes(color = category),
        #             position = position_jitterdodge(jitter.width = 1, dodge.width =0.75), 
        #             alpha = 0.3, size = 0.2) +
        scale_fill_brewer(palette = "Set2") +
        facet_wrap(~scenario, ncol = 2) +
        # Labels and styling
        labs(x = "Decile", y = "Welfare Change (%)", fill = "FE category") +
        coord_cartesian(ylim = quantile(plotdataWelf$decilWelfChange, probs = c(0.01, 0.99), na.rm = TRUE)) +
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
  

  
  if(any(plotlist == 'regWelfBySec' | allExport  )){
    
    plotdf <- aggregate_decileWelfChange(data1 = plotdataWelf, data2 = data2, data3 = data3, 
                                         level = c("full"), region = 'region') %>%
      mutate(category = factor(category, levels = allSec))
    

    for(n in unique(plotdf$scenario)) {
      
      summary_df <- plotdf %>%
        filter(scenario == n) %>%
        group_by(region, period) %>%
        summarise(total_welfChange = sum(welfChange, na.rm = TRUE), .groups = "drop")
      
      # Automate over all scenarios
      p[[paste0('regWelfBySec',n)]] <- list(
        plot = ggplot(plotdf[plotdf$scenario == n,], aes(x = factor(period), y = welfChange, fill = category)) +
          geom_col(position = "stack") +
          # geom_line(data = summary_df, aes(x = factor(period), y = total_welfChange, group = 1), 
          #           color = "darkblue", size = 0.4, inherit.aes = FALSE) +
          geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
          # geom_point(data = summary_df, aes(x = factor(period), y = total_welfChange), 
          #            color = "black", size = 0.8, inherit.aes = FALSE) +
          facet_wrap(~ region) +
          labs(
            x = "Year",
            y = "Welfare Change (%)",
            fill = "Category"
          ) +
          theme_minimal() +
          scale_fill_paletteer_d("dutchmasters::view_of_Delft") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)),
        
        width = 10,
        height = 8
        
      ) 
      
  }
    
}
      
#----------End 4.1 Plot by individual channel:regional (welfare change)---------
   

  
  
  
  
#----------------------5.1 global inequality effect (Gini/Theil)----------------
  
  
  if(any(plotlist == 'ineqWorld' | allExport  )){
    
    plotdf <- plotdataIneq[['ineq']] %>%
      filter( period <= 2100,
              region == 'World',
              category == 'Total',
              variable %in% c("ineq|Gini", "ineq|GiniPost",
                              "ineq|TheilT", "ineq|TheilTPost",
                              "ineq|TheilL", "ineq|TheilLPost")) %>%
      mutate(state = case_when(
        variable %in% c("ineq|Gini", "ineq|TheilT", "ineq|TheilL") ~ "before",
        TRUE ~ "after"
      ),
      
      measure = case_when(
        variable %in% c("ineq|Gini", "ineq|GiniPost") ~ "Gini",
        variable %in% c("ineq|TheilT", "ineq|TheilTPost") ~ "TheilT",
        TRUE ~ "TheilL"
      )
      )
  
    # Automate over all scenarios
    p[['ineqWorld']] <- list(
      
      plot = ggplot(plotdf, 
                    aes(x = period, y = value, linetype = state, color = measure)) +
        geom_line() + 
        scale_fill_brewer(palette = "Set2") +
        facet_wrap(~scenario, ncol = 2) +
        # Labels and styling
        labs(x = "Decile", y = "Inequality metrics", fill = "FE category") +
        coord_cartesian(ylim = quantile(plotdf$value, probs = c(0.01, 0.999), na.rm = TRUE)) +
        scale_color_brewer(palette = "Set2") +
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
  
#-----------------End 5.1 global inequality effect (Gini/Theil)----------------

  
  
  
  
  
  
  
#----------------------5.2 regional inequality effect (Gini/Theil)----------------
  
  
  if(any(plotlist == 'ineqReg' | allExport  )){
    
    plotdf <- plotdataIneq[['ineq']] %>%
      filter( period <= 2100,
              region != 'World',
              category == 'Total',
              variable %in% c("ineq|Gini", "ineq|GiniPost",
                              "ineq|TheilT","ineq|TheilTPost",
                              "ineq|TheilL","ineq|TheilLPost")) %>%
      mutate(measure = case_when(
        variable %in% c("ineq|Gini", "ineq|GiniPost") ~ "Gini",
        variable %in% c("ineq|TheilT","ineq|TheilTPost") ~ "TheilT",
        TRUE ~ "TheilL"),
        
        state = case_when(
          variable %in% c("ineq|Gini", "ineq|TheilT", "ineq|TheilL") ~ "before",
          TRUE ~ "after"
        ),
        
        state = factor(state, levels = c("before","after") )
        )
    
    # Automate over all scenarios
    p[['ineqReg']] <- list(
      
      plot = ggplot(plotdf, 
                    aes(x = period, y = value, 
                        color = interaction(measure), 
                        linetype = interaction(scenario, state))) +
        geom_line() + 
        facet_wrap(~region, ncol = 3) +
        labs(x = "Decile", y = "Inequality metrics", 
             color = "Measure", linetype = "Scenario-State") +
        coord_cartesian(ylim = quantile(plotdf$value, probs = c(0.01, 0.99), na.rm = TRUE)) +
        scale_color_brewer(palette = "Set2") +
        scale_linetype_manual(
          values = c(
            "C_SSP2-PkBudg1000.before" = "solid",
            "C_SSP2-PkBudg1000.after" = "dashed",
            "C_SSP2-PkBudg650.before" = "solid",
            "C_SSP2-PkBudg650.after" = "dotted"
          ),
          labels = c(
            "C_SSP2-PkBudg1000.before" = "SSP_NPi2025",
            "C_SSP2-PkBudg1000.after" = "PkBudg1000",
            "C_SSP2-PkBudg650.before" = "SSP_NPi2025",
            "C_SSP2-PkBudg650.after" = "PkBudg650"
          )
        ) + 
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom",
          legend.direction = "horizontal"
        )
      ,
      
      width = 10,
      height = 8
      
    )
    
    
    
  }
  
#-----------------End 5.2 regional inequality effect (Gini/Theil)----------------
  
  
  

  
  
  
  #-------------5.3 regional inequality effect by Channel (Theil)----------------
  
  
  if(any(plotlist == 'ineqTheilTRegBySec' | allExport  )){
    
    plotdf <- plotdataIneq[['ineq']] %>%
      filter( period <= 2100,
              period >= 2025,
              region == exampleReg,
              category != 'Total',
              variable %in% c("ineq|deltTheilTShapley")) %>%
      mutate(category = factor(category, levels = allSec))
    
    stack_sums <- plotdf %>%
      group_by(period, scenario) %>%
      summarise(total = sum(value, na.rm = TRUE), .groups = "drop")

    
    p[[paste0('ineqTheilTRegBySec_',exampleReg)]] <- list(
      
      
      
      plot = ggplot(plotdf, aes(x = period, y = value, fill = category)) +
        geom_bar(
          stat = "identity",
          position = position_stack(),
          width = 2
        ) +
        scale_fill_paletteer_d("dutchmasters::view_of_Delft") +
        scale_x_continuous(breaks = unique(plotdf$period),
                           labels = unique(plotdf$period)) +
        coord_cartesian( ylim = quantile(stack_sums$total, probs = c(0.01, 0.99), na.rm = TRUE)) +
        theme_minimal() +
        labs(
          x = "Year",
          y = "Inequality Change (Theil Index)",
          fill = "Category",
          shape = "Scenario",
        ) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        facet_wrap(~scenario, ncol = 2)
      ,
      
      width = 8,
      height = 3
      
    )
    
    
    
  }
  
  if(any(plotlist == 'ineqTheilLRegBySec' | allExport  )){
    
    plotdf <- plotdataIneq[['ineq']] %>%
      filter( period <= 2100,
              period >= 2025,
              region == exampleReg,
              category != 'Total',
              variable %in% c("ineq|deltTheilLShapley")) %>%
      mutate(  category = factor(category, levels = allSec) )
    
    stack_sums <- plotdf %>%
      group_by(period, scenario) %>%
      summarise(total = sum(value, na.rm = TRUE), .groups = "drop")
    
    
    p[[paste0('ineqTheilLRegBySec_',exampleReg)]] <- list(
      
      plot = ggplot(plotdf, aes(x = period, y = value, fill = category)) +
        geom_bar(
          stat = "identity",
          position = position_stack(),
          width = 2
        ) +
        scale_fill_paletteer_d("dutchmasters::view_of_Delft") +
        scale_x_continuous(breaks = unique(plotdf$period),
                           labels = unique(plotdf$period))+
        theme_minimal() +
        coord_cartesian( ylim = quantile(stack_sums$total, probs = c(0.01, 0.99), na.rm = TRUE)) +
        labs(
          x = "Year",
          y = "Inequality Change (Theil Index)",
          fill = "Category",
          shape = "Scenario",
        ) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        facet_wrap(~scenario, ncol = 2)
      ,
      
      width = 8,
      height = 3
      
    )
    
    
    
  }
  
  
  if(any(plotlist == 'ineqTheilLRegBySecbyScen' | allExport  )){
    
    
    for(scen in c( unique(plotdataIneq[['ineq']]$scenario))){
      
      
      plotdf <- plotdataIneq[['ineq']] %>%
        filter( period <= 2100,
                period >= 2025,
                scenario == scen,
                region != 'World',
                category != 'Total',
                variable %in% c("ineq|deltTheilLShapley")) %>%
        mutate(  category = factor(category, levels = allSec) )
      
      stack_sums <- plotdf %>%
        group_by(period, scenario, region) %>%
        summarise(total = sum(value, na.rm = TRUE), .groups = "drop")
      
      p[[paste0('ineqTheilLRegBySec_',scen)]] <- list(
        
        plot = ggplot(plotdf, aes(x = period, y = value, fill = category)) +
          geom_bar(
            stat = "identity",
            position = position_stack(),
            width = 2
          ) +
          scale_fill_paletteer_d("dutchmasters::view_of_Delft") +
          
          scale_x_continuous(breaks = unique(plotdf$period),
                             labels = unique(plotdf$period))+
          theme_minimal() +
          labs(
            x = "Year",
            y = "Inequality Change (Theil Index)",
            fill = "Category",
          ) +
          coord_cartesian( ylim = quantile(stack_sums$total, probs = c(0.01, 0.99), na.rm = TRUE)) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          facet_wrap(~region, ncol = 3)
        ,
        
        width = 10,
        height = 8
        
      )
      
    }
   
    
    
    
  }
  
  #-------------End 5.3 regional inequality effect by Channel (Theil)-----------
  
  
  
  
  
  
  #-------------5.4 regional inequality contribution to Global----------------
  
  #inequality Contribution of between and within country inequality
  if(any(plotlist == 'ineqGlobalBetweenWithinTheilT' | allExport  )){
    
    plotdf <- plotdataIneq[['theilTDecomp']]%>%
      mutate(`Tt.i|delta` = Tt.i - `Tt.i|base`,
             `Tt.b|delta` = Tt.b - `Tt.b|base`,
             `Tt.iw|delta` = Tt.iw - `Tt.iw|base`) %>%
      group_by(scenario,period) %>%
      summarise( `Tt.i|delta` = sum(`Tt.i|delta`),
                 `Tt.b|delta` = sum(`Tt.b|delta`),
                 `Tt.iw|delta` = sum(`Tt.iw|delta`),
                 .groups = "drop") %>%
      pivot_longer(cols = c(`Tt.b|delta`, `Tt.iw|delta`, `Tt.i|delta`),
                   names_to = "component",
                   values_to = "value") %>%
      filter(period <= 2100) %>%
      mutate(component = recode(component,
                                "Tt.b|delta" = "Between",
                                "Tt.iw|delta" = "Within",
                                "Tt.i|delta" = "Total"))
    
    p[['ineqGlobalBetweenWithinTheilT']] <- list(
      
      plot = ggplot(plotdf, aes(x = period, y = value, group = component)) +
        # Area plots for Within and Between
        geom_area(
          data = filter(plotdf, component %in% c("Within", "Between")),
          aes(fill = component),
          position = "stack",
          alpha = 0.6
        ) +
        # Line plot for Total
        geom_line(
          data = filter(plotdf, component == "Total"),
          aes(color = component),
          size = 1.2
        ) +
        facet_wrap(~scenario) +
        labs(
          x = "Year",
          y = "Change in Inequality (Theil)",
          title = "Theil Inequality Decomposition over Time by Scenario",
          fill = "Component",
          color = "Theil"
        ) +
        scale_x_continuous(breaks = unique(plotdf$period),
                           labels = unique(plotdf$period)) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.text = element_text(size = 7),      
              legend.title = element_text(size = 8)   )
        
      ,
      
      width = 8,
      height = 3
      
    )
    
  }
    
  
  
  
  if(any(plotlist == 'ineqGlobalBetweenWithinTheilL' | allExport  )){
    
    plotdf <- plotdataIneq[['theilLDecomp']]%>%
      mutate(`Tl.i|delta` = Tl.i - `Tl.i|base`,
             `Tl.ib|delta` = Tl.ib - `Tl.ib|base`,
             `Tl.iw|delta` = Tl.iw - `Tl.iw|base`) %>%
      group_by(scenario,period) %>%
      summarise( `Tl.i|delta` = sum(`Tl.i|delta`),
                 `Tl.ib|delta` = sum(`Tl.ib|delta`),
                 `Tl.iw|delta` = sum(`Tl.iw|delta`),
                 .groups = "drop") %>%
      pivot_longer(cols = c(`Tl.ib|delta`, `Tl.iw|delta`, `Tl.i|delta`),
                   names_to = "component",
                   values_to = "value") %>%
      filter(period <= 2100) %>%
      mutate(component = recode(component,
                                "Tl.ib|delta" = "Between",
                                "Tl.iw|delta" = "Within",
                                "Tl.i|delta" = "Total"))
    
    p[['ineqGlobalBetweenWithinTheilL']] <- list(
      
      plot = ggplot(plotdf, aes(x = period, y = value, group = component)) +
        # Area plots for Within and Between
        geom_area(
          data = filter(plotdf, component %in% c("Within", "Between")),
          aes(fill = component),
          position = "stack",
          alpha = 0.6
        ) +
        # Line plot for Total
        geom_line(
          data = filter(plotdf, component == "Total"),
          aes(color = component),
          size = 1.2
        ) +
        facet_wrap(~scenario) +
        labs(
          x = "Year",
          y = "Change in Inequality (Theil)",
          title = "Theil Inequality Decomposition over Time by Scenario",
          fill = "Component",
          color = "Theil"
        ) +
        scale_x_continuous(breaks = unique(plotdf$period),
                           labels = unique(plotdf$period)) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.text = element_text(size = 7),      
              legend.title = element_text(size = 8)   )
      
      ,
      
      width = 8,
      height = 3
      
    )
    
  }
  
  
  
    #Regional contribution to within region theil index
  if(any(plotlist == 'ineqGlobalWithinRegTheilT' | allExport  )){
      
      plotdf <- plotdataIneq[['theilTDecomp']]%>%
        mutate(`Tt.i|delta` = Tt.i - `Tt.i|base`,
               `Tt.b|delta` = Tt.b - `Tt.b|base`,
               `Tt.iw|delta` = Tt.iw - `Tt.iw|base`) %>%
        group_by(scenario,period,region) %>%
        summarise( `Tt.iw|delta` = sum(`Tt.iw|delta`),
                   .groups = "drop") %>%
        rename(within = "Tt.iw|delta") %>%
        filter(period <= 2100)

      # plotdf <- plotdataIneq[['theilDecomp']]%>%
      #   mutate(`Tl.i|delta` = Tl.i - `Tl.i|base`,
      #          `Tl.ib|delta` = Tl.ib - `Tl.ib|base`,
      #          `Tl.iw|delta` = Tl.iw - `Tl.iw|base`) %>%
      #   group_by(scenario,period,region) %>%
      #   summarise( `Tl.iw|delta` = sum(`Tl.iw|delta`),
      #              .groups = "drop") %>%
      #   rename(within = "Tl.iw|delta") %>%
      #   filter(period <= 2100) 
      
      plotdf <- plotdf %>% 
        group_by(scenario, period) %>%
        summarise(within = sum(within),
                  .groups = "drop") %>%
        mutate(region = 'total') %>%
        bind_rows(plotdf)

      
      p[['ineqGlobalWithinRegTheilT']] <- list(
        
        plot = ggplot(plotdf, aes(x = period, y = within, group = region)) +
          # Area plots for Within and Between
          geom_area(
            data = filter(plotdf, region != 'total'),
            aes(fill = region),
            position = "stack",
            alpha = 0.6
          ) +
          geom_line(
            data = filter(plotdf, region == "total"),
            aes(color = "Total"),
            size = 1,
            linetype = 2
          ) +
          facet_wrap(~scenario) +
          labs(
            x = "Year",
            y = "Change in Inequality (Theil)",
            title = "Theil Inequality Decomposition over Time by Scenario",
            fill = "Component",
            color = "Theil"
          ) +
          scale_x_continuous(breaks = unique(plotdf$period),
                             labels = unique(plotdf$period)) +
          scale_fill_brewer(palette = "Paired") +
          scale_color_manual(values = c("Total" = "black")) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                legend.text = element_text(size = 6),      
                legend.title = element_text(size = 8)   )
        
        ,
        
        width = 8,
        height = 5
        
      )
    }
      
      #TheilL
      
  if(any(plotlist == 'ineqGlobalWithinRegTheilL' | allExport  )){
     plotdf <- plotdataIneq[['theilLDecomp']]%>%
        mutate(`Tl.i|delta` = Tl.i - `Tl.i|base`,
               `Tl.ib|delta` = Tl.ib - `Tl.ib|base`,
               `Tl.iw|delta` = Tl.iw - `Tl.iw|base`) %>%
        group_by(scenario,period,region) %>%
        summarise( `Tl.iw|delta` = sum(`Tl.iw|delta`),
                   .groups = "drop") %>%
        rename(within = "Tl.iw|delta") %>%
        filter(period <= 2100)
      
      plotdf <- plotdf %>% 
        group_by(scenario, period) %>%
        summarise(within = sum(within),
                  .groups = "drop") %>%
        mutate(region = 'total') %>%
        bind_rows(plotdf)
      
      p[['ineqGlobalWithinRegTheilL']] <- list(
        
        plot = ggplot(plotdf, aes(x = period, y = within, group = region)) +
          # Area plots for Within and Between
          geom_area(
            data = filter(plotdf, region != 'total'),
            aes(fill = region),
            position = "stack",
            alpha = 0.6
          ) +
          geom_line(
            data = filter(plotdf, region == "total"),
            aes(color = "Total"),
            size = 1,
            linetype = 2
          ) +
          facet_wrap(~scenario) +
          labs(
            x = "Year",
            y = "Change in Inequality (Theil)",
            title = "Theil Inequality Decomposition over Time by Scenario",
            fill = "Component",
            color = "Theil"
          ) +
          scale_x_continuous(breaks = unique(plotdf$period),
                             labels = unique(plotdf$period)) +
          scale_fill_brewer(palette = "Paired") +
          scale_color_manual(values = c("Total" = "black")) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                  legend.text = element_text(size = 6),      
                  legend.title = element_text(size = 8)      
                )
        
        ,
        
        width = 8,
        height = 5
        
      )
    

    
    
  }
  
  #-------------End 5.4 regional inequality effect by Channel (Theil)-----------
  
  
    
  
  if(isDisplay){
    print(p)
  }
  
  
  
  if(isExport|allExport){
    for (name in names(p)) {
      ggsave(
        filename = paste0(outputPath,'/', micro_model, capitalize_first(fixed_point),"/", name, ".tiff"),
        plot = p[[name]]$plot,
        width = p[[name]]$width,
        height = p[[name]]$height,
        dpi = 300
      )
    }
  }
  


}
