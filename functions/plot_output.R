#This is the function to plot welfare chagne



plot_output <- function(outputPath, data1, data2, data3, plotdataIneq,  plotlist='NA' , micro_model, fixed_point, exampleReg = 'EUR', isDisplay = T, isExport = FALSE, allExport=FALSE ) {
  
  plotdataWelfWithTransfEpc <- data1 %>%
    filter(category != 'Consumption With NeutTransf') %>%
    filter( period %notin% c(2010,2110,2130,2150)) 
  
  plotdataWelf <- data1 %>%
    filter(category != 'Consumption With EpcTransf') %>%
    filter( period %notin% c(2010,2110,2130,2150)) 
  
  data3 <- data3 %>%
    filter(period %notin% c(2010,2110,2130,2150)) 
  

  
  theme_set(
    theme_minimal(base_family = "Arial", base_size = 9) +
      theme(
        plot.title = element_text(hjust = 0.5, size = 10),
        axis.text.x = element_text(size = 9,angle = 45, hjust = 1),
        axis.text.y = element_text(size = 9),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text  = element_text(size = 9),
        legend.position = "bottom",
        legend.direction = "horizontal",
        panel.grid.major.y = element_line(
                color = "grey70",     # light grey
                linewidth = 0.1,      # thinner line
                linetype = "dashed"   # dashed style
                ),
        panel.grid.major.x = element_line(
                color = "grey70",     # light grey
                linewidth = 0.1,      # thinner line
                linetype = "dashed"   # dashed style
              ),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "white", color = NA)

      )
  )
  
  numSector <- length(unique(plotdataWelf$category))
  foodSec <- c("Animal products","Empty calories","Fruits vegetables nuts","Staples")
  eneSec <- c("Building electricity", "Building gases", "Building other fuels","Transport energy")
  allSec <- c(foodSec, eneSec, 'Other commodities','Consumption')
  p=list()

#-------------------------1.Plots all aggregated welfare change------------------------
  
  if(any(plotlist == "welfByPeriod") | allExport){
    #todo: is using weighted average better?
    
    dataDecile <- plotdataWelf %>%
      filter(period <= 2100 ) %>%
      group_by(scenario, period, region, decileGroup) %>%
      summarise(sumWelfChange = sum(decilWelfChange, na.rm = TRUE), .groups = "drop")%>%
      mutate(scenario = factor(scenario, levels = c(
        "C_SSP2-loOS-def", 
        "C_SSP2-hiOS-def" 
      )))

    avg_df_period <- dataDecile %>%
      group_by(scenario, period) %>%
      summarise(meanWelfChange = mean(sumWelfChange, na.rm = TRUE), .groups = "drop") %>%
      mutate(scenario = factor(scenario, levels = c(
        "C_SSP2-loOS-def", 
        "C_SSP2-hiOS-def" 
      )))
      
    
    
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
        scale_color_manual(
          values = c("C_SSP2-PkBudg1000" = "#e8ab67",
                     "C_SSP2-PkBudg650"  = "#264f78",
                     "C_SSP2-hiOS-def" = "#e8ab67",
                     "C_SSP2-loOS-def" = "#264f78"),
          labels = c("C_SSP2-PkBudg1000" = "2°C",
                     "C_SSP2-PkBudg650"  = "1.5°C",
                     "C_SSP2-hiOS-def" = "1.5°C HO",
                     "C_SSP2-loOS-def" = "1.5°C LO")
          ) +
        
        # One-by-one geom_line for each scenario
        geom_line(data = avg_df_period %>% filter(scenario %in% c("C_SSP2-PkBudg1000", "C_SSP2-hiOS-def"), period >= 2025),
                  aes(x = period - 1, y = meanWelfChange),
                  color = "#ba7a31", linewidth = 0.7) +
        geom_line(data = avg_df_period %>% filter(scenario %in% c("C_SSP2-PkBudg650", "C_SSP2-loOS-def"), period >= 2025),
                  aes(x = period + 1, y = meanWelfChange),
                  color = "#264f78", linewidth = 0.7)+
        
        #  Custom color mapping for boxes and lines
        scale_fill_manual(
          values = c("C_SSP2-PkBudg1000" = "#e8ab67", 
                     "C_SSP2-PkBudg650"  = "#779ec6",
                     "C_SSP2-hiOS-def" = "#e8ab67", 
                     "C_SSP2-loOS-def"  = "#779ec6"),
          labels = c("C_SSP2-PkBudg1000" = "2°C",
                     "C_SSP2-PkBudg650"  = "1.5°C",
                     "C_SSP2-hiOS-def" = "1.5°C HO",
                     "C_SSP2-loOS-def" = "1.5°C LO")) +
        scale_x_continuous(breaks = unique(plotdataWelf$period),
                           labels = unique(plotdataWelf$period)) +
        
        # Labels and styling
        labs(x = "Year", y = "Real consumpition Change (%)", fill = "Scenario", color = "Scenario") +
        coord_cartesian(ylim = quantile(dataDecile$sumWelfChange, probs = c(0.02, 0.99), na.rm = TRUE))
        # + theme_minimal()
        # + theme(
        #   axis.text.x = element_text(angle = 45, hjust = 1),
        #   legend.position = "bottom",
        #   legend.direction = "horizontal",
        #   panel.grid.major.y = element_line(color = "grey70", linewidth = 0.1, linetype = "dashed"),
        #   panel.grid.major.x = element_line(color = "grey70", linewidth = 0.1, linetype = "dashed"),
        #   panel.grid.minor = element_blank(),
        #   panel.background = element_rect(fill = "white", color = NA)
        # )
      ,
      
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
        scale_fill_manual(
          values = c("C_SSP2-PkBudg1000" = "#e8ab67",  
                     "C_SSP2-PkBudg650"  = "#779ec6",
                     "C_SSP2-hiOS-def" = "#e8ab67", 
                     "C_SSP2-loOS-def" = "#779ec6"),
          
          labels = c("C_SSP2-PkBudg1000" = "2°C",
                     "C_SSP2-PkBudg650"  = "1.5°C",
                     "C_SSP2-hiOS-def" = "1.5°C HO",
                     "C_SSP2-loOS-def" = "1.5°C LO")
          
        ) +
        scale_color_manual(
          values = c("C_SSP2-PkBudg1000" = "#ba7a31",
                                      "C_SSP2-PkBudg650"  = "#264f78",
                                      "C_SSP2-hiOS-def" = "#e8ab67", 
                                      "C_SSP2-loOS-def" = "#779ec6"),
          labels = c("C_SSP2-PkBudg1000" = "2°C",
                   "C_SSP2-PkBudg650"  = "1.5°C",
                   "C_SSP2-hiOS-def" = "1.5°C HO",
                   "C_SSP2-loOS-def" = "1.5°C LO")
        ) +
        stat_summary(fun = mean, geom = "point", 
                     aes(group = scenario), 
                     position = position_dodge(width = 0.7), 
                     shape = 20, size = 2.5, color = "gray30") +
        
        labs(x = "Income Decile Group", y = "Real consumpition Change (%)", fill = "Scenario", color = "Scenario") +
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
        scale_color_manual(
          values = c("C_SSP2-PkBudg1000" = "#ba7a31",
                     "C_SSP2-PkBudg650"  = "#264f78",
                     "C_SSP2-hiOS-def" = "#e8ab67", 
                     "C_SSP2-loOS-def" = "#779ec6"),
          labels = c("C_SSP2-PkBudg1000" = "2°C",
                     "C_SSP2-PkBudg650"  = "1.5°C",
                     "C_SSP2-hiOS-def" = "1.5°C HO",
                     "C_SSP2-loOS-def" = "1.5°C LO")
        ) +
        # One-by-one geom_line for each scenario
        geom_line(data = avg_df_period %>% filter(scenario %in% c("C_SSP2-PkBudg1000", "C_SSP2-hiOS-def"), period >= 2025),
                  aes(x = period - 1, y = meanWelfChange),
                  color = "#ba7a31", linewidth = 0.7) +
        
        geom_line(data = avg_df_period %>% filter(scenario %in% c("C_SSP2-PkBudg650", "C_SSP2-loOS-def" ), period >= 2025),
                  aes(x = period + 1, y = meanWelfChange),
                  color = "#264f78", linewidth = 0.7)+
        
        #  Custom color mapping for boxes and lines
        scale_fill_manual(
          values = c("C_SSP2-PkBudg1000" = "#e8ab67",  
                     "C_SSP2-PkBudg650"  = "#779ec6",
                     "C_SSP2-hiOS-def" = "#e8ab67", 
                     "C_SSP2-loOS-def" = "#779ec6"),
          
          labels = c("C_SSP2-PkBudg1000" = "2°C",
                     "C_SSP2-PkBudg650"  = "1.5°C",
                     "C_SSP2-hiOS-def" = "1.5°C HO",
                     "C_SSP2-loOS-def" = "1.5°C LO")
          
        ) +
        scale_x_continuous(breaks = unique(plotdataWelf$period),
                           labels = unique(plotdataWelf$period))+
        
        # Labels and styling
        labs(x = "Period", y = "Real Consumption Change (%)", fill = "Scenario", color = "Scenario") +
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
        scale_fill_manual(
          values = c("C_SSP2-PkBudg1000" = "#e8ab67",  
                     "C_SSP2-PkBudg650"  = "#779ec6",
                     "C_SSP2-hiOS-def" = "#e8ab67", 
                     "C_SSP2-loOS-def" = "#779ec6"),
          
          labels = c("C_SSP2-PkBudg1000" = "2°C",
                     "C_SSP2-PkBudg650"  = "1.5°C",
                     "C_SSP2-hiOS-def" = "1.5°C HO",
                     "C_SSP2-loOS-def" = "1.5°C LO")
          
        ) +
        scale_color_manual(
          values = c("C_SSP2-PkBudg1000" = "#ba7a31",
                     "C_SSP2-PkBudg650"  = "#264f78",
                     "C_SSP2-hiOS-def" = "#e8ab67", 
                     "C_SSP2-loOS-def" = "#779ec6"),
          labels = c("C_SSP2-PkBudg1000" = "2°C",
                     "C_SSP2-PkBudg650"  = "1.5°C",
                     "C_SSP2-hiOS-def" = "1.5°C HO",
                     "C_SSP2-loOS-def" = "1.5°C LO")
        )+
        stat_summary(fun = mean, geom = "point", 
                     aes(group = scenario), 
                     position = position_dodge(width = 0.7), 
                     shape = 20, size = 2.5, color = "gray30") +
        
        labs(x = "Income Decile Group", y = "Real Consumption Change (%)", fill = "Scenario", color = "Scenario") +
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
        scale_color_manual(
          values = c("C_SSP2-PkBudg1000" = "#ba7a31",
                     "C_SSP2-PkBudg650"  = "#264f78",
                     "C_SSP2-hiOS-def" = "#e8ab67", 
                     "C_SSP2-loOS-def" = "#779ec6"),
          labels = c("C_SSP2-PkBudg1000" = "2°C",
                     "C_SSP2-PkBudg650"  = "1.5°C",
                     "C_SSP2-hiOS-def" = "1.5°C HO",
                     "C_SSP2-loOS-def" = "1.5°C LO")
        ) +
        
        # One-by-one geom_line for each scenario
        geom_line(data = avg_df_period %>% filter(scenario %in% c("C_SSP2-PkBudg1000","C_SSP2-hiOS-def"), period >= 2025),
                  aes(x = period - 1, y = meanWelfChange),
                  color = "#ba7a31", linewidth = 0.7) +
        
        geom_line(data = avg_df_period %>% filter(scenario %in% c("C_SSP2-PkBudg650","C_SSP2-loOS-def"), period >= 2025),
                  aes(x = period + 1, y = meanWelfChange),
                  color = "#264f78", linewidth = 0.7)+
        
        #  Custom color mapping for boxes and lines
        scale_fill_manual(
          values = c("C_SSP2-PkBudg1000" = "#e8ab67",  
                     "C_SSP2-PkBudg650"  = "#779ec6",
                     "C_SSP2-hiOS-def" = "#e8ab67", 
                     "C_SSP2-loOS-def" = "#779ec6"),
          
          labels = c("C_SSP2-PkBudg1000" = "2°C",
                     "C_SSP2-PkBudg650"  = "1.5°C",
                     "C_SSP2-hiOS-def" = "1.5°C HO",
                     "C_SSP2-loOS-def" = "1.5°C LO")
          
        ) +
        scale_x_continuous(breaks = unique(plotdataWelf$period),
                           labels = unique(plotdataWelf$period))+
        
        # Labels and styling
        labs(x = "Period", y = "Real Consumption Change (%)", fill = "Scenario", color = "Scenario") +
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
        scale_fill_manual(
          values = c("C_SSP2-PkBudg1000" = "#e8ab67",  
                     "C_SSP2-PkBudg650"  = "#779ec6",
                     "C_SSP2-hiOS-def" = "#e8ab67", 
                     "C_SSP2-loOS-def" = "#779ec6"),
          
          labels = c("C_SSP2-PkBudg1000" = "2°C",
                     "C_SSP2-PkBudg650"  = "1.5°C",
                     "C_SSP2-hiOS-def" = "1.5°C HO",
                     "C_SSP2-loOS-def" = "1.5°C LO")
          
        ) +
        scale_color_manual(
          values = c("C_SSP2-PkBudg1000" = "#ba7a31",
                     "C_SSP2-PkBudg650"  = "#264f78",
                     "C_SSP2-hiOS-def" = "#e8ab67", 
                     "C_SSP2-loOS-def" = "#779ec6"),
          labels = c("C_SSP2-PkBudg1000" = "2°C",
                     "C_SSP2-PkBudg650"  = "1.5°C",
                     "C_SSP2-hiOS-def" = "1.5°C HO",
                     "C_SSP2-loOS-def" = "1.5°C LO")
        ) +
        stat_summary(fun = mean, geom = "point", 
                     aes(group = scenario), 
                     position = position_dodge(width = 0.7), 
                     shape = 20, size = 2.5, color = "gray30") +
        
        labs(x = "Income Decile Group", y = "Real Consumption Change (%)", fill = "Scenario", color = "Scenario") +
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
  

#----------2.3. contribution food and energy aggregated(welfare change)---------

  if(any(plotlist=="foodEneContribution")| allExport ){
    
    df <- aggregate_decileWelfChange(data1 = plotdataWelf, data2 = data2, 
                               level = c("fullSec"), region = 'global') %>%
      mutate(category = factor(category, levels = allSec))
    
    plotdf <- df %>%
      filter(period <= 2100,
             category %in% foodSec) %>%
      group_by(scenario, period, ) %>%
      summarise(foodShare = sum(welfChange, na.rm = TRUE), .groups = "drop") 
    
    plotdf <- df %>%
      filter(period <= 2100,
             category %in% eneSec) %>%
      group_by(scenario, period) %>%
      summarise(eneShare = sum(welfChange, na.rm = TRUE), .groups = "drop") %>%
      left_join(plotdf, by=c('scenario','period')) %>%
      pivot_longer(cols = c('eneShare','foodShare'), names_to = 'category',
                   values_to = 'value') %>%
      group_by(scenario, period) %>%
      mutate(
        value = value / sum(value, na.rm = TRUE),
        value = ifelse(is.na(value), 0, value)
      ) %>%
      ungroup() %>%
      mutate(scenario = factor(scenario, levels = c(
        "C_SSP2-loOS-def", 
        "C_SSP2-hiOS-def" 
      )))
    
    # Automate over all scenarios
    p[['foodEneContribution']] <- list(
      plot =
        ggplot(plotdf, aes(x = period, y = value, fill = category)) +
        geom_col(position = "stack") +
        facet_wrap(~ scenario, ncol = 2,
                   labeller = as_labeller(
                     c("C_SSP2-PkBudg1000" = "2°C",
                       "C_SSP2-PkBudg650"  = "1.5°C",
                       "C_SSP2-loOS-def"  = "1.5°C LO",
                       "C_SSP2-hiOS-def" = "1.5°C HO"
                       )
                   )) +
        labs(
          x = "Year",
          y = "Real Consumption Change (%)",
          fill = "Category"
        ) +
        scale_fill_paletteer_d("ButterflyColors::astraptes_fulgerator",
                               labels = c("foodShare" = "Food",
                                          "eneShare"  = "Energy")) +
        guides(fill = guide_legend(reverse = TRUE)) +
        scale_x_continuous(breaks = unique(plotdataWelf$period),
                           labels = unique(plotdataWelf$period))
        
      ,
      
      width = 8,
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
        scale_fill_manual(
          values = c("C_SSP2-PkBudg1000" = "#e8ab67",  
                     "C_SSP2-PkBudg650"  = "#779ec6",
                     "C_SSP2-hiOS-def" = "#e8ab67", 
                     "C_SSP2-loOS-def" = "#779ec6"),
          
          labels = c("C_SSP2-PkBudg1000" = "2°C",
                     "C_SSP2-PkBudg650"  = "1.5°C",
                     "C_SSP2-hiOS-def" = "1.5°C HO",
                     "C_SSP2-loOS-def" = "1.5°C LO")
          
        ) +
        scale_color_manual(
          values = c("C_SSP2-PkBudg1000" = "#ba7a31",
                     "C_SSP2-PkBudg650"  = "#264f78",
                     "C_SSP2-hiOS-def" = "#e8ab67", 
                     "C_SSP2-loOS-def" = "#779ec6"),
          labels = c("C_SSP2-PkBudg1000" = "2°C",
                     "C_SSP2-PkBudg650"  = "1.5°C",
                     "C_SSP2-hiOS-def" = "1.5°C HO",
                     "C_SSP2-loOS-def" = "1.5°C LO")
        ) +
        stat_summary(fun = mean, geom = "point", 
                     aes(group = scenario), 
                     position = position_dodge(width = 0.7), 
                     shape = 20, size = 2.5, color = "gray30") +
        
        labs(x = "Income Decile Group", y = "Real Consumption Change (%)", fill = "Scenario", color = "Scenario") +
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

        # One-by-one geom_line for each scenario
        geom_line(data = avg_df_period %>% filter(scenario %in% c("C_SSP2-PkBudg1000", "C_SSP2-hiOS-def"), period >= 2025),
                  aes(x = period - 1, y = meanWelfChange),
                  color = "#ba7a31", size = 0.7) +
        
        geom_line(data = avg_df_period %>% filter(scenario %in% c("C_SSP2-PkBudg650","C_SSP2-loOS-def"), period >= 2025),
                  aes(x = period + 1, y = meanWelfChange),
                  color = "#264f78", size = 0.7)+
      
        scale_fill_manual(
          values = c("C_SSP2-PkBudg1000" = "#e8ab67",  
                     "C_SSP2-PkBudg650"  = "#779ec6",
                     "C_SSP2-hiOS-def" = "#e8ab67", 
                     "C_SSP2-loOS-def" = "#779ec6"),
          
          labels = c("C_SSP2-PkBudg1000" = "2°C",
                     "C_SSP2-PkBudg650"  = "1.5°C",
                     "C_SSP2-hiOS-def" = "1.5°C HO",
                     "C_SSP2-loOS-def" = "1.5°C LO")
          
        ) +
        scale_color_manual(
          values = c("C_SSP2-PkBudg1000" = "#ba7a31",
                     "C_SSP2-PkBudg650"  = "#264f78",
                     "C_SSP2-hiOS-def" = "#e8ab67", 
                     "C_SSP2-loOS-def" = "#779ec6"),
          labels = c("C_SSP2-PkBudg1000" = "2°C",
                     "C_SSP2-PkBudg650"  = "1.5°C",
                     "C_SSP2-hiOS-def" = "1.5°C HO",
                     "C_SSP2-loOS-def" = "1.5°C LO")
        ) +
        scale_x_continuous(breaks = unique(plotdataWelf$period),
                           labels = unique(plotdataWelf$period)) +
        
        # Labels and styling
        labs(x = "Period", y = "Real Consumption Change (%)", fill = "Scenario", color = "Scenario") +
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
        scale_fill_manual(
          values = c("C_SSP2-PkBudg1000" = "#e8ab67",  
                     "C_SSP2-PkBudg650"  = "#779ec6",
                     "C_SSP2-hiOS-def" = "#e8ab67", 
                     "C_SSP2-loOS-def" = "#779ec6"),
          
          labels = c("C_SSP2-PkBudg1000" = "2°C",
                     "C_SSP2-PkBudg650"  = "1.5°C",
                     "C_SSP2-hiOS-def" = "1.5°C HO",
                     "C_SSP2-loOS-def" = "1.5°C LO")
          
        ) +
        scale_color_manual(
          values = c("C_SSP2-PkBudg1000" = "#ba7a31",
                     "C_SSP2-PkBudg650"  = "#264f78",
                     "C_SSP2-hiOS-def" = "#e8ab67", 
                     "C_SSP2-loOS-def" = "#779ec6"),
          labels = c("C_SSP2-PkBudg1000" = "2°C",
                     "C_SSP2-PkBudg650"  = "1.5°C",
                     "C_SSP2-hiOS-def" = "1.5°C HO",
                     "C_SSP2-loOS-def" = "1.5°C LO")
        ) +
        stat_summary(fun = mean, geom = "point", 
                     aes(group = scenario), 
                     position = position_dodge(width = 0.7), 
                     shape = 20, size = 2.5, color = "gray30") +
        
        labs(x = "Income Decile Group", y = "Real Consumption Change (%)", fill = "Scenario", color = "Scenario") +
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
        scale_fill_manual(
          values = c("C_SSP2-PkBudg1000" = "#e8ab67",  
                     "C_SSP2-PkBudg650"  = "#779ec6",
                     "C_SSP2-hiOS-def" = "#e8ab67", 
                     "C_SSP2-loOS-def" = "#779ec6"),
          
          labels = c("C_SSP2-PkBudg1000" = "2°C",
                     "C_SSP2-PkBudg650"  = "1.5°C",
                     "C_SSP2-hiOS-def" = "1.5°C HO",
                     "C_SSP2-loOS-def" = "1.5°C LO")
          
        ) +
        scale_color_manual(
          values = c("C_SSP2-PkBudg1000" = "#ba7a31",
                     "C_SSP2-PkBudg650"  = "#264f78",
                     "C_SSP2-hiOS-def" = "#e8ab67", 
                     "C_SSP2-loOS-def" = "#779ec6"),
          labels = c("C_SSP2-PkBudg1000" = "2°C",
                     "C_SSP2-PkBudg650"  = "1.5°C",
                     "C_SSP2-hiOS-def" = "1.5°C HO",
                     "C_SSP2-loOS-def" = "1.5°C LO")
        ) +
        # One-by-one geom_line for each scenario
        geom_line(data = avg_df_period %>% filter(scenario %in% c("C_SSP2-PkBudg1000","C_SSP2-hiOS-def"), period >= 2025),
                  aes(x = period - 1, y = meanWelfChange),
                  color = "#ba7a31", size = 0.7) +
        
        geom_line(data = avg_df_period %>% filter(scenario %in% c("C_SSP2-PkBudg650","C_SSP2-loOS-def"), period >= 2025),
                  aes(x = period + 1, y = meanWelfChange),
                  color = "#264f78", size = 0.7) +
    
        scale_x_continuous(breaks = unique(plotdataWelf$period),
                           labels = unique(plotdataWelf$period))+
        
        # Labels and styling
        labs(x = "Period", y = "Real Consumption Change (%)", fill = "Scenario", color = "Scenario") +
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
        scale_fill_manual(
          values = c("C_SSP2-PkBudg1000" = "#e8ab67",  
                     "C_SSP2-PkBudg650"  = "#779ec6",
                     "C_SSP2-hiOS-def" = "#e8ab67", 
                     "C_SSP2-loOS-def" = "#779ec6"),
          
          labels = c("C_SSP2-PkBudg1000" = "2°C",
                     "C_SSP2-PkBudg650"  = "1.5°C",
                     "C_SSP2-hiOS-def" = "1.5°C HO",
                     "C_SSP2-loOS-def" = "1.5°C LO")
          
        ) +
        scale_color_manual(
          values = c("C_SSP2-PkBudg1000" = "#ba7a31",
                     "C_SSP2-PkBudg650"  = "#264f78",
                     "C_SSP2-hiOS-def" = "#e8ab67", 
                     "C_SSP2-loOS-def" = "#779ec6"),
          labels = c("C_SSP2-PkBudg1000" = "2°C",
                     "C_SSP2-PkBudg650"  = "1.5°C",
                     "C_SSP2-hiOS-def" = "1.5°C HO",
                     "C_SSP2-loOS-def" = "1.5°C LO")
        ) +
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
        scale_fill_manual(
          values = c("C_SSP2-PkBudg1000" = "#e8ab67",  
                     "C_SSP2-PkBudg650"  = "#779ec6",
                     "C_SSP2-hiOS-def" = "#e8ab67", 
                     "C_SSP2-loOS-def" = "#779ec6"),
          
          labels = c("C_SSP2-PkBudg1000" = "2°C",
                     "C_SSP2-PkBudg650"  = "1.5°C",
                     "C_SSP2-hiOS-def" = "1.5°C HO",
                     "C_SSP2-loOS-def" = "1.5°C LO")
          
        ) +
        scale_color_manual(
          values = c("C_SSP2-PkBudg1000" = "#ba7a31",
                     "C_SSP2-PkBudg650"  = "#264f78",
                     "C_SSP2-hiOS-def" = "#e8ab67", 
                     "C_SSP2-loOS-def" = "#779ec6"),
          labels = c("C_SSP2-PkBudg1000" = "2°C",
                     "C_SSP2-PkBudg650"  = "1.5°C",
                     "C_SSP2-hiOS-def" = "1.5°C HO",
                     "C_SSP2-loOS-def" = "1.5°C LO")
        ) +
        
        # One-by-one geom_line for each scenario
        geom_line(data = avg_df_period %>% filter(scenario %in% c("C_SSP2-PkBudg1000","C_SSP2-hiOS-def"), period >= 2025),
                  aes(x = period - 1, y = meanWelfChange),
                  color = "#ba7a31", linewidth = 0.7) +
        
        geom_line(data = avg_df_period %>% filter(scenario %in% c("C_SSP2-PkBudg650","C_SSP2-loOS-def"), period >= 2025),
                  aes(x = period + 1, y = meanWelfChange),
                  color = "#264f78", size = 0.7)+
        
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
      filter( period <= 2100,
              category != 'Consumption With NeutTransf') %>%
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
        labs(x = "Decile", y = "Real Consumption Change (%)", fill = "FE category") +
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
    
    plotdf <- aggregate_decileWelfChange(data1 = plotdataWelf, data2 = data2, 
                                         level = c("fullSec"), region = 'global') %>%
      mutate(category = factor(category, levels = allSec))
    
    
    # Automate over all scenarios
    p[['globalWelfBySec']] <- list(
      plot = ggplot(plotdf, aes(x = factor(period), y = welfChange, fill = category)) +
        geom_col(position = "stack") +
        geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
        facet_wrap(~ scenario) +
        labs(
          x = "Year",
          y = "Real Consumption Change (%)",
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
      
      width = 8,
      height = 3
      
    )
    
  }
  

  
  if(any(plotlist == 'regWelfBySec' | allExport  )){
    
    plotdf <- aggregate_decileWelfChange(data1 = plotdataWelf, data2 = data2, 
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
              category == 'TotalWithTransfNeut',
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
        facet_wrap(~scenario, ncol = 2,
                   labeller = as_labeller(
                     c("C_SSP2-PkBudg1000" = "2°C",
                       "C_SSP2-PkBudg650"  = "1.5°C",
                       "C_SSP2-hiOS-def" = "1.5°C HO",
                       "C_SSP2-loOS-def"  = "1.5°C LO")
                   )
                   ) +
        # Labels and styling
        labs(x = "Year", y = "Inequality metrics", fill = "FE category") +
        coord_cartesian(ylim = quantile(plotdf$value, probs = c(0.01, 0.999), na.rm = TRUE)) +
        scale_color_paletteer_d("dutchmasters::pearl_earring") +
        scale_linetype_manual(
          name   = "Scenario",            
          values = c("before" = "solid",       
                     "after"  = "dashed"),   
          labels = c("before" = "Baseline",     
                     "after"  = "Policy")
        )+
        scale_x_continuous(breaks = unique(plotdf$period),
                           labels = unique(plotdf$period)) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom",
          legend.direction = "horizontal",
          panel.grid.major.y = element_line(color = "grey70", linewidth = 0.1, linetype = "dashed"),
          panel.grid.major.x = element_line(color = "grey70", linewidth = 0.1, linetype = "dashed"),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "white", color = NA)
        ),
      
      width = 8,
      height = 3
      
    )
    
    
      
  }
  
  
  
  if(any(plotlist == 'ineqWorldWithTransf_Gini' | allExport  )){
    
    plotdf <- plotdataIneq[['ineq']] %>%
      filter( period <= 2100,
              region == 'World',
              category %in% c('TotalWithTransfNeut','TotalWithTransfEpc'),
              variable == 'ineq|deltGini') %>% 
      mutate(value = value * 100) %>% #unit transformed from coefficient to points 
      mutate(scenario = factor(scenario, levels = c(
        "C_SSP2-loOS-def", 
        "C_SSP2-hiOS-def" 
      )))
    
    
    # Automate over all scenarios
    p[['ineqWorldWithTransf_Gini']] <- list(
      
      plot = ggplot(plotdf, 
                    aes(x = period, y = value , linetype = category)) +
        geom_line( color = '#e8ab67', size = 0.8) + 
        scale_fill_brewer(palette = "Set2") +
        facet_wrap(~scenario, ncol = 2,
                   labeller = as_labeller(
                     c("C_SSP2-PkBudg1000" = "2°C",
                       "C_SSP2-PkBudg650"  = "1.5°C",
                       "C_SSP2-hiOS-def" = "1.5°C HO",
                       "C_SSP2-loOS-def"  = "1.5°C LO"))
                   ) +
        geom_hline(yintercept = 0, linetype = "solid", color = "grey80")+
        # Labels and styling
        labs(x = "Year", y = "Gini change from reference (points)", fill = "FE category") +
        coord_cartesian(ylim = quantile(plotdf$value, probs = c(0.01, 0.999), na.rm = TRUE)) +
        # scale_color_paletteer_d("dutchmasters::pearl_earring",
        #                         name = "Metric",
        #                         labels = c(
        #                           "ineq|deltGini" = 'Gini',
        #                           "ineq|deltTheilL" = "TheilL",
        #                           "ineq|deltTheilT" = "TheilT"
        #                         )) +
        scale_linetype_manual(name = "Compensation",
                              values = c("TotalWithTransfNeut" = "solid", 
                                         "TotalWithTransfEpc" = "dashed"),
                              labels = c("TotalWithTransfNeut" = "Neutral",
                                         "TotalWithTransfEpc" = "Smoothed Equal Transfer")) +
        scale_x_continuous(breaks = unique(plotdataWelf$period),
                           labels = unique(plotdataWelf$period)) +
        theme(axis.title.x = element_blank())
        # theme_minimal() +
        # theme(
        #   axis.text.x = element_text(angle = 45, hjust = 1),
        #   axis.title.x = element_blank(),
        #   legend.position = "bottom",
        #   legend.direction = "horizontal",
        #   panel.grid.major.y = element_line(
        #     color = "grey70",     # light grey
        #     linewidth = 0.1,      # thinner line
        #     linetype = "dashed"   # dashed style
        #   ),
        #   panel.grid.major.x = element_line(
        #     color = "grey70",     # light grey
        #     linewidth = 0.1,      # thinner line
        #     linetype = "dashed"   # dashed style
        #   ),
        #   panel.grid.minor.x = element_blank(),
        #   panel.grid.minor.y = element_blank(),
        #   panel.background = element_rect(fill = "white", color = NA)
        # )
      ,
      
      width = 8,
      height = 3
      
    )
    
    
    
  }
  
  
  if(any(plotlist == 'ineqWorldWithTransf_Theil' | allExport  )){
    
    plotdf <- plotdataIneq[['ineq']] %>%
      filter( period <= 2100,
              region == 'World',
              category %in% c('TotalWithTransfNeut','TotalWithTransfEpc'),
              variable %in% c('ineq|deltTheilL','ineq|deltTheilT')) %>%
      mutate(scenario = factor(scenario, levels = c(
        "C_SSP2-loOS-def", 
        "C_SSP2-hiOS-def" 
      )))
    
    # Automate over all scenarios
    p[['ineqWorldWithTransf_Theil']] <- list(
      
      plot = ggplot(plotdf, 
                    aes(x = period, y = value , color = variable, linetype = category)) +
        geom_line(size = 0.8) + 
        scale_fill_brewer(palette = "Set2") +
        facet_wrap(~scenario, ncol = 2,
                   labeller = as_labeller(
                     c("C_SSP2-PkBudg1000" = "2°C",
                       "C_SSP2-PkBudg650"  = "1.5°C",
                       "C_SSP2-hiOS-def" = "1.5°C HO",
                       "C_SSP2-loOS-def"  = "1.5°C LO"))
        ) +
        geom_hline(yintercept = 0, linetype = "solid", color = "grey80")+
        # Labels and styling
        labs(x = "Year", y = "Gini change from reference (points)", fill = "FE category") +
        coord_cartesian(ylim = quantile(plotdf$value, probs = c(0.01, 0.999), na.rm = TRUE)) +
        scale_color_paletteer_d("dutchmasters::view_of_Delft",
                                name = "Metric",
                                labels = c(
                                  "ineq|deltTheilL" = "TheilL",
                                  "ineq|deltTheilT" = "TheilT"
                                )) +
        scale_linetype_manual(name = "Compensation",
                              values = c("TotalWithTransfNeut" = "solid", 
                                         "TotalWithTransfEpc" = "dashed"),
                              labels = c("TotalWithTransfNeut" = "Neutral",
                                         "TotalWithTransfEpc" = "Smoothed Equal Transfer")) +
        scale_x_continuous(breaks = unique(plotdataWelf$period),
                           labels = unique(plotdataWelf$period)) 
        # theme_minimal() +
        # theme(
        #   axis.text.x = element_text(angle = 45, hjust = 1),
        #   legend.position = "bottom",
        #   legend.direction = "horizontal",
        #   panel.grid.major.y = element_line(
        #     color = "grey70",     # light grey
        #     linewidth = 0.1,      # thinner line
        #     linetype = "dashed"   # dashed style
        #   ),
        #   panel.grid.major.x = element_line(
        #     color = "grey70",     # light grey
        #     linewidth = 0.1,      # thinner line
        #     linetype = "dashed"   # dashed style
        #   ),
        #   panel.grid.minor.x = element_blank(),
        #   panel.grid.minor.y = element_blank(),
        #   panel.background = element_rect(fill = "white", color = NA)
        # )
      ,
      
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
              category == 'TotalWithTransfNeut',
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
        scale_color_paletteer_d("dutchmasters::pearl_earring") +
        scale_linetype_manual(
          values = c(
            "C_SSP2-PkBudg1000.before" = "solid",
            "C_SSP2-PkBudg1000.after" = "dashed",
            "C_SSP2-PkBudg650.before" = "solid",
            "C_SSP2-PkBudg650.after" = "dotted",
            "C_SSP2-hiOS-def.before" = "solid",
            "C_SSP2-hiOS-def.after" = "dashed",
            "C_SSP2-loOS-def.before" = "solid",
            "C_SSP2-loOS-def.after" = "dotted"
          ),
          labels = c(
            "C_SSP2-PkBudg1000.before" = "SSP_NPi2025",
            "C_SSP2-PkBudg1000.after" = "PkBudg1000",
            "C_SSP2-PkBudg650.before" = "SSP_NPi2025",
            "C_SSP2-PkBudg650.after" = "PkBudg650",
            "C_SSP2-hiOS-def.before" = "SSP_NPi2025",
            "C_SSP2-hiOS-def.after" = "SSP2-1.5°C HO",
            "C_SSP2-loOS-def.before" = "SSP_NPi2025",
            "C_SSP2-loOS-def.after" = "SSP2-1.5°C LO"
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
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              panel.grid.major.y = element_line(color = "grey70", linewidth = 0.1, linetype = "dashed"),
              panel.grid.major.x = element_line(color = "grey70", linewidth = 0.1, linetype = "dashed"),
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "white", color = NA)) +
        facet_wrap(~scenario, ncol = 2)
      ,
      
      width = 8,
      height = 3
      
    )
    
    
    
  }
  
  if(any(plotlist == 'ineqTheilLRegBySec' | allExport  )){
    
    plotdf <- plotdataIneq[['ineq']] %>%
      filter( period <= 2100,
              period >= 2015,
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
    

   
    plotdf <- plotdataIneq[['ineq']] %>%
      filter(
        between(period, 2025, 2100),
        region != "World",
        !str_starts(coalesce(category, ""), "Total"),
        variable == "ineq|deltTheilLShapley"
      ) %>%
      mutate(
        category   = factor(category, levels = allSec),
        period_fac = factor(period),                 # keep labels
        period_num = as.numeric(period_fac),         # base x positions
        scen_off   = recode(scenario,                # nudge scenarios left/right
                            "C_SSP2-PkBudg1000" = -0.22,
                            "C_SSP2-PkBudg650"  =  0.22,
                            "C_SSP2-hiOS-def" = -0.22,
                            "C_SSP2-loOS-def" = 0.22,
                            .default = 0),
        x = period_num + scen_off
      ) %>%
      mutate(scenario = factor(scenario, levels = c(
        "C_SSP2-loOS-def", 
        "C_SSP2-hiOS-def" 
      )))
    
    totals <- plotdf %>%
      group_by(region, period_fac, period_num, scen_off, scenario, x) %>%
      summarise(total = sum(value, na.rm = TRUE)+0.005, .groups = "drop") %>%
      mutate(scenario = factor(scenario, levels = c(
        "C_SSP2-loOS-def", 
        "C_SSP2-hiOS-def" 
      )))
    
    p[[paste0('ineqTheilLRegBySec')]] <- list(
    
      plot = ggplot(plotdf, aes(x = x, y = value, fill = category)) +
        geom_col(width = 0.40) +
        # add the scenario markers
        geom_point(
          data = totals,
          aes(x = x, y = total, shape = scenario),
          inherit.aes = FALSE,
          size = 1,
          colour = "black"
        ) +
        scale_shape_manual(
          name = "Scenario",
          values = c(
          "C_SSP2-PkBudg1000" = 4,   # filled circle
          "C_SSP2-PkBudg650"  = 3,    # filled triangle
          "C_SSP2-hiOS-def" = 4,
          "C_SSP2-loOS-def" = 3
        ),
        labels = c(
          "C_SSP2-PkBudg1000" = "2°C",
          "C_SSP2-PkBudg650"  = "1.5°C",
          "C_SSP2-hiOS-def" = "1.5°C HO",
          "C_SSP2-loOS-def" = "1.5°C LO"
        )
        )+
        facet_wrap(~ region, ncol = 3) +
        scale_fill_paletteer_d("dutchmasters::view_of_Delft") +
        scale_x_continuous( breaks = sort(unique(plotdf$period_num)), 
                            labels = levels(plotdf$period_fac) ) +
        labs(x = "Year",
             y = "Theil'L change from reference",
             fill = "Sectors",
             shape = "Scenario") +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom",
          legend.direction = "horizontal",
          panel.grid.major.y = element_line(color = "grey70", linewidth = 0.1, linetype = "dashed"),
          panel.grid.major.x = element_line(color = "grey70", linewidth = 0.1, linetype = "dashed"),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "white", color = NA)
        ) +
        guides(fill = guide_legend(nrow = 3), 
               shape = guide_legend(nrow = 2))
      ,

          width = 10,
          height = 8
    
      )


    
  }
  
  if(any(plotlist == 'ineqTheilTRegBySecbyScen' | allExport  )){
    
    plotdf <- plotdataIneq[['ineq']] %>%
      filter(period >= 2025, period <= 2100,
             region != "World",
             category %notin% c("Total", "Consumption"),
             variable == "ineq|deltTheilTShapley") %>%
      mutate(
        category   = factor(category, levels = allSec),
        period_fac = factor(period),                 # keep labels
        period_num = as.numeric(period_fac),         # base x positions
        scen_off   = recode(scenario,                # nudge scenarios left/right
                            "C_SSP2-PkBudg1000" = -0.22,
                            "C_SSP2-PkBudg650"  =  0.22,
                            .default = 0),
        x = period_num + scen_off
      )
    
    totals <- plotdf %>%
      group_by(region, period_fac, period_num, scen_off, scenario, x) %>%
      summarise(total = sum(value, na.rm = TRUE)+0.005, .groups = "drop")
    
    p[[paste0('ineqTheilTRegBySec')]] <- list(
      
      plot = ggplot(plotdf, aes(x = x, y = value, fill = category)) +
        geom_col(width = 0.40) +
        # add the scenario markers
        geom_point(
          data = totals,
          aes(x = x, y = total, shape = scenario),
          inherit.aes = FALSE,
          size = 1,
          colour = "black"
        ) +
        scale_shape_manual(
          name = "Scenario",
          values = c(
            "C_SSP2-PkBudg1000" = 4,   # filled circle
            "C_SSP2-PkBudg650"  = 3,    # filled triangle
            "C_SSP2-hiOS-def" = 4,
            "C_SSP2-loOS-def" = 3
            
          ),
          labels = c(
            "C_SSP2-PkBudg1000" = "2°C",
            "C_SSP2-PkBudg650"  = "1.5°C",
            "C_SSP2-hiOS-def" = "1.5°C HO",
            "C_SSP2-loOS-def" = "1.5°C LO"
          )
        )+
        facet_wrap(~ region, ncol = 3) +
        scale_fill_paletteer_d("dutchmasters::view_of_Delft") +
        scale_x_continuous( breaks = sort(unique(plotdf$period_num)), 
                            labels = levels(plotdf$period_fac) ) +
        labs(x = "Year",
             y = "Inequality Change (TheilT)",
             fill = "Sectors",
             shape = "Scenario") +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom",
          legend.direction = "horizontal",
          panel.grid.major.y = element_line(color = "grey70", linewidth = 0.1, linetype = "dashed"),
          panel.grid.major.x = element_line(color = "grey70", linewidth = 0.1, linetype = "dashed"),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "white", color = NA)
        ) +
        guides(fill = guide_legend(nrow = 3), 
               shape = guide_legend(nrow = 2))
      ,
      
      width = 10,
      height = 8
      
    )
    
    
    
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
          color = "black",
          linetype = 'dashed',
          size = 0.8
        ) +
        facet_wrap(~scenario,
                   labeller = as_labeller(
                     c("C_SSP2-PkBudg1000" = "2°C",
                       "C_SSP2-PkBudg650"  = "1.5°C",
                       "C_SSP2-hiOS-def" = "1.5°C HO",
                       "C_SSP2-loOS-def" = "1.5°C LO")
                     )) +
        labs(
          x = "Year",
          y = "Change in Inequality (Theil)",
          title = "Theil Inequality Decomposition over Time by Scenario",
          fill = "Component",
          color = "Theil"
        ) +
        scale_x_continuous(breaks = unique(plotdf$period),
                           labels = unique(plotdf$period)) +
        paletteer::scale_fill_paletteer_d(name = "Region", "NatParksPalettes::Yellowstone") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.text = element_text(size = 6),      
              legend.title = element_text(size = 8,vjust = 0.9),
              panel.grid.major.y = element_line(
                color = "grey70",     # light grey
                linewidth = 0.1,      # thinner line
                linetype = "dashed"   # dashed style
              ),
              panel.grid.major.x = element_line(
                color = "grey70",     # light grey
                linewidth = 0.1,      # thinner line
                linetype = "dashed"   # dashed style
              ),
              panel.grid.minor.x = element_blank(),
              panel.grid.minor.y = element_blank(),
              panel.background = element_rect(fill = "white", color = NA)
        )
        
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
                                "Tl.i|delta" = "Total")) %>%
      mutate(scenario = factor(scenario, levels = c(
        
        "C_SSP2-loOS-def", 
        "C_SSP2-hiOS-def" 
      )))
    
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
        # geom_line(
        #   data = filter(plotdf, component == "Total"),
        #   color = "black",
        #   linetype = 'dashed',
        #   aes(color = component),
        #   size = 0.8
        # ) +
        facet_wrap(~scenario, ncol = 2,
                   labeller = as_labeller(
                     c("C_SSP2-PkBudg1000" = "2°C",
                       "C_SSP2-PkBudg650"  = "1.5°C",
                       "C_SSP2-hiOS-def" = "1.5°C HO",
                       "C_SSP2-loOS-def" = "1.5°C LO")
                   )
        ) +
        labs(
          x = "Year",
          y = "Theil'L change from reference",
          #title = "Theil Inequality Decomposition over Time by Scenario",
          fill = "Component",
          color = "Theil"
        ) +
        scale_x_continuous(breaks = unique(plotdf$period),
                           labels = unique(plotdf$period)) +
        paletteer::scale_fill_paletteer_d(name = "Contribution", "NatParksPalettes::Yellowstone",
                                          labels = c('Between' = 'Between-country',
                                                     'Within' = 'Within-country')) 
      
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
            alpha = 0.5
          ) +
          geom_line(
            data = filter(plotdf, region == "total"),
            aes(color = "Total"),
            size = 1,
            linetype = 2
          ) +
          facet_wrap(~scenario, ncol = 2,
                     labeller = as_labeller(
                       c("C_SSP2-PkBudg1000" = "2°C",
                         "C_SSP2-PkBudg650"  = "1.5°C",
                         "C_SSP2-hiOS-def" = "1.5°C HO",
                         "C_SSP2-loOS-def" = "1.5°C LO")
                     )
          ) +
          labs(
            x = "Year",
            y = "Change in Inequality (Theil)",
            title = "Theil Inequality Decomposition over Time by Scenario",
            fill = "Component",
            color = "Theil"
          ) +
          scale_x_continuous(breaks = unique(plotdf$period),
                             labels = unique(plotdf$period)) +
          scale_fill_paletteer_d("PrettyCols::Summer")+
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
        bind_rows(plotdf) %>%
        mutate(scenario = factor(scenario, levels = c(
          "C_SSP2-loOS-def", 
          "C_SSP2-hiOS-def" 
        )))
      
      p[['ineqGlobalWithinRegTheilL']] <- list(
        
        plot = ggplot(plotdf, aes(x = period, y = within, group = region)) +
          # Area plots for Within and Between
          geom_area(
            data = filter(plotdf, region != 'total'),
            aes(fill = region),
            position = "stack",
            alpha = 0.5
          ) +
          facet_wrap(~scenario, ncol = 2,
                     labeller = as_labeller(
                       c("C_SSP2-PkBudg1000" = "2°C",
                         "C_SSP2-PkBudg650"  = "1.5°C",
                         "C_SSP2-hiOS-def" = "1.5°C HO",
                         "C_SSP2-loOS-def" = "1.5°C LO")
                     )) +
          labs(
            x = "Year",
            y = "Theil'L change from reference",
            #title = "Theil Inequality Decomposition over Time by Scenario",
            fill = "Component",
            color = "Theil"
          ) +
          scale_x_continuous(breaks = unique(plotdf$period),
                             labels = unique(plotdf$period)) +
          scale_fill_paletteer_d("PrettyCols::Summer")+
          guides(
            fill = guide_legend( nrow = 2, byrow = TRUE,
                                 title.position = "left",  # keep title on the side
                                 title.hjust = 1,           # left-align text within its box
                                 label.hjust = 0)
            # color = guide_legend( nrow = 2, byrow = TRUE,
            #                      title.position = "left",  # keep title on the side
            #                      title.hjust = 1,           # left-align text within its box
            #                      label.hjust = 0)
          ) +
          theme(strip.text = element_blank())
        
        
        ,
        
        width = 8,
        height = 5
        
      )
    

    
    
  }
  
  #-------------End 5.4 regional inequality effect by Channel (Theil)-----------
  
  
  
  #---------------------------6 Global tax revenue ------------------
  if(any(plotlist == 'taxRevenueWorld' | allExport  )){
    
    
    plotdf <- bind_rows(
      # World total from MAgPIE (constructed)
      data3 %>%
        filter(str_starts(variable, "Taxes"),
               model == "MAgPIE") %>%
        mutate(variable = str_remove(variable, "^Taxes\\|?")) %>%
        group_by(scenario, variable, period, unit, baseline) %>%  # no need to group by model after filtering
        summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
        mutate(region = "World", model = "MAgPIE"),
      # World rows already present in data (any model)
      data3 %>%
        filter(str_starts(variable, "Taxes"),
               region == "World") %>%
        mutate(variable = str_remove(variable, "^Taxes\\|?"))
    ) %>%
      # keep only the two layers we want to stack
      filter(variable %in% c("GHG|REMIND", "GHG|MAGPIE")) %>%
      # control stacking order: bottom -> top
      mutate(
        variable = factor(variable, levels = c("GHG|REMIND", "GHG|MAGPIE")),
        # make sure x is ordered for area stacking
        period = as.integer(period)
      ) %>%
      filter(period <= 2100) %>%
      mutate(scenario = factor(scenario, levels = c(
        "C_SSP2-loOS-def",
        "C_SSP2-hiOS-def"
      )))
    

    sumdf <- plotdf %>%
      group_by(scenario, period) %>%
      summarise(total = sum(value, na.rm = TRUE), .groups = "drop") 
    
      p[[paste0("taxRevenueWorld")]] <- list(
        plot =
          ggplot(plotdf, aes(x = period, y = value, fill = variable)) +
          geom_area(alpha = 0.7, color = NA, position = "stack") +
          geom_line(
            data = sumdf,
            aes(x = period, y = total, color = "Total"),
            inherit.aes = FALSE,
            linewidth = 0.8,
            linetype = "solid"
          ) +
          facet_wrap(
            ~ scenario, ncol = 2,
            labeller = as_labeller(c(
              "C_SSP2-PkBudg1000" = "2°C",
              "C_SSP2-PkBudg650"  = "1.5°C",
              "C_SSP2-hiOS-def"   = "1.5°C HO",
              "C_SSP2-loOS-def"   = "1.5°C LO"
            ))
          ) +
          scale_fill_paletteer_d(
            "dutchmasters::view_of_Delft",
            name   = "Source",
            labels = c("Energy system", "Land sector")
          ) +
          scale_color_manual(
            name   = "",
            values = c("Total" = "#446482"),    
            labels = c("Total" = "Total net revenue")
          ) +
          guides(
            fill  = guide_legend(order = 1),
            color = guide_legend(order = 2, override.aes = list(linetype = "solid", linewidth = 1.2))
          ) +
          scale_x_continuous(breaks = unique(plotdataWelf$period),
                             labels = unique(plotdataWelf$period)) +
          labs(x = "Year", y = "Carbon revenue (billion$)") 
          # theme_minimal() +
          # theme(
          #   legend.position = "bottom",
          #   strip.text = element_blank(),
          #   axis.text.x = element_text(angle = 45, hjust = 1),
          #   panel.grid.major.y = element_line(color = "grey70", linewidth = 0.1, linetype = "dashed"),
          #   panel.grid.major.x = element_line(color = "grey70", linewidth = 0.1, linetype = "dashed"),
          #   panel.grid.minor.x = element_blank(),
          #   panel.grid.minor.y = element_blank(),
          #   panel.background   = element_rect(fill = "white", color = NA)
          # )
        ,
        
        width  = 8,
        height = 3
      )
      
      
    
    
    
  }
  

  
  
  
  #-------------End 6 Global tax revenue-------------
    
  
  #--------------------------7.categorization of sector driver-----------------
  
  if(any(plotlist == 'theilSectorDriver' | allExport  )){
  
    plotdf <- plotdataIneq$ineq %>% filter(variable %in% c("ineq|deltTheilLShapley","ineq|deltTheilTShapley","ineq|TheilT", "ineq|TheilL"),
                                   category %notin% c("Total","TotalWithTransfNeut","TotalWithTransfEpc",
                                                      'Consumption','Other commodities'),
                                   region != 'World') %>%
      pivot_wider(names_from = variable, values_from = value) %>%
      mutate(pChangeTheilL =  `ineq|deltTheilLShapley`/`ineq|TheilL` * 100,
             pChangeTheilT =  `ineq|deltTheilTShapley`/`ineq|TheilT` * 100) %>%
      filter(period >= 2040)
    
    lims <- plotdf %>%
      group_by(category) %>%
      summarise(L = max(abs(c(`pChangeTheilL`,
                              `pChangeTheilT`)), na.rm = TRUE),
                .groups = "drop") %>%
      mutate(xmin = 0, xmax = L, ymin = 0, ymax = L)
    
    regions <- levels(factor(plotdf$region))
    
    shape_vals <- c(21,22,23,24,25,0,1,2,3,4,5,6)  # 12 distinct shapes
    names(shape_vals) <- regions[seq_along(shape_vals)]
    
    p[["theilSectorDriver"]] <- list(
      plot = ggplot(plotdf,
                    aes(x = `pChangeTheilL`,
                        y = `pChangeTheilT`,
                        color = region,
                        shape = region)) +
        geom_point(size = 1.2, alpha = 0.8) +
        geom_blank(data = lims, aes(x = xmin, y = ymin), inherit.aes = FALSE) +
        geom_blank(data = lims, aes(x = xmax, y = ymax), inherit.aes = FALSE) +
        geom_abline(slope = 1, intercept = 0, linetype = 2) +
        facet_wrap(~ category, ncol = 3, scales = "free") +
        theme_minimal() +
        labs(x = "% change, Theil's L", y = "% change, Theil's T") +
        # Put the COLOR scale FIRST and give it the guide with 2 rows
        paletteer::scale_color_paletteer_d(
          name = "Region", palette = "PrettyCols::Summer",
          guide = guide_legend(nrow = 2, byrow = TRUE,
                               title.position = "left",
                               title.hjust = 0, label.hjust = 0,
                               override.aes = list(size = 3, alpha = 1))
        ) +
        # Shape scale uses the same name to merge, but no extra override (avoid warnings)
        scale_shape_manual(name = "Region", values = shape_vals, guide = "legend") +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom",
          legend.box = "vertical",
          legend.title = element_text(hjust = 0),
          strip.text = element_text(face = "bold"),
          panel.grid.major.y = element_line(color = "grey70", linewidth = 0.1, linetype = "dashed"),
          panel.grid.major.x = element_line(color = "grey70", linewidth = 0.1, linetype = "dashed"),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "white", color = NA)
        )
      
      ,
      
      width  = 10,
      height = 8
      
    )
    
    
    
    
    
    
    
    }
  
  
  
  #----------------------End 7. categorization of sector drivers---------------
  
  
  
  #--------------------------8. burden on deciles -----------------
  
  if(any(plotlist == 'secBurdenByDecile' | allExport  )){
    
    plotdf <- aggregate_decileWelfChange(data1 = plotdataWelf, data2 = data2, 
                                         level = c("fullSec"), region = 'globalDecile') %>%
      filter(period %in% c('2030','2050','2100'))
    
    iter = 1
    
    scenario_labels <- c(
      "C_SSP2-PkBudg1000" = "SSP2-2°C",
      "C_SSP2-PkBudg650"  = "SSP2-1.5°C",
      "C_SSP2-hiOS-def"   = "SSP2-1.5°C HO",
      "C_SSP2-loOS-def"   = "SSP2-1.5°C LO"
    )
    
    for(scen in plotdf$scenario %>% unique ) {
      
      plot_base <- ggplot(plotdf %>% filter(scenario == scen), 
                          aes(x = as.factor(decileGroup), y = welfChange, fill = category)) +
        geom_col(position = "stack") +
        geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
        facet_wrap(~ period, ncol = 3) +
        labs(
          title = scen,
          x = "Income decile group",
          y = "Real Consumption Change (%)",
          fill = "Category"
        ) +
        #scale_fill_paletteer_d("ggprism::floral2") +
        #scale_fill_paletteer_d("beyonce::X82") +
        scale_fill_paletteer_d("dutchmasters::view_of_Delft") +
        guides(fill = guide_legend(reverse = TRUE))
      
      # conditionally modify plot
      if (iter == length(plotdf$scenario %>% unique) ) {
        plot_base <- plot_base + 
          theme(legend.position = "none")
      } else if (iter != length(plotdf$scenario %>% unique)){
        plot_base <- plot_base + theme( axis.title.x = element_blank() )
      }
      
      p[[paste0("secBurdenByDecile",scen)]] <- list(
        plot = plot_base
        ,
        
        width  = 8,
        height = 5
        
      )
      
      iter = iter + 1
    }


    
    
    
    
    
    
    
  }
  
  
  
  #----------------------End 8. burden on deciles---------------
  
  
  
  
  if(isDisplay){
    print(p)
  }
  
  
  
  if(isExport|allExport){
    
    dir.create(paste0(outputPath,'/', micro_model, capitalize_first(fixed_point)), recursive = TRUE, showWarnings = FALSE)
    
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
  
  
  return(p)


}
