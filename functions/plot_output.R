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
  foodSec <- c("Empty calories","Animal products","Fruits vegetables nuts","Staples")
  eneSec <- c("Building electricity", "Building gases", "Building other fuels","Transport energy")
  allSec <- c(foodSec,eneSec, 'Consumption')
  
  scenario_labels <- c(
    "C_SSP2-PkBudg1000" = "SSP2-2°C",
    "C_SSP2-PkBudg650"  = "SSP2-1.5°C",
    "C_SSP2-hiOS-def"   = "SSP2-1.5°C HO",
    "C_SSP2-loOS-def"   = "SSP2-1.5°C LO"
  )
  
  mySecPalette <- rev(c(
    "#053138FF", "#9FDFED", "#0B8CA9FF", "#AEC7BEFF",
    "#FAF3CEFF", "#CFE690FF", "#F2AB70FF", "#FEC5A0FF"
  ))
  
  myScenPalette <- c(
    "C_SSP2-PkBudg1000" = "#e8ab67",  # 2°C
    "C_SSP2-PkBudg650"  = "#779ec6",  # 1.5°C
    "C_SSP2-hiOS-def"   = "#e8ab67",  # 1.5°C HO (same as 1000)
    "C_SSP2-loOS-def"   = "#779ec6"   # 1.5°C LO (same as 650)
  )
  
  p=list()

#-------------------------1.Plots all aggregated welfare change------------------------
  
  if(any(plotlist == "welfByPeriod") | allExport){
    #todo: is using weighted average better?
    
    dataDecile <- plotdataWelf %>%
      filter(period <= 2100 ) %>%
      group_by(scenario, period, region, decileGroup) %>%
      summarise(sumWelfChange = sum(decilWelfChange, na.rm = TRUE), .groups = "drop")%>%
      mutate(scenario = factor(scenario, levels = c(
        "C_SSP2-PkBudg1000",
        "C_SSP2-PkBudg650",
        "C_SSP2-loOS-def", 
        "C_SSP2-hiOS-def" 
      )))

    avg_df_period <- dataDecile %>%
      group_by(scenario, period) %>%
      summarise(meanWelfChange = mean(sumWelfChange, na.rm = TRUE), .groups = "drop") %>%
      mutate(scenario = factor(scenario, levels = c(
        "C_SSP2-PkBudg1000",
        "C_SSP2-PkBudg650",
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
        "C_SSP2-PkBudg1000",
        "C_SSP2-PkBudg650",
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
        scale_fill_manual(values = mySecPalette) +
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
      filter(category != 'Other commodities') %>%
      mutate(category = factor(category, levels = allSec))

    
    
    # Automate over all scenarios
    p[['globalWelfBySec']] <- list(
      plot = ggplot(plotdf, aes(x = factor(period), y = welfChange, fill = category)) +
        geom_col(position = "stack") +
        geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
        facet_wrap(~ scenario,
                   labeller = as_labeller(
                     c("C_SSP2-PkBudg1000" = "2°C",
                       "C_SSP2-PkBudg650"  = "1.5°C",
                       "C_SSP2-hiOS-def" = "1.5°C HO",
                       "C_SSP2-loOS-def"  = "1.5°C LO")
                   )) +
        labs(
          x = "Year",
          y = "Real Consumption Change (%)",
          fill = "Category"
        ) +
        theme_minimal() +
        #scale_fill_paletteer_d("ggprism::floral2") +
        #scale_fill_paletteer_d("beyonce::X82") +
        scale_fill_manual(values = mySecPalette) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))+
        guides(fill = guide_legend(reverse = TRUE))
      
      ,
      
      width = 8,
      height = 3
      
    )
  }

  if(any(plotlist=='regionalWelfBySec')| allExport ){
    
    plotdf <- aggregate_decileWelfChange(data1 = plotdataWelf, data2 = data2, 
                                         level = c("fullSec"), region = 'region') %>%
      filter(category != 'Other commodities') %>%
      mutate(category = factor(category, levels = allSec))
    

    
    # Automate over all scenarios
    p[['regionalWelfBySec']] <- list(
      plot = ggplot(plotdf, aes(x = factor(period), y = welfChange, fill = category)) +
        geom_col(position = "stack") +
        geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
        facet_wrap(~ region + scenario, ncol = 4,
                   scales = "free_y") +
        labs(
          x = "Year",
          y = "Real Consumption Change (%)",
          fill = "Category"
        ) +
        theme_minimal() +
        scale_fill_manual(values = mySecPalette) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        guides(fill = guide_legend(reverse = TRUE))
      
      ,
      
      width = 8,
      height = 8
      
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
        scale_fill_manual(values = mySecPalette) +
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
          scale_fill_manual(values = mySecPalette) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)),
        
        width = 10,
        height = 8
        
      ) 
      
  }
    
}
      
#----------End 4.1 Plot by individual channel:regional (welfare change)---------
   

  
  
  
  
#----------------------5.1 global inequality effect (Gini/Theil)----------------
  
  
  if(any(plotlist == 'ineqWorld_Gini' | allExport  )){
    
    plotdf <- plotdataIneq[['ineq']] %>%
      filter(
        period <= 2100,
        region == "World",
        category %in% c("TotalWithTransfNeut", "Reference", "TotalWithTransfEpc"),
        variable %in% c("ineq|Gini")
      )
    
    plotdf_ref <- plotdf %>%
      filter(scenario == "C_SSP2-NPi2025")
    
    # build all scenario names like "C_SSP2-hiOS-def-PkBudg650" etc.
    scen_names <- paste0("C_", all_runscens, "-", all_budgets)
    
    plotdf <- plotdf %>%
      filter(scenario != "C_SSP2-NPi2025") %>%
      bind_rows(
        map_dfr(
          scen_names,
          ~ plotdf_ref %>% mutate(scenario = .x)
        )
      )


    
  
    # Automate over all scenarios
    p[['ineqWorld_Gini']] <- list(
      
      plot = ggplot(plotdf, 
                    aes(x = period, y = value, linetype = category, color = category)) +
        geom_line() + 
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
        # unified legend for both color + linetype (same variable, same breaks, same labels)
        
        scale_color_paletteer_d(
          "nbapalettes::warriors_city",
          name   = "Recycling scheme",
          breaks = c("Reference", "TotalWithTransfNeut", "TotalWithTransfEpc"),
          labels = c(
            "Reference"           = "Reference",
            "TotalWithTransfNeut" = "Neutral",
            "TotalWithTransfEpc"  = "EPC"
          )
        ) +
        scale_linetype_manual(
          name   = "Recycling scheme",  # same name as color
          breaks = c("Reference", "TotalWithTransfNeut", "TotalWithTransfEpc"),
          values = c(
            "Reference"           = "solid",
            "TotalWithTransfNeut" = "dotdash",
            "TotalWithTransfEpc"  = "dotted"
          ),
          labels = c(
            "Reference"           = "Reference",
            "TotalWithTransfNeut" = "Neutral",
            "TotalWithTransfEpc"  = "EPC"
          )
        ) +
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
  
  
  
  if(any(plotlist == 'ineqWorldWithTransf_GiniRela' | allExport  )){
    
    plotdf <- plotdataIneq[['ineq']] %>%
      filter( period <= 2100 ,
              region == 'World',
              category %in% c('TotalWithTransfNeut','Reference','TotalWithTransfEpc'),
              variable %in% c('ineq|Gini'))
    
    plotdf_ref <- plotdf %>%
      filter(scenario == "C_SSP2-NPi2025") %>%
      select(-scenario,-category) %>%
      rename(ref = value)
    
    plotdf <- plotdf %>% filter(scenario != 'C_SSP2-NPi2025') %>%
      left_join(plotdf_ref, by = c('region', 'period', 'variable')) %>%
      mutate(value = (value - ref) * 100,
             variable ='ineq|deltaGini') %>%
      select(-ref)  %>%
      mutate(scenario = factor(scenario, levels = c(
        "C_SSP2-PkBudg1000",
        "C_SSP2-PkBudg650",
        "C_SSP2-loOS-def", 
        "C_SSP2-hiOS-def" 
      )))

    
    
    # Automate over all scenarios
    p[['ineqWorldWithTransf_GiniRela']] <- list(
      
      plot = ggplot(plotdf, 
                    aes(x = period, y = value, linetype = category, color = scenario)) +
        geom_line(size = 0.8) + 
        scale_color_manual(
          name   = "Scenario",  # legend title
          values = myScenPalette,
          breaks = c("C_SSP2-PkBudg1000", "C_SSP2-PkBudg650", 
                     "C_SSP2-hiOS-def", "C_SSP2-loOS-def"),
          labels = c(
            "C_SSP2-PkBudg1000" = "2°C",
            "C_SSP2-PkBudg650"  = "1.5°C",
            "C_SSP2-hiOS-def"   = "1.5°C HO",
            "C_SSP2-loOS-def"   = "1.5°C LO"
          )
        ) +
        scale_linetype_manual(name = "Compensation",
                                        values = c("TotalWithTransfNeut" = "solid", 
                                                   "TotalWithTransfEpc" = "dashed"),
                                        labels = c("TotalWithTransfNeut" = "Neutral",
                                                   "TotalWithTransfEpc" = "EPC")) +
        scale_x_continuous(breaks = unique(plotdataWelf$period),
                           labels = unique(plotdataWelf$period)) +
        geom_hline(yintercept = 0, linetype = "solid", color = "grey80")+
        # Labels and styling
        labs(x = "Year", y = "Gini change from reference (points)", fill = "FE category") +
        #coord_cartesian(ylim = quantile(plotdf$value, probs = c(0.01, 0.999), na.rm = TRUE)) +
        ylim(-1,2.2) + 

        theme(axis.title.x = element_blank())
      ,
      
      width = 5,
      height = 4
      
    )
    
    
    
  }

  
  if(any(plotlist == 'ineqWorldWithTransf_TheilLRela' | allExport  )){
    
    plotdf <- plotdataIneq[['ineq']] %>%
      filter( period <= 2100 ,
              region == 'World',
              category %in% c('TotalWithTransfNeut','Reference','TotalWithTransfEpc'),
              variable %in% c('ineq|TheilL'))
    
    plotdf_ref <- plotdf %>%
      filter(scenario == "C_SSP2-NPi2025") %>%
      select(-scenario,-category) %>%
      rename(ref = value)
    
    plotdf <- plotdf %>% filter(scenario != 'C_SSP2-NPi2025') %>%
      left_join(plotdf_ref, by = c('region', 'period', 'variable')) %>%
      mutate(value = (value - ref),
             variable ='ineq|deltaTheilL') %>%
      select(-ref)  %>%
      mutate(scenario = factor(scenario, levels = c(
        "C_SSP2-PkBudg1000",
        "C_SSP2-PkBudg650",
        "C_SSP2-loOS-def", 
        "C_SSP2-hiOS-def" 
      )))
    
    
    
    # Automate over all scenarios
    p[['ineqWorldWithTransf_TheilLRela']] <- list(
      
      plot = ggplot(plotdf, 
                    aes(x = period, y = value , linetype = category, color = scenario)) +
        geom_line(size = 0.8) + 
        scale_color_manual(
          name   = "Scenario",  # legend title
          values = myScenPalette,
          breaks = c("C_SSP2-PkBudg1000", "C_SSP2-PkBudg650", 
                     "C_SSP2-hiOS-def", "C_SSP2-loOS-def"),
          labels = c(
            "C_SSP2-PkBudg1000" = "2°C",
            "C_SSP2-PkBudg650"  = "1.5°C",
            "C_SSP2-hiOS-def"   = "1.5°C HO",
            "C_SSP2-loOS-def"   = "1.5°C LO"
          )
        )+
        geom_hline(yintercept = 0, linetype = "solid", color = "grey80")+
        # Labels and styling
        labs(x = "Year", y = "Theil'L change from reference", fill = "FE category") +
        coord_cartesian(ylim = quantile(plotdf$value, probs = c(0.01, 0.999), na.rm = TRUE)) +
        scale_linetype_manual(name = "Compensation",
                              values = c("TotalWithTransfNeut" = "solid", 
                                         "TotalWithTransfEpc" = "dashed"),
                              labels = c("TotalWithTransfNeut" = "Neutral",
                                         "TotalWithTransfEpc" = "EPC")) +
        scale_x_continuous(breaks = unique(plotdataWelf$period),
                           labels = unique(plotdataWelf$period)) +
        theme(axis.title.x = element_blank())
      ,
      
      width = 5,
      height = 4
      
    )
    
    
    
  }
  
  if(any(plotlist == 'ineqWorldWithTransf_TheilTRela' | allExport  )){
    
    plotdf <- plotdataIneq[['ineq']] %>%
      filter( period <= 2100 ,
              region == 'World',
              category %in% c('TotalWithTransfNeut','Reference','TotalWithTransfEpc'),
              variable %in% c('ineq|TheilT'))
    
    plotdf_ref <- plotdf %>%
      filter(scenario == "C_SSP2-NPi2025") %>%
      select(-scenario,-category) %>%
      rename(ref = value)
    
    plotdf <- plotdf %>% filter(scenario != 'C_SSP2-NPi2025') %>%
      left_join(plotdf_ref, by = c('region', 'period', 'variable')) %>%
      mutate(value = (value - ref),
             variable ='ineq|deltaTheilT') %>%
      select(-ref)  %>%
      mutate(scenario = factor(scenario, levels = c(
        "C_SSP2-PkBudg1000",
        "C_SSP2-PkBudg650",
        "C_SSP2-loOS-def", 
        "C_SSP2-hiOS-def" 
      )))
    
    
    
    # Automate over all scenarios
    p[['ineqWorldWithTransf_TheilTRela']] <- list(
      
      plot = ggplot(plotdf, 
                    aes(x = period, y = value , linetype = category, color = scenario)) +
        geom_line(size = 0.8) + 
        scale_color_manual(
          name   = "Scenario",  # legend title
          values = myScenPalette,
          breaks = c("C_SSP2-PkBudg1000", "C_SSP2-PkBudg650", 
                     "C_SSP2-hiOS-def", "C_SSP2-loOS-def"),
          labels = c(
            "C_SSP2-PkBudg1000" = "2°C",
            "C_SSP2-PkBudg650"  = "1.5°C",
            "C_SSP2-hiOS-def"   = "1.5°C HO",
            "C_SSP2-loOS-def"   = "1.5°C LO"
          )
        )+
        geom_hline(yintercept = 0, linetype = "solid", color = "grey80")+
        # Labels and styling
        labs(x = "Year", y = "Theil'T change from reference", fill = "FE category") +
        coord_cartesian(ylim = quantile(plotdf$value, probs = c(0.01, 0.999), na.rm = TRUE)) +
        scale_linetype_manual(name = "Compensation",
                              values = c("TotalWithTransfNeut" = "solid", 
                                         "TotalWithTransfEpc" = "dashed"),
                              labels = c("TotalWithTransfNeut" = "Neutral",
                                         "TotalWithTransfEpc" = "EPC")) +
        scale_x_continuous(breaks = unique(plotdataWelf$period),
                           labels = unique(plotdataWelf$period)) +
        theme(axis.title.x = element_blank())
      ,
      
      width = 5,
      height = 4
      
    )
    
    
    
  }
  
#-----------------End 5.1 global inequality effect (Gini/Theil)----------------

  
  
  
  
  
  
  
#----------------------5.2 regional inequality effect----------------
  
  
  if(any(plotlist == 'ineqReg_Gini' | allExport  )){
    
    plotdf <- plotdataIneq[['ineq']] %>%
      filter( period <= 2100 ,
              region != 'World',
              category %in% c('TotalWithTransfNeut','Reference','TotalWithTransfEpc'),
              variable %in% c('ineq|Gini'))
    
    plotdf_ref <- plotdf %>%
      filter(scenario == "C_SSP2-NPi2025")
    
    scen_names <- paste0("C_", all_runscens, "-", all_budgets)
    
    plotdf <- plotdf %>%
      filter(scenario != "C_SSP2-NPi2025") %>%
      bind_rows(
        map_dfr(
          scen_names,
          ~ plotdf_ref %>% mutate(scenario = .x)
        )
      )
    
    
    
    # Automate over all scenarios
    p[['ineqReg_Gini']] <- list(
      
      plot = ggplot(plotdf, 
                    aes(x = period, y = value, 
                        color = scenario, 
                        linetype = category )) +
        geom_line() + 
        facet_wrap(~region, ncol = 3,
                   scales = "free_y") +
        labs(x = "Decile", y = "Inequality metrics", 
             color = "Measure", linetype = "Scenario-State") +
        #coord_cartesian(ylim = quantile(plotdf$value, probs = c(0.01, 0.99), na.rm = TRUE)) +
        scale_color_manual(
          name   = "Scenario",  # legend title
          values = myScenPalette,
          breaks = c("C_SSP2-PkBudg1000", "C_SSP2-PkBudg650", 
                     "C_SSP2-hiOS-def", "C_SSP2-loOS-def"),
          labels = c(
            "C_SSP2-PkBudg1000" = "2°C",
            "C_SSP2-PkBudg650"  = "1.5°C",
            "C_SSP2-hiOS-def"   = "1.5°C HO",
            "C_SSP2-loOS-def"   = "1.5°C LO"
          )
        ) +
        scale_linetype_manual(name = "Compensation",
                              values = c("Reference" = "solid",
                                         "TotalWithTransfNeut" = "dotdash", 
                                         "TotalWithTransfEpc" = "dotted"),
                              labels = c("Reference" = "Reference",
                                         "TotalWithTransfNeut" = "Neutral",
                                         "TotalWithTransfEpc" = "EPC")) +
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
  
  
  
  if(any(plotlist == 'ineqReg_GiniRela' | allExport  )){
    
    plotdf <- plotdataIneq[['ineq']] %>%
      filter( period <= 2100 ,
              region != 'World',
              category %in% c('TotalWithTransfNeut','Reference','TotalWithTransfEpc'),
              variable %in% c('ineq|Gini'))
    
    plotdf_ref <- plotdf %>%
      filter(scenario == "C_SSP2-NPi2025") %>%
      select(-scenario,-category) %>%
      rename(ref = value)
    
    plotdf <- plotdf %>% filter(scenario != 'C_SSP2-NPi2025') %>%
      left_join(plotdf_ref, by = c('region', 'period', 'variable' )) %>%
      mutate(value = (value - ref) * 100,
             variable ='ineq|deltaGini') %>%
      select(-ref)  %>%
      mutate(scenario = factor(scenario, levels = c(
        "C_SSP2-PkBudg1000", 
        "C_SSP2-PkBudg650",
        "C_SSP2-loOS-def", 
        "C_SSP2-hiOS-def" 
      )))
    
    
    # Automate over all scenarios
    p[['ineqReg_GiniRela']] <- list(
      
      plot = ggplot(plotdf, 
                    aes(x = period, y = value, 
                        linetype = category, color = scenario)) +
        geom_line() + 
        geom_hline(yintercept = 0)+
        facet_wrap(~region, ncol = 3,
                   scales = "free_y" ) +
        labs(x = "Decile", y = "Gini Change from reference (points)", 
             color = "Measure", linetype = "Scenario-State") +
        #coord_cartesian(ylim = quantile(plotdf$value, probs = c(0.01, 0.99), na.rm = TRUE)) +
        scale_color_manual(
          name   = "Scenario",  # legend title
          values = myScenPalette,
          breaks = c("C_SSP2-PkBudg1000", "C_SSP2-PkBudg650", 
                     "C_SSP2-hiOS-def", "C_SSP2-loOS-def"),
          labels = c(
            "C_SSP2-PkBudg1000" = "2°C",
            "C_SSP2-PkBudg650"  = "1.5°C",
            "C_SSP2-hiOS-def"   = "1.5°C HO",
            "C_SSP2-loOS-def"   = "1.5°C LO"
          )
        ) +
        scale_linetype_manual(name = "Compensation",
                              values = c("TotalWithTransfNeut" = "solid", 
                                         "TotalWithTransfEpc" = "dashed"),
                              labels = c("TotalWithTransfNeut" = "Neutral",
                                         "TotalWithTransfEpc" = "EPC"))
      ,
      
      width = 10,
      height = 8
      
    )
    
    
    
  }
  
  if(any(plotlist == 'ineqReg_TheilLRela' | allExport  )){
    
    plotdf <- plotdataIneq[['ineq']] %>%
      filter( period <= 2100 ,
              region != 'World',
              category %in% c('TotalWithTransfNeut','Reference','TotalWithTransfEpc'),
              variable %in% c('ineq|TheilL'))
    
    plotdf_ref <- plotdf %>%
      filter(scenario == "C_SSP2-NPi2025") %>%
      select(-scenario,-category) %>%
      rename(ref = value)
    
    plotdf <- plotdf %>% filter(scenario != 'C_SSP2-NPi2025') %>%
      left_join(plotdf_ref, by = c('region', 'period', 'variable' )) %>%
      mutate(value = (value - ref) * 100,
             variable ='ineq|deltaTheilL') %>%
      select(-ref)  %>%
      mutate(scenario = factor(scenario, levels = c(
        "C_SSP2-PkBudg1000", 
        "C_SSP2-PkBudg650",
        "C_SSP2-loOS-def", 
        "C_SSP2-hiOS-def" 
      )))
    
    
    # Automate over all scenarios
    p[['ineqReg_TheilLRela']] <- list(
      
      plot = ggplot(plotdf, 
                    aes(x = period, y = value, 
                        linetype = category, color = scenario)) +
        geom_line() + 
        geom_hline(yintercept = 0)+
        facet_wrap(~region, ncol = 3,
                   scales = "free_y" ) +
        labs(x = "Decile", y = "Theil'L Change from reference (points)", 
             color = "Measure", linetype = "Scenario-State") +
        #coord_cartesian(ylim = quantile(plotdf$value, probs = c(0.01, 0.99), na.rm = TRUE)) +
        scale_color_manual(
          name   = "Scenario",  # legend title
          values = myScenPalette,
          breaks = c("C_SSP2-PkBudg1000", "C_SSP2-PkBudg650", 
                     "C_SSP2-hiOS-def", "C_SSP2-loOS-def"),
          labels = c(
            "C_SSP2-PkBudg1000" = "2°C",
            "C_SSP2-PkBudg650"  = "1.5°C",
            "C_SSP2-hiOS-def"   = "1.5°C HO",
            "C_SSP2-loOS-def"   = "1.5°C LO"
          )
        ) +
        scale_linetype_manual(name = "Compensation",
                              values = c("TotalWithTransfNeut" = "solid", 
                                         "TotalWithTransfEpc" = "dashed"),
                              labels = c("TotalWithTransfNeut" = "Neutral",
                                         "TotalWithTransfEpc" = "EPC"))
      ,
      
      width = 10,
      height = 8
      
    )
    
    
    
  }
  
#---------------------------------End------------------------------------------
  
  
  

  
  
  
  #-------------5.3 regional inequality effect by Channel ----------------
  
  if(any(plotlist == 'ineqRegBySec_Gini' | allExport  )){
    
    
    plotdf <- plotdataIneq[['ineq']] %>%
      filter(
        between(period, 2025, 2100),
        region != "World",
        !str_starts(coalesce(category, ""), "Total"),
        category != 'Other commodities',
        variable == "ineq|shapleyGini"
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
        "C_SSP2-PkBudg1000", 
        "C_SSP2-PkBudg650",
        "C_SSP2-loOS-def", 
        "C_SSP2-hiOS-def" 
      ))) 
   
    
    totals <- plotdf %>%
      group_by(region, period_fac, period_num, scen_off, scenario, x) %>%
      summarise(total = sum(value, na.rm = TRUE)+0.005, .groups = "drop") %>%
      mutate(scenario = factor(scenario, levels = c(
        "C_SSP2-PkBudg1000", 
        "C_SSP2-PkBudg650",
        "C_SSP2-loOS-def", 
        "C_SSP2-hiOS-def" 
      ))) 
    
    p[[paste0('ineqRegBySec_Gini')]] <- list(
    
      plot = ggplot(plotdf, aes(x = x, y = value * 100, fill = category)) +
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
        scale_fill_manual(values = mySecPalette) +
        scale_x_continuous( breaks = sort(unique(plotdf$period_num)), 
                            labels = levels(plotdf$period_fac) ) +
        labs(x = "Year",
             y = "Gini change from reference (points)",
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
  
  if(any(plotlist == 'ineqRegBySec_TheilT' | allExport  )){
  
  
  plotdf <- plotdataIneq[['ineq']] %>%
    filter(
      between(period, 2025, 2100),
      region != "World",
      !str_starts(coalesce(category, ""), "Total"),
      category != 'Other commodities',
      variable == "ineq|shapleyTheilT"
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
      "C_SSP2-PkBudg1000", 
      "C_SSP2-PkBudg650",
      "C_SSP2-loOS-def", 
      "C_SSP2-hiOS-def" 
    ))) 
  
  
  totals <- plotdf %>%
    group_by(region, period_fac, period_num, scen_off, scenario, x) %>%
    summarise(total = sum(value, na.rm = TRUE)+0.005, .groups = "drop") %>%
    mutate(scenario = factor(scenario, levels = c(
      "C_SSP2-PkBudg1000", 
      "C_SSP2-PkBudg650",
      "C_SSP2-loOS-def", 
      "C_SSP2-hiOS-def" 
    ))) 
  
  p[[paste0('ineqRegBySec_TheilT')]] <- list(
    
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
      scale_fill_manual(values = mySecPalette) +
      scale_x_continuous( breaks = sort(unique(plotdf$period_num)), 
                          labels = levels(plotdf$period_fac) ) +
      labs(x = "Year",
           y = "Theil'T change from reference",
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
  
  if(any(plotlist == 'ineqRegBySec_TheilL' | allExport  )){
    
    
    plotdf <- plotdataIneq[['ineq']] %>%
      filter(
        between(period, 2025, 2100),
        region != "World",
        !str_starts(coalesce(category, ""), "Total"),
        category != 'Other commodities',
        variable == "ineq|shapleyTheilL"
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
        "C_SSP2-PkBudg1000", 
        "C_SSP2-PkBudg650",
        "C_SSP2-loOS-def", 
        "C_SSP2-hiOS-def" 
      ))) 
    
    
    totals <- plotdf %>%
      group_by(region, period_fac, period_num, scen_off, scenario, x) %>%
      summarise(total = sum(value, na.rm = TRUE)+0.005, .groups = "drop") %>%
      mutate(scenario = factor(scenario, levels = c(
        "C_SSP2-PkBudg1000", 
        "C_SSP2-PkBudg650",
        "C_SSP2-loOS-def", 
        "C_SSP2-hiOS-def" 
      ))) 
    
    p[[paste0('ineqRegBySec_TheilL')]] <- list(
      
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
        scale_fill_manual(values = mySecPalette) +
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
  
  
  
  
  #-------------End 5.3 regional inequality effect by Channel (Theil)-----------
  
  
  
  
  
  
  #-------------5.4 regional inequality contribution to Global----------------
  
  #inequality Contribution of between and within country inequality
  if(any(plotlist == 'ineqGlobalBetweenWithinTheilT_Neut' | allExport  )){
    
    plotdf <- plotdataIneq[['theilTDecompNeut']]%>%
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
                                "Tt.i|delta" = "Total")) %>%
      mutate(scenario = factor(scenario, levels = c(
        "C_SSP2-PkBudg1000", 
        "C_SSP2-PkBudg650",
        "C_SSP2-loOS-def", 
        "C_SSP2-hiOS-def" 
      )))
    
    
    p[['ineqGlobalBetweenWithinTheilT_Neut']] <- list(
      
      plot = ggplot(plotdf, aes(x = period, y = value, group = component)) +
        # Area plots for Within and Between
        geom_area(
          data = filter(plotdf, component %in% c("Within", "Between")),
          aes(fill = component),
          position = "stack",
          alpha = 0.6
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
        paletteer::scale_fill_paletteer_d(name = "Contribution", "NatParksPalettes::Yellowstone",
                                          labels = c('Between' = 'Between-country',
                                                     'Within' = 'Within-country')) 
      
      ,
      
      width = 8,
      height = 3
      
    )
    
  }
  
  if(any(plotlist == 'ineqGlobalBetweenWithinTheilL_Neut' | allExport  )){
    
    plotdf <- plotdataIneq[['theilLDecompNeut']]%>%
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
        "C_SSP2-PkBudg1000",
        "C_SSP2-PkBudg650",
        "C_SSP2-loOS-def", 
        "C_SSP2-hiOS-def" 
      )))
    
    p[['ineqGlobalBetweenWithinTheilL_Neut']] <- list(
      
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
  
  if(any(plotlist == 'ineqGlobalBetweenWithinTheilT_Epc' | allExport  )){
    
    plotdf <- plotdataIneq[['theilTDecompEpc']]%>%
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
                                "Tt.i|delta" = "Total")) %>%
      mutate(scenario = factor(scenario, levels = c(
        "C_SSP2-PkBudg1000",
        "C_SSP2-PkBudg650",
        "C_SSP2-loOS-def", 
        "C_SSP2-hiOS-def" 
      )))
    
    
    p[['ineqGlobalBetweenWithinTheilT_Epc']] <- list(
      
      plot = ggplot(plotdf, aes(x = period, y = value, group = component)) +
        # Area plots for Within and Between
        geom_area(
          data = filter(plotdf, component %in% c("Within", "Between")),
          aes(fill = component),
          position = "stack",
          alpha = 0.6
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
        paletteer::scale_fill_paletteer_d(name = "Contribution", "NatParksPalettes::Yellowstone",
                                          labels = c('Between' = 'Between-country',
                                                     'Within' = 'Within-country')) 
        
      ,
      
      width = 8,
      height = 3
      
    )
    
  }
  
  if(any(plotlist == 'ineqGlobalBetweenWithinTheilL_Epc' | allExport  )){
    
    plotdf <- plotdataIneq[['theilLDecompEpc']]%>%
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
        "C_SSP2-PkBudg1000",
        "C_SSP2-PkBudg650",
        "C_SSP2-loOS-def", 
        "C_SSP2-hiOS-def" 
      )))
    
    p[['ineqGlobalBetweenWithinTheilL_Epc']] <- list(
      
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
  if(any(plotlist == 'ineqGlobalWithinRegTheilT_Epc' | allExport  )){
      
      plotdf <- plotdataIneq[['theilTDecompEpc']]%>%
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
        bind_rows(plotdf) %>%
        mutate(scenario = factor(scenario, levels = c(
          "C_SSP2-PkBudg1000",
          "C_SSP2-PkBudg650",
          "C_SSP2-loOS-def", 
          "C_SSP2-hiOS-def" 
        )))

      
      p[['ineqGlobalWithinRegTheilT_Epc']] <- list(
        
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
          scale_color_manual(values = c("Total" = "black")) 
        
        ,
        
        width = 8,
        height = 5
        
      )
    }
      
  if(any(plotlist == 'ineqGlobalWithinRegTheilL_Epc' | allExport  )){
     
    plotdf <- plotdataIneq[['theilLDecompEpc']]%>%
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
          "C_SSP2-PkBudg1000",
          "C_SSP2-PkBudg650",
          "C_SSP2-loOS-def", 
          "C_SSP2-hiOS-def" 
        )))
      
      p[['ineqGlobalWithinRegTheilL_Epc']] <- list(
        
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
  
  if(any(plotlist == 'ineqGlobalWithinRegTheilT_Neut' | allExport  )){
      
      plotdf <- plotdataIneq[['theilTDecompNeut']]%>%
        mutate(`Tt.i|delta` = Tt.i - `Tt.i|base`,
               `Tt.b|delta` = Tt.b - `Tt.b|base`,
               `Tt.iw|delta` = Tt.iw - `Tt.iw|base`) %>%
        group_by(scenario,period,region) %>%
        summarise( `Tt.iw|delta` = sum(`Tt.iw|delta`),
                   .groups = "drop") %>%
        rename(within = "Tt.iw|delta") %>%
        filter(period <= 2100)
      
      plotdf <- plotdf %>% 
        group_by(scenario, period) %>%
        summarise(within = sum(within),
                  .groups = "drop") %>%
        mutate(region = 'total') %>%
        bind_rows(plotdf) %>%
        mutate(scenario = factor(scenario, levels = c(
          "C_SSP2-PkBudg1000",
          "C_SSP2-PkBudg650",
          "C_SSP2-loOS-def", 
          "C_SSP2-hiOS-def" 
        )))

      
      p[['ineqGlobalWithinRegTheilT_Neut']] <- list(
        
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
          scale_color_manual(values = c("Total" = "black")) 
        
        ,
        
        width = 8,
        height = 5
        
      )
    }

  if(any(plotlist == 'ineqGlobalWithinRegTheilL_Neut' | allExport  )){
    
    plotdf <- plotdataIneq[['theilLDecompNeut']]%>%
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
        "C_SSP2-PkBudg1000",
        "C_SSP2-PkBudg650",
        "C_SSP2-loOS-def", 
        "C_SSP2-hiOS-def" 
      )))
    
    p[['ineqGlobalWithinRegTheilL_Neut']] <- list(
      
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
    
    revenue_smoothed <- compute_transfer(data3, data2, climaFund = 0,
                                         fund_return_scale = 1,
                                         payg = 1,, revenueRep = 1) %>%
      group_by(scenario,period) %>%
      summarise(value = sum(revenue)) %>%
      filter(period <= 2100)
    
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
        "C_SSP2-PkBudg1000",
        "C_SSP2-PkBudg650",
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
          geom_line(
            data = revenue_smoothed,
            aes(x= period, y =value),
            color = "#446482",
            inherit.aes = FALSE,
            linewidth = 0.8,
            linetype = "dashed"
          )+
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
  

  
  # if(any(plotlist == 'taxRevenueLAM' | allExport  )){
  #   
  #   revenue_smoothed <- compute_transfer(data3, data2, revenue = 1) %>%
  #     filter(period <=2100) %>%
  #     filter(region == 'LAM') %>%
  #     rename(value = 'revenue_smoothed') %>%
  #     filter(scenario =='C_SSP2-hiOS-def')
  #   
  #   plotdf <- bind_rows(
  #     # World rows already present in data (any model)
  #     data3 %>%
  #       filter(str_starts(variable, "Taxes"),
  #              region == "LAM") %>%
  #       mutate(variable = str_remove(variable, "^Taxes\\|?"))
  #   ) %>%
  #     # keep only the two layers we want to stack
  #     filter(variable %in% c("GHG|REMIND", "GHG|MAGPIE")) %>%
  #     # control stacking order: bottom -> top
  #     mutate(
  #       variable = factor(variable, levels = c("GHG|REMIND", "GHG|MAGPIE")),
  #       # make sure x is ordered for area stacking
  #       period = as.integer(period)
  #     ) %>%
  #     filter(period <= 2100) %>%
  #     mutate(scenario = factor(scenario, levels = c(
  #       "C_SSP2-loOS-def",
  #       "C_SSP2-hiOS-def"
  #     ))) %>%
  #     filter(scenario =='C_SSP2-hiOS-def')
  #   
  #   
  #   sumdf <- plotdf %>%
  #     group_by(scenario, period) %>%
  #     summarise(total = sum(value, na.rm = TRUE), .groups = "drop") 
  #   
  #   p[[paste0("taxRevenueLAM")]] <- list(
  #     plot =
  #       ggplot(plotdf, aes(x = period, y = value, fill = variable)) +
  #       geom_area(alpha = 0.7, color = NA, position = "stack") +
  #       geom_line(
  #         data = sumdf,
  #         aes(x = period, y = total, color = "Total"),
  #         inherit.aes = FALSE,
  #         linewidth = 0.8,
  #         linetype = "solid"
  #       ) +
  #       geom_line(
  #         data = revenue_smoothed,
  #         aes(x= period, y =value),
  #         color = "#446482",
  #         inherit.aes = FALSE,
  #         linewidth = 0.8,
  #         linetype = "dashed"
  #       )+
  #       facet_wrap(
  #         ~ scenario, ncol = 2,
  #         labeller = as_labeller(c(
  #           "C_SSP2-PkBudg1000" = "2°C",
  #           "C_SSP2-PkBudg650"  = "1.5°C",
  #           "C_SSP2-hiOS-def"   = "1.5°C HO",
  #           "C_SSP2-loOS-def"   = "1.5°C LO"
  #         ))
  #       ) +
  #       scale_fill_paletteer_d(
  #         "dutchmasters::view_of_Delft",
  #         name   = "Source",
  #         labels = c("Energy system", "Land sector")
  #       ) +
  #       scale_color_manual(
  #         name   = "",
  #         values = c("Total" = "#446482"),    
  #         labels = c("Total" = "Total net revenue")
  #       ) +
  #       guides(
  #         fill  = guide_legend(order = 1),
  #         color = guide_legend(order = 2, override.aes = list(linetype = "solid", linewidth = 1.2))
  #       ) +
  #       scale_x_continuous(breaks = unique(plotdataWelf$period),
  #                          labels = unique(plotdataWelf$period)) +
  #       labs(x = "Year", y = "Carbon revenue (billion$)") 
  #     # theme_minimal() +
  #     # theme(
  #     #   legend.position = "bottom",
  #     #   strip.text = element_blank(),
  #     #   axis.text.x = element_text(angle = 45, hjust = 1),
  #     #   panel.grid.major.y = element_line(color = "grey70", linewidth = 0.1, linetype = "dashed"),
  #     #   panel.grid.major.x = element_line(color = "grey70", linewidth = 0.1, linetype = "dashed"),
  #     #   panel.grid.minor.x = element_blank(),
  #     #   panel.grid.minor.y = element_blank(),
  #     #   panel.background   = element_rect(fill = "white", color = NA)
  #     # )
  #     ,
  #     
  #     width  = 4,
  #     height = 3
  #   )
  #   
  #   
  #   
  #   
  #   
  # } 
  # 
  #-------------End 6 Global tax revenue-------------
    
  
  #--------------------------7.categorization of sector driver-----------------
  
  if(any(plotlist == 'shockVsExposurebySec'| allExport)) {
    
    
    plotdf <- data3 %>% 
      filter(
        str_starts(variable, "relaPrice") |
          str_starts(variable, "share"),
        scenario != "C_SSP2-NPi2025",
        period %in% c(2040,2070,2100)
      ) %>%
      separate(
        variable,
        into = c("var_main", "var_sub", "var_sub1"),   # rename as you like
        sep  = "\\|"                       # escape the pipe!
      ) %>%
      mutate(
        category = if_else(
          is.na(var_sub1),
          var_sub,
          str_c(var_sub, var_sub1, sep = " ")
        )
      ) %>%
      select(-model, -baseline, -unit, -var_sub, -var_sub1) %>%
      mutate(
        category = case_when(
          category == "Buildings Electricity"  ~ "Building electricity",
          category == "Buildings Gases"        ~ "Building gases",
          category == "Buildings Other fuels"  ~ "Building other fuels",
          category == "Transport FE"           ~ "Transport energy",
          TRUE                                 ~ category
        )
      ) %>%
      pivot_wider(names_from = var_main, values_from = value) %>%
      filter(category != 'Other commodities') %>%
      mutate(relaPrice = (relaPrice -1) * 100,
             share = share *100,
             category = factor(category, levels = allSec),
             period = factor(period, levels = c(2040,2070,2100))) %>%
      arrange(region, category, period)
    
    
    shareMean <- mean(plotdf$share)
    priceMean <- mean(plotdf$relaPrice)
    
    quads <- tibble(
      area = factor(c("Low risk", "Medium risk", "Medium risk", "High risk"),
                    levels = c("Low risk", "Medium risk", "High risk")),
      xmin = c(-Inf,  shareMean, -Inf,      shareMean),
      xmax = c(shareMean, Inf,  shareMean,   Inf),
      ymin = c(-Inf,  -Inf,     priceMean,   priceMean),
      ymax = c(priceMean, priceMean, Inf,    Inf)
    )
    
    for(scen in plotdf$scenario %>% unique){
      
      p[[paste0("shockVsExposurebySec_",scen)]] <- list(
        plot = ggplot(
          plotdf %>% filter(scenario == scen),
          aes(x = share, y = relaPrice, color = category)
        ) +
          geom_vline(xintercept = shareMean, linetype = 'dashed', color = 'grey30') +
          geom_hline(yintercept = priceMean, linetype = 'dashed', color = 'grey30') +
          geom_rect(
            data = quads,
            aes(xmin = xmin, xmax = xmax,
                ymin = ymin, ymax = ymax,
                fill = area),
            inherit.aes = FALSE,
            alpha = 0.4
          ) +
          scale_fill_manual(
            name   = "Risk level",
            values = c(
              "Low risk"    = "#86BA73",
              "Medium risk" = "#F78828",
              "High risk"   = "#5700B8"
            )
          )+
          geom_point(aes(shape = factor(period))) +              # different shapes
          geom_text(
            aes(label = period),                                 # put the year on the point
            vjust = -0.6,                                        # nudge label a bit above
            size = 1.8,
            show.legend = FALSE
          ) +
          geom_path(
            aes(group = category)
          ) +
          facet_wrap(~ region, ncol= 3, scales = "free") +
          scale_y_continuous(
            expand = expansion(mult = c(0.05, 0.15))
            # bottom 5% extra, top 25% extra (tweak as you like)
          ) +
          scale_color_manual(values = mySecPalette) +
          labs(
            x = "Consumption share (%)",        
            y = "Price chagne from reference (%)",
            color = "Category",
            shape = "Year"
          ) +
          theme_bw() +
          theme(
            panel.grid   = element_blank(),
            panel.border = element_rect(color = "black", fill = NA)
          ),
        
        width  = 10,
        height = 8
        
      )
    }
    
    
    
  }
  
  
  
  
  
  

  #----------------------End 7. categorization of sector drivers---------------
  
  
  
  #--------------------------8. burden on deciles -----------------
  
  if(any(plotlist == 'secBurdenByGlobalDecile' | allExport  )){
    
    plotdf <- aggregate_decileWelfChange(data1 = plotdataWelf, data2 = data2, 
                                         level = c("fullSec"), region = 'globalDecile') %>%
      filter(period %in% c('2030','2050','2100'),
             category != 'Other commodities') %>%
      mutate(category = factor(category, levels = allSec))
    
    iter = 1

    for(scen in plotdf$scenario %>% unique ) {
      
      plot_base <- ggplot(plotdf %>% filter(scenario == scen), 
                          aes(x = as.factor(decileGroup), y = welfChange, fill = category)) +
        geom_col(position = "stack") +
        geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
        facet_wrap(~ period, ncol = 3) +
        labs(
          title = scenario_labels[scen],
          x = "Income decile group",
          y = "Real Consumption Change (%)",
          fill = "Category"
        ) +
        #scale_fill_paletteer_d("NatParksPalettes::Olympic",direction = -1) +
        scale_fill_manual(values = mySecPalette)+
        guides(fill = guide_legend(reverse = T)) +
        theme(
          legend.position  = "right",
          legend.direction = "vertical"
        )
      
      
      p[[paste0("secBurdenByGlobalDecile",scen)]] <- list(
        plot = plot_base
        ,
        
        width  = 8,
        height = 5
        
      )
      
      iter = iter + 1
    }


    
    
    
    
    
    
    
  }
  
  
  if(any(plotlist == 'secBurdenByRegDecile' | allExport  )){
    
    plotdf <- aggregate_decileWelfChange(data1 = plotdataWelf, data2 = data2, 
                                         level = c("fullSec"), region = 'decile') %>%
      filter(period %in% c('2030','2050','2100'),
             category != 'Other commodities') %>%
      mutate(category = factor(category, levels = allSec) )
    
    
    iter = 1

    
    for(scen in plotdf$scenario %>% unique ) {
      
      plot_base <- ggplot(plotdf %>% filter(scenario == scen), 
                          aes(x = as.factor(decileGroup), y = welfChange, fill = category)) +
        geom_col(position = "stack") +
        geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
        facet_grid(region ~ period, scales = "free_y")+
        labs(
          title = scenario_labels[scen],
          x = "Income decile group",
          y = "Real Consumption Change (%)",
          fill = "Category"
        ) +
        #scale_fill_paletteer_d("ggprism::floral2") +
        #scale_fill_paletteer_d("NatParksPalettes::Olympic") +
        scale_fill_manual(values = mySecPalette)+
        guides(fill = guide_legend(reverse = FALSE)) +
        theme(
          legend.position  = "right",
          legend.direction = "vertical"
        )
      
      # # conditionally modify plot
      # if (iter == length(plotdf$scenario %>% unique) ) {
      #   plot_base <- plot_base + 
      #     theme(legend.position = "none")
      # } else if (iter != length(plotdf$scenario %>% unique)){
      #   plot_base <- plot_base + theme( axis.title.x = element_blank() )
      # }
      
      p[[paste0("secBurdenByRegDecile",scen)]] <- list(
        plot = plot_base,
        
        width  = 12,
        height = 15
        
      )
      
      iter = iter + 1
    }
    
    
    
  }
  
  
  #----------------------End 8. burden on deciles---------------
  
  
  
  #--------------------------9. Remind Region map----------------------------
  
  if(any(plotlist == 'remindRegionMap' | allExport  )){
  
    
    
    # Read in regional mapping   
    if(regions =='H12') {
      
      regionMapping <- read_delim("input/regionmappingH12.csv", 
                                  delim = ";", escape_double = FALSE, col_types = cols(X = col_skip()), 
                                  trim_ws = TRUE,
                                  show_col_types = FALSE) %>%
        rename(geo = CountryCode,
               region = RegionCode) 
    } else if (regions == 'H21') {
      
      regionMapping <- read_delim("input/regionmapping_21_EU11.csv", 
                                  delim = ";", escape_double = FALSE, col_types = cols(X = col_skip(), 
                                                                                       missingH12 = col_skip()), 
                                  trim_ws = TRUE,
                                  show_col_types = FALSE) %>%
        rename(geo = CountryCode,
               region = RegionCode)
    }
    
    
    gini <- plotdataIneq[['ineq']] %>%
      filter( period <= 2100,
              region != 'World',
              variable %in% c('ineq|Gini'))
    
    
    gini_ref <- gini %>% filter(scenario == 'C_SSP2-NPi2025') %>%
      select(-scenario,-category) %>%
      rename(ref = value) %>%
      filter(period == 2040)

    # 1. World country polygons
    world <- rnaturalearth::ne_countries(
      scale = "medium",
      returnclass = "sf"
    ) %>%
      filter(iso_a3 != "ATA")
    
    # 2. Join REMIND region mapping by ISO3 code
    world_reg <- world %>%
      left_join(regionMapping, by = c("iso_a3" = "geo")) %>%
      filter(!is.na(region))
    
    world_gini <- world_reg %>%
      left_join(gini_ref, by = "region")
    
    centroids <- world_reg %>%
      group_by(region) %>%
      summarise(.groups = "drop") %>%           # union just to get one geom per region
      st_point_on_surface() %>%                # safe-ish centroid inside polygon
      mutate(
        lon = st_coordinates(.)[, 1],
        lat = st_coordinates(.)[, 2]
      ) %>%
      st_drop_geometry()
    

    
    centroids_gini <- centroids %>%
      left_join(gini_ref, by = "region")
    
    region_points <- world_gini %>%
      group_by(region) %>%
      summarise(
        gini_val = unique(ref)[1],  # or max(gini_val, na.rm = TRUE)
        .groups  = "drop"
      ) %>%
      st_point_on_surface() %>%   # safe-ish "centroid" inside polygon
      mutate(
        lon = st_coordinates(.)[, 1],
        lat = st_coordinates(.)[, 2]
      ) %>%
      st_drop_geometry() %>%
      # custom label position for OAS
      mutate(
        lon = if_else(region == "OAS", 110, lon),  # ~ Southeast Asia longitude
        lat = if_else(region == "OAS",   5,  lat)  # ~ Southeast Asia latitude
      )
    
    
    p[['remindRegionMap']] <- list(
      plot = ggplot() +
        # REMIND regions: colored by region
        geom_sf(
          data  = world_reg,
          aes(fill = region),
          color    = "grey60",
          linewidth = 0.2,
          alpha    = 0.7
        ) +
        # # Gini dots
        # geom_point(
        #   data = region_points,
        #   aes(x = lon, y = lat, color = gini_val),
        #   size = 9,
        #   alpha = 1
        # ) +
        # Region labels
        geom_text(
          data = region_points,
          aes(x = lon, y = lat, label = region),
          hjust   = 0,
          nudge_x = 5,   # move label to the right
          size    = 3
        ) +
        coord_sf(expand = FALSE) +
        # DISCRETE fill scale for regions
        scale_fill_paletteer_d("PrettyCols::Summer", name = "REMIND region") +
        # CONTINUOUS color scale for Gini dots
        scale_color_gradient(
          name     = "Gini (2040)",
          low      = "white",
          high     = "#aa0002",
          na.value = "grey90"
        ) +
        theme_minimal() +
        theme(
          axis.text  = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank()
        )
      ,
      
      width  = 8,
      height = 5
      
    )
    
    
    
  }
  
  
  
  
  if(any(plotlist == 'regionIneqChangeBar_Gini' | allExport  )){
    
    for(scen in paste0( 'C_',all_runscens,'-',all_budgets)){
      
      gini <- plotdataIneq[['ineq']] %>%
        filter( period <= 2100,
                region != 'World',
                variable %in% c('ineq|Gini'))
      
      gini_ref <- gini %>% filter(scenario == 'C_SSP2-NPi2025') %>%
        select(-scenario,-category) %>%
        rename(ref = value)
      
      
      plotdf <- gini %>%
        filter(scenario != 'C_SSP2-NPi2025') %>%
        left_join(gini_ref, by = c('region', 'period', 'variable')) %>%
        rename(pol = value) %>%
        mutate(change = (pol - ref) * 100,
               variable ='ineq|deltaGini') %>%
        mutate(scenario = factor(scenario, levels = c(
          "C_SSP2-loOS-def", 
          "C_SSP2-hiOS-def",
          "C_SSP2-PkBudg1000",
          "C_SSP2-PkBudg650"
          
        )),
        category = factor(category, levels = c(
          'TotalWithTransfNeut',
          'TotalWithTransfEpc'
        ))) %>% 
        filter( period %in% c(2040, 2060, 2100),
                scenario == scen)
      
      regOrder <- plotdf %>% 
        filter(
          period   == 2060,
          scenario == scen,
          category == "TotalWithTransfNeut"
        ) %>%
        arrange(change) %>%          # ascending; use desc(change) for reverse
        pull(region)
      
      max_abs <- 5
      
      plotdf_cap <- plotdf %>%
        mutate(
          region = factor(region, levels = regOrder),
          change_cap  = pmax(pmin(change, max_abs), -max_abs),
          is_clipped  = abs(change) > max_abs,
          sign_change = sign(change),
          label_y     = change_cap +2.5 + 0.5 * sign_change  # a bit beyond capped bar
        )
      
      
      p[[paste0('regionIneqChangeBar_Gini_',scen)]] <- list(
        
        plot = ggplot(plotdf_cap, aes(
          x    = region,
          y    = change_cap,
          fill = region
        )) +
          geom_col() +
          geom_hline(yintercept = 0, color = "grey60") +
          geom_text(
            data = subset(plotdf_cap, is_clipped),
            aes(y = label_y, label = round(change, 2)),
            size = 3
          ) +
          scale_y_continuous(limits = c(-5, 5)) + 
          coord_flip() +
          scale_fill_paletteer_d("PrettyCols::Summer", name = "REMIND region") +
          labs(
            x = NULL,
            y = "Gini change (points)",
            title    = "Gini change from reference",
            subtitle = scen
          ) +
          facet_grid(category ~ period) +
          theme_minimal() +
          theme(
            panel.grid.major.y = element_blank(),
            legend.position    = "right",
            panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.4)
          )
        
        ,
        
        width  = 10,
        height = 6
        
      )
    }
  }
  
  
  
  
  
  
  
  #------------------------------End-----------------------------------------
  
  
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
