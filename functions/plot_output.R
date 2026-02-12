#This is the function to plot welfare chagne



plot_output <- function(outputPath, 
                        decileWelfChange,  
                        decileConsShare,
                        data,
                        anchRealCons,
                        ineqAll,
                        ineqChannel,
                        plotlist='NA' , micro_model, fixed_point, 
                        exampleReg = 'EUR', isDisplay = T, 
                        isExport = FALSE, allExport=FALSE ) {
  
  dfDecileWelfChange <- decileWelfChange %>% dplyr::filter(period <= 2100)
  dfDecileConsShare  <- decileConsShare  %>% dplyr::filter(period <= 2100)
  dfData             <- data             %>% dplyr::filter(period <= 2100)
  dfAnchRealCons     <- anchRealCons     %>% dplyr::filter(period <= 2100)
  dfIneqAll <- purrr::map( ineqAll,
    ~ dplyr::filter(.x, period <= 2100)
  )
  dfIneqChannel      <- ineqChannel      %>% dplyr::filter(period <= 2100)
  
  

  
  theme_set(
    theme_minimal(base_family = "Arial", base_size = 9) +
      theme(
        plot.title = element_text(hjust = 0.5, size = 10),
        axis.text.x = element_text(size = 9,angle = 45, hjust = 1),
        axis.text.y = element_text(size = 9),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 8),
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
  
  numSector <- length(unique(dfDecileWelfChange$category))
  foodSec <- c("Empty calories","Animal products","Fruits vegetables nuts","Staples")
  eneSec <- c("Building electricity", "Building gases", "Building other fuels","Transport energy")
  allSec <- c(foodSec,eneSec, 'Consumption')
  
  # Category label mapping
  categoryLabels <- c(
    "Empty calories" = "Processed food",
    "Animal products" = "Animal products",
    "Fruits vegetables nuts" = "Fruits vegetables nuts",
    "Staples" = "Staples",
    "Building electricity" = "Building electricity",
    "Building gases" = "Building gases",
    "Building other fuels" = "Building other fuels",
    "Transport energy" = "Transport energy",
    "Consumption" = "Consumption",
    "Other commodities" = "Other commodities"
  )
  
  scenario_levels <- c(
    "C_SSP2-PkBudg1000",
    "C_SSP2-PkBudg650",
    "C_SSP2-loOS-def", 
    "C_SSP2-hiOS-def" 
  )
  
  scenario_labels <- c(
    "C_SSP2-PkBudg1000" = "2°C",
    "C_SSP2-PkBudg650"  = "1.5°C",
    "C_SSP2-hiOS-def"   = "1.5°C HO",
    "C_SSP2-loOS-def"   = "1.5°C LO"
  )
  
  mySecPalette <- rev(c("#053138FF", "#9FDFED", "#0B8CA9FF", "#AEC7BEFF",
    "#FAF3CEFF", "#CFE690FF", "#F2AB70FF", "#FEC5A0FF")
  )
  
  myScenPalette <- c(
    "C_SSP2-PkBudg1000" = "#e8ab67",  # 2°C
    "C_SSP2-PkBudg650"  = "#779ec6",  # 1.5°C
    "C_SSP2-hiOS-def"   = "#e8ab67",  # 1.5°C HO (same as 1000)
    "C_SSP2-loOS-def"   = "#779ec6"   # 1.5°C LO (same as 650)
  )
  
  regSelect <- c('CHA', 'EUR', 'IND', 'LAM', 'SSA', 'USA')
  
  region_labels <- c(
    "OAS" = "Other Asia",
    "CAZ" = "Canada, Australia, New Zealand",
    "CHA" = "China",
    "EUR" = "EU",
    "IND" = "India",
    "JPN" = "Japan",
    "LAM" = "Latin America",
    "MEA" = "Middle East and North Africa",
    "NEU" = "Other Europe",
    "REF" = "Reforming economies",
    "SSA" = "Sub-Saharan Africa",
    "USA" = "United States"
  )
  
  period <- unique(dfDecileConsShare$period)
  
  p=list()

#-------------------------1.Plots all aggregated welfare change------------------------
  
  if(any(plotlist == "welfByPeriodNeut") | allExport){

    plotdf <- dfAnchRealCons %>% 
      filter(scheme == 'Neut') %>%
      select(scenario,period,relChange_pc_vs_base) %>%
      mutate(scenario = factor(scenario, levels = scenario_levels))

    avg_df_period <- plotdf %>%
      group_by(scenario, period) %>%
      summarise(meanRelChange = mean(relChange_pc_vs_base, na.rm = TRUE), .groups = "drop") %>%
      mutate(scenario = factor(scenario, levels = scenario_levels))
      
    
    p[['welfByPeriodNeut']] <- list(
      
      plot = ggplot( plotdf, 
                     aes(x = period, y = relChange_pc_vs_base, fill = scenario)) +
        
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
          values = myScenPalette,
          labels = scenario_labels
        ) +
        
        # One-by-one geom_line for each scenario
        geom_line(data = avg_df_period %>% filter(scenario %in% c("C_SSP2-PkBudg650", "C_SSP2-hiOS-def"), period >= 2025),
                  aes(x = period + 1, y = meanRelChange),
                  color = "#ba7a31", linewidth = 0.7) +
        geom_line(data = avg_df_period %>% filter(scenario %in% c("C_SSP2-PkBudg1000", "C_SSP2-loOS-def"), period >= 2025),
                  aes(x = period - 1, y = meanRelChange),
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
        scale_x_continuous(breaks =  period,
                           labels =  period) +
        
        # Labels and styling
        labs(x = "Year", y = "Real consumpition Change (%)", fill = "Scenario", color = "Scenario") +
        coord_cartesian(ylim = quantile(plotdf$relChange_pc_vs_base, probs = c(0.01, 0.99), na.rm = TRUE))
      ,
      
      width = 5,
      height = 3
      
    )
  
    
    
    }
  
  if(any(plotlist == "welfByPeriodEpc") | allExport){
    
    plotdf <- dfAnchRealCons %>% 
      filter(scheme == 'EPC') %>%
      select(scenario,period,relChange_pc_vs_base) %>%
      mutate(scenario = factor(scenario, levels = scenario_levels))
    
    avg_df_period <- plotdf %>%
      group_by(scenario, period) %>%
      summarise(meanRelChange = mean(relChange_pc_vs_base, na.rm = TRUE), .groups = "drop") %>%
      mutate(scenario = factor(scenario, levels = scenario_levels))
    
    
    p[['welfByPeriodEpc']] <- list(
      
      plot = ggplot( plotdf, 
                     aes(x = period, y = relChange_pc_vs_base, fill = scenario)) +
        
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
          values = myScenPalette,
          labels = scenario_labels
        ) +
        
        # One-by-one geom_line for each scenario
        geom_line(data = avg_df_period %>% filter(scenario %in% c("C_SSP2-PkBudg650", "C_SSP2-hiOS-def"), period >= 2025),
                  aes(x = period + 1, y = meanRelChange),
                  color = "#ba7a31", linewidth = 0.7) +
        geom_line(data = avg_df_period %>% filter(scenario %in% c("C_SSP2-PkBudg1000", "C_SSP2-loOS-def"), period >= 2025),
                  aes(x = period - 1, y = meanRelChange),
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
        scale_x_continuous(breaks =  period,
                           labels =  period) +
        
        # Labels and styling
        labs(x = "Year", y = "Real consumpition Change (%)", fill = "Scenario", color = "Scenario") +
        coord_cartesian(ylim = quantile(plotdf$relChange_pc_vs_base, probs = c(0.01, 0.99), na.rm = TRUE))
      ,
      
      width = 5,
      height = 3
      
    )
    
    
    
  }
  
  if(any(plotlist == "welfByDecileNeut") | allExport){
    
    plotdf <- dfAnchRealCons %>% 
      filter(scheme == 'Neut') %>%
      select(scenario, decileGroup ,relChange_pc_vs_base) %>%
      mutate(scenario = factor(scenario, levels = scenario_levels))
    
    avg_df_decile <- plotdf %>%
      group_by(scenario, decileGroup) %>%
      summarise(meanRelChange = mean(relChange_pc_vs_base, na.rm = TRUE), .groups = "drop") %>%
      mutate(scenario = factor(scenario, levels = scenario_levels))
    
    p[["welfByDecileNeut"]] <- list (
      plot = ggplot(plotdf, aes(x = factor(decileGroup), y = relChange_pc_vs_base, fill = scenario)) +
        geom_boxplot(outlier.shape = NA, width = 0.4, position = position_dodge(width = 0.5), color = "grey20") +
        geom_jitter(aes(color = scenario), position = position_jitterdodge(jitter.width = 0.5, dodge.width = 0.5), alpha = 0.3, size = 0.2) +
        geom_line(data = avg_df_decile, aes(x = decileGroup, y = meanRelChange, group = scenario, color = scenario), linewidth = 0.7) +
        stat_summary(fun = mean, geom = "point", aes(group = scenario), position = position_dodge(width = 0.7), shape = 20, size = 2.5, color = "gray30") +
        scale_fill_manual(values = myScenPalette, labels = scenario_labels) +
        scale_color_manual(values = myScenPalette, labels = scenario_labels) +
        labs(x = "Income Decile Group", y = "Real consumpition Change (%)", fill = "Scenario", color = "Scenario") +
        theme_minimal() +
        coord_cartesian(ylim = quantile(plotdf$relChange_pc_vs_base, probs = c(0.02, 0.99), na.rm = TRUE)) +
        theme(axis.text.x = element_text(angle = 0), legend.position = "bottom", legend.direction = "horizontal"),
      width = 6,
      height = 4
    )
    
  }
  
  if(any(plotlist == "welfByDecileEpc") | allExport){
    
    plotdf <- dfAnchRealCons %>% 
      filter(scheme == 'EPC') %>%
      select(scenario, decileGroup ,relChange_pc_vs_base) %>%
      mutate(scenario = factor(scenario, levels = scenario_levels))
    
    avg_df_decile <- plotdf %>%
      group_by(scenario, decileGroup) %>%
      summarise(meanRelChange = mean(relChange_pc_vs_base, na.rm = TRUE), .groups = "drop") %>%
      mutate(scenario = factor(scenario, levels = scenario_levels))
    
    p[["welfByDecileEpc"]] <- list (
      plot = ggplot(plotdf, aes(x = factor(decileGroup), y = relChange_pc_vs_base, fill = scenario)) +
        geom_boxplot(outlier.shape = NA, width = 0.4, position = position_dodge(width = 0.5), color = "grey20") +
        geom_jitter(aes(color = scenario), position = position_jitterdodge(jitter.width = 0.5, dodge.width = 0.5), alpha = 0.3, size = 0.2) +
        geom_line(data = avg_df_decile, aes(x = decileGroup, y = meanRelChange, group = scenario, color = scenario), linewidth = 0.7) +
        stat_summary(fun = mean, geom = "point", aes(group = scenario), position = position_dodge(width = 0.7), shape = 20, size = 2.5, color = "gray30") +
        scale_fill_manual(values = myScenPalette, labels = scenario_labels) +
        scale_color_manual(values = myScenPalette, labels = scenario_labels) +
        labs(x = "Income Decile Group", y = "Real consumpition Change (%)", fill = "Scenario", color = "Scenario") +
        theme_minimal() +
        coord_cartesian(ylim = quantile(plotdf$relChange_pc_vs_base, probs = c(0.02, 0.99), na.rm = TRUE)) +
        theme(axis.text.x = element_text(angle = 0), legend.position = "bottom", legend.direction = "horizontal"),
      width = 6,
      height = 4
    )
    
  }
  
  if(any(plotlist == "welfByDecileEpcLowInc") | allExport){
    
    plotdf <- dfAnchRealCons %>% 
      filter(scheme == 'EPC',
             region %in% c('LAM','SSA','REF','IND','OAS','MEA','CHA')) %>%
      select(scenario, decileGroup ,relChange_pc_vs_base) %>%
      mutate(scenario = factor(scenario, levels = scenario_levels))
    
    avg_df_decile <- plotdf %>%
      group_by(scenario, decileGroup) %>%
      summarise(meanRelChange = mean(relChange_pc_vs_base, na.rm = TRUE), .groups = "drop") %>%
      mutate(scenario = factor(scenario, levels = scenario_levels))
    
    p[["welfByDecileEpcLowInc"]] <- list (
      
      plot = ggplot(plotdf, 
                    aes(x = factor(decileGroup), y = relChange_pc_vs_base, fill = scenario)) +
        
        geom_boxplot(outlier.shape = NA, width = 0.4, 
                     position = position_dodge(width = 0.5), color = "grey20") +
        
        geom_jitter(aes(color = scenario),
                    position = position_jitterdodge(jitter.width = 0.5, dodge.width = 0.5), 
                    alpha = 0.3, size = 0.2) +
        
        # 🔹 Add mean lines by scenario
        geom_line(data = avg_df_decile, 
                  aes(x = decileGroup, y = meanRelChange, group = scenario, color = scenario), 
                  linewidth = 0.7) +
        
        stat_summary(fun = mean, geom = "point", 
                     aes(group = scenario), 
                     position = position_dodge(width = 0.7), 
                     shape = 20, size = 2.5, color = "gray30") +
        
        scale_fill_manual(
          values = myScenPalette,
          labels = scenario_labels
        ) +
        
        scale_color_manual(
          values = myScenPalette,
          labels = scenario_labels
        ) +
        
        labs(x = "Income Decile Group", y = "Real consumpition Change(%)", fill = "Scenario", color = "Scenario") +
        theme_minimal() +
        coord_cartesian(ylim = quantile(plotdf$relChange_pc_vs_base, probs = c(0.02, 0.99), na.rm = TRUE)) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom",
          legend.direction = "horizontal"
        )
      ,
      
      width = 5,
      height = 3 
      
    )
    
  }
  
  if(any(plotlist == "welfByPeriodShaded") | allExport){
    
    dataDecileNeut <- dfAnchRealCons %>% 
      filter(scheme == 'Neut') %>%
      select(scenario,period,region,relChange_pc_vs_base) %>%
      mutate(scenario = factor(scenario, levels = scenario_levels))
    
    dataDecileEpc <- dfAnchRealCons %>% 
      filter(scheme == 'EPC') %>%
      select(scenario,period,region,relChange_pc_vs_base) %>%
      mutate(scenario = factor(scenario, levels = scenario_levels))
    
    dataDecile <- bind_rows(
      dataDecileNeut %>% mutate(category = "Neut"),
      dataDecileEpc  %>% mutate(category = "EPC")
    ) %>%
      mutate(
        scenario = factor(scenario, levels = scenario_levels),
        category  = factor(category,  levels = c("Neut", "EPC")),
        period    = as.integer(period)
      ) 
    plotdf <- dataDecile %>%
      group_by(period, scenario, category) %>%
      summarise(
        med = median(relChange_pc_vs_base, na.rm = TRUE),
        p25 = quantile(relChange_pc_vs_base, 0.25, na.rm = TRUE),
        p75 = quantile(relChange_pc_vs_base, 0.75, na.rm = TRUE),
        mean = mean(relChange_pc_vs_base, na.rm = TRUE),
        .groups = "drop"
      )
    
    
    p[['welfByPeriodShaded']] <- list(
      
      plot = ggplot(
        plotdf,
        aes(x = period, y = med, color = scenario, linetype = category, group = interaction(scenario, category))
      ) +
        # optional: show where transfer tends to lose relevance (adjust if you want)
        annotate("rect", xmin = 2070, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.06) +
        geom_hline(yintercept = 0, linewidth = 0.4) +
        geom_ribbon(aes(ymin = p25, ymax = p75, fill = scenario), alpha = 0.12, color = NA, show.legend = FALSE) +
        geom_line(linewidth = 0.8) +
        geom_point(size = 2) +
        scale_color_manual(
          name = "Scenario",
          values = myScenPalette,
          breaks = c("C_SSP2-PkBudg1000", "C_SSP2-PkBudg650", 
                     "C_SSP2-hiOS-def", "C_SSP2-loOS-def"),
          labels = scenario_labels
        ) +
        scale_fill_manual(
          values = myScenPalette,
          breaks = c("C_SSP2-PkBudg1000", "C_SSP2-PkBudg650", 
                     "C_SSP2-hiOS-def", "C_SSP2-loOS-def"),
          labels = scenario_labels,
          guide = "none"
        ) +
        scale_linetype_manual(
          name = "Compensation",
          values = c("Neut" = "solid", "EPC" = "dashed"),
          labels = c("Neut" = "Neut", "EPC" = "EPC")
        ) +
        scale_x_continuous(breaks = sort(unique(plotdf$period))) +
        labs(
          x = "Year",
          y = "Real consumption change (%)"
        ) +
        guides(color = guide_legend(order = 2),
               linetype = guide_legend(order = 1)) +
        theme_bw() +
        theme(legend.position = "bottom", panel.grid.minor = element_blank(),
              axis.text.x = element_text(angle = 45, hjust = 1))
      ,
      
      width = 6,
      height = 4
      
    )
    
    
    
  }
  
  if(any(plotlist == "welfByPeriodShadedD1") | allExport){
    
    dataDecileNeut <- dfAnchRealCons %>% 
      filter(scheme == 'Neut') %>%
      select(scenario,period,region,decileGroup,relChange_pc_vs_base) %>%
      mutate(scenario = factor(scenario, levels = scenario_levels))
    
    dataDecileEpc <- dfAnchRealCons %>% 
      filter(scheme == 'EPC') %>%
      select(scenario,period,region,decileGroup,relChange_pc_vs_base) %>%
      mutate(scenario = factor(scenario, levels = scenario_levels))
    
    loPattern <- "loOS"   # low overshoot scenarios contain this substring
    hoPattern <- "hiOS"   # high overshoot scenarios contain this substring
    
    dataDecile <- bind_rows(
      dataDecileNeut %>% mutate(transfer = "Neut"),
      dataDecileEpc  %>% mutate(transfer = "EPC")
    ) %>%
      filter(decileGroup == 1) %>%
      mutate(
        scenario = case_when(
          str_detect(as.character(scenario), loPattern) ~ "1.5°C LO",
          str_detect(as.character(scenario), hoPattern) ~ "1.5°C HO",
          TRUE ~ NA_character_
        ),
        scenario = factor(scenario, levels = c("1.5°C LO", "1.5°C HO")),
        transfer  = factor(transfer,  levels = c("Neut", "EPC")),
        period    = as.integer(period)
      ) 
    
    plotdf <- dataDecile %>%
      group_by(period, scenario, region, transfer) %>%
      summarise(relChange_pc_vs_base = mean(relChange_pc_vs_base, na.rm =T)) %>%
      group_by(period, scenario, transfer) %>%
      summarise(
        med = median(relChange_pc_vs_base, na.rm = TRUE),
        p25 = quantile(relChange_pc_vs_base, 0.25, na.rm = TRUE),
        p75 = quantile(relChange_pc_vs_base, 0.75, na.rm = TRUE),
        mean = mean(relChange_pc_vs_base, na.rm = TRUE),
        .groups = "drop"
      )
    
    
    p[['welfByPeriodShadedD1']] <- list(
      
      plot = ggplot(
        plotdf,
        aes(x = period, y = med, color = scenario, linetype = transfer, group = interaction(scenario, transfer))
      ) +
        # optional: show where transfer tends to lose relevance (adjust if you want)
        annotate("rect", xmin = 2070, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.06) +
        geom_hline(yintercept = 0, linewidth = 0.4) +
        geom_ribbon(aes(ymin = p25, ymax = p75, fill = scenario), alpha = 0.12, color = NA, show.legend = FALSE) +
        geom_line(linewidth = 1.0) +
        geom_point(size = 1.8) +
        scale_color_manual(
          values = c(
            "1.5°C LO" = "#779ec6",
            "1.5°C HO" = "#e8ab67"
          )
        ) +
        scale_fill_manual(
          values = c(
            "1.5°C LO" = "#779ec6",
            "1.5°C HO" = "#e8ab67"
          )
        ) +
        scale_x_continuous(breaks = sort(unique(plotdf$period))) +
        labs(
          x = "Year",
          y = "Real consumption change (%)",
          color = "Scenario",
          linetype = "Transfers",
          subtitle = "All income deciles"
        ) +
        theme_bw() +
        theme(legend.position = "bottom", panel.grid.minor = element_blank())
      ,
      
      width = 5,
      height = 3
      
    )
    
    
  }
  
#---------------------End:Plots all aggregated----------------------------------  

  

#-------------------------2.reg plots all aggregated(welfare change)------------  
  
  if(any(plotlist == "welfByDecileRegNeut") | allExport){
    
    plotdf <- dfAnchRealCons %>% 
      filter(scheme == 'Neut') %>%
      select(scenario, period, region, decileGroup, relChange_pc_vs_base) %>%
      mutate(scenario = factor(scenario, levels = scenario_levels))
    
    avg_df_decile <- plotdf %>%
      group_by(scenario,decileGroup, region) %>%
      summarise(meanRelChange = mean(relChange_pc_vs_base, na.rm = TRUE), .groups = "drop") %>%
      mutate(scenario = factor(scenario, levels = scenario_levels))
    
    p[["welfByDecileRegNeut"]] <- list(
      
      plot = ggplot(plotdf, 
                    aes(x = factor(decileGroup), y = relChange_pc_vs_base, fill = scenario)) +
        geom_boxplot(outlier.shape = NA, width = 0.5, 
                     position = position_dodge(width = 0.6), color = "grey20") +
        geom_jitter(aes(color = scenario),
                    position = position_jitterdodge(jitter.width = 0.5, dodge.width = 0.5), 
                    alpha = 0.3, size = 0.2) +
        
        # 🔹 Add mean lines by scenario
        geom_line(data = avg_df_decile, 
                  aes(x = decileGroup, y = meanRelChange, group = scenario, color = scenario), 
                  size = 0.7) +
        scale_fill_manual(
          values = myScenPalette,
          
          labels = scenario_labels
          
        ) +
        scale_color_manual(
          values = myScenPalette,
          labels = scenario_labels
        ) +
        stat_summary(fun = mean, geom = "point", 
                     aes(group = scenario), 
                     position = position_dodge(width = 0.7), 
                     shape = 20, size = 2.5, color = "gray30") +
        
        labs(x = "Income Decile Group", y = "Real Consumption Change (%)", fill = "Scenario", color = "Scenario") +
        facet_wrap(~ region)+
        theme_minimal() +
        coord_cartesian(ylim = quantile(plotdf$relChange_pc_vs_base, probs = c(0.01, 0.99), na.rm = TRUE)) +
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
  
  if(any(plotlist == "welfByDecileRegEpc") | allExport){
    
    plotdf <- dfAnchRealCons %>% 
      filter(scheme == 'EPC') %>%
      select(scenario, period, region, decileGroup, relChange_pc_vs_base) %>%
      mutate(scenario = factor(scenario, levels = scenario_levels))
    
    avg_df_decile <- plotdf %>%
      group_by(scenario,decileGroup, region) %>%
      summarise(meanRelChange = mean(relChange_pc_vs_base, na.rm = TRUE), .groups = "drop") %>%
      mutate(scenario = factor(scenario, levels = scenario_levels))
    
    
    
    p[["welfByDecileRegEpc"]] <- list(
      
      plot = ggplot(plotdf, 
                    aes(x = factor(decileGroup), y = relChange_pc_vs_base, fill = scenario)) +
        geom_boxplot(outlier.shape = NA, width = 0.5, 
                     position = position_dodge(width = 0.6), color = "grey20") +
        geom_jitter(aes(color = scenario),
                    position = position_jitterdodge(jitter.width = 0.5, dodge.width = 0.5), 
                    alpha = 0.3, size = 0.2) +
        
        # 🔹 Add mean lines by scenario
        geom_line(data = avg_df_decile, 
                  aes(x = decileGroup, y = meanRelChange, group = scenario, color = scenario), 
                  size = 0.7) +
        scale_fill_manual(
          values = myScenPalette,
          
          labels = scenario_labels
          
        ) +
        scale_color_manual(
          values = myScenPalette,
          labels = scenario_labels
        ) +
        stat_summary(fun = mean, geom = "point", 
                     aes(group = scenario), 
                     position = position_dodge(width = 0.7), 
                     shape = 20, size = 2.5, color = "gray30") +
        
        labs(x = "Income Decile Group", y = "Real Consumption Change (%)", fill = "Scenario", color = "Scenario") +
        facet_wrap(~ region)+
        theme_minimal() +
        coord_cartesian(ylim = quantile(plotdf$relChange_pc_vs_base, probs = c(0.01, 0.99), na.rm = TRUE)) +
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
  
  if(any(plotlist=='welfByPeriodRegNeut')| allExport ){
    
    plotdf <- dfAnchRealCons %>% 
      filter(scheme == 'Neut') %>%
      select(scenario, period, region, decileGroup, relChange_pc_vs_base) %>%
      mutate(scenario = factor(scenario, levels = scenario_levels))
    
    avg_df_period <- plotdf %>%
      group_by(scenario, period, region) %>%
      summarise(meanRelChange = mean(relChange_pc_vs_base, na.rm = TRUE), .groups = "drop") %>%
      mutate(scenario = factor(scenario, levels = scenario_levels))
    
    
    p[["welfByPeriodRegNeut"]] <- list(
      plot = ggplot(plotdf, 
                    aes(x = period, y = relChange_pc_vs_base, fill = scenario)) +
        
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
                  aes(x = period - 1, y = meanRelChange),
                  color = "#ba7a31", size = 0.7) +
        
        geom_line(data = avg_df_period %>% filter(scenario %in% c("C_SSP2-PkBudg650","C_SSP2-loOS-def"), period >= 2025),
                  aes(x = period + 1, y = meanRelChange),
                  color = "#264f78", size = 0.7)+
      
        scale_fill_manual(
          values = myScenPalette,
          labels = scenario_labels
        ) +
        scale_color_manual(
          values = myScenPalette,
          labels = scenario_labels
        ) +
        scale_x_continuous(breaks = unique(plotdf$period),
                           labels = unique(plotdf$period)) +
        
        # Labels and styling
        labs(x = "Period", y = "Real Consumption Change (%)", fill = "Scenario", color = "Scenario") +
        coord_cartesian(ylim = quantile(plotdf$relChange_pc_vs_base, probs = c(0.002, 0.99), na.rm = TRUE)) +
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
  
  if(any(plotlist=='welfByPeriodRegEpc')| allExport ){
    
    plotdf <- dfAnchRealCons %>% 
      filter(scheme == 'EPC') %>%
      select(scenario, period, region, decileGroup, relChange_pc_vs_base) %>%
      mutate(scenario = factor(scenario, levels = scenario_levels))
    
    avg_df_period <- plotdf %>%
      group_by(scenario, period, region) %>%
      summarise(meanRelChange = mean(relChange_pc_vs_base, na.rm = TRUE), .groups = "drop") %>%
      mutate(scenario = factor(scenario, levels = scenario_levels))
    
    p[["welfByPeriodRegEpc"]] <- list(
      plot = ggplot(plotdf, 
                    aes(x = period, y = relChange_pc_vs_base, fill = scenario)) +
        
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
                  aes(x = period - 1, y = meanRelChange),
                  color = "#ba7a31", size = 0.7) +
        
        geom_line(data = avg_df_period %>% filter(scenario %in% c("C_SSP2-PkBudg650","C_SSP2-loOS-def"), period >= 2025),
                  aes(x = period + 1, y = meanRelChange),
                  color = "#264f78", size = 0.7)+
        
        scale_fill_manual(
          values = myScenPalette,
          labels = scenario_labels
        ) +
        scale_color_manual(
          values = myScenPalette,
          labels = scenario_labels
        ) +
        scale_x_continuous(breaks = unique(plotdf$period),
                           labels = unique(plotdf$period)) +
        
        # Labels and styling
        labs(x = "Period", y = "Real Consumption Change (%)", fill = "Scenario", color = "Scenario") +
        coord_cartesian(ylim = quantile(plotdf$relChange_pc_vs_base, probs = c(0.002, 0.99), na.rm = TRUE)) +
        facet_wrap(~ region)+
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
  
#-----------------------------End. 2.reg plots all aggregated------------------- 
  
  
  
  
  
#-----------3. Plot by individual channel:Global (COLI change log points)-------
  
  if(any(plotlist=='globalColiBySec')| allExport ){

    plotdf <- aggregate_decileWelfChange(data1 =  dfDecileWelfChange,
                                         data2 = dfDecileConsShare,
                                         secLevel = c("fullSec"),
                                         scope = 'global') %>%
      filter(category != 'Other commodities') %>%
      mutate(category = factor(category, levels = allSec))

    # Automate over all scenarios
    p[['globalColiBySec']] <- list(
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
          y = "Contribution to COLI change (log points)",
          fill = "Category"
        ) +
        theme_minimal() +
        #scale_fill_paletteer_d("ggprism::floral2") +
        #scale_fill_paletteer_d("beyonce::X82") +
        scale_fill_manual(values = mySecPalette, labels = categoryLabels) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))+
        guides(fill = guide_legend(reverse = TRUE))

      ,

      width = 8,
      height = 3

    )
  }

  if(any(plotlist=='regColiBySec')| allExport ){
    
    plotdf <- aggregate_decileWelfChange(data1 =  dfDecileWelfChange, 
                                         data2 = dfDecileConsShare, 
                                         secLevel = c("all"), 
                                         scope = 'region') %>%
      filter(category %in% c(foodSec,eneSec) ) %>%
      mutate(category = factor(category, levels = c(foodSec,eneSec)))

    # Automate over all scenarios
    p[['regColiBySec']] <- list(
      plot = ggplot(plotdf, aes(x = factor(period), y = welfChange, fill = category)) +
        geom_col(position = "stack") +
        geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
        facet_wrap(~ region + scenario, ncol = 4,
                   scales = "free_y") +
        labs(
          x = "Year",
          y = "Contribution to COLI change (log points)",
          fill = "Category"
        ) +
        theme_minimal() +
        scale_fill_manual(values = mySecPalette, labels = categoryLabels) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        guides(fill = guide_legend(reverse = TRUE))
      ,
      
      width = 8,
      height = 8
      
    )
  }
  
  if(any(plotlist=='regColiBySecSelect')| allExport ){

    
    plotdfWorld <- aggregate_decileWelfChange(data1 =  dfDecileWelfChange,
                                         data2 = dfDecileConsShare,
                                         secLevel = c("fullSec"),
                                         scope = 'global') %>%
      filter(category != 'Other commodities') %>%
      mutate(category = factor(category, levels = c(foodSec,eneSec)),
             region = 'World')
    
    plotdfReg <- aggregate_decileWelfChange(data1 =  dfDecileWelfChange, 
                                         data2 = dfDecileConsShare, 
                                         secLevel = c("fullSec"), 
                                         scope = 'region') %>%
      filter(category != 'Other commodities',
             region %in% regSelect) %>%
      mutate(category = factor(category, levels = c(foodSec,eneSec)))
    
    plotdf <- plotdfWorld %>%
      rbind(plotdfReg) %>%
      mutate(region = factor(region, levels = c('World',regSelect)),
             scenario = factor(scenario, levels = scenario_levels))
    
    # Automate over all scenarios
    p[['regColiBySecSelect']] <- list(
      plot = ggplot(plotdf %>% filter(region != 'World'), aes(x = period, y = - welfChange, fill = category)) +
        geom_area(position = "stack", alpha = 0.9) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
        facet_grid(
          region ~ scenario,
          scales = "free_y",
          labeller = labeller(scenario = as_labeller(scenario_labels),
                             region = as_labeller(region_labels))
        ) +
        scale_x_continuous(breaks = sort(unique(plotdf$period))) +
        scale_fill_manual(values = mySecPalette, labels = categoryLabels) +
        labs(
          x = "Year",
          y = "Contribution to COLI change (log points)",
          fill = "Category"
        ) +
        theme_minimal() +
        theme(strip.text.y = element_text(angle = 0, hjust = 0),
              strip.background = element_blank(),
              axis.text.x = element_text(angle = 45, hjust = 1),
              panel.grid.minor = element_blank()) +
        guides(fill = guide_legend(reverse = TRUE))
      ,
      
      width = 8,
      height = 8
      
    )
  }
  
  if(any(plotlist == 'regColiBySec' | allExport  )){
    
    plotdf <- aggregate_decileWelfChange(data1 =  dfDecileWelfChange, 
                                         data2 = dfDecileConsShare, 
                                         secLevel = c("all"), 
                                         scope = 'region') %>%
      filter(category %in% c(foodSec,eneSec) ) %>%
      mutate(category = factor(category, levels = c(foodSec,eneSec)))
    

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
            y = "Contribution to COLI change (log points)",
            fill = "Category"
          ) +
          theme_minimal() +
          scale_fill_manual(values = mySecPalette, labels = categoryLabels) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)),
        
        width = 10,
        height = 8
        
      ) 
      
  }
    
}
      
#----------End 3 Plot by individual channel:regional (COLI change log points)---------
   

  
  
  
  
#----------------------4 global inequality effect (Gini/Theil)----------------
  
  if(any(plotlist == 'ineqWorld_Gini' | allExport  )){
    
    plotdf <- dfIneqAll$ineqAll %>%
      filter(region == 'World',
             variable == "ineq|Gini")
    
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
                     scenario_labels
                   )
                   ) +
        # Labels and styling
        labs(x = "Year", y = "Inequality metrics") +
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
    
    plotdf <- dfIneqAll$ineqAll %>%
      filter(region == 'World',
             variable == "ineq|Gini")
    
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
      )),
            category = recode(category,
                            "TotalWithTransfNeut" = "Neut",
                            "TotalWithTransfEpc" = "EPC"),
            category = factor(category, levels = c("Neut", "EPC")))

    
    
    # Automate over all scenarios
    p[['ineqWorldWithTransf_GiniRela']] <- list(
      
      plot = ggplot(plotdf, 
                    aes(x = period, y = value, linetype = category, color = scenario)) +
        geom_line(size = 0.8) + 
        geom_point(size = 2) +
        scale_color_manual(
          name   = "Scenario",  # legend title
          values = myScenPalette,
          breaks = c("C_SSP2-PkBudg1000", "C_SSP2-PkBudg650", 
                     "C_SSP2-hiOS-def", "C_SSP2-loOS-def"),
          labels = scenario_labels
        ) +
        scale_linetype_manual(name = "Compensation",
                              values = c("Neut" = "solid", 
                                         "EPC" = "dashed"),
                              labels = c("Neut" = "Neut",
                                         "EPC" = "EPC")) +
        scale_x_continuous(breaks = unique(plotdf$period),
                           labels = unique(plotdf$period)) +
        geom_hline(yintercept = 0, linetype = "solid", color = "grey80")+
        # Labels and styling
        labs(x = "Year", y = "Global Gini change from reference (pp)") +
        coord_cartesian(ylim = quantile(plotdf$value, probs = c(0.01, 0.999), na.rm = TRUE)) +
        guides(color = guide_legend(order = 2),
               linetype = guide_legend(order = 1)) +
        theme_bw() +
        theme(axis.title.x = element_blank(), legend.position = "bottom", panel.grid.minor = element_blank(),
              axis.text.x = element_text(angle = 45, hjust = 1))
      ,
      
      width = 5,
      height = 4
      
    )
    
    
    
  }
  
  if(any(plotlist == 'ineqWorldWithTransf_TheilLRela' | allExport  )){
    
    plotdf <- dfIneqAll$ineqAll %>%
      filter(region == 'World',
             variable == "ineq|TheilL")
    
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
          labels = scenario_labels
        )+
        geom_hline(yintercept = 0, linetype = "solid", color = "grey80")+
        # Labels and styling
        labs(x = "Year", y = "Theil'L change from reference") +
        coord_cartesian(ylim = quantile(plotdf$value, probs = c(0.01, 0.999), na.rm = TRUE)) +
        scale_linetype_manual(name = "Compensation",
                              values = c("TotalWithTransfNeut" = "solid", 
                                         "TotalWithTransfEpc" = "dashed"),
                              labels = c("TotalWithTransfNeut" = "Neutral",
                                         "TotalWithTransfEpc" = "EPC")) +
        scale_x_continuous(breaks = unique(plotdf$period),
                           labels = unique(plotdf$period)) +
        theme(axis.title.x = element_blank())
      ,
      
      width = 5,
      height = 4
      
    )
    
    
    
  }
  
  if(any(plotlist == 'ineqWorldWithTransf_TheilTRela' | allExport  )){
    
    plotdf <- dfIneqAll$ineqAll %>%
      filter(region == 'World',
             variable == "ineq|TheilT")
    
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
        labs(x = "Year", y = "Theil'T change from reference") +
        coord_cartesian(ylim = quantile(plotdf$value, probs = c(0.01, 0.999), na.rm = TRUE)) +
        scale_linetype_manual(name = "Compensation",
                              values = c("TotalWithTransfNeut" = "solid", 
                                         "TotalWithTransfEpc" = "dashed"),
                              labels = c("TotalWithTransfNeut" = "Neutral",
                                         "TotalWithTransfEpc" = "EPC")) +
        scale_x_continuous(breaks = unique(plotdf$period),
                           labels = unique(plotdf$period)) +
        theme(axis.title.x = element_blank())
      ,
      
      width = 5,
      height = 4
      
    )
    
    
    
  }
  
#-----------------End 4 global inequality effect (Gini/Theil)----------------

  
  
  
  
  
  
  
#----------------------5 regional inequality effect----------------
  
  if(any(plotlist == 'ineqReg_Gini' | allExport  )){
    
    plotdf <- dfIneqAll$ineqAll %>%
      filter(region != 'World',
             variable == "ineq|Gini")
    
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
    
    plotdf <-  dfIneqAll$ineqAll %>%
      filter(region != 'World',
             variable == "ineq|Gini")
    
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
          labels = scenario_labels
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
    
    plotdf <- dfIneqAll$ineqAll %>%
      filter(region != 'World',
             variable == "ineq|TheilL")
    
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
          labels = scenario_labels
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
  
  if(any(plotlist == 'ineqReg_TheilTRela' | allExport  )){
    
    plotdf <- dfIneqAll$ineqAll %>%
      filter(region != 'World',
             variable == "ineq|TheilT")
    
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
    p[['ineqReg_TheilTRela']] <- list(
      
      plot = ggplot(plotdf, 
                    aes(x = period, y = value, 
                        linetype = category, color = scenario)) +
        geom_line() + 
        geom_hline(yintercept = 0)+
        facet_wrap(~region, ncol = 3,
                   scales = "free_y" ) +
        labs(x = "Decile", y = "Theil'T Change from reference (points)", 
             color = "Measure", linetype = "Scenario-State") +
        #coord_cartesian(ylim = quantile(plotdf$value, probs = c(0.01, 0.99), na.rm = TRUE)) +
        scale_color_manual(
          name   = "Scenario",  # legend title
          values = myScenPalette,
          breaks = c("C_SSP2-PkBudg1000", "C_SSP2-PkBudg650", 
                     "C_SSP2-hiOS-def", "C_SSP2-loOS-def"),
          labels = scenario_labels
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
  
  
  

  
  
  
#-------------5.1 regional inequality effect by Channel ----------------
  
  if(any(plotlist == 'ineqRegBySec_Gini' | allExport  )){
    
    plotdf <- dfIneqChannel %>% 
      filter(region != 'World',
             category != 'Other commodities',
             variable == 'ineq|shapleyGini') %>%
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
      mutate(scenario = factor(scenario, levels = scenario_levels)) 
    
    totals <- plotdf %>%
      group_by(region, period_fac, period_num, scen_off, scenario, x) %>%
      summarise(total = sum(value, na.rm = TRUE)+0.005, .groups = "drop") %>%
      mutate(scenario = factor(scenario, levels = scenario_levels)) 
    
    p[[paste0('ineqRegBySec_Gini')]] <- list(
    
      plot = ggplot(plotdf, aes(x = x, y = value * 100, fill = category)) +
        geom_col(width = 0.40) +
        # add the scenario markers
        geom_point(
          data = totals,
          aes(x = x, y = total * 100, shape = scenario),
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
        labels = scenario_labels
        )+
        facet_wrap(~ region, ncol = 3) +
        scale_fill_manual(values = mySecPalette, labels = categoryLabels) +
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
  
  if(any(plotlist == 'ineqRegBySecSelected_Gini' | allExport  )){
    
    plotdf <- dfIneqChannel %>% 
      filter(region %in% regSelect,
             category != 'Other commodities',
             variable == 'ineq|shapleyGini') %>%
      mutate(scenario = factor(scenario, levels = scenario_levels),
             category = factor(category, levels = c(foodSec,eneSec)),
             region = factor(region, levels = regSelect)) 
    
    
    p[[paste0('ineqRegBySecSelected_Gini')]] <- list(
      
      plot = ggplot(plotdf, aes(x = period, y = value * 100, fill = category)) +
        geom_col() +
        # add the scenario markers
        facet_grid(
          region ~ scenario,
          scales = "free_y",
          labeller = labeller(scenario = as_labeller(scenario_labels),
                             region = as_labeller(region_labels))
        )+
        scale_fill_manual(values = mySecPalette, labels = categoryLabels) +
        scale_x_continuous(breaks = sort(unique(plotdf$period))) +
        labs(x = "Year",
             y = "Regional Gini change from reference (pp)",
             fill = "Category") +
        theme_minimal() +
        theme(strip.text.y = element_text(angle = 0, hjust = 0),
              strip.background = element_blank(),
              axis.text.x = element_text(angle = 45, hjust = 1),
              panel.grid.minor = element_blank()) +
        guides(fill = guide_legend(reverse = TRUE))
      ,
      
      width = 10,
      height = 8
      
    )
    
    
    
  }
  
  if(any(plotlist == 'ineqRegBySec_TheilT' | allExport  )){
  
  plotdf <-  dfIneqChannel %>% 
    filter(region != 'World',
           category != 'Other commodities',
           variable == 'ineq|shapleyTheilT') %>%
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
    mutate(scenario = factor(scenario, levels = scenario_levels)) 
  
  
  totals <- plotdf %>%
    group_by(region, period_fac, period_num, scen_off, scenario, x) %>%
    summarise(total = sum(value, na.rm = TRUE)+0.005, .groups = "drop") %>%
    mutate(scenario = factor(scenario, levels = scenario_levels)) 
  
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
        labels = scenario_labels
      )+
      facet_wrap(~ region, ncol = 3,
                 labeller = as_labeller(region_labels)) +
      scale_fill_manual(values = mySecPalette, labels = categoryLabels) +
      scale_x_continuous(breaks = sort(unique(plotdf$period))) +
      scale_x_continuous( breaks = sort(unique(plotdf$period_num)), 
                          labels = levels(plotdf$period_fac) ) +
      labs(x = "Year",
           y = "Theil'T change from reference",
           fill = "Category",
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
      )
    ,
    
    width = 10,
    height = 8
  )
  
  
  
  }
  
  if(any(plotlist == 'ineqRegBySec_TheilL' | allExport  )){
    
    plotdf <-  dfIneqChannel %>% 
      filter(region != 'World',
             category != 'Other commodities',
             variable == 'ineq|shapleyTheilL') %>%
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
      mutate(scenario = factor(scenario, levels = scenario_levels)) 
    
    
    totals <- plotdf %>%
      group_by(region, period_fac, period_num, scen_off, scenario, x) %>%
      summarise(total = sum(value, na.rm = TRUE) + 0.005, .groups = "drop") %>%
      mutate(scenario = factor(scenario, levels = scenario_levels)) 
    
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
          labels = scenario_labels
        )+
        facet_wrap(~ region, ncol = 3) +
        scale_fill_manual(values = mySecPalette, labels = categoryLabels) +
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
  
#-------------End 5.3 regional inequality effect by Channel-----------
  
  
  
  
  
  
#-------------5.2 regional inequality contribution to Global----------------
  
  #inequality Contribution of between and within country inequality
  if(any(plotlist == 'ineqGlobalBetweenWithinTheilT_Neut' | allExport  )){
    
    plotdf <- dfIneqAll$theilTDecomp %>%
      filter(scheme == 'Neut') %>%
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
      mutate(scenario = factor(scenario, levels = scenario_levels ))
    
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
                     scenario_labels
                     )) +
        labs(
          x = "Year",
          y = "Change in Inequality (Theil'T)",
          title = "Theil Inequality Decomposition over Time by Scenario",
          fill = "Component"
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
    
    plotdf <- dfIneqAll$theilLDecomp %>%
      filter(scheme == 'Neut') %>%
      mutate(`Tl.i|delta` = Tl.i - `Tl.i|base`,
             `Tl.b|delta` = Tl.b - `Tl.b|base`,
             `Tl.iw|delta` = Tl.iw - `Tl.iw|base`) %>%
      group_by(scenario,period) %>%
      summarise( `Tl.i|delta` = sum(`Tl.i|delta`),
                 `Tl.b|delta` = sum(`Tl.b|delta`),
                 `Tl.iw|delta` = sum(`Tl.iw|delta`),
                 .groups = "drop") %>%
      pivot_longer(cols = c(`Tl.b|delta`, `Tl.iw|delta`, `Tl.i|delta`),
                   names_to = "component",
                   values_to = "value") %>%
      filter(period <= 2100) %>%
      mutate(component = recode(component,
                                "Tl.b|delta" = "Between",
                                "Tl.iw|delta" = "Within",
                                "Tl.i|delta" = "Total")) %>%
      mutate(scenario = factor(scenario, levels = scenario_levels ))
    
    p[['ineqGlobalBetweenWithinTheilL_Neut']] <- list(
      
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
                     scenario_labels
                   )) +
        labs(
          x = "Year",
          y = "Change in Inequality (Theil'L)",
          title = "Theil Inequality Decomposition over Time by Scenario",
          fill = "Component"
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
    
    plotdf <- dfIneqAll$theilTDecomp %>%
      filter(scheme == 'EPC') %>%
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
      mutate(scenario = factor(scenario, levels = scenario_levels ))
    
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
                     scenario_labels
                   )) +
        labs(
          x = "Year",
          y = "Change in Inequality (Theil'T)",
          title = "Theil Inequality Decomposition over Time by Scenario",
          fill = "Component"
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
    
    plotdf <- dfIneqAll$theilLDecomp %>%
      filter(scheme == 'EPC') %>%
      mutate(`Tl.i|delta` = Tl.i - `Tl.i|base`,
             `Tl.b|delta` = Tl.b - `Tl.b|base`,
             `Tl.iw|delta` = Tl.iw - `Tl.iw|base`) %>%
      group_by(scenario,period) %>%
      summarise( `Tl.i|delta` = sum(`Tl.i|delta`),
                 `Tl.b|delta` = sum(`Tl.b|delta`),
                 `Tl.iw|delta` = sum(`Tl.iw|delta`),
                 .groups = "drop") %>%
      pivot_longer(cols = c(`Tl.b|delta`, `Tl.iw|delta`, `Tl.i|delta`),
                   names_to = "component",
                   values_to = "value") %>%
      filter(period <= 2100) %>%
      mutate(component = recode(component,
                                "Tl.b|delta" = "Between",
                                "Tl.iw|delta" = "Within",
                                "Tl.i|delta" = "Total")) %>%
      mutate(scenario = factor(scenario, levels = scenario_levels ))
    
    p[['ineqGlobalBetweenWithinTheilL_Neut']] <- list(
      
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
                     scenario_labels
                   )) +
        labs(
          x = "Year",
          y = "Change in Inequality (Theil'L)",
          title = "Theil Inequality Decomposition over Time by Scenario",
          fill = "Component"
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
      
      plotdf <- dfIneqAll$theilTDecomp %>%
        filter(scheme == 'EPC') %>%
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
        mutate(scenario = factor(scenario, levels = scenario_levels))

      
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
            fill = "Component"
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
     
    plotdf <-  dfIneqAll$theilLDecomp %>%
      filter(scheme == 'EPC') %>%
        mutate(`Tl.i|delta` = Tl.i - `Tl.i|base`,
               `Tl.b|delta` = Tl.b - `Tl.b|base`,
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
        mutate(scenario = factor(scenario, levels = scenario_levels))
      
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
            fill = "Component"
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
      
    plotdf <- dfIneqAll$theilTDecomp %>%
      filter(scheme == 'Neut') %>%
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
      mutate(scenario = factor(scenario, levels =scenario_levels))
    
    
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
          fill = "Component"
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
    
    plotdf <-  dfIneqAll$theilLDecomp %>%
      filter(scheme == 'Neut') %>%
      mutate(`Tl.i|delta` = Tl.i - `Tl.i|base`,
             `Tl.b|delta` = Tl.b - `Tl.b|base`,
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
      mutate(scenario = factor(scenario, levels = scenario_levels))
    
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
          fill = "Component"
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
  
#-------------End 5.2 regional inequality effect by Channel (Theil)-----------
  
  
  
#---------------------------6 Global tax revenue -------------------------------
  
  if(any(plotlist == 'taxRevenueOptionWorld' | allExport  )){
    
    revenue1 <- compute_transfer(dfData, dfDecileConsShare, taxBase = 'GHG',
                                 climaFund = 0,
                                 fund_return_scale = 1,
                                 payg = 1, revenueRep = 1)
    
    revenue2 <- compute_transfer(dfData, dfDecileConsShare, taxBase = 'GHGwoLUC',
                                 climaFund = 0,
                                 fund_return_scale = 1,
                                 payg = 1, revenueRep = 1)
    
    revenue3 <- compute_transfer(dfData, dfDecileConsShare, taxBase = 'CO2woLUC',
                                 climaFund = 0,
                                 fund_return_scale = 1,
                                 payg = 1, revenueRep = 1)
    
    rev_df <- bind_rows(
      revenue1 %>% mutate(taxBase = "GHG"),
      revenue2 %>% mutate(taxBase = "GHG wo LUC"),
      revenue3 %>% mutate(taxBase = "CO2 wo LUC")
    ) %>%
      group_by(period, scenario, taxBase) %>%
      summarise(
        revenue = sum(revenue, na.rm = TRUE),
        .groups = "drop"
      )
    
    gdp_scen <- dfData %>%
      filter(
        region == "World",
        variable == "GDP|MER",
        scenario %in% unique(rev_df$scenario)
      ) %>%
      transmute(
        period   = as.numeric(period),
        scenario = scenario,
        gdpMER   = value
      ) 
    
    plotdf <- rev_df %>%
      mutate(period = as.numeric(period)) %>%
      left_join(gdp_scen, by = c("period", "scenario")) %>%
      mutate(
        revShareGDP = 100 * revenue / gdpMER
      ) %>%
      filter(!is.na(gdpMER)) %>%
      mutate(
        taxBase = factor(
          taxBase,
          levels = c("GHG", "GHG wo LUC", "CO2 wo LUC")
        )
      )
    
    p[[paste0("taxRevenueOptionWorld")]] <- list(
      plot = ggplot( plotdf, aes(
                       x = period,
                       y = revShareGDP,
                       color = scenario,
                       linetype = taxBase,
                       group = interaction(scenario, taxBase)
                       )) +
        geom_hline(yintercept = 0, linewidth = 0.4) +
        geom_line(linewidth = 1) +
        geom_point(shape = 17, size = 2) +
        scale_color_manual(
          values = myScenPalette,
          labels = stringr::str_wrap(scenario_labels, 25)
        ) +
        scale_linetype_manual(
          values = c("solid", "dashed", "dotted"),
          labels = stringr::str_wrap(levels(plotdf$taxBase), 25)
        ) +
        labs(
          x = "Year",
          y = "Revenue (% of World GDP|MER)",
          color = "Scenario",
          linetype = "Tax"
        ) +
        scale_x_continuous(breaks = sort(unique(plotdf$period))) +
        theme_bw() +
        theme(
          legend.position = "bottom",
          legend.box = "vertical",
          legend.text = element_text(lineheight = 0.8, size = 9),
          legend.key.height = unit(0.35, "lines"),
          legend.spacing.y = unit(0.05, "cm"),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
        
      ,
      
      width  = 6,
      height = 4
    )
    
    
    
    
    
  }

#-------------------End 6 Global tax revenue------------------------------------
    
  
#--------------------------7.categorization of sector driver-----------------
  
  if(any(plotlist == 'shockVsExposurebySec'| allExport)) {
    
    plotdf <- dfData %>% 
      filter(
        str_starts(variable, "relaPrice") |
          str_starts(variable, "share"),
        scenario != "C_SSP2-NPi2025",
        period %in% c(2035,2060,2100)
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
             period = factor(period, levels = c(2035,2060,2100))) %>%
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
          scale_color_manual(values = mySecPalette, labels = categoryLabels) +
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
          )
        ,
        
        width  = 10,
        height = 8
        
      )
    }
    
  }
  
  
  if(any(plotlist == 'categoryColiVsIneq' | allExport  )){
    
    mySecPalette_dark <- darken(mySecPalette, amount = 0.4)
    
    coli <- aggregate_decileWelfChange(
      data1 = dfDecileWelfChange,
      data2 = dfDecileConsShare,
      secLevel = "fullSec",
      scope = "region"
    ) %>%
      filter(category != "Other commodities",
             region %in% regSelect) %>%
      mutate(category = factor(category, levels = c(foodSec, eneSec)))
    
    ineq <- dfIneqChannel %>%
      filter(region %in% regSelect,
             category != "Other commodities",
             variable == "ineq|shapleyGini") %>%
      mutate(
        scenario = factor(scenario, levels = scenario_levels),
        category = factor(category, levels = c(foodSec, eneSec)),
        region   = factor(region, levels = regSelect)
      ) %>%
      transmute(scenario, region, period, category, ineq = value * 100)  # <-- scale up
    
    d <- inner_join(coli, ineq, by = c("scenario","region","period","category"))
    
    
    d_sum <- d %>%
      group_by(category) %>%
      summarise(
        x_med = median(-welfChange, na.rm = TRUE),
        y_med = median(ineq, na.rm = TRUE),
        x_lo  = quantile(-welfChange, 0.25, na.rm = TRUE),
        x_hi  = quantile(-welfChange, 0.75, na.rm = TRUE),
        y_lo  = quantile(ineq, 0.25, na.rm = TRUE),
        y_hi  = quantile(ineq, 0.75, na.rm = TRUE),
        .groups = "drop"
      )
    
    
    p[[paste0('categoryColiVsIneq')]] <- list(
      
      plot = ggplot(d_sum, aes(x = x_med, y = y_med, colour = category)) +
        geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3) +
        geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.3) +
        geom_errorbar(aes(ymin = y_lo, ymax = y_hi), width = 0) +
        geom_errorbarh(aes(xmin = x_lo, xmax = x_hi), width = 0) +   # <- changed
        geom_point(size = 3) +
        scale_colour_manual(values = mySecPalette) +
        ggnewscale::new_scale_colour() +
        ggrepel::geom_text_repel(
          aes(label = category, colour = category),
          size = 3,
          max.overlaps = Inf,          # <- changed
          show.legend = FALSE
        ) +
        scale_colour_manual(values = mySecPalette_dark) +
        
        theme_minimal() +
        theme(legend.position = "none") +
        labs(
          x = "Contribution to COLI change (log points)",
          y = "Contribution to change of Gini (pp)",
          colour = "Sector"
        )
      ,
      
      width = 4,
      height = 4
      
    )
    
    
    
  }
  
  
#----------------------End 7. categorization of sector drivers---------------
  
  
  
#--------------------------8. burden on deciles -----------------
  
  if(any(plotlist == 'decileColiGlobalbyDecile' | allExport  )){
    
    plotdf <- aggregate_decileWelfChange(data1 = dfDecileWelfChange, 
                                         data2 = dfDecileConsShare, 
                                         secLevel = c("fullSec"), 
                                         scope = 'globalDecile') %>%
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
          y = "Contribution to COLI change (log points)",
          fill = "Category"
        ) +
        #scale_fill_paletteer_d("NatParksPalettes::Olympic",direction = -1) +
        scale_fill_manual(values = mySecPalette, labels = categoryLabels)+
        guides(fill = guide_legend(reverse = T)) +
        theme(
          legend.position  = "right",
          legend.direction = "vertical"
        )
      
      
      p[[paste0("decileColiGlobalbyDecile",scen)]] <- list(
        plot = plot_base
        ,
        
        width  = 8,
        height = 5
        
      )
      iter = iter + 1
    }


    
    
    
    
    
    
    
  }
  
  if(any(plotlist == 'decileColiRegbyDecile' | allExport  )){
    
    plotdf <- aggregate_decileWelfChange(data1 = dfDecileWelfChange, 
                                         data2 = dfDecileConsShare, 
                                         secLevel = c("all"), 
                                         scope = 'decile') %>%
      filter(period %in% c('2030','2050','2100'),
             category %in% c(foodSec,eneSec)) %>%
      mutate(category = factor(category, levels = c(foodSec,eneSec)))
    
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
          y = "Contribution to COLI change (log points)",
          fill = "Category"
        ) +
        #scale_fill_paletteer_d("ggprism::floral2") +
        #scale_fill_paletteer_d("NatParksPalettes::Olympic") +
        scale_fill_manual(values = mySecPalette, labels = categoryLabels)+
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
      
      p[[paste0("decileColiRegbyDecile",scen)]] <- list(
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
    if(regions == 'H12') {
      
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
    
    gini <- dfIneqAll$ineqAll %>%
      filter( period <= 2100,
              region != 'World',
              variable %in% c('ineq|Gini'))

    gini_ref <- gini %>% filter(scenario == 'C_SSP2-NPi2025') %>%
      select(-scenario,-category) %>%
      rename(ref = value) %>%
      filter(period == 2040)

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
    
    ref <- dfIneqAll$ineqAll %>%
      filter(period == 2060, region != "World", variable == "ineq|Gini",
             scenario == "C_SSP2-NPi2025") %>%
      summarise(ref = mean(value, na.rm = TRUE), .by = region)
    
    regOrder <- dfIneqAll$ineqAll %>%
      filter(period == 2060, region != "World", variable == "ineq|Gini",
             category == "TotalWithTransfNeut", scenario == 'C_SSP2-loOS-def') %>%
      summarise(pol = mean(value, na.rm = TRUE), .by = region) %>%
      inner_join(ref, by = "region") %>%
      mutate(change = (pol - ref) * 100) %>%
      arrange(change) %>%
      pull(region)
    
    world_reg <- world_reg %>%
      mutate(region = factor(region, levels = regOrder))
    
    p[['remindRegionMap']] <- list(
      plot = ggplot(world_reg) +
        # REMIND regions: colored by region
        geom_sf(
          aes(fill = region),
          color    = "grey60",
          linewidth = 0.2
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
        # DISCRETE fill scale for regions with full names
        scale_fill_paletteer_d("PrettyCols::Summer", 
                               name = "REMIND region",
                               labels = region_labels) +
        guides(fill = "none") +
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
    
    ref <- dfIneqAll$ineqAll %>%
      filter(period == 2060, region != "World", variable == "ineq|Gini",
             scenario == "C_SSP2-NPi2025") %>%
      summarise(ref = mean(value, na.rm = TRUE), .by = region)
    
    regOrder <- dfIneqAll$ineqAll %>%
      filter(period == 2060, region != "World", variable == "ineq|Gini",
             category == "TotalWithTransfNeut", scenario == 'C_SSP2-loOS-def') %>%
      summarise(pol = mean(value, na.rm = TRUE), .by = region) %>%
      inner_join(ref, by = "region") %>%
      mutate(change = (pol - ref) * 100) %>%
      arrange(change) %>%
      pull(region)
    
    for(scen in paste0( 'C_',all_runscens,'-',all_budgets)){
      
      gini <- dfIneqAll$ineqAll %>%
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
        mutate(scenario = factor(scenario, levels = scenario_levels),
        category = factor(category, levels = c(
          'TotalWithTransfNeut',
          'TotalWithTransfEpc'
        ))) %>% 
        filter( period %in% c(2040, 2060, 2100),
                scenario == scen)
      
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
          scale_fill_paletteer_d("PrettyCols::Summer", 
                                 name = "REMIND region",
                                 labels = region_labels) +
          labs(
            x = NULL,
            y = "Gini change from NPi (pp)",
            # title    = "Gini change from reference",
            subtitle =  scenario_labels[[scen]]
          ) +
          facet_grid(category ~ period, 
                     labeller = labeller(
                       category = c(
                         TotalWithTransfNeut = "Neutral",
                         TotalWithTransfEpc  = "EPC"
                       ))) +
          theme_minimal() +
          theme(
            panel.grid.major.y = element_blank(),
            legend.position    = "right",
            panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.4),
            axis.text.y        = element_text(size = 8)
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
