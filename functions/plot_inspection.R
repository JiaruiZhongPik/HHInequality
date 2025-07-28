#This produces plots for inspecting shocks and exposures




plot_inspection <- function( outputPath, 
                             dataPrice,
                             dataExposure,
                             plotlist='NA' ,
                             isDisplay = T, isExport = FALSE, 
                             allExport=FALSE ) {
 
  p <- list()
  
  #-------------------1. inspecting price shocks--------------------------------
  if(any(plotlist == "shock") | allExport){
    
    
      plotdf <- dataPrice %>% 
        filter (grepl("^deltPrice", variable)) %>%
        mutate(variable = str_remove(variable, "^deltPrice\\|"))
      
      for(scen in unique(plotdf$scenario)){
        
        plotdf1 <- plotdf %>%
          filter( scenario == scen )
        
        p[[paste0('shock', scen)]] <- list(
          plot = ggplot(plotdf1, aes(x = period, y = value, color = variable)) +
            geom_line(linewidth = 1) +
            facet_wrap(~ region, scales = "free_y") +  # optional: free_y if regions differ a lot
            scale_color_paletteer_d("ggthemes::Classic_10") +
            labs(
              x = "Year",
              y = "Value",
              color = "Variable",
              linetype = "Scenario"
            ) +
            theme_minimal() +
            theme(
              legend.position = "bottom",
              axis.text.x = element_text(angle = 45, hjust = 1),
              strip.text = element_text(face = "bold")
            )
          
          ,
          
          width = 10,
          height = 8
          
        )
         
      }
      

      
   
    
  }
  
  #-------------------End. inspecting price shocks------------------------------
  
  
  #----------------2. inspecting exposure/expnditure share by region------------
  if(any(plotlist == "exposureByReg") | allExport){

    plotdf <- dataPrice %>%
      filter(region != 'World'
      ) %>%
      select(-baseline,-model) %>%
      calc_addVariable("FEShare|Household" = "(`FE|Buildings|Gases` * `Price|Buildings|Gases` +
                   `FE|Buildings|Electricity` * `Price|Buildings|Electricity`+
                   `FE|Buildings|Other fuels` * `Price|Buildings|Other fuels`+
                   `FE|++|Transport` * `Price|Transport|FE`) /
                   `Consumption`",
                       "share|Building gases" = "`FE|Buildings|Gases` * `Price|Buildings|Gases`/ `Consumption`",
                       "share|Building electricity" = "`FE|Buildings|Electricity` * `Price|Buildings|Electricity`/`Consumption`",
                       "share|Building other fuels" = "`FE|Buildings|Other fuels` * `Price|Buildings|Other fuels`/`Consumption`",
                       "share|Transport energy" = "`FE|++|Transport` * `Price|Transport|FE`/ `Consumption`"
      ) %>% 
      filter (grepl("^share", variable)) %>%
      mutate(variable = str_remove(variable, "^share\\|"))
    
    
    for(scen in unique(plotdf$scenario)){
      
      
      p[[paste0('exposureByReg', scen)]] <- list(
        plot = ggplot(plotdf %>% filter(scenario == scen)
                        , aes(x = period, y = value, color = variable, linetype = scenario)) +
          geom_line(linewidth = 1) +
          facet_wrap(~ region, scales = "free_y") +  # optional: free_y if regions differ a lot scales = "free_y"
          scale_color_paletteer_d("ggthemes::Classic_10") +
          labs(
            x = "Year",
            y = "Value",
            color = "Variable",
            linetype = "Scenario"
          ) +
          theme_minimal() +
          theme(
            legend.position = "bottom",
            axis.text.x = element_text(angle = 45, hjust = 1),
            strip.text = element_text(face = "bold")
          )
        
        ,
        
        width = 10,
        height = 8
        
      )
      
    }
    
  }
  
  #----------------End. inspecting price shocks----------------------------------
  
  
  #------3. inspecting exposure/expnditure share by region and category---------
  if(any(plotlist == "exposureByRegCate") | allExport){
    
    plotdf <- dataPrice %>%
      filter(region != 'World'
      ) %>%
      select(-baseline,-model) %>%
      calc_addVariable("FEShare|Household" = "(`FE|Buildings|Gases` * `Price|Buildings|Gases` +
                   `FE|Buildings|Electricity` * `Price|Buildings|Electricity`+
                   `FE|Buildings|Other fuels` * `Price|Buildings|Other fuels`+
                   `FE|++|Transport` * `Price|Transport|FE`) /
                   `Consumption`",
                       "share|Building gases" = "`FE|Buildings|Gases` * `Price|Buildings|Gases`/ `Consumption`",
                       "share|Building electricity" = "`FE|Buildings|Electricity` * `Price|Buildings|Electricity`/`Consumption`",
                       "share|Building other fuels" = "`FE|Buildings|Other fuels` * `Price|Buildings|Other fuels`/`Consumption`",
                       "share|Transport energy" = "`FE|++|Transport` * `Price|Transport|FE`/ `Consumption`"
      ) %>% 
      filter (grepl("^share", variable)) %>%
      mutate(variable = str_remove(variable, "^share\\|"))
    

      
      p[['exposureByRegCate']] <- list(
        plot = ggplot(plotdf
                      , aes(x = period, y = value, linetype = scenario, color = variable)) +
          geom_line(linewidth = 1) +
          facet_wrap(~ region, scales = "free_y") +  # optional: free_y if regions differ a lot scales = "free_y"
          scale_color_paletteer_d("ggthemes::Classic_10") +
          labs(
            x = "Year",
            y = "Value",
            color = "Variable",
            linetype = "Scenario"
          ) +
          theme_minimal() +
          theme(
            legend.position = "bottom",
            axis.text.x = element_text(angle = 45, hjust = 1),
            strip.text = element_text(face = "bold")
          )
        
        ,
        
        width = 10,
        height = 8
        
      )
      
  
    
  }
  
  #----------------End. inspecting price shocks---------------------------
  
  
  
  
  
  
  #--------------------Export--------------------------------------------
  if(isDisplay){
    print(p)
  }
  
  
  
  if(isExport|allExport){
    
    dir.create(paste0(outputPath,"/inspection"), recursive = TRUE, showWarnings = FALSE)
    
    for (name in names(p)) {
      ggsave(
        filename = paste0(outputPath,"/inspection/", name, ".tiff"),
        plot = p[[name]]$plot,
        width = p[[name]]$width,
        height = p[[name]]$height,
        dpi = 300
      )
    }
  }
  
  
    
}  
