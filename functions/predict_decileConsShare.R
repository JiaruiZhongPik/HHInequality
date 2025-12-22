# This simulates for HH welfare change given known price changes.

predict_decileConsShare <- function(data, coef, gini_baseline, regression_model = 'logitTransOLS', missingMapping = NA, countryExample = NA, 
                                    isDisplay = FALSE, isExport = FALSE){
  if(isExport == T){
    dir.create(paste0(outputPath), recursive = TRUE, showWarnings = FALSE)
  }
 
  #Compute regional annual expenditure share
  data1<-
    data %>%
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
                     )
  
  #Todo: maybe worth to add readCons to mrremind, so this can be computed directly
  if(gini_baseline == 'rao'){
    #Read Gini for SSPs
    shareSSP <- read.csv(paste0('input/f_consShare_',regions,'_',gini_baseline,'.cs4r' ), skip = 6, header = FALSE) %>%
      rename( period = V1,
              region = V2,
              scenario = V3,
              decileGroup = V4,
              consShare = V5) %>%
      mutate( gdp_scenario =   gsub("^gdp_", "", scenario)) %>%
      select( -scenario ) %>%
      filter(grepl("^SSP[0-9]$", gdp_scenario))
  } else if(gini_baseline == 'poblete05'){
    
    shareSSP <- read.csv(paste0('input/f_consShare_',regions,'_',gini_baseline,'.cs4r' ), skip = 6, header = FALSE) %>%
      rename( period = V1,
              region = V2,
              scenario = V3,
              decileGroup = V4,
              consShare = V5) %>%
      mutate( gdp_scenario =   gsub("^gdp_", "", scenario)) %>%
      select( -scenario ) %>%
      filter(grepl("^SSP[0-9]$", gdp_scenario))
    
  } else if(gini_baseline == 'poblete07'){
    
    shareSSP <- read.csv(paste0('input/f_consShare_',regions,'_',gini_baseline,'.cs4r' ), skip = 6, header = FALSE) %>%
      rename( period = V1,
              region = V2,
              scenario = V3,
              decileGroup = V4,
              consShare = V5) %>%
      mutate( gdp_scenario =   gsub("^gdp_", "", scenario)) %>%
      select( -scenario ) %>%
      filter(grepl("^SSP[0-9]$", gdp_scenario))
    
    
  } else{
    print('no Gini baseline')
    }
  
  
  #Read Gini for SDPs, from Min et al.(2024)
  #Note: 1) the shape data use older SSP1 data, therefore the inequality measure computed from Shape data
  # is different from the consumption share that is computed using MRREMIND.
  #Note: 2) Shape SDP doesn't provide historic data, take that from SSP1  
  shareSDP <- prepare_GiniSDP() %>%
    mutate( gdp_scenario = as.character( gdp_scenario))
  
  ssp1_hist <- shareSSP %>%
    filter(gdp_scenario == "SSP1", period %in% c(2000,2005, 2010, 2015))
  
  sdp_scenarios <- shareSDP %>%
    distinct(gdp_scenario)
  
  filled_hist <- cross_join(ssp1_hist, sdp_scenarios) %>%
    mutate(gdp_scenario = gdp_scenario.y) %>%
    select(-gdp_scenario.x, -gdp_scenario.y)
  
  shareSDP_filled <- bind_rows(shareSDP, filled_hist)
  
  
  # combine data and also generate the consumption shares for the SDP scenario SSP2EU, which take the number from SSP2
  consShare<- bind_rows(shareSSP,shareSDP_filled) 
  consShare<- consShare %>%
    filter(gdp_scenario == "SSP2") %>%
    mutate(gdp_scenario = "SSP2EU") %>%
    bind_rows(consShare)

  # select scenarios that are used for the anlaysis
  consShare <- consShare %>%
    filter(gdp_scenario %in% all_runscens,
           period %in% unique(data$period))
    
  plotdf <-  data1 %>%
    select(-unit) %>%
    filter(
      variable %in% c("Consumption", "FEShare|Household", "Population") |
        str_starts(variable, "share\\|")
    ) %>%
    pivot_wider(names_from = variable, values_from = value) %>%
    mutate(consumptionCA = Consumption / Population *1000)
  
  dataDecile <- data1 %>%
    calc_addVariable("consumptionCA" = "(`Consumption` *1e9)/(`Population` *1e6)",units = c("US$2017") )%>%
    filter( variable == 'consumptionCA' , 
            !region == 'World' ) %>%
    slice(rep(1:n(), each = 10)) %>% 
    group_by(region,period,scenario) %>%
    mutate(decileGroup = 1:10,
           gdp_scenario = sub(".*((SSP[0-9]+[A-Z]*?)|(SDP_[^\\-]+)).*", "\\1", scenario)) %>%  
    ungroup()  %>%
    right_join(consShare, by = c("period", "region",'gdp_scenario','decileGroup'))%>%
    mutate(consumptionCa = value * consShare *10) %>%
    select( -value, -gdp_scenario, -consShare, -variable)
  
  
  #Fill in missing regions with selected comparable regions
  if(is.data.frame(missingMapping)){
    missingFilled <- coef %>%
      filter(region == "EUR") %>%
      mutate(region_source = region) %>%  # keep a copy of the original
      select(-region) %>%
      crossing(region = missingMapping$missing)
    
    coef <- bind_rows(coef, missingFilled) %>%
      select(-region_source)
  }
  
  coef_expanded <- crossing(scenario = unique(data1$scenario), coef,
                            period = unique(data1$period)) %>%
    mutate( variable = paste (sector, regressor, sep='|')) %>%
    select(-sector, -regressor ) %>%
    mutate(model = 'empiric',
           scenario= str_remove(scenario, "-(rem|mag)-\\d+$") ) 
  
  
  if( length(unique(coef$region))==1){
    coef_expanded <- crossing(region = setdiff(unique(data$region), "World"), coef_expanded %>% select(-region))
  }
  
  
  if (regression_model == 'PolynomialLM'){
    
    if(length(unique(coef$sector)) == 9){
      
      if(length(unique(coef$region)) == 1){
        
        fixedEffects <- data1 %>%
          filter(variable %in% c(
            "Consumption", "Population", 
            "share|Building gases",
            "share|Building electricity",
            "share|Building other fuels",
            "share|Transport energy", 
            "share|Animal products",
            "share|Staples",
            "share|Fruits vegetables nuts",
            "share|Empty calories"
          )) %>%
          select(-unit) %>%
          pivot_wider(names_from = variable, values_from = value) %>%
          left_join(
            coef_expanded %>%
              pivot_wider(names_from = variable, values_from = value),
            by = c("scenario", "period", "region")
          ) %>%
          mutate( `fixedEff+intercept|Building gases` =  `share|Building gases` -
                    `Building gases|log(exp)` * log((Consumption* 1e9) / (Population * 1e6)) -
                    `Building gases|I(log(exp)^2)` * log((Consumption* 1e9)/(Population * 1e6 ))^2,
                  
                  `fixedEff+intercept|Building electricity` = `share|Building electricity` -
                    `Building electricity|log(exp)` * log((Consumption* 1e9) / (Population * 1e6)) -
                    `Building electricity|I(log(exp)^2)` * log((Consumption* 1e9)/(Population * 1e6 ))^2,
                  
                  `fixedEff+intercept|Building other fuels` = `share|Building other fuels`-
                    `Building other fuels|log(exp)` * log((Consumption* 1e9) / (Population * 1e6)) -
                    `Building other fuels|I(log(exp)^2)` * log((Consumption* 1e9)/(Population * 1e6 ))^2,
                  
                  `fixedEff+intercept|Transport energy` = `share|Transport energy` -
                    `Transport energy|log(exp)` * log((Consumption* 1e9) / (Population * 1e6)) -
                    `Transport energy|I(log(exp)^2)` * log((Consumption* 1e9)/(Population * 1e6 ))^2,
                  
                  `fixedEff+intercept|Animal products` =  `share|Animal products` -
                    `Animal products|log(exp)` * log((Consumption* 1e9) / (Population * 1e6)) -
                    `Animal products|I(log(exp)^2)` * log((Consumption* 1e9)/(Population * 1e6 ))^2,
                  
                  `fixedEff+intercept|Staples` =  `share|Staples`  -
                    `Animal products|log(exp)` * log((Consumption* 1e9) / (Population * 1e6)) -
                    `Animal products|I(log(exp)^2)` * log((Consumption* 1e9)/(Population * 1e6 ))^2,
                  
                  `fixedEff+intercept|Fruits vegetables nuts` =  `share|Fruits vegetables nuts` -
                    `Fruits vegetables nuts|log(exp)` * log((Consumption* 1e9) / (Population * 1e6)) -
                    `Fruits vegetables nuts|I(log(exp)^2)` * log((Consumption* 1e9)/(Population * 1e6 ))^2,
                  
                  `fixedEff+intercept|Empty calories` =  `share|Empty calories` -
                    `Empty calories|log(exp)` * log((Consumption* 1e9) / (Population * 1e6)) -
                    `Empty calories|I(log(exp)^2)` * log((Consumption* 1e9)/(Population * 1e6 ))^2

              
          ) %>%
          select(scenario, model, region, period, starts_with("fixedEff+intercept|"))
        
        

        
        #Compute decile-specific energy budget share  
        
        dataFull <- dataDecile %>%
          select( -unit) %>%
          left_join(fixedEffects[ , !(names(fixedEffects) %in% c('unit','model') )], by = c('scenario','region','period') ) %>%
          left_join(
            coef_expanded %>%
              select(-model) %>%
              pivot_wider(names_from = variable, values_from = value),
            by = c("scenario", "period", "region")
          ) %>%
          mutate( `share|Building gases` = `Building gases|log(exp)` * log(consumptionCa) +
                    `Building gases|I(log(exp)^2)`  * log(consumptionCa) ^ 2+ `fixedEff+intercept|Building gases`,
                  
                  `share|Building electricity` = `Building electricity|log(exp)` * log(consumptionCa) +
                    `Building electricity|I(log(exp)^2)`  * log(consumptionCa) ^ 2+ `fixedEff+intercept|Building electricity`,
                  
                  `share|Building other fuels` = `Building other fuels|log(exp)` * log(consumptionCa) +
                    `Building other fuels|I(log(exp)^2)`  * log(consumptionCa) ^ 2+ `fixedEff+intercept|Building other fuels`,
                  
                  `share|Transport energy` = `Transport energy|log(exp)` * log(consumptionCa) +
                    `Transport energy|I(log(exp)^2)`  * log(consumptionCa) ^ 2+ `fixedEff+intercept|Transport energy`,
                  
                  `share|Staples` = `Staples|log(exp)` * log(consumptionCa) +
                    `Staples|I(log(exp)^2)`  * log(consumptionCa) ^ 2 + `fixedEff+intercept|Staples`,
                  
                  `share|Animal products` = `Animal products|log(exp)` * log(consumptionCa) +
                    `Animal products|I(log(exp)^2)`  * log(consumptionCa) ^ 2 + `fixedEff+intercept|Animal products`,
                  
                  `share|Fruits vegetables nuts` = `Fruits vegetables nuts|log(exp)` * log(consumptionCa) +
                    `Fruits vegetables nuts|I(log(exp)^2)`  * log(consumptionCa) ^ 2 + `fixedEff+intercept|Fruits vegetables nuts`   ,
                  
                  `share|Empty calories` = `Empty calories|log(exp)` * log(consumptionCa) +
                    `Empty calories|I(log(exp)^2)`  * log(consumptionCa) ^ 2 + `fixedEff+intercept|Empty calories`,
                  
                  `share|Other commodities` = 1-`share|Building gases`-`share|Building electricity`-`share|Building other fuels`- `share|Transport energy`
                  -`share|Staples`-`share|Animal products`-`share|Fruits vegetables nuts`-`share|Empty calories`
      
                              
          ) 
      }
      
    }else if( length(unique(coef$sector)) ==3 ){
      
      #Compute the adjusted fixed effects according to annual expenditure share 
      #todo: add food fixed effects
      fixedEffects <- data1 %>%
        filter( variable %in% c('Consumption', 'Population', 'FEShare|Household')) %>%
        select(-unit,-baseline)%>%
        pivot_wider(names_from = variable,  values_from = value) %>%
        left_join(
          coef_expanded %>%
            select(-model) %>%
            pivot_wider(names_from = variable, values_from = value),
          by = c("scenario", "period", "region")
        ) %>%
        mutate( `fixedEff+intercept|Energy` = `FEShare|Household`-
                  `Energy|log(exp)` * log((Consumption* 1e9) / (Population * 1e6)) -
                  `Energy|I(log(exp)^2)` * log((Consumption* 1e9)/(Population * 1e6 ))^2) %>%
        select(scenario, model, region, period, starts_with("fixedEff|"))
      

      
      dataFull <- dataDecile %>%
        select( -unit, -baseline,-model) %>%
        left_join(fixedEffects[ , !(names(fixedEffects) %in% c('unit','model') )], by = c('scenario','region','period') ) %>%
        left_join(
          coef_expanded %>%
            select(-model) %>%
            pivot_wider(names_from = variable, values_from = value),
          by = c("scenario", "period", "region")
        ) %>%
        mutate( `share|Energy` = `Energy|log(exp)` * log(consumptionCa) +
                  `Energy|I(log(exp)^2)`  * log(consumptionCa) ^ 2 + `fixedEff+intercept|Energy`,
                
                `share|Food` = `Food|log(exp)` * log(consumptionCa) +
                  `Food|I(log(exp)^2)`  * log(consumptionCa) ^ 2, # todo: add fixed effects for food
                
                `share|Other commodities` = 1-`share|Energy`-`share|Food`
                
        ) 
      
    }
    

    
  }else if (regression_model =='logitTransOLS'){
    
    #Compute the adjusted fixed effects according to annual expenditure share
    
    if(length(unique(coef$sector)) == 9){
     
       fixedEffects <- data1 %>%
        filter(variable %in% c(
          "Consumption", "Population", 
          "share|Building gases",
          "share|Building electricity",
          "share|Building other fuels",
          "share|Transport energy",
          "share|Animal products",
          "share|Staples",
          "share|Fruits vegetables nuts",
          "share|Empty calories"
        )) %>%
        select(-unit) %>%
        pivot_wider(names_from = variable, values_from = value) %>%
        left_join(
          coef_expanded %>%
            #select(-model) %>%
            pivot_wider(names_from = variable, values_from = value),
          by = c("scenario", "period", "region")
        ) %>%
        mutate( `fixedEff+intercept|Building gases` =  log(`share|Building gases`/(1-`share|Building gases`))-
                  `Building gases|log(exp)` * log((Consumption* 1e9) / (Population * 1e6)) -
                  `Building gases|I(log(exp)^2)` * log((Consumption* 1e9)/(Population * 1e6 ))^2,
                
                `fixedEff+intercept|Building electricity` = log(`share|Building electricity`/(1-`share|Building electricity`)) -
                  `Building electricity|log(exp)` * log((Consumption* 1e9) / (Population * 1e6)) -
                  `Building electricity|I(log(exp)^2)` * log((Consumption* 1e9)/(Population * 1e6 ))^2,
                
                `fixedEff+intercept|Building other fuels` = log(`share|Building other fuels`/(1-`share|Building other fuels`)) -
                  `Building other fuels|log(exp)` * log((Consumption* 1e9) / (Population * 1e6)) -
                  `Building other fuels|I(log(exp)^2)` * log((Consumption* 1e9)/(Population * 1e6 ))^2,
                
                `fixedEff+intercept|Transport energy` = log(`share|Transport energy`/(1-`share|Transport energy`)) -
                  `Transport energy|log(exp)` * log((Consumption* 1e9) / (Population * 1e6)) -
                  `Transport energy|I(log(exp)^2)` * log((Consumption* 1e9)/(Population * 1e6 ))^2,
                
                `fixedEff+intercept|Animal products` = log(`share|Animal products`/(1-`share|Animal products`)) -
                  `Animal products|log(exp)` * log((Consumption* 1e9) / (Population * 1e6)) -
                  `Animal products|I(log(exp)^2)` * log((Consumption* 1e9)/(Population * 1e6 ))^2,
                
                `fixedEff+intercept|Staples` = log(`share|Staples`/(1-`share|Staples`)) -
                  `Staples|log(exp)` * log((Consumption* 1e9) / (Population * 1e6)) -
                  `Staples|I(log(exp)^2)` * log((Consumption* 1e9)/(Population * 1e6 ))^2,
                
                `fixedEff+intercept|Fruits vegetables nuts` = log(`share|Fruits vegetables nuts`/(1-`share|Fruits vegetables nuts`)) -
                  `Fruits vegetables nuts|log(exp)` * log((Consumption* 1e9) / (Population * 1e6)) -
                  `Fruits vegetables nuts|I(log(exp)^2)` * log((Consumption* 1e9)/(Population * 1e6 ))^2,
                
                `fixedEff+intercept|Empty calories` = log(`share|Empty calories`/(1-`share|Empty calories`)) -
                  `Empty calories|log(exp)` * log((Consumption* 1e9) / (Population * 1e6)) -
                  `Empty calories|I(log(exp)^2)` * log((Consumption* 1e9)/(Population * 1e6 ))^2,
                
                
        ) %>%
        select(scenario, model, region, period, starts_with("fixedEff+intercept|"))
      
      
      #Compute decile-specific energy budget share  
      
      
      
      dataFull <- dataDecile %>%
        select( -unit) %>%
        left_join(fixedEffects[ , !(names(fixedEffects) %in% c('unit','model') )], by = c('scenario','region','period') ) %>%
        left_join(
          coef_expanded %>%
            select(-model) %>%
            pivot_wider(names_from = variable, values_from = value),
          by = c("scenario", "period", "region")
        ) %>%
        mutate( `shareLogit|Building gases` = `Building gases|log(exp)` * log(consumptionCa) +
                  `Building gases|I(log(exp)^2)`  * log(consumptionCa) ^ 2+ `fixedEff+intercept|Building gases`,
                
                `shareLogit|Building electricity` = `Building electricity|log(exp)` * log(consumptionCa) +
                  `Building electricity|I(log(exp)^2)`  * log(consumptionCa) ^ 2+ `fixedEff+intercept|Building electricity`,
                
                `shareLogit|Building other fuels` = `Building other fuels|log(exp)` * log(consumptionCa) +
                  `Building other fuels|I(log(exp)^2)`  * log(consumptionCa) ^ 2+ `fixedEff+intercept|Building other fuels`,
                
                `shareLogit|Transport energy` =  `Transport energy|log(exp)` * log(consumptionCa) +
                  `Transport energy|I(log(exp)^2)`  * log(consumptionCa) ^ 2+ `fixedEff+intercept|Transport energy`,
                
                `shareLogit|Staples` =  `Staples|log(exp)` * log(consumptionCa) +
                  `Staples|I(log(exp)^2)`  * log(consumptionCa) ^ 2 + `fixedEff+intercept|Staples`,
                
                `shareLogit|Animal products` = `Animal products|log(exp)` * log(consumptionCa) +
                  `Animal products|I(log(exp)^2)`  * log(consumptionCa) ^ 2 + `fixedEff+intercept|Animal products`,
                
                `shareLogit|Fruits vegetables nuts` = `Fruits vegetables nuts|log(exp)` * log(consumptionCa) +
                  `Fruits vegetables nuts|I(log(exp)^2)`  * log(consumptionCa) ^ 2 + `fixedEff+intercept|Fruits vegetables nuts`,   
                
                `shareLogit|Empty calories` = `Empty calories|log(exp)` * log(consumptionCa) +
                  `Empty calories|I(log(exp)^2)`  * log(consumptionCa) ^ 2 + `fixedEff+intercept|Empty calories`,
                
                `share|Building gases` = 1 / (1 + exp(-`shareLogit|Building gases`)),
                
                `share|Building electricity` = 1 / (1 + exp(-`shareLogit|Building electricity`)),
                
                `share|Building other fuels` = 1 / (1 + exp(-`shareLogit|Building other fuels`)),
                
                `share|Transport energy` = 1 / (1 + exp(-`shareLogit|Transport energy`)),
                
                `share|Staples` = 1 / (1 + exp(-`shareLogit|Staples`)),
                
                `share|Animal products` = 1 / (1 + exp(-`shareLogit|Animal products`)),
                
                `share|Fruits vegetables nuts` = 1 / (1 + exp(-`shareLogit|Fruits vegetables nuts`)),
                
                `share|Empty calories` = 1 / (1 + exp(-`shareLogit|Empty calories`)),
                
                `share|Other commodities` = 1-`share|Building gases`-`share|Building electricity`-`share|Building other fuels`- `share|Transport energy`
                -`share|Staples`-`share|Animal products`-`share|Fruits vegetables nuts`-`share|Empty calories`
                
        ) 

      
      }else if( length(unique(coef$sector)) ==3 ){
        warning('not maintained')
        #Compute the adjusted fixed effects according to annual expenditure share 
        # Code as total, unfinished 
        fixedEffects <- data1%>%
          filter( variable %in% c('Consumption', 'Population', 'FEShare|Household')) %>%
          select(-unit,-baseline)%>%
          pivot_wider(names_from = variable,  values_from = value) %>%
          left_join(
            coef_expanded %>%
              select(-model) %>%
              pivot_wider(names_from = variable, values_from = value),
            by = c("scenario", "period", "region")
          ) %>%
          mutate( `fixedEff+intercept|Energy` = log(`FEShare|Household`/(1-`FEShare|Household`))-
                    `Energy|log(exp)` * log((Consumption* 1e9) / (Population * 1e6)) -
                    `Energy|I(log(exp)^2)` * log((Consumption* 1e9)/(Population * 1e6 ))^2) %>%
          select(scenario, model, region, period, starts_with("fixedEff|"))


        dataFull <- dataDecile %>%
          select( -unit, -baseline,-model) %>%
          left_join(fixedEffects[ , !(names(fixedEffects) %in% c('unit','model') )], by = c('scenario','region','period') ) %>%
          left_join(
            coef_expanded %>%
              select(-model) %>%
              pivot_wider(names_from = variable, values_from = value),
            by = c("scenario", "period", "region")
          ) %>%
          mutate( `shareLogit|Energy` = `Energy|log(exp)` * log(consumptionCa) +
                    `Energy|I(log(exp)^2)`  * log(consumptionCa) ^ 2+ `fixedEff+intercept|Energy`,
                  
                  `shareLogit|Food` = `Food|log(exp)` * log(consumptionCa) +
                    `Food|I(log(exp)^2)`  * log(consumptionCa) ^ 2, # todo: add fixed effects for food
                  
                  `share|Energy` = 1 / (1 + exp(-`shareLogit|Energy`)),
                  
                  `share|Food` = 1 / (1 + exp(-`shareLogit|Food`)),
                  
                  `share|Other commodities` = 1-`share|Energy`-`share|Food`
                  
          ) 
        
         
      }
    }
   
  
  
  
  dfOutput<- dataFull %>%
    select(scenario, region, period, decileGroup, consumptionCa, starts_with("share|"))
  
  robust_ylim <- function(x, lower = 0.01, upper = 0.99, buffer = 0.05) {
    q <- quantile(x, c(lower, upper), na.rm = TRUE)
    pad <- buffer * diff(q)
    c(q[1] - pad, q[2] + pad)
  }

  #Plot routine, seprate for s3 and s9 case
  if(length(unique(coef$sector)) ==3){
    p1 <- ggplot(plotdf, aes(x = log(consumptionCA), y = `share|Household` , color = factor(region) )) +
      geom_point(alpha = 0.6)+
      facet_wrap(~scenario)+
      labs(title = "Average HH FE share (REMIND)", x = "log(ConsumptionCa), US$2017", y = "Share") +
      theme_minimal()+
      ylim(-0.1,0.4) +
      scale_color_discrete(name = "Region")+ 
      geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.5)
    
    p2 <- ggplot(dfOutput[dfOutput$decileGroup%in%c(1,10),], aes(x=log(consumptionCa), y = `share|Energy`, color = factor(decileGroup))) +
      geom_point(alpha = 0.6)+
      facet_wrap(~scenario)+
      ylim(-0.1,0.4)+
      labs(title = "Projected HH Fe share", x = "log(ConsumptionCa), US$2017", y = "Share") +
      theme_minimal()+
      scale_color_discrete(name = "Decile group")+ 
      geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.5)
    
    combined_plot<-p1+p2

    if(isDisplay){
      print(combined_plot)
    } 
    
    if(isExport){
     
      ggsave(paste0(outputPath,"/FE share projection_",regression_model,'.tiff'), combined_plot, width = 12, height =5, units = "in", dpi = 300)
    }

  } else if(length(unique(coef$sector)) ==9){

    
    # Function to generate plots per sector
    plot_sector <- function(sector) {
      # Convert sector to its equivalent in dfOutput
      dfOutput_sector <- sub("FEShare", "share", sector)
      
      y_vals <- plotdf[[sector]]  # `sector` is assumed to be a character string
      
      ylims <- robust_ylim(y_vals)
      
      p1 <- ggplot(
        plotdf,
        aes(x = log(consumptionCA), y = !!sym(sector), color = factor(region))
      ) +
        geom_point(alpha = 0.6) +
        facet_wrap(~scenario) +
        labs(
          title = paste("REMIND-MAgPIE -", sector),
          x = "log(ConsumptionCa), US$2017",
          y = "Share"
        ) +
        theme_minimal() +
        ylim(ylims) +
        scale_color_brewer(palette = "Paired") +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.5)
      
      
      #Creat P2
      df_filtered <- dfOutput[dfOutput$decileGroup %in% c(1, 10), ]
      
      y_vals <- df_filtered[[dfOutput_sector]]  # make sure dfOutput_sector is a character string
      
      ylims <- robust_ylim(y_vals)
      
      p2 <- ggplot(
        dfOutput[dfOutput$decileGroup %in% c(1, 10), ],
        aes(x = log(consumptionCa), y = !!sym(dfOutput_sector), 
            color = factor(region), shape = factor(decileGroup))
      ) +
        geom_point(alpha = 0.6) +
        facet_wrap(~scenario) +
        ylim(ylims) +
        labs(
          title = paste("Projected share -", sector),
          x = "log(ConsumptionCa), US$2017",
          y = "Share"
        ) +
        theme_minimal() +
        scale_color_brewer(palette = "Paired") +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.5)
      
      p1+p2
      
    }
    
     # Define sectors you want to loop through
     sectors <- c(
       "share|Building gases",
       "share|Building electricity",
       "share|Building other fuels",
       "share|Transport energy"
     )
     
     # Generate combined plots for each sector
     combined_plots <- purrr::map(sectors, plot_sector)
     
     # Combine and display all plots in a grid
     all_combined_plot <- wrap_plots(combined_plots, ncol = 1, guides = "collect") &
       theme(legend.position = "bottom") 
     
     ggsave(
       filename = paste0(outputPath, "/combined_FE_share_plot_gcd.tiff"),
       plot = all_combined_plot,
       width = 10,
       height = 12,
       dpi = 300,
       compression = "lzw"
     )
     
     # Define sectors you want to loop through
     sectors <- c(
       "share|Animal products",
       "share|Staples",
       "share|Fruits vegetables nuts",
       "share|Empty calories"
     )
     
     
     # Generate combined plots for each sector
     combined_plots <- purrr::map(sectors, plot_sector)
     
     # Combine and display all plots in a grid
     all_combined_plot <- wrap_plots(combined_plots, ncol = 1, guides = "collect") &
       theme(legend.position = "bottom") 
     
     ggsave(
       filename = paste0(outputPath, "/combined_Food_share_plot_gcd.tiff"),
       plot = all_combined_plot,
       width = 10,
       height = 12,
       dpi = 300,
       compression = "lzw"
     ) 
     
     
  if(!all(is.na(countryExample))){
   

    # Function to generate plots per sector
    plot_sector <- function(sector) {
      # Convert sector to its equivalent in dfOutput
      dfOutput_sector <- sub("FEShare", "share", sector)
      
      # Create p1
      p1 <- ggplot(
        plotdf[plotdf$region == region, ],
        aes(x = log(consumptionCA), y = !!sym(sector), color = factor(region))
      ) +
        geom_point(alpha = 0.6) +
        facet_wrap(~scenario) +
        labs(
          title = paste("REMIND -", sector),
          x = "log(ConsumptionCa), US$2017",
          y = "Share"
        ) +
        theme_minimal() +
        ylim(
          quantile(plotdf[plotdf$region == region, ][[sector]], probs = c(0.01, 0.99), na.rm = TRUE)
        ) +
        scale_color_discrete(name = "Region") +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.5)
      
      # Create p2
      p2 <- ggplot(
        dfOutput[dfOutput$region == region, ],
        aes(x = log(consumptionCa), y = !!sym(dfOutput_sector), color = factor(decileGroup))
      ) +
        geom_point(alpha = 0.6) +
        facet_wrap(~scenario) +
        ylim(
          quantile(dfOutput[dfOutput$region == region, ][[dfOutput_sector]], probs = c(0.01, 0.99), na.rm = TRUE)
        ) +
        labs(
          title = paste("Projected share -", sector),
          x = "log(ConsumptionCa), US$2017",
          y = "Share"
        ) +
        theme_minimal() +
        scale_color_discrete(name = "Decile group") +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.5)
      
      p1+p2
      
    }
     
    # Define sectors you want to loop through
    sectors <- c(
      "share|Building gases",
      "share|Building electricity",
      "share|Building other fuels",
      "share|Transport energy"
    )

    for (region in countryExample){
      combined_plots <- purrr::map(sectors, plot_sector)
      
      # Combine and display all plots in a grid
      all_combined_plot <- wrap_plots(combined_plots, ncol = 1, guides = "collect") &
        theme(legend.position = "bottom") 
      
      ggsave(
        filename = paste0(outputPath,"/combined_FE_share_plot_gcd_",region,".tiff"),
        plot = all_combined_plot,
        width = 10,
        height = 12,
        dpi = 300,
        compression = "lzw" )
    }


    
    # Define sectors you want to loop through
    sectors <- c(
      "share|Animal products",
      "share|Staples",
      "share|Fruits vegetables nuts",
      "share|Empty calories"
    )
    
    for (region in countryExample){
    # Generate combined plots for each sector
    combined_plots <- purrr::map(sectors, plot_sector)
    
    # Combine and display all plots in a grid
    all_combined_plot <- wrap_plots(combined_plots, ncol = 1, guides = "collect") &
      theme(legend.position = "bottom") 
    
    ggsave(
      filename = paste0(outputPath,"/combined_food_share_plot_gcd_",region,".tiff"),
      plot = all_combined_plot,
      width = 10,
      height = 12,
      dpi = 300,
      compression = "lzw"
    )
    }
    
  }
       
  
  }
  
  

  
  
  return (dfOutput)
}




