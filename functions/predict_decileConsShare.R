# This simulates for HH welfare change given known price changes.

predict_decileConsShare <- function(data, regression_model = 'logitTransOLS',isDisplay = FALSE, isExport = FALSE){
  #TODO: Future food expenditure needs to be added
  
  
  #Compute regional annual expenditure share
  data<-
    data%>%
    calc_addVariable("FEShare|Household" = "(`FE|Buildings|Gases` * `Price|Buildings|Gases` +
                   `FE|Buildings|Electricity` * `Price|Buildings|Electricity`+
                   `FE|Buildings|Other fuels` * `Price|Buildings|Other fuels`+
                   `FE|++|Transport` * `Price|Transport|FE`) /
                   `Consumption`") %>%
    filter(variable == "FEShare|Household") %>%
    mutate(unit ='ratio') %>%
    bind_rows(data)
  
  #Compute consumption expenditure of decile income groups
  consShare <- read.csv(paste0('input/f_consShare_H12_',gini_baseline,'.cs4r' ), skip = 6, header = FALSE)%>%
    rename( period = V1,
            region = V2,
            scenario = V3,
            decileGroup = V4,
            consShare = V5) %>%
    mutate( gdp_scenario =   gsub("^gdp_", "", scenario) ) %>%
    filter(gdp_scenario %in% all_runscens,
           period %in% unique(data$period)) %>%
    select(-scenario)
  
  
  plotdf =   data %>%
    select(-unit) %>%
    filter ( variable %in% c('Consumption','FEShare|Household', 'Population')) %>%
    pivot_wider(names_from = variable, values_from = value) %>%
    mutate(consumptionCA = Consumption / Population *1000)
  
  dataDecile<-   data %>%
    calc_addVariable("consumptionCA" = "(`Consumption` *1e9)/(`Population` *1e6)",units = c("US$2017") )%>%
    filter( variable == 'consumptionCA' , 
            scenario %in% paste0('C_',all_runscens,'-',all_budgets,'-rem','-5'),
            !region == 'World' ) %>%
    slice(rep(1:n(), each = 10)) %>% 
    group_by(region,period,scenario) %>%
    mutate(decileGroup = 1:10,
           gdp_scenario = sub(".*(SSP[0-9]+).*", "\\1", scenario)) %>%  
    ungroup()  %>%
    right_join(consShare, by = c("period", "region",'gdp_scenario','decileGroup'))%>%
    mutate(consumptionCa = value * consShare *10) %>%
    select( -value, -gdp_scenario, -consShare, -variable)
  
  
  if (regression_model == 'PolynomialLM'){
    #Compute the adjusted fixed effects according to anual expenditure share
    fixedEffects <- data%>%
      filter( variable %in% c('Consumption', 'Population', 'FEShare|Household')) %>%
      select(-unit,-baseline)%>%
      pivot_wider(names_from = variable,  values_from = value) %>%
      mutate( fixedEffects = `FEShare|Household` - coef['(Intercept)','Energy'] -
                coef['log(exp)','Energy'] * log((Consumption* 1e9) / (Population * 1e6))-
                coef['I(log(exp)^2)','Energy'] * log((Consumption* 1e9)/(Population * 1e6 ))^2) %>%
      select(-Consumption,-Population, -`FEShare|Household`)%>%
      mutate(unit = "unitless")
    
    
    #Compute decile-specific energy budget share  
    dataDecile <- dataDecile %>%
      select( -unit,-baseline ) %>%
      left_join(fixedEffects[ , !(names(fixedEffects) %in% 'unit')], by = c('scenario','model','region','period') ) %>%
      mutate( eneShare = coef['(Intercept)','Energy'] + 
                coef['log(exp)','Energy'] * log(consumptionCa) +
                coef['I(log(exp)^2)','Energy']  * log(consumptionCa) ^ 2 + fixedEffects
      )
    
  }else if (regression_model =='logitTransOLS'){
    
    #Compute the adjusted fixed effects according to anual expenditure share
    fixedEffects <- data%>%
      filter( variable %in% c('Consumption', 'Population', 'FEShare|Household')) %>%
      select(-unit,-baseline)%>%
      pivot_wider(names_from = variable,  values_from = value) %>%
      mutate( fixedEffects = log(`FEShare|Household`/(1-`FEShare|Household`)) - coef['(Intercept)','Energy'] -
                coef['log(exp)','Energy'] * log((Consumption* 1e9) / (Population * 1e6))-
                coef['I(log(exp)^2)','Energy'] * log((Consumption* 1e9)/(Population * 1e6 ))^2) %>%
      select(-Consumption,-Population, -`FEShare|Household`)%>%
      mutate(unit = "unitless")
    
    #Compute decile-specific energy budget share  
    dataDecile <- dataDecile %>%
      select( -unit,-baseline ) %>%
      left_join(fixedEffects[ , !(names(fixedEffects) %in% 'unit')], by = c('scenario','model','region','period') ) %>%
      mutate( eneShareLogit = coef['(Intercept)','Energy'] + 
                coef['log(exp)','Energy'] * log(consumptionCa) +
                coef['I(log(exp)^2)','Energy']  * log(consumptionCa) ^ 2 + fixedEffects,
              eneShare = 1 / (1 + exp(-eneShareLogit)),
              
              foodShareLogit = coef['(Intercept)','Food'] +                            #Todo: Benchmark it to MagPIE Data!!
                coef['log(exp)','Food'] * log(consumptionCa) +
                coef['I(log(exp)^2)','Food']  * log(consumptionCa) ^ 2,
              foodShare = 1 / (1 + exp(-foodShareLogit)),
              
              commShare = 1 - eneShare - foodShare
              
      ) %>%
      select(scenario,model,region,period,decileGroup, consumptionCa, eneShare, foodShare, commShare)
    
  }
  
  #Todo: add validation plot for food
  p1<-ggplot(plotdf, aes(x = log(consumptionCA), y = `FEShare|Household`, color = factor(region) )) +
    geom_point(alpha = 0.6)+
    facet_wrap(~scenario)+
    labs(title = "Average HH FE share (REMIND)", x = "log(ConsumptionCa), US$2017", y = "percentage(%)") +
    theme_minimal()+
    ylim(-0.1,0.4) +
    scale_color_discrete(name = "Region")+ 
    geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 0.5)
  
  p2<- ggplot(dataDecile[dataDecile$decileGroup%in%c(1,10),], aes(x=log(consumptionCa), y = eneShare, color = factor(decileGroup))) +
    geom_point(alpha = 0.6)+
    facet_wrap(~scenario)+
    ylim(-0.1,0.4)+
    labs(title = "Projected HH Fe share", x = "log(ConsumptionCa), US$2017", y = "percentage(%)") +
    theme_minimal()+
    scale_color_discrete(name = "Decile group")+ 
    geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 0.5)
  
  combined_plot<-p1+p2
  
  combined_plot
  if(isDisplay){
    print(combined_plot)
  }
  
  if(isExport){
    ggsave(paste0("figure/FE share projection_",regression_model,'.tiff'), combined_plot, width = 12, height =5, units = "in", dpi = 300)
  }
  
  return (dataDecile)
}




