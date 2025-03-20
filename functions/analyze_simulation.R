# This simulates for HH welfare change given known price changes.


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

#Compute the adjusted fixed effects according to anual expenditure share
data <- data%>%
  filter( variable %in% c('Consumption', 'Population', 'FEShare|Household')) %>%
  select(-unit,-baseline)%>%
  pivot_wider(names_from = variable,  values_from = value) %>%
  mutate( fixedEffects = `FEShare|Household` - coef['(Intercept)','Energy'] -
            coef['log(exp)','Energy'] * log((Consumption* 1e9) / (Population * 1e6))-
            coef['I(log(exp)^2)','Energy'] * log((Consumption* 1e9)/(Population * 1e6 ))^2) %>%
  select(-Consumption,-Population, -`FEShare|Household`)%>%
  mutate(unit = NA,
         variable = 'fixedEffects') %>%
  rename( value = fixedEffects )

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


dataDecile<- data %>%
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
  mutate(consumptionCa = value * consShare) %>%
  select( -value, -gdp_scenario, -consShare) %>%
  rename(value = consumptionCa)
#To do, adjust the consumption for real per capita

#Compute decile-specific energy budget share  

data[data$variable=='fixedEffects',]


dataDecile %>%
  mutate( eneShare = coef['(Intercept)','Energy'] + 
            coef['log(exp)','Energy'] * log(consumptionCa) +
            coef['I(log(exp)^2)','Energy']  * log(consumptionCa) ^ 2 + 
            
            )


