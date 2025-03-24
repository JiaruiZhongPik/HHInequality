#----This function conducts regression with Eurostat consumption data----------


analyze_regression <- function (regression_model = 'PolynomialLM',isDisplay = TRUE) {
  
  hhConsData = prepare_eurostatData()
  
  if( regression_model == 'PolynomialLM'){
    
    modelFoodFe=lm(wFood ~ log(exp) + I(log(exp)^2)  + geo + TIME_PERIOD , hhConsData )
    
    modelEneFe=lm(wEnergy ~ log(exp) + I(log(exp)^2) + geo + TIME_PERIOD , hhConsData )
    
    modelCommFe=lm(wCommodity ~ log(exp) + I(log(exp)^2) + geo + TIME_PERIOD , hhConsData )

    
  } else if (regression_model =='logitTransOLS') {
    
    #Prepare data:clip data to stay strictly between 0 and 1, which is required for logit transformation 
    hhConsData<-hhConsData %>%
      mutate(wFoodClipped = pmin(pmax(wFood, 1e-5), 1 - 1e-5),
             wFoodLogitShare = log(wFoodClipped / (1 - wFoodClipped)),
             
             wEneClipped = pmin(pmax(wEnergy, 1e-5), 1 - 1e-5),
             wEneLogitShare = log(wEneClipped / (1 - wEneClipped)),
             
             wCommClipped = pmin(pmax(wCommodity, 1e-5), 1 - 1e-5),
             wCommLogitShare = log(wCommClipped / (1 - wCommClipped)),
      )
    
    #Regression model
    modelFoodFe <- lm(wFoodLogitShare ~ log(exp) + I(log(exp)^2)  + factor(geo) + factor(TIME_PERIOD), data = hhConsData)
    
    modelEneFe <- lm(wEneLogitShare ~ log(exp) + I(log(exp)^2)  + factor(geo) + factor(TIME_PERIOD), data = hhConsData)
    
    modelCommFe <- lm(wCommLogitShare ~ log(exp) + I(log(exp)^2)  + factor(geo) + factor(TIME_PERIOD), data = hhConsData)
    
    
    }
  
  
  coef = data.frame(
    Food = modelFoodFe$coefficients[c('(Intercept)','log(exp)','I(log(exp)^2)')],
    Energy = modelEneFe$coefficients[c('(Intercept)','log(exp)','I(log(exp)^2)')],
    Commodity = modelCommFe$coefficients[c('(Intercept)','log(exp)','I(log(exp)^2)')]
  )
  if(isDisplay){
    stargazer(modelFoodFe, modelEneFe, modelCommFe, 
              type = "text",
              title = "Regression Results",
              dep.var.labels = c("Share:Food", "Share:Energy", "Share:Commodities"),
              digits = 3,
              keep = c('Constant','exp'))
  }
  
  return(coef)
}



