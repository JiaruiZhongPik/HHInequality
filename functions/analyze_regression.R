#----This function conducts regression with Eurostat consumption data----------


analyze_regression <- function(isDisplay = TRUE) {
  hhConsData = prepare_eurostatData()
  
  modelFoodFe=lm(wFood ~ log(exp) + I(log(exp)^2)  + geo + TIME_PERIOD , hhConsData )
  
  modelEneFe=lm(wEnergy ~ log(exp) + I(log(exp)^2) + geo + TIME_PERIOD , hhConsData )
  
  modelCommFe=lm(wCommodity ~ log(exp) + I(log(exp)^2) + geo + TIME_PERIOD , hhConsData )
  
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

