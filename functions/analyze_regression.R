#----This function conducts regression with Eurostat consumption data----------


analyze_regression <- function (regression_model = 'PolynomialLM', ConsData ='gcd', regionmapping = 'H12',
                                isDisplay = TRUE, isExport = FALSE) {
  
  # Read in regional mapping   
  if(regionmapping =='H12') {
    
    regionMapping<- read_delim("input/regionmappingH12.csv", 
                               delim = ";", escape_double = FALSE, col_types = cols(X = col_skip()), 
                               trim_ws = TRUE,
                               show_col_types = FALSE) %>%
      rename(geo = CountryCode,
             region = RegionCode) 
  } else if (regionmapping == 'H21') {
    
    regionMapping <- read_delim("input/regionmapping_21_EU11.csv", 
                                    delim = ";", escape_double = FALSE, col_types = cols(X = col_skip(), 
                                                                                         missingH12 = col_skip()), 
                                    trim_ws = TRUE,
                                    show_col_types = FALSE) %>%
      rename(geo = CountryCode,
             region = RegionCode)
  }
  

  
  if (ConsData == 'eurostat'){
    
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
     ) %>%
      rownames_to_column(var = 'regressor') %>%
      pivot_longer(cols = c('Food','Energy','Commodity'), names_to = 'sector', values_to = 'value') %>%
      mutate(region = 'pool')
    
    if(isDisplay){
      stargazer(modelFoodFe, modelEneFe, modelCommFe, 
                type = "text",
                title = "Regression Results",
                dep.var.labels = c("Share:Food", "Share:Energy", "Share:Commodities"),
                digits = 3,
                keep = c('Constant','exp'))
    }
  } else if (ConsData == 'gcd'){
    
    dfRaw = prepare_gcdData(isDisplay,isExport)
    
    hhConsData <- dfRaw %>%
      select(-unit) %>%
      pivot_wider(names_from = variable, values_from = value)
    
    
    if (regionmapping == 'country'){
      hhConsData <- hhConsData %>%
        mutate(region = geo)
    } else if (regionmapping =='pool'){
      hhConsData <- hhConsData %>%
        mutate(region = 'pool')
    } else {
      hhConsData <- hhConsData %>%
        merge(regionMapping,  by= 'geo')
    }
    
    #Identify share columns
    share_cols <- colnames(hhConsData)[grepl("^share\\|", colnames(hhConsData))]
    
    #Nest the data by region
    nested_data <- hhConsData %>%
      group_by(region) %>%
      group_split()
    
    
    obCount<-hhConsData %>%
      group_by(region) %>%
      summarise(row_count = n())
    
    print(obCount)
    
    if (regression_model == 'PolynomialLM' ){
      #Run regression for each share and each region
      results <- map(nested_data, function(region_df) {
        region_name <- unique(region_df$region)
        
        region_df <- region_df %>% mutate(geo = as.factor(geo))
        
        map(share_cols, function(col) {
          
          if (nlevels(region_df$geo) < 2) {
            formula <- as.formula(paste0("`", col, "` ~ log(exp) + I(log(exp)^2)"))
          } else {
            formula <- as.formula(paste0("`", col, "` ~ log(exp) + I(log(exp)^2) + geo"))
          }
          
          model <- lm(formula, data = region_df)
          
          tidy(model) %>%
            mutate(share = col, region = region_name)
        }) %>%
          bind_rows()
      })
      
      # Step 4: Combine all results into a single dataframe
      coef <- bind_rows(results) %>%
        filter(term %in% c("(Intercept)", "log(exp)", "I(log(exp)^2)")) %>%
        select(region, share, term, estimate) %>%
        rename(
          sector = share,
          regressor = term,
          value = estimate
        )%>%
        mutate(sector = str_remove(sector, "^share\\|"))
      
      
    } else if (regression_model == 'logitTransOLS') {
      
      results_logit <- map(nested_data, function(region_df) {
        region_name <- unique(region_df$region)
        
        # Ensure geo is factor for fixed effects
        region_df <- region_df %>% mutate(geo = as.factor(geo))
        
        map(share_cols, function(col) {
          # Apply clipping and logit transform to the LHS variable
          y_clipped <- pmin(pmax(region_df[[col]], 1e-5), 1 - 1e-5)
          region_df$y_logit <- log(y_clipped / (1 - y_clipped))  # logit transform
          
          # Choose regression formula
          if (nlevels(region_df$geo) < 2) {
            formula <- y_logit ~ log(exp) + I(log(exp)^2)
          } else {
            formula <- y_logit ~ log(exp) + I(log(exp)^2) + geo
          }
          
          # Fit model
          model <- lm(formula, data = region_df)
          
          tidy(model) %>%
            mutate(share = col, region = region_name)
        }) %>%
          bind_rows()
      })
      
      coef<- bind_rows(results_logit) %>%
        filter(term %in% c("(Intercept)", "log(exp)", "I(log(exp)^2)")) %>%
        select(region, share, term, estimate) %>%
        rename(
          sector = share,
          regressor = term,
          value = estimate
        ) %>%
        mutate(sector = str_remove(sector, "^share\\|"))
      
      
    }
  
    
    # Get missing countries

    
    # Print warning if any are missing
    
    if (regionmapping %in% c('H12','H21')){
      
      missing_countries <- setdiff(unique(regionMapping$region), unique(coef$region))
      
      if ( length(missing_countries) > 0) {
        warning("⚠️ The following countries are missing from the data:\n", 
                paste(missing_countries, collapse = ", "))
      } else {
        message("REMIND regions are complete")
      }
    }
    

    
    
  }

  
  return(coef)
}



