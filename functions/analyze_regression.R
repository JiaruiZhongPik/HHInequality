#----This function conducts regression with Eurostat consumption data----------


analyze_regression <- function (regressModel = 'logitTransOLS', consData ='gcd', 
                                regressRegGrouping = 'pool',
                                prune = T,
                                isDisplay = TRUE, isExport = FALSE) {
  
  # Read in regional mapping   
  if(regressRegGrouping =='H12') {
    
    regionMapping<- read_delim("input/regionmappingH12.csv", 
                               delim = ";", escape_double = FALSE, col_types = cols(X = col_skip()), 
                               trim_ws = TRUE,
                               show_col_types = FALSE) %>%
      rename(geo = CountryCode,
             region = RegionCode) 
  } else if (regressRegGrouping == 'H21') {
    
    regionMapping <- read_delim("input/regionmapping_21_EU11.csv", 
                                    delim = ";", escape_double = FALSE, col_types = cols(X = col_skip(), 
                                                                                         missingH12 = col_skip()), 
                                    trim_ws = TRUE,
                                    show_col_types = FALSE) %>%
      rename(geo = CountryCode,
             region = RegionCode)
  }
  

  
  if (consData == 'eurostat'){
    
    # Note: The Eurostat database contains consumption data for 5 income quintiles
    # across 33 European countries. The consumption basket is split into three
    # categories: energy, food, and other commodities. This dataset is only used
    # at an early stage, when global data is not yet available, and is not used
    # in the final analysis.
    
    warning("The Eurostat database contains only data for EU countries and should be used with caution.")
    
    hhConsData <- prepare_eurostatData()
    
    if( regressModel == 'polynomialLM'){
      
      modelFoodFe = lm(wFood ~ log(exp) + I(log(exp)^2)  + geo + TIME_PERIOD , hhConsData )
      
      modelEneFe = lm(wEnergy ~ log(exp) + I(log(exp)^2) + geo + TIME_PERIOD , hhConsData )
      
      modelCommFe = lm(wCommodity ~ log(exp) + I(log(exp)^2) + geo + TIME_PERIOD , hhConsData )
      
      
    } else if (regressModel =='logitTransOLS') {
      
      #Prepare data:clip data to stay strictly between 0 and 1, which is required for logit transformation 
      hhConsData<- hhConsData %>%
        mutate(wFoodClipped = pmin(pmax(wFood, 1e-4), 1 - 1e-4),
               wFoodLogitShare = log(wFoodClipped / (1 - wFoodClipped)),
               
               wEneClipped = pmin(pmax(wEnergy, 1e-4), 1 - 1e-4),
               wEneLogitShare = log(wEneClipped / (1 - wEneClipped)),
               
               wCommClipped = pmin(pmax(wCommodity, 1e-4), 1 - 1e-4),
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
  } else if (consData == 'gcd'){
    
    #Note: GCD: https://datatopics.worldbank.org/consumption/Aboutdatabase
    # GCD is a one-stop data source on household consumption patterns in developing countries.
    # Coverage for developed countries is very sparse.
    
    #GCD data
    dfRaw <- prepare_gcdData(isDisplay,isExport)

    #Population data
    pop <- calcOutput("Population",aggregate = F, scenario = 'SSP1', year = 2010, round = 8) %>%
      as.data.frame() %>% 
      select(-Cell, -Data1) %>%
      mutate(Year = 2010) %>%
      rename(geo = Region,
             pop = Value,
             year = Year)
    
    #Combine consumption and population data
    hhConsData <- dfRaw %>%
      select(-unit) %>%
      pivot_wider(names_from = variable, values_from = value) %>%
      left_join(pop, by=c('geo', 'year'))
    
    #Add regional grouping tag
    if (regressRegGrouping == 'country'){
      hhConsData <- hhConsData %>%
        mutate(region = geo)
    } else if (regressRegGrouping =='pool'){
      hhConsData <- hhConsData %>%
        mutate(region = 'pool')
    } else {
      hhConsData <- hhConsData %>%
        merge(regionMapping,  by= 'geo')
    }
    
    #Identify share columns
    share_cols <- colnames(hhConsData)[grepl("^share\\|", colnames(hhConsData))]
    
    #Nest data by regional tag
    nested_data <- hhConsData %>%
      group_by(region) %>%
      group_split()
    
    # Count the number of observations for inspection
    obCount <- hhConsData %>%
      group_by(region) %>%
      summarise(row_count = n())
    
    print(obCount)
    
    if (regressModel == 'polynomialLM' ){
      
      # polynomialLM: quadratic polynomial linear regression model.
      # This functional form can imply negative consumption shares at higher
      # future expenditure levels and is therefore no longer used.
      
      warning("This functional form may yield negative future consumption shares. Use with caution.")
      
      # run the regression first mappping over region and then mapping over sector
      results <- map(nested_data, function(region_df) {
        
        region_name <- unique(region_df$region)
        
        region_df <- region_df %>% mutate(geo = as.factor(geo))
        
        map(share_cols, function(col) {
          
          # Add regional fixed effects when multiple regions are present
          if (nlevels(region_df$geo) < 2) {
            formula <- as.formula(paste0("`", col, "` ~ log(exp) + I(log(exp)^2)"))
          } else {
            formula <- as.formula(paste0("`", col, "` ~ log(exp) + I(log(exp)^2) + geo"))
          }
          
          model <- lm(formula, data = region_df, weights = pop)
          
          tidy(model) %>%
            mutate(share = col, region = region_name)
        }) %>%
          bind_rows()
      }
      )
      
      # combine all results
      coef <- bind_rows(results) %>%
        filter(term %in% c("(Intercept)", "log(exp)", "I(log(exp)^2)")) %>%
        select(region, share, term, estimate) %>%
        rename(
          sector = share,
          regressor = term,
          value = estimate
        )%>%
        mutate(sector = str_remove(sector, "^share\\|"))
      
      
    } else if (regressModel == 'logitTransOLS') {
      
      results_logit <- map(nested_data, function(region_df) {
        
        region_name <- unique(region_df$region)
        region_df <- region_df %>% mutate(geo = as.factor(geo))
        
        # run the regression first mappping over region and then mapping over sector
        map(share_cols, function(col) {
          
          # clipping and logit transform to the dependent variable
          y_clipped <- pmin(pmax(region_df[[col]], 1e-4), 1 - 1e-4)
          region_df$y_logit <- log(y_clipped / (1 - y_clipped))  # logit transform
          
          # Add regional fixed effects when multiple regions are present
          if (nlevels(region_df$geo) < 2) {
            formula <- y_logit ~ log(exp) + I(log(exp)^2)
          } else {
            formula <- y_logit ~ log(exp) + I(log(exp)^2) + geo
          }
          
          # Fit model
          model <- lm(formula, data = region_df, weights = pop)
          
          tidy(model) %>%
            mutate(share = col, region = region_name)
        }) %>%
          bind_rows()
      })
      
      coef_export <- bind_rows(results_logit) %>% filter(term %in% c("(Intercept)",'log(exp)',"I(log(exp)^2)"))
      
      if(isExport == T){
        write.csv(coef_export, paste0("output/gcd_estimates_",regressRegGrouping,".csv"), row.names = FALSE)
      }
      
      if(prune == F){
        coef_proned <- bind_rows(results_logit) %>%
          filter(term %in% c("(Intercept)", "log(exp)", "I(log(exp)^2)")) %>%
          select(region, share, term, estimate) %>%
          rename(
            sector = share,
            regressor = term,
            value = estimate
          ) %>%
          mutate(sector = str_remove(sector, "^share\\|"))
      } else if(prune == T){
        
        coef_proned <- bind_rows(results_logit) %>%
          filter(term %in% c("(Intercept)", "log(exp)", "I(log(exp)^2)")) %>%
          select(region, share, term, estimate, p.value) %>%
          mutate(
            estimate = if_else(p.value > 0.1, 0, estimate)
          ) %>%
          select(-p.value) %>%
          rename(
            sector = share,
            regressor = term,
            value = estimate
          ) %>%
          mutate(sector = str_remove(sector, "^share\\|"))

        
      }

      
    }
  
    # Check whether all regions are present
    # Issue a warning if any region is missing in the H12/H21 case
    
    if (regressRegGrouping %in% c('H12','H21')){
      
      missing_countries <- setdiff(unique(regionMapping$region), unique(coef$region))
      
      if ( length(missing_countries) > 0) {
        
        warning("⚠️ The following countries are missing from the data:\n", 
                paste(missing_countries, collapse = ", "))
      
        } else {
        
        message("REMIND regions are complete")
      
          }
    }
    
    

    
    
  }

  
  return(coef_proned)
}


#-------------------------Data coverage check-----------------------------------
