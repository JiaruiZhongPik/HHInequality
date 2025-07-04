#This function reads food expenditure from Magpie and aggregate it to the 4 categories
#Jiarui Zhong, 2025.06.10

read_magpieExpShare <- function(magpiepath, products = "kfo") {
  
  #food  price with value Added
  priceFAH <- FoodDemandModuleConsumerPrices(magpiepath, level = 'iso' )
  priceFAFH <- FoodDemandModuleConsumerPrices(magpiepath, level = 'iso')
  
  #------------------------Value added shares-----------------------------------
  #if uses a coupled version later than June, the share variable could be readed directly from gdx
  #fafhShr <- readGDX(magpiepath, "p15_shr_fafh")
  
  #For older runs, this needs to be computed manually and back engineered
  tempfolder <- local_tempdir()
  untar("S://p/projects/magpie/data/input/archive/additional_data_rev4.63.tgz", exdir = tempfolder)
  f15_fafh_coef <- read.magpie(file.path(tempfolder, "f15_fafh_coef.csv"))
  f15_markup_coef <- read.csv(file.path(tempfolder, "f15_markup_coef.csv"), skip = 1) 
  f15_markup_coef <- as.magpie(f15_markup_coef, spatial = "GLO", temporal = NULL)
  
  gdp <- readGDX(magpiepath, "im_gdp_pc_mer_iso")
  fm_attributes <- readGDX(magpiepath, "fm_attributes")
  fm_nutrition_attributes <- readGDX(magpiepath, "fm_nutrition_attributes")
  p15_kcal_pc_iso <- readGDX(magpiepath, "p15_kcal_pc_iso")[,getYears(f15_markup_coef),]
  
  p15_shr_fafh = f15_fafh_coef[,,"a_fafh"] + f15_fafh_coef[,,"b_fafh"] * gdp
  p15_shr_fafh[p15_shr_fafh > 1] <- 1
  p15_shr_fafh[p15_shr_fafh < 0 ] <- 0

  
  #Note for Magpie versions later than 2025 June, run the following function will produce the result
  #exp <- FoodExpenditure(magpiepath, level = 'iso', valueAdded = TRUE, per_capita = TRUE, product_aggr = FALSE)
  #For earlier results, this has to be done by back engineering
  p15_marketing_margin_fah = f15_markup_coef[,,"fah"][,,"a"] * f15_markup_coef[,,"fah"][,, "b"]^log(gdp) +
    f15_markup_coef[,,"fah"][,, "b"] * fm_attributes[,,"wm"][,,getItems(f15_markup_coef, dim = 3.1)] 
  p15_marketing_margin_fah_kcal = collapseNames(p15_marketing_margin_fah / (fm_nutrition_attributes[,getYears(p15_marketing_margin_fah),getItems(f15_markup_coef, dim = 3.1)][,,"kcal"] * 10^6))
  
  p15_marketing_margin_fafh = f15_markup_coef[,,"fafh"][,,"a"] * (f15_markup_coef[,,"fafh"][,, "b"]^log(gdp) +
                                                                    f15_markup_coef[,,"fafh"][,, "b"]) * fm_attributes[,,"wm"][,,getItems(f15_markup_coef, dim = 3.1)]
  p15_marketing_margin_fafh_kcal = collapseNames(p15_marketing_margin_fafh / (fm_nutrition_attributes[,getYears(p15_marketing_margin_fah),getItems(f15_markup_coef, dim = 3.1)][,,"kcal"]*10^6))
  
  p15_value_added_expenditures_pc = collapseNames(
    p15_shr_fafh[,getYears(p15_kcal_pc_iso),] * p15_kcal_pc_iso * p15_marketing_margin_fafh_kcal[,getYears(p15_kcal_pc_iso),] + 
      (1-p15_shr_fafh[,getYears(p15_kcal_pc_iso),]) * p15_kcal_pc_iso * p15_marketing_margin_fah_kcal[,getYears(p15_kcal_pc_iso),] 
  )
  
  pop <- population(magpiepath, level = 'iso')
  avExp <- p15_value_added_expenditures_pc * pop
  
  price = FoodDemandModuleConsumerPrices(magpiepath)
  value = price * Kcal(gdx = magpiepath, level = 'iso', calibrated = TRUE, 
                       after_shock = TRUE, products = "kfo", product_aggr = FALSE, 
                       per_capita = FALSE)
  
  #convert to mer 
  value <- convertGDP(value,  unit_in = "constant 2017 US$MER",
                      unit_out = "constant 2017 Int$PPP",
                      replace_NAs = "with_USA")
  
  value <- value + avExp
  
  out <- gdxAggregate(gdx = magpiepath, x = value, weight = "Kcal", 
                      to = 'iso', absolute = TRUE, calibrated = TRUE, after_shock = TRUE, 
                      products = "kfo", product_aggr = FALSE, per_capita = FALSE)
  out <- out/pop
  
  out[is.nan(out)] <- 0
  
  if (!products %in% getNames(out)) {
    products <- findset(products)
  }
  exp <- out[, , products] * 365
  
  # aggregate both to 4 categories based on demand 
  lv <- c("livst_rum","livst_pig","livst_chick", "livst_egg", "livst_milk", "fish")
  st <- c("tece", "maiz", "trce", "rice_pro", "soybean", "rapeseed", "groundnut", "sunflower", "puls_pro", 
          "potato", "cassav_sp", "sugr_cane", "sugr_beet", "molasses", "brans", "scp")
  pr = c("oils", "alcohol", "sugar")
  
  mapping <- rbind(data.frame(aggregate  = rep("livestock", length(lv)), k = lv ),
                   data.frame(aggregate = rep("staples", length(st)), k = st ),
                   data.frame(aggregate = "veg", k = "others"),
                   data.frame(aggregate = rep("processed", length(pr)), k =pr ))
  
  weight <- Kcal(magpiepath, level = 'iso', product_aggr = FALSE)
  
  expIso <- toolAggregate(exp, dim = 3, rel = mapping, from = "k", to = "aggregate")
  
  regMap <- toolGetMapping("regionmappingH12.csv", type = "regional") 
  
  weightSec <- toolAggregate(weight,dim = 3, rel = mapping, from = "k", to = "aggregate")
  
  exp <- toolAggregate(expIso, weight = weightSec, dim = 1, rel = regMap, from = "CountryCode", to = "RegionCode")
  
  gdpReg <- readGDX(magpiepath, "im_gdp_pc_mer")
  
  expShare <- exp/gdpReg[,getYears(exp),]
  
  return(expShare)
  
}
