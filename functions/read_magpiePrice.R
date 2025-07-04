#This function reads food price from Magpie and aggregate it to the 4 categories
#Jiarui Zhong, 2025.06.10

read_magpiePrice <- function(magpiepath) {
  
  #food  price with value Added
  priceFAH <- FoodDemandModuleConsumerPrices(magpiepath, level = "iso")
  priceFAFH <- FoodDemandModuleConsumerPrices(magpiepath, level = "iso")
  
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
  fafhShr <-  p15_shr_fafh
  
  fafhShr <- fafhShr[, getYears(priceFAFH), ]
  #----------------------------End reading shares-------------------------------
  
  priceAvg <- priceFAFH * fafhShr + priceFAH * (1-fafhShr)
  
  getNames( priceAvg) <- sub("\\..*$", "", getNames( priceAvg))
  
  
  # aggregate both to 4 categories based on demand 
  lv <- c("livst_rum","livst_pig","livst_chick", "livst_egg", "livst_milk", "fish")
  st <- c("tece", "maiz", "trce", "rice_pro", "soybean", "rapeseed", "groundnut", "sunflower", "puls_pro", 
          "potato", "cassav_sp", "sugr_cane", "sugr_beet", "molasses", "brans", "scp")
  pr = c("oils", "alcohol", "sugar")
  
  mapping <- rbind(data.frame(aggregate  = rep("livestock", length(lv)), k = lv ),
                   data.frame(aggregate = rep("staples", length(st)), k = st ),
                   data.frame(aggregate = "veg", k = "others"),
                   data.frame(aggregate = rep("processed", length(pr)), k =pr ))
  
  weight <- Kcal(magpiepath, level = "iso", product_aggr = FALSE, per_capita = FALSE)
  
  priceIso <- toolAggregate(priceAvg, weight = weight, dim = 3, wdim = 3, rel = mapping, from = "k", to = "aggregate")
  
  regMap <- toolGetMapping("regionmappingH12.csv", type = "regional") 
  
  weightSec <- toolAggregate(weight,dim = 3, rel = mapping, from = "k", to = "aggregate")
  
  price <- toolAggregate(priceIso, weight = weightSec, dim = 1, rel = regMap, from = "CountryCode", to = "RegionCode")
  
  return(price)
  
}
