
read_magpieExpShare <- function(magpiepath, products = "kfo") {
  
  # aggregate both to 4 categories based on demand 
  lv <- c("livst_rum","livst_pig","livst_chick", "livst_egg", "livst_milk", "fish")
  st <- c("tece", "maiz", "trce", "rice_pro", "soybean", "rapeseed", "groundnut", "sunflower", "puls_pro", 
          "potato", "cassav_sp", "sugr_cane", "sugr_beet", "molasses", "brans", "scp")
  pr = c("oils", "alcohol", "sugar")
  
  mapping <- rbind(data.frame(aggregate  = rep("livestock", length(lv)), k = lv ),
                   data.frame(aggregate = rep("staples", length(st)), k = st ),
                   data.frame(aggregate = "veg", k = "others"),
                   data.frame(aggregate = rep("processed", length(pr)), k =pr ))
  
  
  expShare <- FoodExpenditureShare(magpiepath, level = 'reg', products =  products, product_aggr = F, valueAdded = TRUE)
  
  expShareAgg <- toolAggregate(expShare, dim = 3, rel = mapping, from = "k", to = "aggregate")

  return(expShareAgg)
  
}


