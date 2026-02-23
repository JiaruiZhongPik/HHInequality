#----This function conducts the regression analysis on Engel Curves, optionally
#on mcc or gcd dataset.

get_engelCurveCoef <- function(
    #The follwoing 2 options defines the basic regional estimates
    dataSource         = c("mcc", "gcd"),
    regressRegGrouping = "H12",              # "pool" => pooled; otherwise => regional
    mccSumShareRange   = c(0.85, 1.05),
    
    #To manually define overrides of basic regional etimates
    #allowed: "sameRegional","samePooled","mccRegional","mccPooled","gcdRegional","gcdPooled"
    regionOverrides    = NULL,
    
    
    allCoef            = FALSE,
    
    isDisplay          = FALSE
) {
  
  dataSource <- match.arg(dataSource)
  regionList <- if (regressRegGrouping %in% c("H12", "H21")) {
    unique(load_regionMapping(regressRegGrouping)$region)
  } else {
    character(0)
  }
  
  
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || is.na(x)) y else x
  
  # -----------------------
  # fetch regression output
  # -----------------------
  fetchCoef <- function(consData, grouping) {
    if (identical(consData, "mcc")) {
      analyze_regression(
        consData = "mcc",
        regressRegGrouping = grouping,
        allCoef = FALSE,
        isDisplay = isDisplay,
        isExport = FALSE,
        mccSumShareRange = mccSumShareRange
      )
    } else {
      analyze_regression(
        consData = "gcd",
        regressRegGrouping = grouping,
        allCoef = FALSE,
        isDisplay = isDisplay,
        isExport = FALSE
      )
    }
  }
  
  # -----------------------
  # pooled request
  # -----------------------
  if (identical(regressRegGrouping, "pool")) {
    coef <- fetchCoef(dataSource, "pool")
    assertShapeUnique(coef, paste0("pooled_", dataSource))
    return(coef)
  }
  
  # -----------------------
  # Build coefficient bank once
  # -----------------------
  mccRegional <- fetchCoef("mcc", regressRegGrouping)
  mccPooled   <- fetchCoef("mcc", "pool")
  gcdRegional <- fetchCoef("gcd", regressRegGrouping)
  gcdPooled   <- fetchCoef("gcd", "pool")
  
  coefBank <- list(
    mccRegional  = mccRegional,
    mccPooled    = mccPooled,
    gcdRegional  = gcdRegional,
    gcdPooled    = gcdPooled,
    sameRegional = if (dataSource == "mcc") mccRegional else gcdRegional,
    samePooled   = if (dataSource == "mcc") mccPooled   else gcdPooled
  )
  


  
  if (!is.null(regionOverrides) && is.list(regionOverrides)) {
    regionOverrides <- unlist(regionOverrides)
  }
  
  # -----------------------
  # Extract per-region coef according to default + overrides
  # -----------------------
  getRegionCoef <- function(regionCode, sourceKey) {
    tbl <- coefBank[[sourceKey]]
    
    if (is.null(tbl) || nrow(tbl) == 0) {
      stop("coefBank[[", sourceKey, "]] is empty.")
    }
    
    if (grepl("Pooled$", sourceKey)) {
      out <- tbl %>%
        dplyr::filter(.data$region == "pool") %>%
        dplyr::mutate(region = regionCode)
      
      if (nrow(out) == 0) {
        stop("Requested pooled source ", sourceKey, " but region=='pool' rows not found.")
      }
      return(out)
    } else {
      out <- tbl %>%
        dplyr::filter(.data$region == .env$regionCode)
      
      if (nrow(out) == 0) {
        stop("Requested regional source ", sourceKey, " but no rows found for region=", regionCode, ".")
      }
      return(out)
    }
  }
  
  coefPatched <- purrr::map_dfr(regionList, function(r) {
    key <- if (!is.null(regionOverrides)) unname(regionOverrides[r]) else NA_character_
    key <- (key %||% "sameRegional")
    getRegionCoef(r, key)
  }) %>%
    dplyr::arrange(region, sector, regressor)
  
  
  #USA staples has invalid values, also patch it with global categorical estimates
  coefPatched  = coefPatched %>%
    left_join(
      coefBank$mccPooled %>%
        filter(sector == "Staples") %>%
        select(sector, regressor, value_pool = value),
      by = c("sector", "regressor")
    ) %>%
    mutate(
      value = if_else(
        region == "USA" & sector == "Staples",
        value_pool,
        value
      )
    ) %>%
    select(-value_pool)
  

  coefPatched
}


analyze_regression <- function(consData = c("gcd", "mcc"),
                               regressRegGrouping = "pool",
                               allCoef = FALSE,
                               isDisplay = TRUE,
                               isExport = FALSE,
                               export_path = NULL,
                               # optional MCC cleaning:
                               mccSumShareRange = c(0.85,1.05)) {
  
  consData <- match.arg(consData)
  
  regionMapping <- if (regressRegGrouping %in% c("H12", "H21")) {
    load_regionMapping(regressRegGrouping)
  } else {
    NULL
  }
  
  hh <- if (consData == "gcd") {
    prepare_gcdData(isDisplay = isDisplay, isExport = isExport)
  } else {
    prepare_mccData(sumShareRange = mccSumShareRange)
  }

  
  hh <- add_regionTag(hh, regressRegGrouping, regionMapping)
  
  out <- estimate_engelCore(
    hh = hh,
    eps = 1e-6,
    add_country_fe = TRUE,
    allCoef = allCoef
  )
  
  if (isExport) {
    if (is.null(export_path)) {
      export_path <- paste0("output/", consData, "_estimates_", regressRegGrouping, ".csv")
    }
    dir.create(dirname(export_path), recursive = TRUE, showWarnings = FALSE)
    readr::write_csv(out$coef_all, export_path)
  }
  
 out
}







#Helper: regression engine
estimate_engelCore <- function(hh,
                                eps = 1e-6,
                                add_country_fe = TRUE,
                                allCoef = FALSE) {
  

  
  # identify share columns
  share_cols <- names(hh)[grepl("^share\\|", names(hh))]
  if (length(share_cols) == 0) stop("No share| columns found.")
  
  # basic sanity checks
  need_cols <- c("geo", "region", "exp", "weight")
  miss <- setdiff(need_cols, names(hh))
  if (length(miss) > 0) stop("Missing required columns: ", paste(miss, collapse = ", "))
  
  obCount <- hh %>%
    dplyr::count(region, name = "row_count")
  
  nested_data <- hh %>%
    dplyr::group_by(region) %>%
    dplyr::group_split()
  
  results <- purrr::map(nested_data, function(region_df) {
    region_name <- unique(region_df$region)
    
    region_df <- region_df %>%
      dplyr::mutate(geo = as.factor(geo))
    
    purrr::map(share_cols, function(col) {
      
      df <- region_df %>%
        dplyr::filter(!is.na(exp), exp > 0,
                      !is.na(weight), weight > 0,
                      !is.na(.data[[col]]),
                      !is.na(geo))
      
      # if (nrow(df) < 5) {
      #   return(tibble::tibble(term = character(), estimate = numeric(), std.error = numeric(),
      #                         share = col, region = region_name))
      # }
      
      fe_ok <- add_country_fe && dplyr::n_distinct(df$geo) >= 2
      
        # logitTransOLS
        y_clip <- pmin(pmax(df[[col]], eps), 1 - eps)
        df$y_logit <- qlogis(y_clip)
        
        rhs <- "log(exp) + I(log(exp)^2)"
        if (fe_ok) rhs <- paste(rhs, "+ geo")
        fml <- stats::as.formula(paste0("y_logit ~ ", rhs))
        
        fit <- stats::lm(fml, data = df, weights = df$weight)
      
      
      broom::tidy(fit) %>%
        dplyr::mutate(share = col, region = region_name)
    }) %>%
      dplyr::bind_rows()
  }) %>%
    dplyr::bind_rows()
  
  coef_all <- results %>%
    dplyr::mutate(
      sector = stringr::str_remove(share, "^share\\|"),
      regressor = term,
      value = estimate,
      se = std.error
    ) %>%
    dplyr::select(region, sector, regressor, value, se)
  
  coef_main <- coef_all %>%
    dplyr::filter(regressor %in% c("log(exp)", "I(log(exp)^2)", "(Intercept)")) %>%
    dplyr::select(-se)
  
  if (!allCoef) return(coef_main)
  
  return(list(
    coefMain = coef_main,
    coefAll  = coef_all,
    obCount  = obCount
  ))
}
