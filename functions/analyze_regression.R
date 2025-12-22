#----This function conducts the regression analysis on Engel Curves, optionally
#on mcc or gcd dataset.


analyze_regression <- function(regressModel = "logitTransOLS",
                               consData = c("gcd", "mcc"),
                               regressRegGrouping = "pool",
                               allCoef = FALSE,
                               isDisplay = TRUE,
                               isExport = FALSE,
                               export_path = NULL,
                               # optional MCC cleaning:
                               mcc_sum_share_range = c(0.85,1.05)) {
  
  consData <- match.arg(consData)
  
  regionMapping <- if (regressRegGrouping %in% c("H12", "H21")) {
    load_region_mapping(regressRegGrouping)
  } else {
    NULL
  }
  
  hh <- if (consData == "gcd") {
    prep_gcd(isDisplay = isDisplay, isExport = isExport)
  } else {
    prep_mcc(sum_share_range = mcc_sum_share_range)
  }
  
  hh <- add_region_tag(hh, regressRegGrouping, regionMapping)
  
  out <- estimate_engel_core(
    hh = hh,
    regressModel = regressModel,
    eps = 1e-6,
    add_country_fe = TRUE,
    allCoef = allCoef
  )
  
  if (isDisplay) print(out$obCount)
  
  if (isExport) {
    if (is.null(export_path)) {
      export_path <- paste0("output/", consData, "_estimates_", regressRegGrouping, ".csv")
    }
    dir.create(dirname(export_path), recursive = TRUE, showWarnings = FALSE)
    readr::write_csv(out$coef_all, export_path)
  }
  
  out$coef
}


#helper 1: region mapping helper
load_region_mapping <- function(regressRegGrouping) {
  if (regressRegGrouping == "H12") {
    readr::read_delim(
      "input/regionmappingH12.csv",
      delim = ";",
      escape_double = FALSE,
      col_types = readr::cols(X = readr::col_skip()),
      trim_ws = TRUE,
      show_col_types = FALSE
    ) %>%
      dplyr::rename(geo = CountryCode, region = RegionCode)
    
  } else if (regressRegGrouping == "H21") {
    readr::read_delim(
      "input/regionmapping_21_EU11.csv",
      delim = ";",
      escape_double = FALSE,
      col_types = readr::cols(X = readr::col_skip(), missingH12 = readr::col_skip()),
      trim_ws = TRUE,
      show_col_types = FALSE
    ) %>%
      dplyr::rename(geo = CountryCode, region = RegionCode)
    
  } else {
    NULL
  }
}

add_region_tag <- function(df, regressRegGrouping, regionMapping = NULL) {
  if (regressRegGrouping == "country") {
    df %>% dplyr::mutate(region = geo)
  } else if (regressRegGrouping == "pool") {
    df %>% dplyr::mutate(region = "pool")
  } else {
    if (is.null(regionMapping)) stop("regionMapping is NULL but regressRegGrouping requires it.")
    df %>% dplyr::left_join(regionMapping, by = "geo")
  }
}


#Helper2: dataset prep adapters
prep_gcd <- function(isDisplay = FALSE, isExport = FALSE) {
  # prepare_gcdData must return long format:
  # geo, year, incomegroup, variable, value, unit(optional), popShare/pop
  df_long <- prepare_gcdData(isDisplay, isExport) %>%
    dplyr::select(-dplyr::any_of("unit"))
  
  # force unique keys BEFORE pivot_wider (prevents list-cols)
  df_wide <- df_long %>%
    dplyr::group_by(geo, year, incomegroup, variable) %>%
    dplyr::summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = variable, values_from = value)
  
  # unify weight naming
  if ("popShare" %in% names(df_wide)) {
    df_wide <- df_wide %>% dplyr::mutate(weight = popShare)
  } else if ("pop" %in% names(df_wide)) {
    df_wide <- df_wide %>% dplyr::mutate(weight = pop)
  } else {
    stop("GCD data has neither popShare nor pop. Add a weight column.")
  }
  
  df_wide
}


prep_mcc <- function(path = "input/MCC Data Output Deciles.csv",
                     sum_share_range = c(0.85, 1.05), 
                     eps = 1e-12) {
  # Requirements: readr, dplyr, countrycode, stringr (optional)
  
  goods_cols <- c(
    "fruits", "animal", "other_food", "transport",
    "other_fuels", "goods", "gas", "electricity", "staple"
  )
  
  # 1) Read
  df_raw <- readr::read_csv(path, show_col_types = FALSE)
  
  # 2) Basic column checks (fail fast, not mysteriously)
  need <- c("Country", "Income_Group_10", "expenditures_USD_2017", "hh_weights", goods_cols)
  miss <- setdiff(need, names(df_raw))
  if (length(miss) > 0) {
    stop("prep_mcc(): missing columns in input CSV: ", paste(miss, collapse = ", "))
  }
  
  # 3) Reverse logit -> shares in (0,1)
  df_unlogit <- df_raw %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(goods_cols), ~ plogis(.x)))
  
  # 4) Harmonize to GCD-style schema
  df <- df_unlogit %>%
    dplyr::transmute(
      country_name = Country,
      geo = countrycode::countrycode(country_name, origin = "country.name", destination = "iso3c"),
      incomegroup = as.character(Income_Group_10),     # deciles 1..10 but keep as character like GCD
      exp = as.numeric(expenditures_USD_2017),
      pop = as.numeric(hh_weights),
      weight = as.numeric(hh_weights),                # unified weight column
      
      `share|Animal products`        = as.numeric(animal),
      `share|Building electricity`   = as.numeric(electricity),
      `share|Empty calories`         = as.numeric(other_food),
      `share|Transport energy`       = as.numeric(transport),
      `share|Fruits vegetables nuts` = as.numeric(fruits),
      `share|Building gases`         = as.numeric(gas),
      `share|Building other fuels`   = as.numeric(other_fuels),
      `share|Other commodities`      = as.numeric(goods),
      `share|Staples`                = as.numeric(staple)
    )
  
  # 5) Drop failed ISO3 matches (or keep and warn)
  n_bad_iso <- sum(is.na(df$geo) | df$geo == "", na.rm = TRUE)
  if (n_bad_iso > 0) {
    warning("prep_mcc(): ", n_bad_iso, " rows have missing ISO3 (geo) after countrycode(). Dropping them.")
    df <- df %>% dplyr::filter(!is.na(geo), geo != "")
  }
  
  # 6) Optional: filter by sum of shares (closure check)
  share_cols <- names(df)[grepl("^share\\|", names(df))]
  df <- df %>%
    dplyr::mutate(sum_share = rowSums(dplyr::across(dplyr::all_of(share_cols)), na.rm = TRUE))
  
  if (!is.null(sum_share_range)) {
    stopifnot(length(sum_share_range) == 2)
    df <- df %>%
      dplyr::filter(sum_share >= sum_share_range[1] - eps,
                    sum_share <= sum_share_range[2] + eps)
  }
  

   df <- df %>% dplyr::select(-country_name)
  
  df
}

#Helper3: the shared regression engine

estimate_engel_core <- function(hh,
                                regressModel = c("logitTransOLS", "polynomialLM"),
                                eps = 1e-6,
                                add_country_fe = TRUE,
                                allCoef = FALSE) {
  
  regressModel <- match.arg(regressModel)
  
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
      
      if (nrow(df) < 5) {
        return(tibble::tibble(term = character(), estimate = numeric(), std.error = numeric(),
                              share = col, region = region_name))
      }
      
      fe_ok <- add_country_fe && dplyr::n_distinct(df$geo) >= 2
      
      if (regressModel == "polynomialLM") {
        rhs <- "log(exp) + I(log(exp)^2)"
        if (fe_ok) rhs <- paste(rhs, "+ geo")
        fml <- stats::as.formula(paste0("`", col, "` ~ ", rhs))
        
        fit <- stats::lm(fml, data = df, weights = df$weight)
        
      } else {
        # logitTransOLS
        y_clip <- pmin(pmax(df[[col]], eps), 1 - eps)
        df$y_logit <- qlogis(y_clip)
        
        rhs <- "log(exp) + I(log(exp)^2)"
        if (fe_ok) rhs <- paste(rhs, "+ geo")
        fml <- stats::as.formula(paste0("y_logit ~ ", rhs))
        
        fit <- stats::lm(fml, data = df, weights = df$weight)
      }
      
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
    dplyr::filter(regressor %in% c("log(exp)", "I(log(exp)^2)")) %>%
    dplyr::select(-se)
  
  if (!allCoef) coef_all <- coef_main
  
  list(
    coef = coef_main,
    coef_all = coef_all,
    obCount = obCount
  )
}
