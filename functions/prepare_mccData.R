prepare_mccData <- function(path = "input/MCC Data Output Deciles.csv",
                     sumShareRange = c(0.85, 1.05), 
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
  
  if (!is.null(sumShareRange)) {
    stopifnot(length(sumShareRange) == 2)
    df <- df %>%
      dplyr::filter(sum_share >= sumShareRange[1] - eps,
                    sum_share <= sumShareRange[2] + eps)
  }
  
  
  df <- df %>% dplyr::select(-country_name)
  
  df
}