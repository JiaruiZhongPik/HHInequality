


predict_decileConsShare <- function(
    data, coef, gini_baseline,
    countryExample = NA,
    isDisplay = FALSE, isExport = FALSE,
    # blending options
    doBlending = FALSE,
    blendTailProb = 0.95,
    blendEndFactor = 2,
    blendingRegGrouping = "H12",
    mccSumShareRange = c(0.85, 1.05)
) {
  
  shareVars <- c(
    "share|Building gases",
    "share|Building electricity",
    "share|Building other fuels",
    "share|Transport energy",
    "share|Animal products",
    "share|Staples",
    "share|Fruits vegetables nuts",
    "share|Empty calories"
  )
  macroVars <- c("Consumption","Population", shareVars)
  
  if (isExport == TRUE) {
    dir.create(paste0(outputPath), recursive = TRUE, showWarnings = FALSE)
  }
  
  # -----------------------------
  # A) Clean + add macro shares
  # -----------------------------
  data1 <- data %>%
    dplyr::filter(region != "World") %>%
    dplyr::select(-baseline, -model) %>%
    calc_addVariable(
      "FEShare|Household" =
        "(`FE|Buildings|Gases` * `Price|Buildings|Gases` +
          `FE|Buildings|Electricity` * `Price|Buildings|Electricity` +
          `FE|Buildings|Other fuels` * `Price|Buildings|Other fuels` +
          `FE|++|Transport` * `Price|Transport|FE`) / `Consumption`",
      "share|Building gases" =
        "`FE|Buildings|Gases` * `Price|Buildings|Gases` / `Consumption`",
      "share|Building electricity" =
        "`FE|Buildings|Electricity` * `Price|Buildings|Electricity` / `Consumption`",
      "share|Building other fuels" =
        "`FE|Buildings|Other fuels` * `Price|Buildings|Other fuels` / `Consumption`",
      "share|Transport energy" =
        "`FE|++|Transport` * `Price|Transport|FE` / `Consumption`"
    )
  
  
  plotdf <-  data1 %>%
    select(-unit) %>%
    filter(
      variable %in% c("Consumption", "FEShare|Household", "Population") |
        str_starts(variable, "share\\|")
    ) %>%
    pivot_wider(names_from = variable, values_from = value) %>%
    mutate(consumptionCa = Consumption / Population *1000)
  
  # -----------------------------
  # B) Decile shares input (SSP/SDP)
  # -----------------------------
  consShare <- prepareConsShare(
    data = data,
    regions = regions,
    gini_baseline = gini_baseline,
    all_runscens = all_runscens
  )
  
  # -----------------------------
  # C) Build decile-level expenditure points (dataDecile)
  # -----------------------------
  dataDecile <- data1 %>%
    dplyr::select(-unit) %>%
    calc_addVariable(
      "consumptionCA" = "(`Consumption` *1e9)/(`Population` *1e6)",
      units = c("US$2017")
    ) %>%
    dplyr::filter(variable == "consumptionCA") %>%
    dplyr::slice(rep(1:dplyr::n(), each = 10)) %>%
    dplyr::group_by(region, period, scenario) %>%
    dplyr::mutate(
      decileGroup = 1:10,
      gdp_scenario = sub(".*((SSP[0-9]+[A-Z]*?)|(SDP_[^\\-]+)).*", "\\1", scenario)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(consShare, by = c("period", "region", "gdp_scenario", "decileGroup")) %>%
    dplyr::mutate(
      consumptionCa = value * consShare * 10,
      logCons  = log(consumptionCa),
      logCons2 = logCons^2
    ) %>%
    dplyr::select(scenario, region, period, decileGroup, consumptionCa, logCons, logCons2)
  
  # -----------------------------
  # D) Macro representative points (macroWide)
  # -----------------------------
  macroWide <- data1 %>%
    dplyr::filter(variable %in% macroVars) %>%
    dplyr::select(-unit) %>%
    tidyr::pivot_wider(names_from = variable, values_from = value) %>%
    dplyr::mutate(
      expMacro = (Consumption * 1e9) / (Population * 1e6)
    )
  
  # -----------------------------
  # E) Common regression objects
  # -----------------------------

  modeledSectors <- sort(setdiff(unique(coef$sector), "Other commodities"))
  scenarios  <- unique(data1$scenario)
  periods    <- unique(data1$period)
  regionsAll <- sort(unique(data1$region))  # World already removed above
  
  # Keep old Route-A behavior: if coef is single-region, expand to all regions
  coefRegForWide <- if (dplyr::n_distinct(coef$region) == 1) dplyr::mutate(coef, region = "pool") else coef
  
  coefWideReg <- coefToWide(
    coefTbl   = coefRegForWide,
    scenarios = scenarios,
    periods   = periods,
    regions   = regionsAll,
    suffix    = "Reg"
  )
  
  fixedReg <- computeFixedEffLogit(
    macroWide      = macroWide,
    coefWide       = coefWideReg,
    modeledSectors = modeledSectors,
    suffix         = "Reg"
  )
  
  # -----------------------------
  # F) Optional pooled objects (only if blending)
  # -----------------------------
  if (doBlending) {
    coefPool <- analyze_regression(
      consData = "mcc",
      regressRegGrouping = "pool",
      allCoef = FALSE,
      isDisplay = FALSE,
      isExport = FALSE,
      mccSumShareRange = mccSumShareRange
    )
    
    coefWidePool <- coefToWide(
      coefTbl   = coefPool,
      scenarios = scenarios,
      periods   = periods,
      regions   = regionsAll,
      suffix    = "Pool"
    )
    
    fixedPool <- computeFixedEffLogit(
      macroWide      = macroWide,
      coefWide       = coefWidePool,
      modeledSectors = modeledSectors,
      suffix         = "Pool"
    )
    
    #compute historic tail of consumption expenditure
    hhMcc <- prepare_mccData(sumShareRange = mccSumShareRange)
    regionMapping <- if (regressRegGrouping %in% c("H12", "H21")) load_regionMapping(regressRegGrouping) else NULL
    hhMcc <- add_regionTag(hhMcc, regressRegGrouping, regionMapping)
    
    tailMcc <- computeHistTailExpByRegion(
      expTbl = hhMcc %>% dplyr::select(region, exp, weight),
      tailProb = blendTailProb,
      weightCol = "weight"
    )
    
    histTailExpByRegion <- tailMcc %>% filter(region != 'CHA') # CHA is removed as MCC only has data for HK and TWN,therefore not representative
    
    
  }
  
  # -----------------------------
  # G) Predict (single exit)
  # -----------------------------
  if (doBlending) {
    dataFull <- predictSharesLogitBlended(
      dataDecile     = dataDecile,
      coefWideReg    = coefWideReg,
      coefWidePool   = coefWidePool,
      fixedReg       = fixedReg,
      fixedPool      = fixedPool,
      histTailExpByRegion = histTailExpByRegion,
      modeledSectors = modeledSectors,
      endFactor      = blendEndFactor
    )
  } else {
    dataFull <- predictSharesLogitNoBlend(
      dataDecile     = dataDecile,
      coefWideReg    = coefWideReg,
      fixedReg       = fixedReg,
      modeledSectors = modeledSectors
    )
  }
  
  
  #Visualization
  if(length(unique(coef$sector)) ==9){
    
    
    # Function to generate plots per sector
    plot_sector <- function(sector) {
      # Convert sector to its equivalent in dfOutput
      dfOutput_sector <- sub("FEShare", "share", sector)
      
      y_vals <- plotdf[[sector]]  # `sector` is assumed to be a character string
      
      ylims <- robust_ylim(y_vals)
      
      p1 <- ggplot(
        plotdf,
        aes(x = log(consumptionCa), y = !!sym(sector), color = factor(region))
      ) +
        geom_point(alpha = 0.6) +
        facet_wrap(~scenario) +
        labs(
          title = paste("REMIND-MAgPIE -", sector),
          x = "log(ConsumptionCa), US$2017",
          y = "Share"
        ) +
        theme_minimal() +
        ylim(ylims) +
        scale_color_brewer(palette = "Paired") +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.5)
      
      
      #Creat P2
      df_filtered <- dfOutput[dfOutput$decileGroup %in% c(1, 10), ]
      
      y_vals <- df_filtered[[dfOutput_sector]]  # make sure dfOutput_sector is a character string
      
      ylims <- robust_ylim(y_vals)
      
      p2 <- ggplot(
        dfOutput[dfOutput$decileGroup %in% c(1, 10), ],
        aes(x = log(consumptionCa), y = !!sym(dfOutput_sector), 
            color = factor(region), shape = factor(decileGroup))
      ) +
        geom_point(alpha = 0.6) +
        facet_wrap(~scenario) +
        ylim(ylims) +
        labs(
          title = paste("Projected share -", sector),
          x = "log(ConsumptionCa), US$2017",
          y = "Share"
        ) +
        theme_minimal() +
        scale_color_brewer(palette = "Paired") +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.5)
      
      p1+p2
      
    }
    
    # Define sectors you want to loop through
    sectors <- c(
      "share|Building gases",
      "share|Building electricity",
      "share|Building other fuels",
      "share|Transport energy"
    )
    
    # Generate combined plots for each sector
    combined_plots <- purrr::map(sectors, plot_sector)
    
    # Combine and display all plots in a grid
    all_combined_plot <- wrap_plots(combined_plots, ncol = 1, guides = "collect") &
      theme(legend.position = "bottom") 
    
    ggsave(
      filename = paste0(outputPath, "/combined_FE_share_plot_gcd.tiff"),
      plot = all_combined_plot,
      width = 10,
      height = 12,
      dpi = 300,
      compression = "lzw"
    )
    
    # Define sectors you want to loop through
    sectors <- c(
      "share|Animal products",
      "share|Staples",
      "share|Fruits vegetables nuts",
      "share|Empty calories"
    )
    
    
    # Generate combined plots for each sector
    combined_plots <- purrr::map(sectors, plot_sector)
    
    # Combine and display all plots in a grid
    all_combined_plot <- wrap_plots(combined_plots, ncol = 1, guides = "collect") &
      theme(legend.position = "bottom") 
    
    ggsave(
      filename = paste0(outputPath, "/combined_Food_share_plot_gcd.tiff"),
      plot = all_combined_plot,
      width = 10,
      height = 12,
      dpi = 300,
      compression = "lzw"
    ) 
    
    
    if(!all(is.na(countryExample))){
      
      
      # Function to generate plots per sector
      plot_sector <- function(sector) {
        # Convert sector to its equivalent in dfOutput
        dfOutput_sector <- sub("FEShare", "share", sector)
        
        # Create p1
        p1 <- ggplot(
          plotdf[plotdf$region == region, ],
          aes(x = log(consumptionCa), y = !!sym(sector), color = factor(region))
        ) +
          geom_point(alpha = 0.6) +
          facet_wrap(~scenario) +
          labs(
            title = paste("REMIND -", sector),
            x = "log(ConsumptionCa), US$2017",
            y = "Share"
          ) +
          theme_minimal() +
          ylim(
            quantile(plotdf[plotdf$region == region, ][[sector]], probs = c(0.01, 0.99), na.rm = TRUE)
          ) +
          scale_color_discrete(name = "Region") +
          geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.5)
        
        # Create p2
        p2 <- ggplot(
          dfOutput[dfOutput$region == region, ],
          aes(x = log(consumptionCa), y = !!sym(dfOutput_sector), color = factor(decileGroup))
        ) +
          geom_point(alpha = 0.6) +
          facet_wrap(~scenario) +
          ylim(
            quantile(dfOutput[dfOutput$region == region, ][[dfOutput_sector]], probs = c(0.01, 0.99), na.rm = TRUE)
          ) +
          labs(
            title = paste("Projected share -", sector),
            x = "log(ConsumptionCa), US$2017",
            y = "Share"
          ) +
          theme_minimal() +
          scale_color_discrete(name = "Decile group") +
          geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.5)
        
        p1+p2
        
      }
      
      # Define sectors you want to loop through
      sectors <- c(
        "share|Building gases",
        "share|Building electricity",
        "share|Building other fuels",
        "share|Transport energy"
      )
      
      for (region in countryExample){
        combined_plots <- purrr::map(sectors, plot_sector)
        
        # Combine and display all plots in a grid
        all_combined_plot <- wrap_plots(combined_plots, ncol = 1, guides = "collect") &
          theme(legend.position = "bottom") 
        
        ggsave(
          filename = paste0(outputPath,"/combined_FE_share_plot_gcd_",region,".tiff"),
          plot = all_combined_plot,
          width = 10,
          height = 12,
          dpi = 300,
          compression = "lzw" )
      }
      
      
      
      # Define sectors you want to loop through
      sectors <- c(
        "share|Animal products",
        "share|Staples",
        "share|Fruits vegetables nuts",
        "share|Empty calories"
      )
      
      for (region in countryExample){
        # Generate combined plots for each sector
        combined_plots <- purrr::map(sectors, plot_sector)
        
        # Combine and display all plots in a grid
        all_combined_plot <- wrap_plots(combined_plots, ncol = 1, guides = "collect") &
          theme(legend.position = "bottom") 
        
        ggsave(
          filename = paste0(outputPath,"/combined_food_share_plot_gcd_",region,".tiff"),
          plot = all_combined_plot,
          width = 10,
          height = 12,
          dpi = 300,
          compression = "lzw"
        )
      }
      
    }
  }
  
  dataFull <- 
    dataFull %>%
    dplyr::select(scenario, region, period, decileGroup, consumptionCa, dplyr::starts_with("share|"))
  
  return(dataFull)
}


# ===========================================================================
# helpers: This computes weighted quantile, for later computation of regional
# exp quantile. pop/popShare will be used as weights
# ===========================================================================
wtdQuantile <- function(x, w, probs = 0.95) {
  
  stopifnot(length(x) == length(w))
  
  ok <- is.finite(x) & is.finite(w) & w > 0
  x <- x[ok]; w <- w[ok]
  if (length(x) == 0) return(NA_real_)
  
  o <- order(x)
  x <- x[o]; w <- w[o]
  
  cw <- cumsum(w) / sum(w)

  keep <- !duplicated(cw)
  
  approx(
    x = cw[keep],
    y = x[keep],
    xout = probs,
    rule = 2
  )$y
}

# ===========================================================================
# helpers: This function computes a tail expenditure threshold (e.g., P95/P99) 
# from historical data, using weighted quantiles when weights are available and
# falling back to unweighted quantiles otherwise.
# ===========================================================================

computeHistTailExpByRegion <- function(expTbl, tailProb = 0.95, weightCol) {
  
  stopifnot(all(c("region", "exp") %in% names(expTbl)))
  stopifnot(is.numeric(tailProb), tailProb > 0, tailProb < 1)
  stopifnot(!missing(weightCol), length(weightCol) == 1, is.character(weightCol))
  stopifnot(weightCol %in% names(expTbl))
  
  expTbl %>%
    dplyr::filter(
      is.finite(exp), exp > 0,
      is.finite(.data[[weightCol]]), .data[[weightCol]] > 0
    ) %>%
    dplyr::group_by(region) %>%
    dplyr::summarise(
      histTailExp = wtdQuantile(exp, .data[[weightCol]], probs = tailProb),
      .groups = "drop"
    )
}

# ==============================================================================
# It returns a smooth blending weight that transitions from regional to pooled 
# between histTailExp and endFactor * histTailExp. Cubit stpe is used to avoid
# avoid discontinuity/kins when stitching regimes.
# ==============================================================================
blendWeightSmoothstep <- function(exp, histTailExp, endFactor = 2, useLogSpace = TRUE) {
  stopifnot(endFactor > 1)
  
  n <- length(exp)
  if (length(histTailExp) != n) histTailExp <- rep(histTailExp, length.out = n)
  
  endExp <- endFactor * histTailExp
  
  if (useLogSpace) {
    x  <- log(exp)
    x0 <- log(histTailExp)
    x1 <- log(endExp)
  } else {
    x  <- exp
    x0 <- histTailExp
    x1 <- endExp
  }
  
  t <- (x - x0) / (x1 - x0)     # 0 at x0, 1 at x1
  t <- pmax(0, pmin(1, t))      # clamp to [0,1]
  t^2 * (3 - 2 * t)             # smoothstep weight in [0,1]
}



# ==============================================================================
# Formatting the coefficient table
# ==============================================================================
coefToWide <- function(coefTbl, scenarios, periods, regions = NULL, suffix = "") {
  
  out <- tidyr::crossing(
    scenario = stringr::str_remove(scenarios, "-(rem|mag)-\\d+$"),
    period   = periods,
    coefTbl
  ) %>%
    dplyr::mutate(variable = paste(sector, regressor, sep = "|")) %>%
    dplyr::select(region, scenario, period, variable, value) %>%
    tidyr::pivot_wider(names_from = variable, values_from = value)
  
  if (identical(unique(coefTbl$region), "pool")) {
    out <- tidyr::crossing(region = regions, out %>% dplyr::select(-region))
  }
  
  if (nzchar(suffix)) out <- dplyr::rename_with(out, ~ paste0(.x, suffix),
                                                -dplyr::all_of(c("region","scenario","period")))
  out
}



# ==============================================================================
# It reads and prepares the consumption share of each decile in national total
# consumption, according to different sources of Gini projection. The computation 
# of consShare is done in mrremind, change are local yet.
# ==============================================================================
prepareConsShare <- function(data, regions, gini_baseline,
                             all_runscens,
                             hist_periods = c(2000, 2005, 2010, 2015),
                             input_dir = "input") {
  
  # --- SSP consumption shares (from .cs4r) ---
  readConsShareSSP <- function(regions, gini_baseline, input_dir) {
    if (!gini_baseline %in% c("rao", "poblete05", "poblete07")) {
      stop("Unsupported gini_baseline: ", gini_baseline)
    }
    
    path <- file.path(input_dir, paste0("f_consShare_", regions, "_", gini_baseline, ".cs4r"))
    
    read.csv(path, skip = 6, header = FALSE) %>%
      dplyr::rename(
        period      = V1,
        region      = V2,
        scenario    = V3,
        decileGroup = V4,
        consShare   = V5
      ) %>%
      dplyr::mutate(gdp_scenario = gsub("^gdp_", "", scenario)) %>%
      dplyr::select(-scenario) %>%
      dplyr::filter(grepl("^SSP[0-9]$", gdp_scenario))
  }
  
  shareSSP <- readConsShareSSP(regions, gini_baseline, input_dir)
  
  # --- SDP shares (Shape / Min et al. 2024) + fill missing historic from SSP1 ---
  shareSDP <- prepare_GiniSDP() %>%
    dplyr::mutate(gdp_scenario = as.character(gdp_scenario))
  
  ssp1_hist <- shareSSP %>%
    dplyr::filter(gdp_scenario == "SSP1", period %in% hist_periods) %>%
    dplyr::select(-gdp_scenario)   # <- key line: remove duplicate name before crossing
  
  sdp_scenarios <- shareSDP %>%
    dplyr::distinct(gdp_scenario)
  
  filled_hist <- tidyr::crossing(ssp1_hist, sdp_scenarios)
  
  shareSDP_filled <- dplyr::bind_rows(shareSDP, filled_hist)
  
  # --- combine + create SSP2EU from SSP2 ---
  consShare <- dplyr::bind_rows(shareSSP, shareSDP_filled)
  
  consShare <- dplyr::bind_rows(
    consShare,
    consShare %>%
      dplyr::filter(gdp_scenario == "SSP2") %>%
      dplyr::mutate(gdp_scenario = "SSP2EU")
  )
  
  # --- filter to scenarios & periods used in the run ---
  consShare %>%
    dplyr::filter(
      gdp_scenario %in% all_runscens,
      period %in% unique(data$period)
    )
}



# ==============================================================================
# Engine for dynamic level calibration, computes the constant term for each 
# region/year/category
# ==============================================================================
computeFixedEffLogit <- function(macroWide, coefWide, modeledSectors, suffix = "") {

  df <- macroWide %>%
    dplyr::left_join(coefWide, by = c("scenario", "period", "region")) %>%
    dplyr::mutate(consumptionCaRep = (Consumption * 1e9) / (Population * 1e6))
  
  for (s in modeledSectors) {
    shareCol <- paste0("share|", s)
    b1Col <- paste0(s, "|log(exp)", suffix)
    b2Col <- paste0(s, "|I(log(exp)^2)", suffix)
    outCol <- paste0("fixedEffIntercept|", s, suffix)
    
    df[[outCol]] <- qlogis(df[[shareCol]]) -
      df[[b1Col]] * log(df$consumptionCaRep) -
      df[[b2Col]] * (log(df$consumptionCaRep)^2)
  }
  
  keep <- c("scenario","period","region", paste0("fixedEffIntercept|", modeledSectors, suffix))
  df %>% dplyr::select(dplyr::all_of(keep))
}


# ==============================================================================
# Computing shares using or not using blending
# ==============================================================================
predictSharesLogitBlended <- function(
    dataDecile,
    coefWideReg, coefWidePool,
    fixedReg, fixedPool,
    histTailExpByRegion,
    modeledSectors,
    endFactor = 2
    ) {
  
  
  df <- dataDecile %>%
    dplyr::left_join(histTailExpByRegion, by = "region") %>%
    dplyr::left_join(coefWideReg,  by = c("scenario","period","region")) %>%
    dplyr::left_join(coefWidePool, by = c("scenario","period","region")) %>%
    dplyr::left_join(fixedReg,     by = c("scenario","period","region")) %>%
    dplyr::left_join(fixedPool,    by = c("scenario","period","region")) %>%
    dplyr::mutate(
      wBlend = blendWeightSmoothstep(
        exp = consumptionCa,
        histTailExp = histTailExp,
        endFactor = endFactor,
        useLogSpace = TRUE
      )
    )
  
  logExp  <- log(pmax(df$consumptionCa, 1e-12))
  logExp2 <- logExp^2
  
  for (s in modeledSectors) {
    b1Reg  <- df[[paste0(s, "|log(exp)Reg")]]
    b2Reg  <- df[[paste0(s, "|I(log(exp)^2)Reg")]]
    feReg  <- df[[paste0("fixedEffIntercept|", s, "Reg")]]
    
    b1Pool <- df[[paste0(s, "|log(exp)Pool")]]
    b2Pool <- df[[paste0(s, "|I(log(exp)^2)Pool")]]
    fePool <- df[[paste0("fixedEffIntercept|", s, "Pool")]]
    
    w <- df$wBlend
    
    shapeReg  <- b1Reg  * logExp + b2Reg  * logExp2
    shapePool <- b1Pool * logExp + b2Pool * logExp2
    
    shapeBlend <- dplyr::case_when(
      w <= 0 ~ shapeReg,
      w >= 1 ~ shapePool,
      TRUE   ~ (1 - w) * shapeReg + w * shapePool
    )
    
    # FE rule: when w==1, force pooled FE (fixes JPN if feReg is NA)
    fixedBlend <- dplyr::case_when(
      w >= 1 ~ fePool,
      w <= 0 ~ feReg,
      TRUE   ~ (1 - w) * feReg + w * fePool   # or just feReg if you don't want FE blending
    )
    
    yBlend <- shapeBlend + fixedBlend
    df[[paste0("share|", s)]] <- plogis(yBlend)
  }
  
  # closure
  modeledShareCols <- paste0("share|", modeledSectors)
  df[["share|Other commodities"]] <- 1 - rowSums(df[, modeledShareCols, drop = FALSE], na.rm = TRUE)
  
  df
} 

predictSharesLogitNoBlend <- function(
    dataDecile,
    coefWideReg,
    fixedReg,
    modeledSectors
) {
  df <- dataDecile %>%
    dplyr::left_join(coefWideReg, by = c("scenario","period","region")) %>%
    dplyr::left_join(fixedReg,    by = c("scenario","period","region"))
  
  logExp  <- df$logCons
  logExp2 <- df$logCons2
  
  for (s in modeledSectors) {
    b1 <- df[[paste0(s, "|log(exp)Reg")]]
    b2 <- df[[paste0(s, "|I(log(exp)^2)Reg")]]
    fe <- df[[paste0("fixedEffIntercept|", s, "Reg")]]
    
    df[[paste0("share|", s)]] <- plogis(b1 * logExp + b2 * logExp2 + fe)
  }
  
  modeledShareCols <- paste0("share|", modeledSectors)
  df[["share|Other commodities"]] <- 1 - rowSums(df[, modeledShareCols, drop = FALSE], na.rm = TRUE)
  
  df
}


add_regionTag <- function(df, regressRegGrouping, regionMapping = NULL) {
  if (regressRegGrouping == "pool") {
    df %>% dplyr::mutate(region = "pool")
  } else {
    if (is.null(regionMapping)) stop("regionMapping is NULL but regressRegGrouping requires it.")
    df %>% dplyr::left_join(regionMapping, by = "geo")
  }
}
