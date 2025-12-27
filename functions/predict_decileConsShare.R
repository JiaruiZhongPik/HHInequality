# ===========================================================================
# helpers: This computes weighted quantile, for later computation of regional
# exp quantile. pop/popShare will be used as weights
# ===========================================================================
wtdQuantile <- function(x, w, probs = 0.95) {
  stopifnot(length(x) == length(w))
  stopifnot(is.numeric(probs), all(probs > 0), all(probs < 1))
  
  ok <- is.finite(x) & is.finite(w) & w > 0
  x <- x[ok]; w <- w[ok]
  if (length(x) == 0) return(NA_real_)
  
  o <- order(x)
  x <- x[o]; w <- w[o]
  
  cw <- cumsum(w) / sum(w)
  cw <- pmin(pmax(cw, 0), 1) 
  
  idx <- !duplicated(cw, fromLast = TRUE)
  cw_u <- cw[idx]
  x_u  <- x[idx]

  as.numeric(approx(x = cw_u, y = x_u, xout = probs, ties = "ordered", rule = 2)$y)
}

# ===========================================================================
# helpers: This function computes a tail expenditure threshold (e.g., P95/P99) 
# from historical data, using weighted quantiles when weights are available and
# falling back to unweighted quantiles otherwise.
# ===========================================================================
computeHistTailExpByRegion <- function(expTbl, tailProb = 0.95, weightCol = NULL) {
  stopifnot(all(c("region", "exp") %in% names(expTbl)))
  stopifnot(is.numeric(tailProb), tailProb > 0, tailProb < 1)
  
  if (!is.null(weightCol) && weightCol %in% names(expTbl)) {
    
    expTbl %>%
      dplyr::filter(
        !is.na(exp), exp > 0,
        !is.na(.data[[weightCol]]), .data[[weightCol]] > 0
      ) %>%
      dplyr::group_by(region) %>%
      dplyr::summarise(
        histTailExp = wtdQuantile(x = exp, w = .data[[weightCol]], probs = tailProb),
        .groups = "drop"
      )
    
  } else {
    
    expTbl %>%
      dplyr::filter(!is.na(exp), exp > 0) %>%
      dplyr::group_by(region) %>%
      dplyr::summarise(
        histTailExp = as.numeric(stats::quantile(exp, probs = tailProb, na.rm = TRUE)),
        .groups = "drop"
      )
  }
}



# ==============================================================================
# It returns a smooth blending weight that transitions from regional to pooled 
# between histTailExp and endFactor * histTailExp. Cubit stpe is used to avoid
# avoid discontinuity/kins when stitching regimes.
# ==============================================================================
blendWeightSmoothstep <- function(exp, histTailExp, endFactor = 2,
                                  naTo = c("pooled", "regional"),
                                  useLogSpace = TRUE) {
  naTo <- match.arg(naTo)
  stopifnot(is.numeric(endFactor), endFactor > 1)
  
  n <- length(exp)
  if (length(histTailExp) != n) {
    # allow scalar histTailExp, recycle if needed
    histTailExp <- rep(histTailExp, length.out = n)
  }
  
  w <- rep(NA_real_, n)
  
  # fallback mask: missing/invalid exp or missing/invalid histTailExp
  bad <- !is.finite(exp) | exp <= 0 | !is.finite(histTailExp) | histTailExp <= 0
  
  w[bad] <- if (naTo == "pooled") 1 else 0
  
  ok <- !bad
  if (!any(ok)) return(w)
  
  endExp <- endFactor * histTailExp[ok]
  
  if (useLogSpace) {
    x  <- log(pmax(exp[ok], 1e-12))
    x0 <- log(pmax(histTailExp[ok], 1e-12))
    x1 <- log(pmax(endExp, 1e-12))
  } else {
    x  <- exp[ok]
    x0 <- histTailExp[ok]
    x1 <- endExp
  }
  
  denom <- (x1 - x0)

  nearZero <- !is.finite(denom) | abs(denom) < 1e-12
  t <- rep(1, length(x))  # default pooled if degenerate
  t[!nearZero] <- (x[!nearZero] - x0[!nearZero]) / denom[!nearZero]
  
  t <- pmin(pmax(t, 0), 1)
  w[ok] <- t * t * (3 - 2 * t)  
  
  w
}


# ----------------------------
# coef table -> wide columns like "Staples|(Intercept)", etc.
# with suffix to avoid name clash when joining pooled/regional
# ----------------------------
coefToWide <- function(coefTbl, scenarios, periods, regions = NULL, suffix = "") {
  out <- tidyr::crossing(
    scenario = scenarios,
    period   = periods,
    coefTbl
  ) %>%
    dplyr::mutate(
      variable = paste(sector, regressor, sep = "|"),
      scenario = stringr::str_remove(scenario, "-(rem|mag)-\\d+$")
    ) %>%
    dplyr::select(region, scenario, period, variable, value) %>%
    tidyr::pivot_wider(names_from = variable, values_from = value)
  
  # if coefTbl is pooled-only (region == "pool"), expand to all regions
  if (dplyr::n_distinct(coefTbl$region) == 1 && unique(coefTbl$region) == "pool") {
    stopifnot(!is.null(regions))
    out <- tidyr::crossing(
      region = regions,
      out %>% dplyr::select(-region)
    )
  }
  
  if (suffix != "") {
    keyCols <- c("region", "scenario", "period")
    coefCols <- setdiff(names(out), keyCols)
    out <- out %>% dplyr::rename_with(~ paste0(.x, suffix), dplyr::all_of(coefCols))
  }
  
  out
}



getHistTailExpForBlending <- function(
    regressRegGrouping = "H12",
    tailProb = 0.95,
    mccSumShareRange = c(0.85, 1.05),
    patchChaFromGcdRegional = TRUE
) {
  # MCC (main)
  hhMcc <- prep_mcc(sum_share_range = mccSumShareRange)
  regionMapping <- if (regressRegGrouping %in% c("H12", "H21")) load_region_mapping(regressRegGrouping) else NULL
  hhMcc <- add_region_tag(hhMcc, regressRegGrouping, regionMapping)
  
  tailMcc <- computeHistTailExpByRegion(
    expTbl = hhMcc %>% dplyr::select(region, exp, weight),
    tailProb = tailProb,
    weightCol = "weight"
  )
  
  # Patch CHA with GCD regional exp distribution (if requested)
  if (patchChaFromGcdRegional) {
    hhGcd <- prep_gcd(isDisplay = FALSE, isExport = FALSE)
    hhGcd <- add_region_tag(hhGcd, regressRegGrouping, regionMapping)
    
    tailGcd <- computeHistTailExpByRegion(
      expTbl = hhGcd %>% dplyr::select(region, exp, weight),
      tailProb = tailProb,
      weightCol = "weight"
    ) %>% dplyr::filter(region == "CHA")
    
    if (nrow(tailGcd) == 1) {
      tailMcc <- tailMcc %>%
        dplyr::filter(region != "CHA") %>%
        dplyr::bind_rows(tailGcd)
    }
  }
  
  tailMcc
}

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

predictSharesLogitBlended <- function(
    dataDecile,
    coefWideReg, coefWidePool,
    fixedReg, fixedPool,
    histTailExpByRegion,
    modeledSectors,
    endFactor = 2,
    naTo = c("pooled", "regional")
) {
  naTo <- match.arg(naTo)
  
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
        naTo = naTo,
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

blendLogitSafe <- function(yReg, yPool, w) {
  dplyr::case_when(
    is.na(w)          ~ NA_real_,
    w <= 0            ~ yReg,
    w >= 1            ~ yPool,
    TRUE              ~ (1 - w) * yReg + w * yPool
  )
}

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


predict_decileConsShare <- function(
    data, coef, gini_baseline,
    regression_model = "logitTransOLS",
    countryExample = NA,
    isDisplay = FALSE, isExport = FALSE,
    # ---- NEW: blending options ----
    doBlending = FALSE,
    blendTailProb = 0.95,
    blendEndFactor = 2,
    blendNaTo = c("pooled","regional"),
    blendingRegGrouping = "H12",
    mccSumShareRange = c(0.85, 1.05)
    ){
  
  blendNaTo <- match.arg(blendNaTo)
  
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
  
  #export folder
  if(isExport == T){
    dir.create(paste0(outputPath), recursive = TRUE, showWarnings = FALSE)
  }
  
  
  #Compute regional annual expenditure share
  data1 <- 
    data %>%
    filter(region != 'World'
    ) %>%
    select(-baseline,-model) %>%
    calc_addVariable("FEShare|Household" = "(`FE|Buildings|Gases` * `Price|Buildings|Gases` +
                   `FE|Buildings|Electricity` * `Price|Buildings|Electricity`+
                   `FE|Buildings|Other fuels` * `Price|Buildings|Other fuels`+
                   `FE|++|Transport` * `Price|Transport|FE`) /
                   `Consumption`",
                     "share|Building gases" = "`FE|Buildings|Gases` * `Price|Buildings|Gases`/ `Consumption`",
                     "share|Building electricity" = "`FE|Buildings|Electricity` * `Price|Buildings|Electricity`/`Consumption`",
                     "share|Building other fuels" = "`FE|Buildings|Other fuels` * `Price|Buildings|Other fuels`/`Consumption`",
                     "share|Transport energy" = "`FE|++|Transport` * `Price|Transport|FE`/ `Consumption`"
    )
  
  #Get the SSP Gini based consumption shares for deciles
  consShare <- prepareConsShare(
    data = data,
    regions = regions,
    gini_baseline = gini_baseline,
    all_runscens = all_runscens
  )
  
  plotdf <-  data1 %>%
    select(-unit) %>%
    filter(
      variable %in% c("Consumption", "FEShare|Household", "Population") |
        str_starts(variable, "share\\|")
    ) %>%
    pivot_wider(names_from = variable, values_from = value) %>%
    mutate(consumptionCA = Consumption / Population *1000)
  
  dataDecile <- data1 %>%
    calc_addVariable("consumptionCA" = "(`Consumption` *1e9)/(`Population` *1e6)",units = c("US$2017") )%>%
    filter( variable == 'consumptionCA' , 
            !region == 'World' ) %>%
    slice(rep(1:n(), each = 10)) %>% 
    group_by(region,period,scenario) %>%
    mutate(decileGroup = 1:10,
           gdp_scenario = sub(".*((SSP[0-9]+[A-Z]*?)|(SDP_[^\\-]+)).*", "\\1", scenario)) %>%  
    ungroup()  %>%
    right_join(consShare, by = c("period", "region",'gdp_scenario','decileGroup'))%>%
    mutate(consumptionCa = value * consShare *10) %>%
    select( -value, -gdp_scenario, -consShare, -variable) %>%
    dplyr::mutate(
      logCons = log(consumptionCa),
      logCons2 = logCons^2
    )
  
  
  coef_expanded <- crossing(scenario = unique(data1$scenario), coef,
                            period = unique(data1$period)) %>%
    mutate( variable = paste (sector, regressor, sep='|')) %>%
    select(-sector, -regressor ) %>%
    mutate(model = 'empiric',
           scenario= str_remove(scenario, "-(rem|mag)-\\d+$") ) 
  
  
  if( length(unique(coef$region))==1){
    coef_expanded <- crossing(region = setdiff(unique(data$region), "World"), coef_expanded %>% select(-region))
  }
  
  
 
  if (regression_model =='logitTransOLS'){
    
    macroWide <- data1 %>%
      dplyr::filter(variable %in% macroVars) %>%
      dplyr::select(-unit) %>%
      tidyr::pivot_wider(names_from = variable, values_from = value) %>%
      dplyr::mutate(
        expMacro = (Consumption * 1e9) / (Population * 1e6),
        logExpMacro = log(expMacro),
        logExpMacro2 = logExpMacro^2
      )
    
    modeledSectors <- sort(setdiff(unique(coef$sector), "Other commodities"))
    
    predict_noBlend <- function(macroWide, dataDecile, coef_expanded, shareVars) {
      
      coefLong <- coef_expanded %>%
        dplyr::select(scenario, period, region, variable, value) %>%
        tidyr::separate(variable, into = c("sector", "term"), sep = "\\|", extra = "merge", fill = "right") %>%
        dplyr::mutate(term = dplyr::case_when(
          term == "log(exp)" ~ "b1",
          term == "I(log(exp)^2)" ~ "b2",
          term == "(Intercept)" ~ "b0",
          TRUE ~ term
        )) %>%
        dplyr::filter(sector %in% sub("^share\\|","", shareVars)) %>%
        tidyr::pivot_wider(names_from = term, values_from = value)
      
      fixedEffects <- macroWide %>%
        dplyr::select(scenario, period, region, logExpMacro, logExpMacro2, dplyr::all_of(shareVars)) %>%
        tidyr::pivot_longer(cols = dplyr::all_of(shareVars), names_to = "shareVar", values_to = "shareMacro") %>%
        dplyr::mutate(sector = sub("^share\\|","", shareVar)) %>%
        dplyr::left_join(coefLong, by = c("scenario","period","region","sector")) %>%
        dplyr::mutate(fixedEffIntercept = logit(shareMacro) - b1*logExpMacro - b2*logExpMacro2) %>%
        dplyr::select(scenario, period, region, sector, fixedEffIntercept)
      
      predLong <- dataDecile %>%
        tidyr::crossing(sector = sub("^share\\|","", shareVars)) %>%
        dplyr::left_join(coefLong, by = c("scenario","period","region","sector")) %>%
        dplyr::left_join(fixedEffects, by = c("scenario","period","region","sector")) %>%
        dplyr::mutate(
          shareLogit = b1*logCons + b2*logCons2 + fixedEffIntercept,
          share = invLogit(shareLogit)
        )
      
      predWide <- predLong %>%
        dplyr::select(scenario, region, period, decileGroup, consumptionCa, sector, share) %>%
        tidyr::pivot_wider(names_from = sector, values_from = share, names_prefix = "share|") %>%
        dplyr::mutate(
          `share|Other commodities` =
            1 - (`share|Building gases` + `share|Building electricity` + `share|Building other fuels` +
                   `share|Transport energy` + `share|Staples` + `share|Animal products` +
                   `share|Fruits vegetables nuts` + `share|Empty calories`)
        )
      
      predWide
    }
    
    
    if (!doBlending) {
      
      dataFull <- predict_noBlend(macroWide, dataDecile, coef_expanded, shareVars)
      
    } else {
      
      # 1) pooled target is MCC pooled
      coefPool <- analyze_regression(
        regressModel = regression_model,
        consData = "mcc",
        regressRegGrouping = "pool",
        allCoef = FALSE,
        isDisplay = FALSE,
        isExport = FALSE,
        mcc_sum_share_range = mccSumShareRange
      )
      
      scenarios <- unique(data1$scenario)
      periods   <- unique(data1$period)
      regionsAll <- setdiff(unique(data$region), "World")
      
      # 2) coef tables -> wide (suffix to avoid name clashes)
      coefWideReg <- coefToWide(coef,     scenarios, periods, regions = regionsAll, suffix = "Reg")
      coefWidePool <- coefToWide(coefPool, scenarios, periods, regions = regionsAll, suffix = "Pool")
      
      # 3) intercept+FE implied by rep point, for both curves
      fixedReg <- computeFixedEffLogit(macroWide, coefWideReg,  modeledSectors, suffix = "Reg")
      fixedPool <- computeFixedEffLogit(macroWide, coefWidePool, modeledSectors, suffix = "Pool")
      
      # 4) hist tail exp per region (MCC, with CHA patched from GCD regional)
      allRegions <- sort(setdiff(unique(data1$region), "World")) 
      histTailExpByRegion <- getHistTailExpForBlending(
        regressRegGrouping = blendingRegGrouping,
        tailProb = blendTailProb,
        mccSumShareRange = mccSumShareRange,
        patchChaFromGcdRegional = TRUE
      ) %>%
        tidyr::complete(region = allRegions, fill = list(histTailExp = NA_real_))
      
      # 5) predict deciles with smooth blending in logit space
      dataFull <- predictSharesLogitBlended(
        dataDecile = dataDecile,
        coefWideReg = coefWideReg,
        coefWidePool = coefWidePool,
        fixedReg = fixedReg,
        fixedPool = fixedPool,
        histTailExpByRegion = histTailExpByRegion,
        modeledSectors = modeledSectors,
        endFactor = blendEndFactor,
        naTo = blendNaTo
      )
    }
  }
  
  #Output
  dfOutput <- dataFull %>%
    select(scenario, region, period, decileGroup, consumptionCa, starts_with("share|"))
  
  robust_ylim <- function(x, lower = 0.01, upper = 0.99, buffer = 0.05) {
    q <- quantile(x, c(lower, upper), na.rm = TRUE)
    pad <- buffer * diff(q)
    c(q[1] - pad, q[2] + pad)
  }
  
  #Plot routine, seprate for s3 and s9 case
  if(length(unique(coef$sector)) ==9){
    
    
    # Function to generate plots per sector
    plot_sector <- function(sector) {
      # Convert sector to its equivalent in dfOutput
      dfOutput_sector <- sub("FEShare", "share", sector)
      
      y_vals <- plotdf[[sector]]  # `sector` is assumed to be a character string
      
      ylims <- robust_ylim(y_vals)
      
      p1 <- ggplot(
        plotdf,
        aes(x = log(consumptionCA), y = !!sym(sector), color = factor(region))
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
          aes(x = log(consumptionCA), y = !!sym(sector), color = factor(region))
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
  
  
  
  
  
  return (dfOutput)
}

