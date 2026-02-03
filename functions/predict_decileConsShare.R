
# =============================================================================
# Decile-level consumption share projection (Engel curves) with:
#   1) micro prediction from regional Engel curves and pooled Engel curves
#   2) blending in SHARE space (NOT logit space)
#   3) ratio calibration (method A) to ensure macro consistency
#   4) closure + renormalization (Other commodities)
#
# NOTE: This script assumes these external functions already exist in your codebase:
#   - calc_addVariable()
#   - analyze_regression()
#   - prepare_mccData()
#   - prepare_GiniSDP()
#   - load_regionMapping()
# and these objects exist in scope when calling predict_decileConsShare():
#   - regions, all_runscens, outputPath
# =============================================================================


# =============================================================================
# Main entry
# =============================================================================
predict_decileConsShare <- function(
    data, coef, gini_baseline,
    countryExample = NA,
    isDisplay = FALSE, isExport = FALSE,
    # blending options
    doBlending = FALSE,
    blendStartFactor = 1.5,
    blendEndFactor = 10,
    blendingRegGrouping = "H12",
    mccSumShareRange = c(0.85, 1.05),
    ceilingBuff = 0.2
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
    calc_addVariable(
      "consumptionCa" = "(`Consumption` *1e9)/(`Population` *1e6)",
      units = c("US$2017")
    ) %>%
    dplyr::select(-unit) %>%
    dplyr::filter(variable == "consumptionCa") %>%
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
      logCons  = log(pmax(consumptionCa, 1e-12)),
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
      expMacro = (Consumption * 1e9) / (Population * 1e6),
      # add "Other commodities" on macro side too (closure)
      `share|Other commodities` = 1 - (
        `share|Building gases` +
          `share|Building electricity` +
          `share|Building other fuels` +
          `share|Transport energy` +
          `share|Animal products` +
          `share|Staples` +
          `share|Fruits vegetables nuts` +
          `share|Empty calories`
      )
    )
  
  # -----------------------------
  # E) Common regression objects
  # -----------------------------
  modeledSectors <- sort(unique(coef$sector))
  scenarios  <- unique(data1$scenario)
  periods    <- unique(data1$period)
  regionsAll <- sort(unique(data1$region))
  

  coefRegForWide <- if (dplyr::n_distinct(coef$region) == 1) dplyr::mutate(coef, region = "pool") else coef
  
  coefWideReg <- coefToWide(
    coefTbl   = coefRegForWide,
    scenarios = scenarios,
    periods   = periods,
    regions   = regionsAll,
    suffix    = "Reg"
  )
  
  shareCols <- grep("^share\\|", colnames(macroWide), value = TRUE)
  
  # get mcc consumption data
  hhMcc <- prepare_mccData(sumShareRange = mccSumShareRange)
  regionMapping <- if (blendingRegGrouping %in% c("H12", "H21")) load_regionMapping(blendingRegGrouping) else NULL
  hhMcc <- add_regionTag(hhMcc, blendingRegGrouping, regionMapping)
  
  
  ceilingBySector <- hhMcc %>%
    tidyr::pivot_longer(
      cols = dplyr::starts_with("share|"),
      names_to = "sector",
      values_to = "share"
    ) %>%
    dplyr::mutate(sector = stringr::str_remove(sector, "^share\\|")) %>%
    dplyr::filter(is.finite(share), share > 0, share < 1) %>%
    dplyr::group_by(sector) %>%
    dplyr::summarise(
      shareCeiling = quantile(share, probs = 0.99, na.rm = TRUE),
      .groups = "drop"
    )  %>%
    mutate(sectorCol = paste0("share|", sector)) %>%
    select(sectorCol, shareCeiling)
  
  ceilingVec <- ceilingBySector$shareCeiling
  names(ceilingVec) <- ceilingBySector$sectorCol
  
  ceilingVecBuff <- ceilingVec * (ceilingBuff + 1)
  
  
  applyCeilingWide <- function(df, shareCols, ceilingVec, eps = 1e-6) {
    colsUse <- intersect(shareCols, names(ceilingVec))
    df %>%
      mutate(across(all_of(colsUse), ~ pmin(.x, ceilingVec[cur_column()]))) %>%
      mutate(across(all_of(shareCols), ~ pmax(.x, eps)))   
  }
  
  
  
  
  
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
    
    

    # -----------------------------
    # G) Predict with blending + calibration
    # -----------------------------
    # 1) attach wBlend (decile-level)

    #  historic max value
    histMaxExpByGeo <- hhMcc %>%
      dplyr::group_by(region) %>%
      dplyr::summarise(
        histMaxExp = max(exp, na.rm = TRUE),
        .groups = "drop"
      )  %>% dplyr::filter(region != "CHA") # CHA removed as MCC only has HK and TWN -> not representative
    
    
    dfBase <- dataDecile %>%
      dplyr::left_join(histMaxExpByGeo, by = "region") %>%
      dplyr::mutate(
        wBlend = blendWeightSmoothstep(
          exp = consumptionCa,
          histTailExp = histMaxExp,
          startFactor = blendStartFactor,
          endFactor = blendEndFactor,
          useLogSpace = TRUE
        )
      )
    
    # 2) raw predictions using pooled Engel Curve
    sharePoolRaw <- predictSharesLogitRaw(dfBase, coefWidePool,modeledSectors, suffix = "Pool")
    shareRaw <- predictSharesLogitRaw(dfBase, coefWideReg,modeledSectors, suffix = "Reg")
    
    # 3) blend shares
    shareBlend <- blendShares(shareRaw, sharePoolRaw, modeledSectors, wCol = "wBlend") %>%
      select(scenario, region, period, decileGroup, consumptionCa, all_of(shareCols) ) %>%
      applyCeilingWide(shareCols, ceilingVec)
    

    dataCalibrated <- shareBlend %>%
      group_by(scenario, region, period) %>%
      group_modify(function(dfGroup, keys) {
        
        # get the macro target row for this group
        macroRow <- macroWide %>%
          filter(
            scenario == keys$scenario,
            region   == keys$region,
            period   == keys$period
          )
        
        if (nrow(macroRow) != 1) {
          stop("macroWide missing or non-unique for group: ",
               paste(keys$scenario, keys$region, keys$period, sep = " | "))
        }
        
        # build inputs for RAS
        shareRawMat <- as.matrix(dfGroup[, shareCols, drop = FALSE])
        wDecile <- dfGroup$consumptionCa
        
        macroShare <- as.numeric(macroRow[, shareCols, drop = FALSE])
        names(macroShare) <- shareCols
        
        # run calibration
        shareCalMat <- weightedRas(
          shareRawMat = shareRawMat,
          wDecile     = wDecile,
          macroShare  = macroShare
        )
        
        # write back with same columns as input
        dfGroup[, shareCols] <- shareCalMat
        dfGroup
      }) %>%
      ungroup()
    
    
    
    
    
  } else {
    
    
    shareRaw <- predictSharesLogitRaw(dataDecile, coefWideReg, modeledSectors, suffix = "Reg")
    

    shareRaw <- shareRaw %>%
      dplyr::select(scenario, region, period, decileGroup, consumptionCa, dplyr::starts_with("share|")) %>%
      applyCeilingWide(shareCols, ceilingVec)
    
  
    dataCalibrated <- shareRaw %>%
      group_by(scenario, region, period) %>%
      group_modify(function(dfGroup, keys) {
        
        # get the macro target row for this group
        macroRow <- macroWide %>%
          filter(
            scenario == keys$scenario,
            region   == keys$region,
            period   == keys$period
          )
        
        if (nrow(macroRow) != 1) {
          stop("macroWide missing or non-unique for group: ",
               paste(keys$scenario, keys$region, keys$period, sep = " | "))
        }
        
        # build inputs for RAS
        shareRawMat <- as.matrix(dfGroup[, shareCols, drop = FALSE])
        wDecile <- dfGroup$consumptionCa
        
        macroShare <- as.numeric(macroRow[, shareCols, drop = FALSE])
        names(macroShare) <- shareCols
        
        # run calibration
        shareCalMat <- weightedRas(
          shareRawMat = shareRawMat,
          wDecile     = wDecile,
          macroShare  = macroShare
        )
        
        # write back with same columns as input
        dfGroup[, shareCols] <- shareCalMat
        dfGroup
      }) %>%
      ungroup()
  }
  


    
    
    # Function to generate plots per sector
    plot_sector <- function(sector) {
      
      # Convert sector to its equivalent in dataCalibrated
      dataCalibrated_sector <- sub("FEShare", "share", sector)
      
      y_vals <- plotdf[[sector]]  # `sector` is assumed to be a character string
      
      
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
        scale_color_brewer(palette = "Paired") +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.5)
      
      
      #Creat P2
      df_filtered <- dataCalibrated[dataCalibrated$decileGroup %in% c(1, 10), ]
      
      y_vals <- df_filtered[[dataCalibrated_sector]]  # make sure dataCalibrated_sector is a character string
      

      
      p2 <- ggplot(
        dataCalibrated[dataCalibrated$decileGroup %in% c(1, 10), ],
        aes(x = log(consumptionCa), y = !!sym(dataCalibrated_sector), 
            color = factor(region), shape = factor(decileGroup))
      ) +
        geom_point(alpha = 0.6) +
        facet_wrap(~scenario) +
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
    
    if(isExport == T){
      ggsave(
        filename = paste0(outputPath, "/combined_FE_share_plot_gcd.tiff"),
        plot = all_combined_plot,
        width = 10,
        height = 12,
        dpi = 300,
        compression = "lzw"
      )
    }
    
    

    
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
    
    if(isExport == T){
      ggsave(
        filename = paste0(outputPath, "/combined_Food_share_plot_gcd.tiff"),
        plot = all_combined_plot,
        width = 10,
        height = 12,
        dpi = 300,
        compression = "lzw"
      ) 
      
    }

    
    if(!all(is.na(countryExample))){
      
      
      # Function to generate plots per sector
      plot_sector <- function(sector) {
        # Convert sector to its equivalent in dataCalibrated
        dataCalibrated_sector <- sub("FEShare", "share", sector)
        
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
          dataCalibrated[dataCalibrated$region == region, ],
          aes(x = log(consumptionCa), y = !!sym(dataCalibrated_sector), color = factor(decileGroup))
        ) +
          geom_point(alpha = 0.6) +
          facet_wrap(~scenario) +
          ylim(
            quantile(dataCalibrated[dataCalibrated$region == region, ][[dataCalibrated_sector]], probs = c(0.01, 0.99), na.rm = TRUE)
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

  
  
  
  return(dataCalibrated)
}


# =============================================================================
# helpers: Weighted quantile
# =============================================================================
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


# =============================================================================
# helpers: Tail expenditure threshold by region
# =============================================================================
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
      histTailExp = {
        ord <- order(exp)
        x   <- exp[ord]
        w   <- .data[[weightCol]][ord]
        cw  <- cumsum(w) / sum(w)
        x[which(cw >= tailProb)[1]]
      },
      .groups = "drop"
    )
}


# =============================================================================
# helpers: Smoothstep blending weight (0->1) between histTailExp and endFactor*histTailExp
# =============================================================================

blendWeightSmoothstep <- function(exp, histTailExp, startFactor = 2, endFactor = 5, useLogSpace = T) {
  
  n <- length(exp)
  if (length(histTailExp) != n) histTailExp <- rep(histTailExp, length.out = n)
  
  
  endExp <- endFactor * histTailExp
  startExp <- startFactor * histTailExp
  
  if (useLogSpace) {
    x  <- log(pmax(exp, 1e-12))
    x0 <- log(pmax(startExp, 1e-12))
    x1 <- log(pmax(endExp, 1e-12))
  } else {
    x  <- exp
    x0 <- startExp
    x1 <- endExp
  }
  
  t <- (x - x0) / (x1 - x0)
  t <- pmax(0, pmin(1, t))
  t^2 * (3 - 2 * t)
}


# =============================================================================
# helpers: Coefficient table -> wide
# =============================================================================
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
  
  if (nzchar(suffix)) {
    out <- dplyr::rename_with(
      out,
      ~ paste0(.x, suffix),
      -dplyr::all_of(c("region","scenario","period"))
    )
  }
  
  out
}


# =============================================================================
# helpers: Prepare decile consumption shares (SSP/SDP)
# =============================================================================
prepareConsShare <- function(data, regions, gini_baseline,
                             all_runscens,
                             hist_periods = c(2000, 2005, 2010, 2015),
                             input_dir = "input") {
  
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
  
  # SDP shares (Shape / Min et al.) + fill missing historic from SSP1
  shareSDP <- prepare_GiniSDP() %>%
    dplyr::mutate(gdp_scenario = as.character(gdp_scenario))
  
  ssp1_hist <- shareSSP %>%
    dplyr::filter(gdp_scenario == "SSP1", period %in% hist_periods) %>%
    dplyr::select(-gdp_scenario)
  
  sdp_scenarios <- shareSDP %>%
    dplyr::distinct(gdp_scenario)
  
  filled_hist <- tidyr::crossing(ssp1_hist, sdp_scenarios)
  
  shareSDP_filled <- dplyr::bind_rows(shareSDP, filled_hist)
  
  consShare <- dplyr::bind_rows(shareSSP, shareSDP_filled)
  
  # create SSP2EU from SSP2
  consShare <- dplyr::bind_rows(
    consShare,
    consShare %>% dplyr::filter(gdp_scenario == "SSP2") %>% dplyr::mutate(gdp_scenario = "SSP2EU")
  )
  
  consShare %>%
    dplyr::filter(
      gdp_scenario %in% all_runscens,
      period %in% unique(data$period)
    )
}


# =============================================================================
# helpers: Add region tag to microdata for tail computation
# =============================================================================
add_regionTag <- function(df, regressRegGrouping, regionMapping = NULL) {
  if (regressRegGrouping == "pool") {
    df %>% dplyr::mutate(region = "pool")
  } else {
    if (is.null(regionMapping)) stop("regionMapping is NULL but regressRegGrouping requires it.")
    df %>% dplyr::left_join(regionMapping, by = "geo")
  }
}


# =============================================================================
# Micro prediction: raw shares from one coefficient set (Reg or Pool)
# =============================================================================
predictSharesLogitRaw <- function(dataDecile, coefWide, modeledSectors, suffix = "") {
  df <- dataDecile %>%
    dplyr::left_join(coefWide, by = c("scenario","period","region"))
  
  logExp  <- df$logCons
  logExp2 <- df$logCons2
  
  for (s in modeledSectors) {
    b1 <- df[[paste0(s, "|log(exp)", suffix)]]
    b2 <- df[[paste0(s, "|I(log(exp)^2)", suffix)]]
    fe <- df[[paste0(s, "|(Intercept)",suffix)]]
    df[[paste0("share|", s)]] <- plogis(b1 * logExp + b2 * logExp2 + fe)
  }
  
  df
}


# =============================================================================
# SHARE-space blending between dfReg and dfPool using wBlend (NA->pool)
blendShares <- function(dfReg, dfPool, modeledSectors, wCol = "wBlend") {
  keys <- c("scenario","region","period","decileGroup","consumptionCa","logCons","logCons2", wCol)
  
  # sanity: keys must exist
  missReg  <- setdiff(keys, names(dfReg))
  missPool <- setdiff(keys, names(dfPool))
  if (length(missReg)  > 0) stop("dfReg missing keys: ", paste(missReg, collapse = ", "))
  if (length(missPool) > 0) stop("dfPool missing keys: ", paste(missPool, collapse = ", "))
  
  out <- dfReg %>%
    dplyr::select(dplyr::all_of(keys), dplyr::starts_with("share|")) %>%
    dplyr::rename_with(~ paste0(.x, "_reg"), .cols = dplyr::starts_with("share|")) %>%
    dplyr::left_join(
      dfPool %>%
        dplyr::select(dplyr::all_of(keys), dplyr::starts_with("share|")) %>%
        dplyr::rename_with(~ paste0(.x, "_pool"), .cols = dplyr::starts_with("share|")),
      by = keys
    )
  
  w <- out[[wCol]]
  
  for (s in modeledSectors) {
    col <- paste0("share|", s)
    regCol  <- paste0(col, "_reg")
    poolCol <- paste0(col, "_pool")
    
    if (!regCol %in% names(out))  stop("Missing column in blended join: ", regCol)
    if (!poolCol %in% names(out)) stop("Missing column in blended join: ", poolCol)
    
    out[[col]] <- dplyr::case_when(
      is.na(w) ~ out[[poolCol]],
      w <= 0   ~ out[[regCol]],
      w >= 1   ~ out[[poolCol]],
      TRUE     ~ (1 - w) * out[[regCol]] + w * out[[poolCol]]
    )
  }
  
  out %>%
    dplyr::select(
      scenario, region, period, decileGroup, consumptionCa, logCons, logCons2,
      dplyr::starts_with("share|"), dplyr::all_of(wCol)
    )
}



# ----------------------------
# Weighted RAS / IPF calibration
# ----------------------------
weightedRas <- function(shareRawMat, wDecile, macroShare,
                        eps = 1e-12, maxIter = 2000, tol = 1e-11) {
  
  S <- as.matrix(shareRawMat)
  
  w <- as.numeric(wDecile)
  if (any(!is.finite(w)) || all(w <= 0)) stop("wDecile must have positive finite values.")
  w <- pmax(w, 0)
  w <- w / sum(w)
  
  m <- as.numeric(macroShare)
  if (any(!is.finite(m)) || any(m < 0)) stop("macroShare must be finite and non-negative.")
  if (all(m == 0)) stop("macroShare cannot be all zeros.")
  m <- pmax(m, eps)
  m <- m / sum(m)
  
  stopifnot(nrow(S) == length(w))
  stopifnot(ncol(S) == length(m))
  
  # strictly positive to keep feasibility + non-negativity
  S <- pmax(S, eps)
  
  # initial row normalization
  S <- S / pmax(rowSums(S), eps)
  
  for (it in seq_len(maxIter)) {
    S_old <- S
    
    # 1) column scaling to match weighted column margins: w' S = m
    colW <- as.numeric(crossprod(w, S))            # length = nSector
    colFactor <- m / pmax(colW, eps)
    S <- sweep(S, 2, colFactor, "*")
    
    # 2) row normalization to sum to 1
    S <- S / pmax(rowSums(S), eps)
    
    if (max(abs(S - S_old)) < tol) break
  }
  
  S
}
