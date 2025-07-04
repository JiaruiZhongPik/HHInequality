# This file prepares the gcd Data for estimation

prepare_gcdData <- function (isDisplay = FALSE,isExport = FALSE) {
  # Read in sector mapping
  coicop_mapping <- read_delim("input/coicop sector mapping.csv", 
                               delim = ";", escape_double = FALSE, trim_ws = TRUE,
                               show_col_types = FALSE) %>%
    rename(coicop = `COICOP sector`,
           sector =`harmonized sector` 
           )
  
  # Read in regional mapping 
  regionmappingH12 <- read_delim("input/regionmappingH12.csv", 
                                 delim = ";", escape_double = FALSE, trim_ws = TRUE,
                                 show_col_types = FALSE) %>%
    rename(countryname = X)
  
  regionmappingEU21 <- read_delim("input/regionmapping_21_EU11.csv", 
                                  delim = ";", escape_double = FALSE, col_types = cols(X = col_skip()), 
                                  trim_ws = TRUE,
                                  show_col_types = FALSE)
  
  
  # Read in GCD data
  dfRaw <- read_excel("input/WB_GCD_2010_v2014-03_data.xlsx", 
                      col_types = c("text", "text", "skip", 
                                    "skip", "skip", "skip", "skip", "numeric", 
                                    "text", "text", "skip", "skip", "skip", 
                                    "text", "text", "text", "numeric"))%>%
    rename(geo = Countrycode,
           countryname = Countryname,
           year = Year,
           variable = Indicatorname,
           unit = Measurementunit,
           area = Area,
           coicop = Labelofproductorservice,
           incomegroup = Incomegroup,
           value = Value)
  
  df <- dfRaw %>%
    filter( variable == 'Share of product/service in total expenditure (%)',
            area == 'National', # Remove the Rural/Urban devision that we don't need
            incomegroup %in% c('Low','Higher','Lowest','Middle')) %>%
    mutate(variable = 'expShare') %>%
    select( -area) %>%
    filter(!is.na(value))
  
  
  # Harmonize GCD sectors
  dpCountry <- df %>%
    merge(coicop_mapping, by = "coicop", all.x = TRUE) %>%
    group_by(geo, year, incomegroup, coicop) %>%
    summarise(row_count = n(), .groups = "drop") %>%
    filter(!row_count==1) %>%
    distinct(geo) %>%
    pull(geo)
  
  #Countries with sub national data 
  dpCountryName <- unique(df[df$geo %in% dpCountry,] $countryname)
  
  df<- df %>%
    filter(!countryname %in% setdiff(dpCountryName, c('Brazil', 'India', 'South Africa')) )
  
  dfH <- df %>%
    merge(coicop_mapping, by = "coicop", all.x = TRUE) %>%
    group_by(geo, year, incomegroup, sector) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop")%>%
    mutate(unit = 'share',
           sector = case_when(
             sector == "animal products"  ~ "share|Animal products",
             sector == "staple" ~ "share|Staples",
             sector == "fruits&vegetables&nuts" ~ "share|Fruits vegetables nuts",
             sector == "empty calories" ~ "share|Empty calories",
             sector == "final energy for passenger transportation" ~ "share|Transport energy",
             sector == "electricity for building" ~ "share|Building electricity",
             sector == "gas for building" ~ "share|Building gases",
             sector == "heating and other fules for building" ~ "share|Building other fuels",
             sector == "other commodities" ~ "share|Other commodities",
             TRUE ~ sector  # keep all other values unchanged
           )
    )%>%
    rename(variable = sector)
  
  #Check wether for each HH, consumption shares add up to 1
  summary(dfH %>%
            group_by(geo, year, incomegroup)%>%
            summarise(value = sum(value, na.rm = TRUE), .incomegroup = "drop") )
  
  #merge consumption expenditure data
  dfFinal <- dfRaw %>%
    filter( variable == 'Annual expenditure, per capita',
            area == 'National', # Remove the Rural/Urban devision that we don't need
            incomegroup %in% c('Low','Higher','Lowest','Middle'),
            unit =='PPP$') %>%
    mutate(variable = 'exp') %>%
    filter(
      !is.na(value),
      !countryname %in% setdiff(dpCountryName, c('Brazil', 'India', 'South Africa'))
    ) %>%
    select( -area, - countryname, -coicop) %>%
    bind_rows(dfH) 
  
  
  #Visualize country coverage of database
  
  
  # Step 1: Load world map
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  # Step 2: Prepare availability info
  world_mapped <- world %>%
    mutate(data_available = ifelse(iso_a3 %in% dfFinal$geo, "Yes", "No"))
  
  # Step 3: Plot
  plotCoverage <- ggplot(world_mapped) +
    geom_sf(aes(fill = data_available), color = "white", size = 0.1) +
    scale_fill_manual(
      values = c("Yes" = "#2c7bb6", "No" = "#cccccc"),
      name = "Data Available"
    ) +
    labs(
      title = "Country-Level Data Coverage",
      subtitle = "Countries with data are shown in blue",
      caption = "Source: gcd"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      panel.background = element_rect(fill = "lightblue"),
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12)
    )
  
  if(isDisplay){
    print(plotCoverage)
  }
  
  if(isExport){
    ggsave(
      filename = paste0(outputPath,"/gdc_coverage_map.png"),  # or .pdf / .svg
      plot = plotCoverage,               # or replace with your plot object
      width = 10,                       # in inches
      height = 6,
      dpi = 300                         # high resolution
    )
  }
  
 
  
  
  
  return(dfFinal)
  
}






