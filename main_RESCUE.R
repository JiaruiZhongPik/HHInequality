#Load utils
source('utils/utils.R')
source('functions/set_pathScenario.R')
source('functions/read_remindPolicy.R')
source('functions/read_remindBaseline.R')
source('functions/read_magpiePrice.R')
source('functions/read_magpieExpShare.R')
source('functions/read_magpiePolicy.R')
source('functions/read_magpieBase.R')
source('functions/get_estimateMcc.R')
source('functions/prepare_modelData.R')
source('functions/prepare_eurostatData.R')
source('functions/prepare_gcdData.R')
source('functions/prepare_mccData.R')
source('functions/prepare_giniSDP.R')
source('functions/get_engelCurveCoef.R')
source('functions/predict_decileConsShare.R')
source('functions/plot_inspection.R')
source('functions/predict_decileWelfChange.R')
source('functions/plot_output.R')
source('functions/plot_outputNCC.R')
source('functions/aggregate_decileWelfChange.R')
source('functions/compute_anchoredRealCons.R')
source('functions/compute_inequalityOutcomes.R')
source('functions/compute_priceChannelShapley.R')
source('functions/compute_transfer.R')
source('functions/write_results.R')

#Load library
library(remind2)
library(magpie4)
library(mrremind)
library(mrcommons)
library(mrdrivers)
library(GDPuc)
library(quitte)
library(countrycode)
library(readxl)
library(ineq) #install into personal library for running on cluster
library(furrr) #install into personal library for running on cluster
library(assertthat)
library(lfe)
library(glue)
library(viridisLite)
library(tidyverse)
library(readr)
library(dplyr)
library(tidyr)
library(stargazer)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(micEconAids)
library(patchwork)
library(broom)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(gdx)
library(withr)
library(magpiesets)
library(ineq)
library(gtools)
library(dineq)
library(acid)
library(iIneq)
library(paletteer)
library(grid)
library(colorspace)
options(scipen = 999)



#Config setting
scenario_mode <- "coupled"
write_namestring <- "coupled2017"
# rootdir_remind <- "/p/projects/rescue/tier2_scenarios/v1/cpl/remind/output"
# rootdir_magpie <- "/p/projects/rescue/tier2_scenarios/v1/cpl/magpie/output"
rootdir_remind <- "/p/tmp/jiaruizh/RESCUE_tier2/remind/output"
rootdir_magpie <- "/p/tmp/jiaruizh/RESCUE_tier2/remind/magpie/output"
all_runscens <- c("SSP2")
reference_run_name <- "NPi2025"             #For earlier runs, it's "NPi"     
all_budgets <- c("loOS-def","hiOS-def")
REMIND_pattern <- "REMIND_generic*.mif"

#Model setting
regions <- 'H12'                            # options are H12 or H21, seemingly redundant
regressRegGrouping <- 'H12'                 # determines regional heterogeneity of Engel curves, options are: "H12","pool"
consData <- 'mcc'                           # options are: gcd, mcc
gini_baseline <- 'rao'                      # ‘rao’ ； ‘poblete05’ ; 'poblete07'
fixed_point <- 'midpoint'                   # options: "base","policy","midpoint"
micro_model <- 'FOwelfare'                  # options: only "FOwelfare" as of yet; 
taxBase <- 'CO2woLUC'                       # options: ‘CO2woLUC’，'GHGwoLUC'，‘GHG’



outputPath <- paste0("figure/test/",gini_baseline,'_',consData,'Ceiling' ,'-',format(Sys.time(), "%Y-%m-%d_%H-%M-%S"))

#----------------------------Project life-cycle---------------------------------
all_paths = set_pathScenario(reference_run_name, scenario_mode,write_namestring, 
                             REMIND_pattern,rootdir_remind, rootdir_magpie,
                             all_runscens,all_budgets)

#Simulation excludes years before 2015. In REMIND, the 2005 price is not stable and 2010 is affected
#by smoothing. Only use prices after that for simulation.
#Starting year moved to 2025, as GHG revenues from REMIND is positive before that 

data = prepare_modelData(all_paths,isExport = T) %>%
  filter(period >= 2025)


#instead of reading, load saved data for convenience
# load("RESCUE_update.RData")

#Read corrected co2 tax revenue, this is a manual fix for the RESCUE Scenarios, unfinished!!
data <-  read_csv("tmp/taxRemindFixed.csv") %>%
  mutate(variable = 'Taxes|CO2|REMIND',
         unit = 'billion US$2017/yr',
         model = 'REMIND',
         baseline = NA) %>%
  dplyr::mutate(
    scenario = dplyr::case_when(
      scenario == "C_RESCUE-Tier2-hiOS-def" ~ "C_SSP2-hiOS-def",
      scenario == "C_RESCUE-Tier2-loOS-def" ~ "C_SSP2-loOS-def",
      TRUE ~ scenario
    ),
    region = dplyr::case_when(
      region == "GLO" ~ "World",
      TRUE ~ region
    ) 
  ) %>% 
  rename(period = perio) %>%
  rbind(data)


#Get (patched) Engel curve estimates for all regions
coef <- get_engelCurveCoef(
  dataSource = consData,
  regressRegGrouping = regressRegGrouping,
  regionOverrides = c(
    CHA = "mccPooled",
    JPN = "mccPooled"
  ),
  mccSumShareRange = c(0.9, 1.1)
)


#Predict consumption shares for each regional decile
decileConsShare <- predict_decileConsShare(
  data,
  coef,
  gini_baseline = gini_baseline,
  countryExample = coef$region %>% unique(),
  doBlending = F,
  mccSumShareRange = c(0.9, 1.1),
  ceilingBuff = 1.1,
  isExport = T
)


#inspecting intermediate variables, particularly price shock and exposure
plot_inspection(outputPath = outputPath,
                dataPrice = data,
                dataExposure = decileConsShare,
                isDisplay = F, isExport = T, 
                allExport=T)

#predict welfare change of different category
decileWelfChange <- predict_decileWelfChange(data, decileConsShare, 
                                             taxBase = taxBase,
                                             climaFund = 0,   #climate fund is not included as it should consider international transfer as a first step, which is not ready yet. The parameter is kept for future use.
                                             #fund_return_scale = 0.5,
                                             payg = 1,
                                             micro_model = micro_model, 
                                             fixed_point = fixed_point) # unit log different change in %


#anchored decile real consumption
anchRealCons <- compute_anchoredRealCons(
  decileWelfChange = decileWelfChange,
  decileConsShare  = decileConsShare,
  data             = data )


#compute the overall inequality change
ineqAll <- compute_inequalityOutcomes(decileWelfChange,
                              decileConsShare,
                              anchRealCons,
                              data)


#compute channelwise contribution
ineqChannel <- compute_priceChannelShapley(
  decileWelfChange  = decileWelfChange,
  decileConsShare   = decileConsShare,
  data              = data,
  montecarlo = TRUE,
  n_perms = 300,
  seed = 12345
)


#-------Export result------

out_full <- write_results(data, decileConsShare,
                          decileWelfChange, anchRealCons,
                          ineqAll, ineqChannel)


#-------Plot-------
#all plots
plot_output(outputPath = outputPath, 
            decileWelfChange = decileWelfChange, 
            decileConsShare = decileConsShare, 
            anchRealCons = anchRealCons,
            data = data, 
            ineqAll = ineqAll,
            ineqChannel = ineqChannel,
            micro_model = micro_model, fixed_point = fixed_point, allExport = T)

#get ncc plots
source('functions/plot_outputNCC.R')
plot_outputNCC()


#any individual plot
source('functions/plot_output.R')
p <- plot_output(outputPath = outputPath, 
                 decileWelfChange = decileWelfChange, 
                 decileConsShare = decileConsShare, 
                 anchRealCons = anchRealCons,
                 data = data, 
                 ineqAll = ineqAll,
                 ineqChannel = ineqChannel,
                 plotlist = c('regColiBySecSelect'),
                 micro_model = micro_model, fixed_point = fixed_point, isDisplay= T, isExport = T)

#To get all regional plots
# for(r in c(unique(decileWelfChange$region),'World') ){
#   
#   plot_output( outputPath = outputPath, 
#                plotdataWelf = decileWelfChange, 
#                data2 = decileConsShare, 
#                data3 = data, 
#                plotdataIneq = ineq,
#                plotlist=c('ineqTheilTRegBySec','ineqTheilLRegBySec') , 
#                micro_model = micro_model, fixed_point = fixed_point, 
#                exampleReg = r,isDisplay = F, isExport = T)
#   
# }


