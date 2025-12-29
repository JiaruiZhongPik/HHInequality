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
source('functions/analyze_regression.R')
source('functions/prepare_modelData.R')
source('functions/prepare_eurostatData.R')
source('functions/prepare_gcdData.R')
source('functions/prepare_giniSDP.R')
source('functions/analyze_regression.R')
source('functions/predict_decileConsShare.R')
source('functions/plot_inspection.R')
source('functions/predict_decileWelfChange.R')
source('functions/plot_output.R')
source('functions/plot_outputNCC.R')
source('functions/aggregate_decileWelfChange.R')
source('functions/compute_inequalityMetrics.R')
source('functions/compute_transfer.R')


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
options(dplyr.summarise.inform = FALSE,
        scipen = 999)



#Config setting
scenario_mode <- "coupled"
write_namestring <- "coupled2017"
#rootdir_remind <- "/p/projects/remind/runs/REMIND-MAgPIE-2025-04-24/remind/output"
#rootdir_magpie <- "/p/projects/remind/runs/REMIND-MAgPIE-2025-04-24/magpie/output"
rootdir_remind <- "/p/projects/rescue/tier2_scenarios/v1/cpl/remind/output"
rootdir_magpie <- "/p/projects/rescue/tier2_scenarios/v1/cpl/magpie/output"
all_runscens <- c("SSP2")
reference_run_name <- "NPi2025"             #For earlier runs, it's "NPi"     
all_budgets <- c("loOS-def","hiOS-def")
REMIND_pattern <- "REMIND_generic*.mif"

#Model setting
regions <- 'H12'                            # options are H12 or H21, seemingly redundant
regressModel<-"logitTransOLS"               # other available options are  "logitTransOLS", 'polynomialLM'
regressRegGrouping <- 'pool'                 # options are: "H12", "H21", "pool"
consData <- 'mcc'                           # options are: gcd, mcc
gini_baseline <- 'rao'                      # ‘rao’ ； ‘poblete05’ ; 'poblete07'
fixed_point <- 'midpoint'                   # options: "base","policy","midpoint"
micro_model <- 'FOwelfare'                  # options: only "FOwelfare" as of yet; 



outputPath <- paste0("figure/test/",gini_baseline,'_',consData,'RESCUE','-','blended','-',format(Sys.time(), "%Y-%m-%d_%H-%M-%S"))

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
load("RESCUE.RData")


#Engel curve estimation
coef <- get_engelCurveCoef(
  regressModel       = regressModel,
  dataSource         = consData,
  regressRegGrouping = regressRegGrouping,              # used for patched base + gcdRegional patching
  mccSumShareRange   = c(0.85, 1.05),
  # patch rules (used only when engelCurveMode == "patched")
  patchChaFrom       = 'mccPooled',
  patchJpnFrom       = 'mccPooled',
  isDisplay          = FALSE
)



#Predict consumption shares for each regional decile
decileConsShare <- predict_decileConsShare(
  data,
  coef,
  gini_baseline = gini_baseline,
  regression_model = regression_model,
  doBlending = T,
  blendTailProb = 0.99,
  blendEndFactor = 2,
  blendingRegGrouping = "H12",
  mccSumShareRange = c(0.85, 1.05)
)


#inspecting intermediate variables, particularly price shock and exposure
plot_inspection(outputPath = outputPath,
                dataPrice = data,
                dataExposure = decileConsShare,
                isDisplay = F, isExport = T, 
                allExport=T)

#predict welfare change of different category
decileWelfChange <- predict_decileWelfChange(data, decileConsShare, 
                                             climaFund = 0,
                                             #fund_return_scale = 0.5,
                                             payg = 1,
                                             micro_model = micro_model, 
                                             fixed_point = fixed_point) # unit log different change in %

#computes inequality metrics
ineq <- compute_inequalityMetrics(data1 = decileWelfChange, 
                                  data2 = decileConsShare, 
                                  data3 = data,
                                  montecarlo = TRUE, n_perms = 300)

#Todo: a function computes the percentage change of real consumption at the baseline consumption level


#-------Plot-------
#all plots
plot_output(outputPath = outputPath, 
            data1 = decileWelfChange, 
            data2 = decileConsShare, 
            data3 = data, 
            plotdataIneq = ineq,
            micro_model = micro_model, fixed_point = fixed_point, allExport = T)

#get ncc plots

plot_outputNCC()


#any individual plot
source('functions/plot_output.R')
p <- plot_output(outputPath = outputPath, 
                 data1  = decileWelfChange, 
                 data2 = decileConsShare, 
                 data3 = data, 
                 plotdataIneq = ineq,
                 exampleReg = 'IND',
                 plotlist = c('ineqWorldWithTransf_GiniRela'),
                 micro_model = micro_model, fixed_point = fixed_point, isDisplay= F, isExport = T)

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


