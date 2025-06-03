#Load utils
source('utils/utils.R')
source('functions/set_pathScenario.R')
source('functions/read_remindPolicy.R')
source('functions/read_remindBaseline.R')
source('functions/analyze_regression.R')
source('functions/prepare_modelData.R')
source('functions/prepare_eurostatData.R')
source('functions/prepare_gcdData.R')
source('functions/prepare_giniSDP.R')
source('functions/analyze_regression.R')
source('functions/predict_decileConsShare.R')
source('functions/predict_decileWelfChange.R')
source('functions/plot_output.R')


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
options(dplyr.summarise.inform = FALSE)


#Config setting
scenario_mode <- "coupled"
write_namestring <- "coupled2017"
rootdir_remind <- "/p/projects/piam/SHAPE_runs/coupled_2023-05-03_final/remind/output"
rootdir_magpie <- "/p/projects/piam/SHAPE_runs/coupled_2023-05-03_final/magpie/output"

all_runscens <- c(rep("SDP_EI",1),
                  rep("SDP_MC",1),
                  rep("SDP_RC",1),
                  rep("SSP2EU",1),
                  rep("SSP1",1))

all_budgets <- c("PkBudg650-taxInc3", #SDP_EI, other options: "PkBudg500", "PkBudg650-taxInc3-CCimp"                         
                 "PkBudg650",         #SDP_MC, other options: "PkBudg500", "PkBudg650-CCimp",
                 "PkBudg650",         #SDP_RC, other options: "PkBudg500", "PkBudg650-CCimp", 
                 "PkBudg650-taxInc6", #SSP2EU, other options: "PkBudg500", "PkBudg650-taxInc6-CCimp", "NDC", "NPi-CCimp", 
                 "PkBudg650-taxInc3") #SSP1,   other options: "PkBudg500", "NDC"

REMIND_pattern <- "REMIND_generic*rem-5.mif"

#Modelling setting
regression <- 1                             ## if estimate coefficients within the project
regression_model<-"logitTransOLS"           # other available options are  "logitTransOLS", 'PolynomialLM'
regionmapping <- 'pool'                      #options are: "H12", "H21", "country", "pool"
ConsData <- 'gcd'                           #options are: gcd (9 sectors), eurostat(3 sectors)
gini_baseline <- 'raoGini'                  ##iiasaGini or raoGini
fixed_point <- 'midpoint'                   ## options: "base","policy","midpoint"
micro_model <- 'FOwelfare'                  # options: only "FOwelfare" as of yet; 
outputPath <- "figure/test/shape"

#----------------------------Project life-cycle---------------------------------
all_paths = set_pathScenario(scenario_mode, write_namestring, REMIND_pattern, rootdir_remind, rootdir_magpie,all_runscens,all_budgets)

data = prepare_modelData(all_paths)

if(regression){
  coef = analyze_regression(regression_model = regression_model, ConsData = ConsData, regionmapping = regionmapping,
                            isDisplay = TRUE, isExport = T)
} else {
  print('Wait for MCC input')
}

#uses gcd regional results, there are missing data for developed world. 
#I wanted to use EUR for other developed world but found that EUR has only 
#limited observations too, so this is not a good strategy

# missingMapping <- data.frame(
#   missing = c('CAZ', 'JPN', 'USA'),
#   replaceWith = c('EUR', 'EUR', 'EUR'))

#Todo:include sdp gini pathway
decileConsShare <- predict_decileConsShare(data, coef, regression_model, isDisplay=F, isExport=T, countryExample = 'IND')

decileWelfChange <- predict_decileWelfChange(data, decileConsShare, micro_model, fixed_point)


#-------Plot-------
#Todo: This plotting profile needs to be revised
plot_output( outputPath = outputPath, data = decileWelfChange, micro_model = micro_model, fixed_point = fixed_point, allExport = T)

#To get all regional plots
for(r in unique(decileWelfChange$region)){
  
  plot_output( outputPath = outputPath, data = decileWelfChange, plotlist='welfByDecileSecEneRegion' , 
               micro_model = micro_model, fixed_point = fixed_point, exampleReg = r,isDisplay = F, isExport = T)
  
}
