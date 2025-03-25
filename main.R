#Load utils
source('utils/utils.R')
source('functions/set_pathScenario.R')
source('functions/read_remindPolicy.R')
source('functions/read_remindBaseline.R')
source('functions/prepare_modelData.R')
source('functions/prepare_eurostatData.R')
source('functions/analyze_regression.R')
source('functions/predict_decileConsShare.R')
source('functions/predict_decileWelfChange.R')

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
options(dplyr.summarise.inform = FALSE)


#Config setting
scenario_mode <- "coupled"
write_namestring <- "coupled2017"
rootdir_remind <- "/p/projects/remind/runs/REMIND-MAgPIE-2024-11-21/remind/output"
rootdir_magpie <- "/p/projects/remind/runs/REMIND-MAgPIE-2024-11-21/magpie/output"
all_runscens <- c("SSP2")
all_budgets <- c("PkBudg650", "PkBudg1000")
regression <- 1 ##1 for regression with EUROSTAT Data.
regression_model<-"logitTransOLS" # other available options are  "logitTransOLS", 'PolynomialLM'
gini_baseline <- 'raoGini' ##iiasaGini or raoGini
micro_model <- 'FOwelfare'

#Project life-cycle
all_paths = set_pathScenario(scenario_mode,write_namestring,rootdir_remind, rootdir_magpie,all_runscens,all_budgets)

data = prepare_modelData(all_paths)

if(regression){
  coef = analyze_regression(regression_model,TRUE)
} else {
  print('Wait for MCC input')
}

decileConsShare <- predict_decileConsShare(data,isDisplay=T, isExport=F)

decileWelfChange <- predict_decileWelfChange(micro_model, data, decileConsShare, isDisplay = T, isExport = T )



#-------------------------------to do-------------------------------------
output= microsimulation (data,regression,'model')

analysis = display (output)


