#Load utils
source('utils/utils.R')
source('functions/set_pathScenario.R')
source('functions/read_remindPolicy.R')
source('functions/read_remindBaseline.R')
source('functions/prepare_modelData.R')
source('functions/prepare_eurostatData.R')
source('functions/analyze_regression.R')

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
regression <- 0 ##1 for regression with EUROSTAT Data.


#Project life-cycle
all_paths = set_pathScenario(scenario_mode,write_namestring,rootdir_remind, rootdir_magpie,
                          all_runscens,all_budgets)

data = prepare_modelData(all_paths)

if(regression){
  coef = analyze_regression(FALSE)
} else {
  print('Wait for MCC input')
}





output= microsimulation (data,regression,'model')

analysis = display (output)


