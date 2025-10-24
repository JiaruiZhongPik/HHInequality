#Defines the scenarios and path of REMIND runs


set_pathScenario<-function (reference_run_name, scenario_mode,write_namestring, REMIND_pattern, rootdir_remind, rootdir_magpie,
                            all_runscens,all_budgets){
  #if cluster folders are mounted locally
  mountdir <- "S:"

  run_prefix <- "C_"

  remind_iteration <- case_when(
    grepl("EocBudg500-OAE_on", all_budgets) ~ "-rem-10",
    TRUE                                    ~ "-rem-5"
    )
  
  magpie_iteration <- case_when(
    grepl("EocBudg500-OAE_on", all_budgets) ~ "-mag-9",
    TRUE                                    ~ "-mag-4"
  )

  budget_runscens <- case_when(
    grepl("loOS-def|hiOS-def", all_budgets) ~ "RESCUE-Tier2",
    grepl("EocBudg500-OAE_on", all_budgets) ~ "RESCUE-dir-v5p0",
    TRUE                                     ~ all_runscens
  )
  
  reference_runscens <- case_when(
    grepl("EocBudg500-OAE_on", all_budgets) ~ "SSP2EU",
    TRUE                                     ~ all_runscens
  )
  
  reference_run_name <- case_when(
    grepl("EocBudg500-OAE_on", all_budgets) ~ "NPi",
    TRUE                                     ~ reference_run_name
  )
  
  
  # generate file paths based on settings above
  remind_run_all <- mapply(scenario_combiner, budget_runscens, all_budgets, prestring = run_prefix, addstring = remind_iteration, USE.NAMES = FALSE)
  remind_base_all <- mapply(scenario_combiner, reference_runscens,  reference_run_name, prestring = run_prefix, addstring = remind_iteration, USE.NAMES = FALSE)
  magpie_run_all <- mapply(scenario_combiner, budget_runscens, all_budgets, prestring = run_prefix, addstring = magpie_iteration, USE.NAMES = FALSE)
  magpie_base_all <- mapply(scenario_combiner, reference_runscens, reference_run_name, prestring = run_prefix, addstring = magpie_iteration, USE.NAMES = FALSE)
  
  
  all_paths <- 
    tibble(remind_run = remind_run_all,
           remind_base = remind_base_all,
           magpie_run = magpie_run_all,
           magpie_base = magpie_base_all) %>% 
    mutate(remind_path = sapply(remind_run, findReporting,
                                path = file.path(mountdir,rootdir_remind), pattern = REMIND_pattern, USE.NAMES = FALSE),
           remind_path_base = sapply(remind_base, findReporting,
                                     path = file.path(mountdir,rootdir_remind), pattern = REMIND_pattern, USE.NAMES = FALSE),
           magpie_path = sapply(magpie_run, findReporting,
                                path = file.path(mountdir,rootdir_magpie), pattern = "fulldata.gdx", USE.NAMES = FALSE),
           magpie_path_base = sapply(magpie_base, findReporting,
                                     path = file.path(mountdir,rootdir_magpie), pattern = "fulldata.gdx", USE.NAMES = FALSE)
    )
  
  return(all_paths)
}
