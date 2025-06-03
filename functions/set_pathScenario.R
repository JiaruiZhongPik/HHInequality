#Defines the scenarios and path of REMIND runs


set_pathScenario<-function (scenario_mode,write_namestring, REMIND_pattern, rootdir_remind, rootdir_magpie,
                            all_runscens,all_budgets){
  #if cluster folders are mounted locally
  mountdir <- "S:"

  run_prefix <- "C_"
  reference_run_name <- "NPi"
  remind_iteration <- "-rem-5"
  magpie_iteration <- "-mag-4"

  
  # generate file paths based on settings above
  remind_run_all <- mapply(scenario_combiner, all_runscens, all_budgets, prestring = run_prefix, addstring = remind_iteration, USE.NAMES = FALSE)
  remind_base_all <- mapply(scenario_combiner, all_runscens, rep(reference_run_name,length(remind_run_all)), prestring = run_prefix, addstring = remind_iteration, USE.NAMES = FALSE)
  magpie_run_all <- mapply(scenario_combiner, all_runscens, all_budgets, prestring = run_prefix, addstring = magpie_iteration, USE.NAMES = FALSE)
  magpie_base_all <- mapply(scenario_combiner, all_runscens, rep(reference_run_name,length(magpie_run_all)), prestring = run_prefix, addstring = magpie_iteration, USE.NAMES = FALSE)
  
  
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
