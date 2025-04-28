#This function compiles all required data

prepare_modelData <- function (all_paths,isDisplay = F, isExport = F){
  #Remind policy runs with relative changes to defined baseline scenario
  remind_policy <- 
    all_paths %>% 
    select(remind_run, remind_path, remind_path_base) %>% 
    pmap_dfr(function(remind_run,remind_path,remind_path_base){
      print(remind_run)
      print(remind_path)
      print(remind_path_base)
      
      read_remindPolicy(remind_run,remind_path,remind_path_base, isDisplay, isExport) %>%
        #        mutate(scenario = remind_run) %>%
        select(scenario, everything())
    }) 
  
  #Remind baseline run
  remind_base <-all_paths %>% 
    select(remind_path_base) %>%
    unique()%>%
    pmap_dfr(function(remind_path_base){
      print(remind_path_base)
      
      read_remindBaseline(remind_path_base, do_plots = TRUE) %>%
        #        mutate(scenario = remind_run) %>%
        select(scenario, everything())
    }) 
  
  remind_data <- bind_rows(remind_base,remind_policy)
  
  return(remind_data)
  
  #Todo: add Magpie Data
  
}