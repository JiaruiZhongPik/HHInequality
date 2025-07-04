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
  

  
  #Read magpie 
  magpie_policy <-
    all_paths %>%
    select(magpie_run, magpie_path, magpie_base, magpie_path_base) %>%
    pmap_dfr(function(magpie_run,magpie_path,magpie_base,magpie_path_base){
      print(magpie_run)
      print(magpie_path)
      print(magpie_path_base)

      read_magpiePolicy(magpie_run, magpie_path, magpie_base, magpie_path_base)
    })
  
  magpie_base <-all_paths %>% 
    select(magpie_base, magpie_path_base) %>%
    unique() %>%
    pmap_dfr(function(magpie_base, magpie_path_base){
      print(magpie_base)
      
      read_magpieBase(magpie_base, magpie_path_base) %>%
        select(scenario, everything())
    }) 
  
  magpie_data <- bind_rows(magpie_base, magpie_policy) %>%
    rename( period = year ) %>%
    mutate(model = 'MAgPIE',
           period = as.integer(as.character(period)) )

  
  #Combine magpie and remind data
  data = bind_rows(remind_data, magpie_data) %>%
    mutate(
      scenario= str_remove(scenario, "-(rem|mag)-\\d+$")
    ) 
  
  return(data)
  
}
