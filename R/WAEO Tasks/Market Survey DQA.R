library(assertr)

error_handler = function( ... ) {
  args = list(...)
  
  do.call( just_warn, args )
  
  bad_data = args[[1]][[1]]$error_df
  
  these_failed_rows = args$data %>% 
    slice( bad_data$index )
  
  if(!exists("my_failed_rows", inherits=TRUE)) {
    my_failed_rows = NULL
  }
  
  my_failed_rows = cbind(these_failed_rows,bad_data)
  assign( "my_failed_rows", my_failed_rows, envir= .GlobalEnv )
  good_rows = args$data %>% 
    slice(-bad_data$index)
  
  return(good_rows)
}

market_survey_dqa = function(market_data){
  market_data %>% 
    insist(
      within_n_mads(3), 
      improved_yellow_price,
      local_yellow_price,
      dried_cassava_price,
      fresh_cassava_price,
      cassava_flour_price,
      red_bean_price,
      error_fun = error_handler
      )
}

market_error_fun = function(failed_df){
  failed_df %>% 
    group_by(district,ward,waeo) %>% 
    summarise(error_incident=n())
}

market_task_outcome = function(completion_df){
  completion_df %>% 
    left_join(market_error_fun(my_failed_rows),
              by=c("district","ward","waeo")) %>% 
    arrange(district,waeo) %>% 
    mutate(
      error_incident = case_when(
        is.na(error_incident)~as.integer(0),
        TRUE~error_incident
      ),
      # 1 error is weighted x 3
      DQ_score = round((1-(3*error_incident)/24),2)
    )
  
}