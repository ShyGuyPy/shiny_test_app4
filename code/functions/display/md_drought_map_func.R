md_drought_map_func <- function(the_date){
  #url for maryland map always starts this way
  map_url_head = 'https://mde.maryland.gov/programs/Water/droughtinformation/Currentconditions/PublishingImages/DroughtGraphsStarting2019jan31/Drought'
  
  #get todays date
  todays_date = the_date
  
  #set test date variable at todays date
  test_date = todays_date
  
  #set a test for if the url we search exists
  test = FALSE
  
  #while url searched doesn't produce a valid link to map, until then iterate backwards from todays date
  while ( test == FALSE)
  {
    #concat to produce url in expected format
    url_to_date = (paste0('https://mde.maryland.gov/programs/Water/droughtinformation/Currentconditions/PublishingImages/DroughtGraphsStarting2019jan31/Drought',format(test_date,"20%y-%m-%d"),'.png'))
    
    #tests if
    test <- url.exists(url_to_date)
    
    #back one day
    test_date = test_date -1
  }
  return(url_to_date)
}