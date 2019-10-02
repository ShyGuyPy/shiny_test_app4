va_drought_map_func <- function(){
  #this is the url that contains all the map files
  map_url_head ='https://deq1.bse.vt.edu/drought/state/images/maps/'
  
  #creates an object to open 
  test <- curl::curl(map_url_head)
  
  #open and read said object
  open(test)
  out <- readLines(test)
  
  map.df <- as.data.frame(out, row.names = NULL, optional = TRUE)#, col.names = cols )
  colnames(map.df) <- c("html")
  
  ######patterns for finding string fragments for retrieving map
  pattern_id <- "(\\d{10,})"
  pattern_date <- "(\\d{4}[-]\\d{2}[-]\\d{2})"
  pattern_time <- "(\\d{2}[:]\\d{2})"
  ################
  
  #executing above patterns with extract to generate dataframes containing relevant fragments of string
  id.df <- extract(map.df,html, c("id"), regex = pattern_id)
  date.df <- extract(map.df,html, c("date"), regex = pattern_date)
  time.df <- extract(map.df,html, c("time"), regex = pattern_time)
  
  #bind dataframes into one
  maps_full.df <- cbind(map.df,id.df, date.df,time.df)

  #arrange so that most current file is at the top
  maps_full.df <- arrange(maps_full.df, desc(date),desc(time))
  
  #grabs the top item
  map_url_id = maps_full.df$id[1]
  
  #concatinate all relevant parts into a retrievable url
  map_url_full = as.character(paste0(map_url_head,"imageMapFile", map_url_id,".png"))
  
  return(map_url_full)
}