output$demands <- renderPlot({
  
  #gather the data into long format
  demands.df <- gather(demands_raw.df,site, flow, 2:6)
  
  #turn dates to date_time type
  demands.df$DateTime <- as_datetime(as.character(demands.df$DateTime))
  
  #plot the data
  ggplot(demands.df, aes(x = DateTime, y = flow)) + geom_line(aes(linetype = site, colour = site))
  
})