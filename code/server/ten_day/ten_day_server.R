


output$ten_day <- renderPlot({
  
  ten_day.df$date_time <- as_datetime(as.character(ten_day.df$date_time))
  
  ggplot(ten_day.df, aes(x = date_time, y = flow)) + geom_line(aes(linetype = site, colour = site))
  
})



