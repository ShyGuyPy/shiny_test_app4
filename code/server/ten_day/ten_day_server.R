nbr.df <- reactive({
  req(!is.null(todays.date()))
  #----------------------------------------------------------------------------
  #  sub.df <- hourly.reac() %>% 
  sub.df <- hourly.df() %>% 
    dplyr::filter(date_time <= todays.date())
  #----------------------------------------------------------------------------
  req(nrow(sub.df) > 0)
  #----------------------------------------------------------------------------
  lfalls.obs <- sub.df %>% 
    dplyr::filter(site == "lfalls") %>% 
    rolling_min(flow, 240, "obs") %>% 
    dplyr::filter(site == "obs")
  
  #  lfalls.pred <- lowflow.hourly.reac() %>%   
  lfalls.pred <- lowflow.hourly.df() %>% 
    dplyr::filter(site == "lfalls_sim") %>% 
    rolling_min(flow, 240, "sim")
  
  lfalls.df <- dplyr::bind_rows(lfalls.obs, lfalls.pred) %>% 
    tidyr::spread(site, flow) %>% 
    dplyr::filter(!is.na(sim)) %>% 
    tidyr::fill(obs) %>% 
    dplyr::mutate(lfalls_lffs = lfalls_sim - (sim - obs)) %>% 
    dplyr::select(date_time, lfalls_lffs) %>% 
    tidyr::gather(site, flow, lfalls_lffs)
  #----------------------------------------------------------------------------
  final.df <- dplyr::bind_rows(sub.df, lfalls.df) %>% 
    dplyr::filter(!is.na(flow))
  #----------------------------------------------------------------------------
  req(nrow(final.df) > 0)
  return(final.df)
})




output$ten_day <- renderPlot({
  

gen_plots(nbr.df(),
          start.date(),
          end.date(), 
          min.flow = input$min.flow,
          max.flow = input$max.flow,
          gages.checked = input$gages.nbr,
          labels.vec = c("lfalls" = "Little Falls",
                         "lfalls_lffs" = "Little Falls (Low Flow Forecast System)",
                         "luke" = "Luke"),
          linesize.vec = c("lfalls" = 2,
                           "lfalls_lffs" = 1.5,
                           "luke" = 2),
          linetype.vec = c("lfalls" = "solid",
                           "lfalls_lffs" = "dashed",
                           "luke" = "solid"),
          color.vec = c("lfalls" = "#0072B2",
                        "lfalls_lffs" = "#56B4E9",
                        "luke" = "#009E73"),
          x.class = "datetime",
          #            y.lab = y.units(),
          y.lab = y_units,
          nine_day.df = lfalls.natural.mgd.today()
)
  
})