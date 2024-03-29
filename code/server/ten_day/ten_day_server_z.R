#----------------------------------------------------------------------------
observeEvent(input$reset.nbr, {
  updateCheckboxGroupInput(session, "gages.nbr", 
                           selected = c("luke", "lfalls", "lfalls_lffs"))
})
#----------------------------------------------------------------------------
observeEvent(input$clear.nbr, {
  updateCheckboxGroupInput(session, "gages.nbr", "Variables to show:",
                           c("Luke" = "luke",
                             "Little Falls" = "lfalls",
                             "Little Falls (Low Flow Forecast System)" = "lfalls_lffs"),
                           selected = NULL)
})
#----------------------------------------------------------------------------
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

#------------------------------------------------------------------------------
# Adding our estimate of flow at Little Falls in 9 days
lfalls.natural.mgd.df <- reactive({
  req(!is.null(withdrawals.df()), 
      !is.null(daily.df()))
  # First need to create Little Falls "natural" flow - without effects of JRR and Savage dams and withdrawals:
  cfs_to_mgd <- 1.547
  withdrawal.sub <- withdrawals.df() %>% 
    dplyr::select(date_time, site, flow) %>% 
    dplyr::filter(site == "potomac_total")
  if (nrow(withdrawal.sub) == 0) return(NULL)
  withdrawal.sub <- tidyr::spread(withdrawal.sub, site, flow)
  #----------------------------------------------------------------------------
  final.df <- daily.df() %>%
    #dplyr::select(-qual_code) %>% 
    tidyr::spread(site, flow) %>% 
    # First subtract off flow augmentation due to JR and Savage dams:
    mutate(lfalls_natural0 = round(lfalls / cfs_to_mgd),
           ###           net_nbr_aug = (barnum - kitzmiller + bloomington - barton) / cfs_to_mgd,
           net_nbr_aug = round(lag(luke, n = 9)  / cfs_to_mgd),
           ###           lfalls_lags = lag(net_nbr_aug, n = 8) +
           ###             lag(net_nbr_aug, n = 9) +
           ###             lag(net_nbr_aug, n = 10),
           ###           lfalls_natural = lfalls_natural - lfalls_lags / 3) %>% 
           lfalls_natural = lfalls_natural0 - net_nbr_aug) %>%     
    left_join(withdrawal.sub, by = "date_time") %>% 
    # Then eliminate effect of WMA withdrawals:
    mutate(lfalls_natural = lfalls_natural + potomac_total,
           lfalls_9dayfc = 288.79 * exp(0.0009 * lfalls_natural), # + net_nbr_aug, ### - potomac_total,
           lfalls_9dayfc = ifelse(lfalls_9dayfc > lfalls_natural, 
                                  lfalls_natural, lfalls_9dayfc),
           lfalls_9dayfc = lfalls_9dayfc + net_nbr_aug - potomac_total,
           lfalls_9dayfc = dplyr::lead(lfalls_9dayfc, 0),
           lfalls_9dayfc = round(lfalls_9dayfc)) %>% 
    select(date_time, lfalls_natural0, lfalls_natural, luke,
           net_nbr_aug, potomac_total, lfalls_9dayfc)
  #----------------------------------------------------------------------------
  return(final.df)
})
#------------------------------------------------------------------------------
# This value of lfalls_9dayfc is used in the graph:
lfalls.natural.mgd <- reactive({
  req(!is.null(lfalls.natural.mgd.df()),
      !is.null(todays.date()))
  
  final.df <- lfalls.natural.mgd.df() %>%
    mutate(date_time = lead(date_time, 9)) %>%
    filter(date_time == todays.date() + lubridate::days(9)) %>%
    select(date_time, lfalls_9dayfc)
  
  req(!is.na(final.df$lfalls_9dayfc[1]))
  return(final.df)
})
#------------------------------------------------------------------------------
# These dataframe values can be used in text displays:
lfalls.natural.mgd.today <- reactive({
  req(!is.null(lfalls.natural.mgd.df()),
      !is.null(todays.date()))
  
  final.df <- lfalls.natural.mgd.df() %>%
    filter(date_time == todays.date())
  
  #req(nrow(final.df) != 0)
  return(final.df)
})
# 
#------------------------------------------------------------------------------
output$ten_day <- renderPlot({
  validate(
    need(!is.null(nbr.df()),
         "No data available for the selected date range. Please select a new date range.")
  )
  # Should nine.day be converted to cfs when units== cfs?
  #----------------------------------------------------------------------------
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
}) # End output$nbr
#----------------------------------------------------------------------------
# Adding text to give numerical values of 9-day fc and Luke target
#output$nbr_notification_1 <- renderText({
#  if (is.null(lfalls.natural.mgd())) {
#    paste("Little Falls 9-day flow forecast from empirical formula cannot",
#          "be determined with the currently selected 'Todays Date'.")
#  } else {
#    paste("Little Falls 9-day flow forecast from empirical formula is ",
#          lfalls.natural.mgd()$lfalls_9dayfc, " MGD.")
#  }
# 
#})
#------------------------------------------------------------------------------
source("code/server/ten_day/ten_day_notifications.R", local = TRUE)
#------------------------------------------------------------------------------



