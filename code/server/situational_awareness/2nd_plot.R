# Second, creates graphs
#------------------------------------------------------------------
#------------------------------------------------------------------
#
#------------------------------------------------------------------
# Create graph of Potomac River flows
#------------------------------------------------------------------
output$potomacFlows <- renderPlot({
  # Grab ts and prepare for graphing:
  potomac.ts.df <- ts$flows
  #
  potomac.graph.df0 <- left_join(potomac.ts.df, 
                                 potomac.data.df, 
                                 by = "date_time") %>%
    dplyr::select(Date = date_time, 
                  "Little Falls flow" = lfalls_obs, 
                  #                 por_nat = por_nat.x, 
                  "WMA withdrawals" = demand)
  graph_title <- "Potomac River"
  potomac.graph.df <- potomac.graph.df0 %>%
    gather(key = "Flow", 
           value = "MGD", -Date) 
  
  potomac.graph.df <- potomac.graph.df %>%
    filter(Date >= input$plot_range[1],
           Date <= input$plot_range[2])
  ggplot(data = potomac.graph.df, aes(x = Date, y = MGD, group = Flow)) +
    geom_line(aes(color = Flow, size = Flow)) +
    scale_color_manual(values = c("deepskyblue1", "red")) +
    scale_size_manual(values = c(2, 1)) +
    ggtitle(graph_title) +
    theme(plot.title = element_text(size = 20)) +
    
    theme(axis.title.x = element_blank())
}) # end output$potomacFlows
#------------------------------------------------------------------
# Create graph of storage and releases for each reservoir
#------------------------------------------------------------------
output$jrrStorageReleases <- renderPlot({
  graph_title <- "Jennings Randolph"
  jrr.graph <- ts$jrr %>%
    select(Date = date_time,
           "WS stor" = storage_ws, 
           "WQ stor" = storage_wq
           ,
           "WS rel" = outflow_ws, 
           "WQ rel" = outflow_wq
    ) %>%
    gather(key = "Legend", 
           value = "MG", -Date) %>%
    filter(Date >= input$plot_range[1],
           Date <= input$plot_range[2])
  ggplot(data = jrr.graph,
         aes(x = Date, y = MG, group = Legend)) +
    geom_line(aes(color = Legend, size = Legend)) +
    scale_color_manual(values = c("lightgreen", "green",
                                  "lightblue", "blue")) +
    scale_size_manual(values = c(0.5, 1, 0.5, 1)) +
    ggtitle(graph_title) +
    theme(plot.title = element_text(size = 18)) +
    # face = "bold")) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    theme(legend.position = "right", 
          legend.title = element_blank())
}) # end jrr renderPlot testing
#
#------------------------------------------------------------------
# I can't get the following graphing function to work:
# output$senStorageReleases <- renderPlot({
#   sen.graph <- ts$sen
#   graph_title <- "Seneca"
#   display_graph_res_func(graph_title, sen.graph)
# })
output$senStorageReleases <- renderPlot({
  sen.graph <- ts$sen
  graph_title <- "Little Seneca"
  res.graph <- sen.graph %>%
    select(date_time, storage, outflow) %>%
    gather(key = "Legend",
           value = "MG", -date_time) %>%
    filter(date_time >= input$plot_range[1],
           date_time <= input$plot_range[2])
  ggplot(data = res.graph,
         aes(x = date_time, y = MG, group = Legend)) +
    geom_line(aes(color = Legend, size = Legend)) +
    scale_color_manual(values = c("lightblue",
                                  "blue")) +
    scale_size_manual(values = c(0.5, 1)) +
    ggtitle(graph_title) +
    theme(plot.title = element_text(size = 18)) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    theme(legend.position = "none")
}) # end sen renderPlot
#
#------------------------------------------------------------------
output$patStorageReleases <- renderPlot({
  pat.graph <- ts$pat
  graph_title <- "Patuxent"
  res.graph <- pat.graph %>%
    select(date_time, storage, outflow) %>%
    gather(key = "Legend",
           value = "MG", -date_time) %>%
    filter(date_time >= input$plot_range[1],
           date_time <= input$plot_range[2])
  ggplot(data = res.graph,
         aes(x = date_time, y = MG, group = Legend)) +
    geom_line(aes(color = Legend, size = Legend)) +
    scale_color_manual(values = c("lightblue",
                                  "blue")) +
    scale_size_manual(values = c(0.5, 1)) +
    ggtitle(graph_title) +
    theme(plot.title = element_text(size = 18)) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    theme(legend.position = "none")
}) # end pat renderPlot
#
#------------------------------------------------------------------
output$occStorageReleases <- renderPlot({
  occ.graph <- ts$occ
  graph_title <- "Occoquan"
  res.graph <- occ.graph %>%
    select(date_time, storage, outflow) %>%
    gather(key = "Legend",
           value = "MG", -date_time) %>%
    filter(date_time >= input$plot_range[1],
           date_time <= input$plot_range[2])
  ggplot(data = res.graph,
         aes(x = date_time, y = MG, group = Legend)) +
    geom_line(aes(color = Legend, size = Legend)) +
    scale_color_manual(values = c("lightblue",
                                  "blue")) +
    scale_size_manual(values = c(0.5, 1)) +
    ggtitle(graph_title) +
    theme(plot.title = element_text(size = 18)) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    theme(legend.position = "none")
}) # end occ renderPlot
#
#------------------------------------------------------------------
#------------------------------------------------------------------