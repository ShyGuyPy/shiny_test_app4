#******************************************************************
# server.R defines reactive values & uses observeEvent to do simulation,
# then produces output
#******************************************************************
shinyServer(function(input, output, session) {
  
  #situational awareness tab
  source("code/server/situational_awareness/1st_sim.R", local=TRUE)
  source("code/server/situational_awareness/2nd_plot.R", local=TRUE)
  source("code/server/situational_awareness/3rd_output1.R", local=TRUE)
  source("code/server/situational_awareness/4th_output2.R", local=TRUE)
  source("code/server/situational_awareness/5th_output3.R", local=TRUE)
  
  #imported from zach's 2018 
  source("code/server/dates_server.R", local = TRUE)
  
  #one day tab
  
  #ten day tab
  
  source("code/server/ten_day/ten_day_server.R", local=TRUE)
  
  #long term tab
  
  #download and visualize tab
  
  }) # end shinyServer

