#----------------------------------------------------------------
#----------------------------------------------------------------
# First, do the simulation
#----------------------------------------------------------------
#----------------------------------------------------------------
# Run the main simulation to the hard-coded input, date_today
#    - ts here is the precursor of the set of reactive values


ts0 <- list(sen = sen.ts.df0,
            jrr = jrr.ts.df0,
            pat = pat.ts.df0,
            occ = occ.ts.df0,
            flows = potomac.ts.df0,
            states = state.ts.df0)



#
# 2019 - drop this first simulation and define ts from the ts0's
# ts <- sim_main_func(date_today0,
#                     dr_va0, # VA upstream demand reduction
#                     dr_md_cent0, # MD-Central upstream demand reduction
#                     dr_md_west0, # MD-Western upstream demand reduction
#                     mos_1day0, # COOP 1-day margin of safety
#                     dr_wma_override0, # WMA demand reduction override
#                     ts0) 
#
# # Now make ts reactive, initializing to results from above
# ts <- reactiveValues(flows = ts$flows, 
#                      sen = ts$sen, 
#                      jrr = ts$jrr,
#                      pat = ts$pat,
#                      occ = ts$occ,
#                      states = ts$states)
ts <- reactiveValues(flows = ts0$flows,
                     sen = ts0$sen,
                     jrr = ts0$jrr,
                     pat = ts0$pat,
                     occ = ts0$occ,
                     states = ts0$states)
#
# Allow the user to re-run the simulation 
#   - say, if end date (aka DREXtoday) changes
observeEvent(input$run_main, {
  ts <- sim_main_func(input$DREXtoday,
                      input$dr_va,
                      input$dr_md_cent,
                      input$dr_md_west, 
                      input$mos_1day,
                      input$dr_wma_override,
                      ts)
})
#
# Allow the user to add chunks of days to the simulation
observeEvent(input$run_add, {
  ts <- sim_add_days_func(input$chunkofdays,
                          input$dr_va,
                          input$dr_md_cent,
                          input$dr_md_west, 
                          input$mos_1day,
                          input$dr_wma_override,
                          ts)
})

#
# Allow the user to write simulation output time series to files
observeEvent(input$write_ts, {
  write.csv(ts$flows, paste(ts_output, "output_flows.csv"))
  write.csv(ts$sen, paste(ts_output, "output_sen.csv"))
  write.csv(ts$jrr, paste(ts_output, "output_jrr.csv"))
  write.csv(ts$occ, paste(ts_output, "output_occ.csv"))
  write.csv(ts$pat, paste(ts_output, "output_pat.csv"))
  write.csv(ts$states, paste(ts_output, "output_states.csv"))
})
#------------------------------------------------------------------
#------------------------------------------------------------------