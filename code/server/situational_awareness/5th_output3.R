
#NoVa warning status squares
output$boxes2  <- renderUI({
  #this code finds the last value in ts and outputs a number value between 0-3 that 
  #when run through function warning_color_func outputs a color variable that is linked 
  #to a css style color output value in global.R
  #for NoVa
  state.indices <- last(ts$states)
  i_p_va_nova <- state.indices$p_va_nova[1]
  color_p_va_nova <- warning_color_func(i_p_va_nova)
  i_gw_va_nova <- state.indices$gw_va_nova[1]
  color_gw_va_nova <- warning_color_func(i_gw_va_nova)
  i_sw_va_nova <- state.indices$sw_va_nova[1]
  color_sw_va_nova <- warning_color_func(i_sw_va_nova)
  i_r_va_nova <- state.indices$r_va_nova[1]
  color_r_va_nova <- warning_color_func(i_r_va_nova)
  
  #for Shenandoah
  i_p_va_shen <- state.indices$p_va_shen[1]
  color_p_va_shen <- warning_color_func(i_p_va_shen)
  i_gw_va_shen <- state.indices$gw_va_shen[1]
  color_gw_va_shen <- warning_color_func(i_gw_va_shen)
  i_sw_va_shen <- state.indices$sw_va_shen[1]
  color_sw_va_shen <- warning_color_func(i_sw_va_shen)
  i_r_va_shen<- state.indices$r_va_shen[1]
  color_r_va_shen <- warning_color_func(i_r_va_shen)
  
  #Virginia flow benefit
  flows.today <- last(ts$flows)
  dQ_va <- flows.today$dQ_va[1]
  
  #this is html in a format taht shiny will accept.  This along with main.css structures the 
  #properties of the Virginia Drought Status section
  div(class="topbox_main", p(class= "title", "VIRGINIA DROUGHT STATUS"),
      div(class="topbox1", 
          div(class="square", style=color_p_va_shen,#"background-color:yellow"
              div(class="my_content",
                  div(class="table",
                      div(class="table-cell",
                          p(class="p4","P")
                      )))), 
          div(class="square", style=color_gw_va_shen,#"background-color:red",
              div(class="my_content",
                  div(class="table",
                      div(class="table-cell",
                          p(class="p4","GW")
                      )))),
          div(class="square", style=color_sw_va_shen,#"background-color:orange",
              div(class="my_content",
                  div(class="table",
                      div(class="table-cell",
                          p(class="p4","SW")
                      )))),
          div(class="square", style="background-color:grey",#color_r_va_shen,#"background-color:green",
              div(class="my_content",
                  div(class="table",
                      div(class="table-cell",
                          p(class="p4","R")
                      )))),
          div(class="ibox", style = "background-color:white",
              div(class="my_content",
                  div(class="table",
                      div(class="table-cell",
                          p(class = "p5",paste0("Shenandoah "))#,text_stage2))
                      ))))
      ), #end of topbox1
      
      div(class="topboxmiddle",
          div(class="tallsquare", style="background-color:white",
              div(class="my_content",
                  div(class="table",
                      div(class="table-cell"
                      ))))
      ),
      div(class="topbox2", 
          div(class="square", style=color_p_va_nova,
              div(class="my_content",
                  div(class="table",
                      div(class="table-cell",
                          p(class="p4","P")
                      )))),
          div(class="square", style=color_gw_va_nova,
              div(class="my_content",
                  div(class="table",
                      div(class="table-cell",
                          p(class="p4","GW")
                      )))),
          div(class="square", style=color_sw_va_nova,
              div(class="my_content",
                  div(class="table",
                      div(class="table-cell",
                          p(class="p4","SW")
                      )))),
          #            div(class="square", style=color_r_va_nova,
          div(class="square", style="background-color:grey",
              div(class="my_content",
                  div(class="table",
                      div(class="table-cell",
                          p(class="p4","R")
                      )))),
          
          div(class="ibox", style = "background-color:white",
              div(class="my_content",
                  div(class="table",
                      div(class="table-cell",
                          p(class = "p5",paste0("NoVa "))#,text_stage2))
                      ))))
      ), #end of topbox2
      div(class="topboxmiddle",
          div(class="tallsquare", style="background-color:white",
              div(class="my_content",
                  div(class="table",
                      div(class="table-cell"
                      ))))
      ),
      div(class="sidebox",
          div(class="squareside1", style = "background-color:white",
              div(class="my_content",
                  div(class="table",
                      div(class="table-cell",
                          p(class="p3","Flow benefit, MGD")
                      )))),
          div(class="squareside2", style="background-color:silver",
              div(class="my_content",
                  div(class="table",
                      div(class="table-cell3", #style= "text-align:right",
                          p(style="font-size:15px;",dQ_va)
                      ))))
      ), #end of sidebox
      div(class="VA_right_box",style="float:right",
          div(class="keytopbox2",
              div(class="keysquare2",style="background:white",
                  div(class="p7","P = precipitation")
              ),
              div(class="keysquare2",style="background:white",
                  div(class="p7","GW = ground water")
              ),
              div(class="keysquare2",style="background:white",
                  div(class="p7","SW = surface water")
              ),
              div(class="keysquare2",style="background:white",
                  div(class="p7","R = reservoir")
              )
          )
      )
  )#end of topbox_main
})

#------------------------------------------------------------------
#code for outputting colors based on values in data(data and log are toy)
#some of this needs to be placed in functions section

# p_data_percent <- eventReactive(test_date$test_date_value, {
#   date_func(my_data_p$date, my_data_p$p_percent_normal, test_date$test_date_value)
# 
# })
# 
# 
# precip_value <- eventReactive(test_date$test_date_value,{#a_index,{
#   case_when(
#     p_data_percent() <= 0 ~ green,#"background-color:purple", #"#000000",
#     p_data_percent() > .0 && p_data_percent() <= .20 ~ red,#"background-color:red", #"#cc3300",
#     p_data_percent() > .20 && p_data_percent() <= .40 ~ orange,#"background-color:orange",  #"#ff9966",
#     p_data_percent() > .40 && p_data_percent() <= .60 ~ yellow,#"background-color:yellow",  #"#ffcc00",
#     p_data_percent() > .60 && p_data_percent() <= .80 ~ green,#"background-color:green", #"#99cc33",
#     p_data_percent() > .80 && p_data_percent() < 1 ~  navy, #"background-color:navy" #"#339900"
#     TRUE ~ black
#     
#   )
#   
# })
# 
# q_data_percent <- eventReactive(test_date$test_date_value, {
#   date_func(my_data_q$date, my_data_q$flow_cfs, test_date$test_date_value)
# })
# 
# q_value <- eventReactive(test_date$test_date_value,{
#   case_when(
#     q_data_percent() <= 0 ~ red,#"background-color:purple", #"#000000",
#     q_data_percent() > 0 && q_data_percent() <= 100 ~ red,#"background-color:red", #"#cc3300",
#     q_data_percent() > 100 && q_data_percent() <= 200 ~ orange,#"background-color:orange",  #"#ff9966",
#     q_data_percent() > 200 && q_data_percent() <= 300 ~ yellow,#"background-color:yellow",  #"#ffcc00",
#     q_data_percent() > 300 && q_data_percent() <= 400 ~ green,#"background-color:green", #"#99cc33",
#     #q_data_percent() > 400 && q_data_percent() < 500 ~  navy, #"background-color:navy" #"#339900"
#     q_data_percent() > 400 ~ navy,
#     TRUE ~ black
#   )
# })
# 
# s_data_percent <- eventReactive(test_date$test_date_value, {
#   date_func(my_data_s$date, my_data_s$storage_days, test_date$test_date_value)
# })
# 
# s_value <- eventReactive(test_date$test_date_value,{
#   case_when(
#     s_data_percent() <= 0 ~ yellow,#"background-color:purple", #"#000000",
#     s_data_percent() > 0 && s_data_percent() <= 60 ~ red,#"background-color:red", #"#cc3300",
#     s_data_percent() > 60 && s_data_percent() <= 90 ~ orange,#"background-color:orange",  #"#ff9966",
#     s_data_percent() > 90 && s_data_percent() <= 120 ~ yellow,#"background-color:yellow",  #"#ffcc00",
#     s_data_percent() > 120 && s_data_percent() <= 500 ~ green,#"background-color:green", #"#99cc33",
#     s_data_percent() > 500 && s_data_percent() <= 1130 ~  navy, #"background-color:navy" #"#339900"
#     TRUE ~ black
#   )
# })
# 
# g_data_percent <- eventReactive(test_date$test_date_value, {
#   date_func(my_data_g$date, my_data_g$flow_cfs, test_date$test_date_value)
# })
# 
# g_value <- eventReactive(test_date$test_date_value,{
#   case_when(
#     g_data_percent() <= 0 ~ orange,#"background-color:purple", #"#000000",
#     g_data_percent() > 0 && g_data_percent() <= 55 ~ red,#"background-color:red", #"#cc3300",
#     g_data_percent() > 55 && g_data_percent() <= 110 ~ orange,#"background-color:orange",  #"#ff9966",
#     g_data_percent() > 110 && g_data_percent() <= 165 ~ yellow,#"background-color:yellow",  #"#ffcc00",
#     g_data_percent() > 165 && g_data_percent() <= 220 ~ green,#"background-color:green", #"#99cc33",
#     g_data_percent() > 220 && g_data_percent() < 275 ~  navy, #"background-color:navy" #"#339900"
#     TRUE ~ black
#   )
# })

#------------------------------------------------------------------