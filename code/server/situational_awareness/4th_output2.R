#------------------------------------------------------------------
# Create state drought status boxes
#------------------------------------------------------------------
#
# # Luke - here's how you can access the values 
# #        from within a render function:
# state.indices <- last(ts$states)
# # The indicator values are:
# #   0 = NORMAL
# #   1 = WATCH
# #   2 = WARNING
# #   3 = EMERGENCY
# #
# # The indicator names are
# #    gw_va_shen, p_va_shen, sw_va_shen, r_va_shen,
# #    gw_va_nova, p_va_nova, sw_va_nova, r_va_nova,
# #    region_md_cent, region_md_west
# # -----------------------------------
# # The VA and MD flow changes:
# flows.today <- last(ts$flows)
# dQ_va <- flows.today$dQ_va[1]
# dQ_md <- flows.today$dQ_md[1]
#------------------------------------------------------------------
###Maryland drought warning map

#title output
output$MD_title <- renderUI({
  div(p(class= "title","MARYLAND DROUGHT STATUS"))
})
output$boxes  <- renderUI({
  #MD flow benefit
  flows.today <- last(ts$flows)
  dQ_md <- flows.today$dQ_md[1]
  
  #div(class="topbox_main", p(class= "title","MARYLAND DROUGHT STATUS"),
  #the image link below is a placeholder for an interactive leaflet map forthcoming
  
  #img( src="https://md.water.usgs.gov/drought/MDE-Drought2017-02-28.png", height="160px", width="360px"),
  
  
  #this is html in a format taht shiny will accept.  This along with main.css structures the 
  #properties of the Maryland Drought Status section
  div(class="topbox_main",
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
                      div(class="table-cell3", #style="text-align:right;",
                          p(style="font-size:15px;", dQ_md)
                      ))))
      ), #end of sidebox
      div(class="MD_right_box", style="float:right",
          div(class="keytopbox",
              div(class="keysquare",style=green,
                  div(class="p6","NORMAL")
              ),
              div(class="keysquare",style=yellow,
                  div(class="p6","WATCH")
              ),
              div(class="keysquare",style=orange,
                  div(class="p6","WARNING")
              ),
              div(class="keysquare",style=red,
                  div(class="p6","EMERGENCY")
              )
          )
      )
  ) #end of topbox_main
  
})

output$mymap <- renderLeaflet({
  #for MD drough map
  state.indices <- last(ts$states)
  i_region_md_cent <- state.indices$region_md_cent[1]
  color_i_region_md_cent <- warning_color_map_func(i_region_md_cent)
  i_region_md_west <- state.indices$region_md_west[1]
  color_i_region_md_west <- warning_color_map_func(i_region_md_west)
  
  leaflet(options= leafletOptions(minZoom = 6.7, maxZoom=6.7, zoomControl=FALSE)) %>%
    addPolygons(data = clipcentral_t, color="black", fillColor = color_i_region_md_cent, opacity = 1, weight = 1,
                fillOpacity = 1) %>%
    addPolygons(data = western_region_t, color="black", fillColor = color_i_region_md_west, opacity = 1, weight= 1,
                fillOpacity = 1) %>%
    addLabelOnlyMarkers(lng = -78.5, lat = 39.2, label = "Western",
                        labelOptions = labelOptions(noHide = T, direction = "bottom",
                                                    style = list(
                                                      "color" = "black",
                                                      "font-family" = "helvetica",
                                                      #"font-style" = "italic",
                                                      #"box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                                      "font-size" = "15px",
                                                      "border-color" = "white",#"rgba(0,0,0,0.5)",
                                                      "font-weight" = "bold"
                                                    ))) %>%
    addLabelOnlyMarkers(lng = -76.5, lat = 39.2,
                        label = "Central",
                        labelOptions = labelOptions(noHide = T, direction = "bottom",
                                                    style = list(
                                                      "color" = "black",
                                                      "font-family" = "helvetica",
                                                      #"font-style" = "italic",
                                                      #"box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                                      "font-size" = "15px",
                                                      "border-color" = "white",#rgba(0,0,0,0.5)",
                                                      "font-weight" = "bold"
                                                    )))
})

#------------------------------------------------------------------