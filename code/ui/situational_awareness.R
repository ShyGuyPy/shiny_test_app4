tabPanel("Situational Awareness",
  fluidRow( # major row that contains whole body
    column( # major column that contains whole body
      width = 12,
      #
      # now add the content
      column(  # this is the 1st main column - with the graphs
        width = 6,
        fluidRow( # row with Potomac flow graph
          box(
            title = NULL,
            width = NULL,
            plotOutput("potomacFlows", height = "220px")
          )
        ),
        fluidRow( # row with 2 reservoir graphs
          h3("Reservoir storage (million gallons)"),
          column(
            width = 6,
            box(
              title = NULL,
              width = NULL,
              plotOutput("jrrStorageReleases", height = "190px")
            )
          ),
          column(
            width = 6,
            box(
              title = NULL,
              width = NULL,
              plotOutput("occStorageReleases", height = "190px")
            )
          )
        ), # end row with jrr and occ graphs
        fluidRow(
          column(
            width = 6,
            box(
              #                    title = "Little Seneca storage",
              title = NULL,
              width = NULL,
              plotOutput("senStorageReleases", height = "190px")
            )
          ),
          column(
            width = 6,
            box(
              title = NULL,
              width = NULL,
              plotOutput("patStorageReleases", height = "190px")
            )
          )
        ) # end row with sen and pat graphs 
        #                ) # end of row with all 4 reservoir graphs
      ), # end of 1st main column - with graphs
      column( # this is the 2nd main column - with values & triggers
        width = 6,
        valueBoxOutput("por_flow", width = NULL),
        valueBoxOutput("lfalls_obs", width = NULL),
        # infoBoxOutput("coop_ops", width = NULL),
        # infoBoxOutput("lfaa_alert", width = NULL),
        # infoBoxOutput("mwcog_stage", width = NULL),
        
        ##these three boxes were custom built replace the three infoboxes 
        # that are commented out above
        box(
          title=NULL,
          width=NULL,
          height=50,
          htmlOutput(outputId = "coop_ops")
        ),
        box(
          title=NULL,
          width=NULL,
          height=50,
          htmlOutput(outputId = "lfaa_alert")
        ),
        box(
          title=NULL,
          width=NULL,
          height=50,
          htmlOutput(outputId = "mwcog_stage")
        ),
        
        #these two boxes are for outputting the Maryland drought map 
        #and Virginia drought squares
        box(
          title = NULL,#"MARYLAND DROUGHT STATUS",
          width = NULL,#6,
          height = 220,
          htmlOutput(outputId="MD_title"),
          box(
            tags$img(alt="Drought Status Map:2019-05-31",
                     src= "https://mde.maryland.gov/programs/Water/droughtinformation/Currentconditions/PublishingImages/DroughtGraphsStarting2019jan31/Drought2019-05-31.png",
                     style="width:250px;height:150px;border:0;")
            #leafletOutput("mymap", height =140, width =300)
          ),
          box(
            htmlOutput(outputId = "boxes")
          )
        ),
        #tags$p("Western region: Drought Watch; Central region: Drought Warning")),
        box(
          title = NULL,#"VIRGINIA DROUGHT STATUS",
          width = NULL,#6,
          height = 220,
          
          box(
            tags$img(alt="Drought Status Map:2019-05-31",
                     src= "https://deq1.bse.vt.edu/drought/state/images/maps/imageMapFile15627929031512.png",
                     style="width:200px;height:150px;border:0;")
          ),
          
          
          htmlOutput(outputId = "boxes2")
        )
        #"NoVa: Drought Watch; Shenandoah: Drought Emergency")
      ) # end of 2nd main column
    ) # end of major column that contains whole body
  ), # end of major row that contains whole body
  fluidRow( # Temporary row to display some output for QAing
    valueBoxOutput("QA_out", width = NULL) 
  ) # end fluidRow for QAing purposes
) # end of tab panel