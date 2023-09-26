library(shiny)

shinyUI(
  fluidPage(
    fluidRow(
      column(width = 12,
        ##### HEADER #####--------------------------------------------------
        titlePanel("ROBIN explorer"),
        navbarPage(title="ROBIN explorer",
                   id="station",
                   selected="station_selector",
                   
          ##### MAIN STATION TAB #####--------------------------------------
          tabPanel("Station Selector",
                   value="station_selector",
            fluidPage(
              fluidRow(
                #### OPTIONS COLUMN ####-----------------------------------
                column(width = 3,
                  h3("Station selection"),
                  selectInput("country_pick",
                    label = "Country:",
                    choices = country_list,
                    selected = "United States of America"
                  ),
                  selectInput("station_pick",
                    label = "Station:",
                    choices = setNames(stationListRaw$localNumber, stationListRaw$station),
                    selected = "1532000"
                  ),
                  selectInput("plot_pick",
                    label = "Plot:",
                    choices = plot_list,
                    selected = "peakflow"
                  ),
                  selectInput("period_pick",
                              )
                  h3("Data preview"),
                  tableOutput("gdftable")
                  
                ),#column (station options)
                #### PLOT COLUMN ####--------------------------------------
                column(width = 9,
                  plotOutput("outputFigure")
                )#column (plots)
              )#fluidRow
            )#fluidPage
          )#,#tabPanel (station_selector)
          
          ##### TRIANGLE TAB #####-----------------------------------------
          # tabPanel("Triangle Trends",
          #          value="triangle",
          #   fluidPage(
          #     fluidRow(
          #       column(width = 3,
          #         wellPanel(
          #           h3("Multi-temporal Trend Summary"),
          #           helpText("Select the station to explore",
          #                    "the AMAX trends multi-temporal analysis.",
          #                    "Adjust the slider and click 'Update' to refresh the",
          #                    "plot."),
          #           
          #           selectizeInput(
          #             "station_no_T",
          #             label = "NRFA Station:",
          #             choices = station_list$df),
          #           
          #           #actionButton("update_btn_T", label="Update")
          #           
          #         )#wellPanel
          #         
          #        ),#column (selectors)
          #       
          #        column(width = 9,
          #           #plotOutput("mk_triangle", width="800px", height="650px"),
          #           
          #           sliderInput("dateslide_triangle",
          #                           "Date:",
          #                           min=1900,
          #                           max=2020,
          #                           value=c(1951, 2010),
          #                           step=1,
          #                           width="100%",
          #                           sep=""),
          #           
          #           helpText("Each grid square shows the trend observed if the data",
          #                    "is restricted to a specific time period. Red indicates negative",
          #                    "trends, blue indicates positive. Green points highlight significant trends.")
          #       )  # column (all Triangle)
          #     )  # fluidRow
          #   )  # fluidPage
          # ),# tabPanel (Triangle)
        )#navbarPage
      )#column
    )#fluidRow
  )#fluidPage
)#shinyUI
