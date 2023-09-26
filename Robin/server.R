library(shiny)
library(tidyverse)
library(lfstat)
library(lmomco)
library(lubridate)
library(circular)
library(trend)
library(strucchange)
library(changepoint)
library(Kendall)
library(zyp)
library(RODBC)
options(show.error.messages = T)
#options(warn=-1)

source("global.R")

shinyServer(function(input, output, session) {
  
  station_list <- reactiveValues(
    df=data.frame(country="UK",
                  station="Glasgow")
  )
  
  data_list <- reactiveValues(
    df=data.frame(STATION="1001",
                  DAY=ymd("2002-01-01"),
                  FLOW=1,
                  FLAG="A"),
    lf=data.frame(day=1, month=1, year=2000,
                  flow=1, hyear=2000, baseflow=0, date=ymd("2000-01-01")),
    amax=data.frame(year=2000,
                    flow=1),
    pot=data.frame(date=ymd("2002-01-01"),
                   flow=1),
    qxx=data.frame(year=2000,
                   period="Qu1",
                   Q05=0,
                   Q70=1,
                   Q90=2,
                   Q95=3)
  )
  
  plotg <- reactiveValues(
    g=NA
  )
  
##### NORMAL PLOTS #####------------------------------------------------------  
  
  observe({
    station_list$df <- stationListRaw[stationListRaw$country==input$country_pick,]
    newchoices <- setNames(station_list$df$localNumber, station_list$df$station)
    #print(length(newchoices))
    updateSelectInput(session, "station_pick", choices=newchoices)
  }) 
  
  observe({
    data_list$df <-  sqlQuery(channel,
      sprintf("SELECT * from STETUR.ROBIN_GDF where STATION='%s'",
              input$station_pick))
    data_list$lf <- lfstat_prep(data_list$df[,2:3])
    data_list$amax <- amax_prep(data_list$df)
    data_list$pot <- pot_prep(data_list$lf)
  })
  
  output$gdftable <- renderTable({
    dldf <- data_list$df
    dldf$DAY <- format(dldf$DAY, "%Y-%m-%d")
    dldf[1:10,]
    })
  
  output$outputFigure <- renderPlot({
    if (input$plot_pick == "peakflow") {
      g <- ggplot(data_list$amax) +
        geom_step(aes(x = date, y = amax)) +
        geom_point(data = data_list$pot, aes(x = date, y = flow),
                   colour = "red") +
        labs(x="Date", y="Flow")
        
    }else if(input$plot_pick == "qxx"){
      data_list$qxx$time <- 
      g <- ggplot(data_list$qxx) +
        geom_line(aes(x = DAY, y = FLOW), colour = "purple")
    }else if(input$plot_pick == "highflow"){
      
    }else if(input$plot_pick == "lowflow"){
      
    }else{
      
    }
    g
  })
  
  # observe({
  # 
  #     browser()
  #     if(input$plot_pick == "peakflow"){
  #       plotg$g <- ggplot(data_list$amax) + 
  #         geom_line(aes(x=yeasr, y=amax)) +
  #         geom_point(data=data_list$pot, aes(x=date, y=flow), colour="red")
  #     }else{
  #       plotg$g <- ggplot(data_list$df) +
  #         geom_line(aes(x=DAY, y=FLOW), colour="purple")
  #     }
  #   })
  
  
##### TRIANGLE PLOT #####---------------------------------------------------
  
  # # Triangle-plot dataframe
  # tr <- reactiveValues(df=data.frame( date=as.Date("2000-01-01"),
  #                                     year=2000,
  #                                     year0=0,
  #                                     flow=100),
  #                      real=TRUE)  # real station flag
  # 
  # # Triangle plot slider range
  # rangesT <- reactiveValues(x = c(2004,2015))
  # 
  # 
  # observeEvent(input$dateslide_triangle,
  #       {rangesT$x <- c(as.Date(paste0(input$dateslide_triangle[1]-1,"-10-01")),
  #                       as.Date(paste0(input$dateslide_triangle[2],"-09-30")))
  #       })
  

  
  # observeEvent(toListen_A(),{
  #   api_call_path <- input$station_no_T
  #   
  #   # Extract AMAX
  #   tr_json_raw <- fromJSON(file=api_call_path)
  #   tr_raw <- matrix(unlist(tr_json_raw$`data-stream`), ncol=2, byrow=T)
  #   colnames(tr_raw) <- c("Date", "Flow")
  #   tr_raw <- data.frame(tr_raw, stringsAsFactors = F)
  #   tr_raw$Date <- as.Date(tr_raw$Date)
  #   tr_raw$Flow <- as.numeric(tr_raw$Flow)
  #   
  #   # if fewer than 2 rows of daily data (unlikely), treat station as missing.
  #   if(nrow(tr_raw) < 2){
  #    tr$df <- data.frame(date=as.Date("2000-01-01"),
  #                        year=2000,
  #                        year0=0,
  #                        flow=0)
  #    
  #    # Default values
  #    showNotification(
  #      "Insufficient station data found. Please check the station ID.",
  #      type="error")
  #    tr$real <- FALSE
  #    
  #   }else{
  #    # Include Hydrological Year
  #    #print(findHydrolYr(as.Date(tr_raw$Date)))
  #    tr$df <- data.frame(date= as.Date(tr_raw$Date),
  #                        year =findHydrolYr(tr_raw$Date)[,2],
  #                        flow = as.numeric(tr_raw$Flow))
  #    
  #    if(input$dateslide_triangle[1] < min(tr$df$year) |
  #       input$dateslide_triangle[2] > max(tr$df$year)){
  #      updateSliderInput(session, "dateslide_triangle", min = tr$df$year[1],
  #                        max = tr$df$year[nrow(tr$df)],
  #                        value=c(tr$df$year[1], tr$df$year[nrow(tr$df)])
  #      )
  #      rangesT$x <- c(as.Date(paste0(tr$df$year[1]-1,"-10-01")),
  #                     as.Date(paste0(tr$df$year[nrow(tr$df)],"-09-30")))
  #    }else{
  #      updateSliderInput(session, "dateslide_triangle", min = tr$df$year[1],
  #                        max = tr$df$year[nrow(tr$df)])
  #    }
  #    
  #    tr$df <- subset(tr$df, date >= rangesT$x[1] & date <= rangesT$x[2])
  #    # year0 is shifted to make TSE more interpretable
  #    tr$df$year0 <- tr$df$year - floor(median(tr$df$year))
  #    if(length(unique(tr$df$year))!=length(tr$df$year)){
  #      stop("two events in same year.")
  #    }
  #    tr$real <- TRUE
  #   }
  #   #print(lubridate::time_length(difftime(as.Date(rangesT$x[2]), as.Date(rangesT$x[1])), "years"))
  #   if(lubridate::time_length(difftime(as.Date(rangesT$x[2]),
  #                                     as.Date(rangesT$x[1])), "years") < 27){
  #    showNotification(
  #      paste0("Multi-temporal analysis requires a window of at least 26 years.",
  #             "Adjust the slider and click 'update'."),
  #      type="error")
  #    tr$real <- FALSE
  #   }
  #   })

})
