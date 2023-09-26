# Robin project
# Adam Griffin
# 2022-05-06

# Read in files
  # follow standard format
  # Basic plots of time series
  # Smoothed plots?

read_in_robin <- function(csv, partial=TRUE, ...){
  # wrapper for read.csv to convert date strings to date objects.
  # csv     filename
  # partial if FALSE, assumes first two columns are Agency and Station.
  if(partial){
    nameVec <- c("Day", "Flow", "Flag")
  }else{
    nameVec <- c("Agency", "Station", "Day", "Flow", "Flag")
  }
  z <- suppressWarnings(
    setNames(read.csv(csv, col.names=nameVec, fill=T, ...), nameVec))
  z$Day <- lubridate::ymd_hms(z$Day, truncated=3)
  z$id <- seq(1, nrow(z))
  z$Flag[is.na(z$Flag)] <- ""
  #z$Flag <- factor(z$Flag)
  z
}

lf_seas_fun <- function(lf_xts, percile){
  
}

read_in_robin_db <- function(station, uid, pwd){
  channel <- odbcConnect(dsn = "wla", uid = uid, pwd = pwd)
  z <- sqlQuery(channel,
      paste0("SELECT * FROM ROBIN.GAUGED_DAILY_FLOWS WHERE ROBIN_ID = '",station,"';"),
      believeNRows=F)
  odbcCloseAll()
  colnames(z) <- c("Station", "Day", "Flow", "Flag")
  z$Day <- lubridate::ymd_hms(z$Day, truncated=3)
  z$id <- seq(1, nrow(z))
  z$Flag[is.na(z$Flag)] <- ""
  z
}

#' Check suitability of station for ROBIN analysis
#'
#' This function checks for suitability for analysis within the ROBIN global 
#' hydrometric network. It checks for record length, percentage of missing data
#' and whether there exist any continuous gaps of over two years.
#' @param flow dataframe with DAY and FLAG columns.
#' @param missingFlag string or vector of strings used to indicate missing data
#' 
#' @return a list of the stationLevel (0,1,2) and trendFlag (Boolean)
#' 
#' @export
check_for_level <- function(flow, missingFlag="M"){
  flow$year <- lubridate::year(flow$Day)
  nYears <- floor((max(flow$Day) - min(flow$Day))/365.25)
  lenFlag <- ifelse(nYears >= 20, ifelse(nYears >= 40, 1, 2), 0)
  #flow$Flag[is.na(flow$Flag)] <- ""
  longGap <- any(rle(
    flow$Flag)$lengths[rle(flow$Flag)$values %in% missingFlag] > 365*2)
  if (longGap) print("More than 2 complete consecutive years missing.")
  
  manyMissed <- (sum(flow$Flag != "", na.rm=T) > 0.1*nrow(flow))
  if (manyMissed) {
    print("More than 10% missing data")
    lenFlag <- 0
  }
  trendFlag <- (!longGap & lenFlag > 0)
  if (trendFlag) print("Suitable for trend analysis")
  return(list(stationLevel=lenFlag,
              trendFlag=trendFlag)
         )
}
# Extract statistics
  # Percentiles of flow #periodPercentile
  # Peaks (AMAX POT)
  # Droughts
  # Missing data?
  # Dealing with ephemerality

# Perform trend analysis
  # Pettitt test for changes in average behaviour (breakpoint)
  # Theil-Sen
  # AMAX should be independent
  # Percentiles of flow may not be?
  # Conflating correlated with "driven by a common process".
  # lag1
  # Mann Kendall
# Kendall::MannKendall(x)
  # Seasonal Mann Kendall
# Kendall::SeasonalMannKendall(x)
  # Mann-Kendall-Snyers (rolling window MK)
  # triangle plots 

quarterly_summary <- function(lf_flow, qxx=0.9){
  lfstat::apply.seasonal(lf_xts, fun=\(x){quantile(x,qxx,na.rm=T)},
                         varying=c("Qu1"="1999-01-01","Qu2"="1999-04-01",
                                   "Qu3"="1999-07-01","Qu4"="1999-10-01")) %>%
    as.data.frame() %>%
    dplyr::mutate(year=rownames(.), Qxx=1-qxx) %>%
    tidyr::pivot_longer(cols=1:4, names_to="Season", values_to="Flow")
}

yearly_summary <- function(lf_flow, qxx=0.9){
  xts::apply.yearly(lf_xts, FUN=\(x){quantile(x,qxx,na.rm=T)}) %>%
    as.data.frame() %>%
    dplyr::mutate(year=lubridate::year(rownames(.)), Qxx=1-qxx)
}

monthly_summary <- function(lf_flow, qxx=0.9){
  xts::apply.monthly(lf_xts, FUN=\(x){quantile(x,qxx,na.rm=T)}) %>%
    as.data.frame() %>%
    mutate(year=lubridate::year(rownames(.)),
           month=lubridate::month(rownames(.)),
           Qxx=1-qxx)
}

pettittTest <- function(x, y) {
  naVec <- !is.na(y) # deal with missing
  y <- y[naVec] 
  x <- x[naVec]
  xy <- cbind(x, y)
  pettittTest <- trend::pettitt.TEST(xy, 0.05) # Pettit test @ 0.05 sig level
  return(c(pettittTest$XEa[[1]], pettittTest$pval))  # Changepoint year, p-val
}

pettitt_tc <- function(x){
  # wrapper for pettitt.test to catch errors
  tryCatch(trend::pettitt.test(x),
           error=\(e)list(estimate=1, p.value=1),
           warning=\(e)list(estimate=1, p.value=1))
}

meanvar_tc <- function(x){
  # wrapper for changepoint::cpt.meanvar to catch errors
  tryCatch(cpt.meanvar(x, minseglen=5, test.stat="Gamma")@ncpts.max,
           error=\(e) NA,
           warning=\(e) NA)
}


HurstFun <- function(x) {
  xtemp <- x
  xtemp <- xtemp[!is.na(xtemp)] # no NAs
  xtemp2 <-  hurstexp(xtemp)[1]
  return(as.numeric(xtemp2))
}


# autoAxis systematically determines an appropriate scale for a date axis.
# series is a single datetime series object
# ... any other graphical par arguments to the axis function (see '?axis')
autoAxis <- function(series, ...){
  X <- as.Date(range(series))
  Xseq <- seq(X[1], X[2], by='month')
  Xlen <- findInterval(length(Xseq), c(0,12,36,60,300))
  Xseq <- switch(Xlen,
                 seq(X[1], X[2], by='month'),
                 seq(X[1], X[2], by='3 months'),
                 seq(X[1], X[2], by='6 months'),
                 seq(X[1], X[2], by='year'),
                 seq(X[1], X[2], by='5 years'))
  Xlab <- switch(Xlen,
                 "%b %Y","%b %Y","%b %Y","%Y", "%Y")
  
  graphics::axis(1, at=Xseq, labels=format(Xseq, Xlab), ...)
  if(length(seq(X[1], X[2], by="month")) < 20){
    graphics::axis(1, at=seq(X[1], X[2], by='month'), labels=F, tick=T)
  }else{
    graphics::axis(1, at=seq(X[1], X[2], by='year'), labels=F, tick=T)
  }
}


#' Compute hydrological year and day-of-year
#'
#' Return the hydrological year and day-of-year. The UK hydrological year starts
#' at 9am on 1st October. For example, the 1901-1902 hydrological year is
#' denoted 1902. The hydrological day that starts at 9am, 4th October is
#' denoted DOY = 4.
#' @param datetime vector of datetime objects
#' @param starttime start time of a hydrological year, doesn't matter which one. Format is YYYY-MM-DD HH:MM:SS
#' @return A dataframe of two columns, DOY and hydrological year.
#'
#' @examples \dontrun{
#' datetimes(as.POSIXlt((1:365+0.25)*76400, origin="1990-01-01"))
#' }
#' @export
findHydrolYr <- function (datetime, starttime="2000-10-01 09:00:00") {
  # returns the hydrological year and DOY (starts at 9am, 1st October)
  # e.g. the 2018-19 hydrol year is denoted 2019.
  # the hydrol day that starts at 9am 3rd May is denoted 3rd May (or DOY equiv)
  stda <- lubridate::ymd_hms(starttime, truncated=3)
  dt <- lubridate::ymd_hms(datetime)
  hms_flag <- TRUE
  if(all(is.na(dt))){
    dt <- lubridate::ymd(datetime)
    stda <- lubridate::ymd(starttime)
    hms_flag <- FALSE
  }
  wd <- dt
  if(hms_flag){
    wd[which(lubridate::hour(dt) < lubridate::hour(stda))] <-
      wd[which(lubridate::hour(dt) < lubridate::hour(stda))] - lubridate::days(1)
  }
  y <- lubridate::year(wd)
  y[which(lubridate::month(wd) > lubridate::month(stda)-1)] <-
    y[which(lubridate::month(wd) > lubridate::month(stda)-1)] + 1
  startwy <- stda
  year(startwy) <- y-1
  nday <- as.numeric(dt - startwy)
  return(data.frame(DOY=nday, yr=y))
}



#' Plotting of AMAX series with trend lines
#'
#' Uses a time-series and fitted parameters to add lines indicating estimated
#' magnitudes of events of given return periods.
#'
#' @param x time-series dataframe of year and flow
#' @param param vector of GLO parameters (loc, sca, sha) for stationary distributions and (loc_int, loc_slope, sca, sha)
#' for non-stationary distributions
#' @param start start year of plot
#' @param end end year of plot
#' @param rp vector of return periods (in years for AMAX data)
#' @param NST if true, plots non-stationary return period lines.
#'
#' @return a plot with the time-series and lines indicating magnitude of
#' events with given return periods over time.
#'
#' @export
amaxPlot <- function(x, param, start, end, rp=2, NST=FALSE){
  if(length(rp)<1){
    stop("provide return periods.")
  }
  colnames(x) <- c("date", "flow")
  x$year0 <- x$year - stats::median(x$year)
  qq <- 1-(1/rp)
  graphics::par(mar=c(3,4,0.4,0.4), mgp=c(2,1,0))
  vird <- viridis::viridis(length(rp))
  graphics::plot(x$date, x$flow,
                 xlim=c(start, end),
                 ylim=c(0, 2*max(x$flow)),
                 xlab="Date",
                 ylab=expression(paste("Annual Maximum Flow (", m^3~s^-1, ")")))
  if(length(rp)>0){
    for(i in 1:length(rp)){
      if(NST){
        qm <- param[1] + param[2]*(x$year0) +
          (param[3]/param[4])*(1 - ((1-qq[i])/(qq[i]))^param[4])
      }else{
        qm <- rep(qglo(qq[i], loc=param[1], scale=param[2], sh=param[3]),
                  length(x$date))
      }
      graphics::lines(x$date, qm, col=vird[i], lwd=1.5, )
      graphics::text(x$date[length(x$date)]+10,
                     qm[length(qm)],
                     label=rp[i], pos=4)
    }
  }
  if(NST){
    pval <- signif(KendallZScore(x$flow), 4)
    sigstar <- ifelse(abs(pval)>1.96, "*", "")
    graphics::text(start, 2*max(x$flow),
                   paste0("MKZs = ", pval, sigstar),
                   pos=4, cex=1.5)
  }
  graphics::legend("topright", legend=rp,
                   col=vird, lwd=1,
                   title="Return Periods")
}



#' import data from API for triangle plotting
#'
#' This takes AMAX data from the NRFA API, and reformats it into shape for a
#' multi-temporal analysis of trend. The analysis needs at least 26 years of data as
#' it uses a 25-year rolling window.
#'
#'
#' @param st_no string or numerical station NRFA identifier
#' @param start start year
#' @param end end year
#'
#' @export
importTriangle <- function(st_no, start, end){
  ### TODO: REPLACE WITH GET STATION FUNCTION
  api_call_path <- paste0("https://nrfaapps.ceh.ac.uk/nrfa/ws/time-series?",
                          "format=nrfa-csv&data-type=amax-flow&station=",
                          st_no)
  
  # Extract AMAX
  tr_raw <- utils::read.csv(api_call_path, skip=21, col.names=c("Date","Flow"),
                            stringsAsFactors=FALSE)
  
  # if fewer than 2 rows of daily data (unlikely), treat station as missing.
  if(nrow(tr_raw) < 2){
    df <- data.frame(date=as.Date("2000-01-01"),
                     year=2000,
                     year0=0,
                     flow=0)
    
    # Default values
    stop(
      "Insufficient station data found. Please check the station ID.")
    
  }else{
    # Include Hydrological Year
    df <- data.frame(date= as.Date(tr_raw$Date),
                     year =findHydrolYr(tr_raw$Date)[,2],
                     flow = as.numeric(tr_raw$Flow))
    
    df <- subset(df, df$year >= start & df$year <= end)
    # year0 is shifted to make TSE more interpretable
    df$year0 <- df$year - floor(stats::median(df$year))
    if(length(unique(df$year))!=length(df$year)){
      stop("two events in same year.")
    }
  }
  if(nrow(df) < 27){
    stop("Multi-temporal analysis requires a window of at least 26 years.")
  }
  df
}

#' Triangle plotting of trends for all observed time windows
#'
#' This function takes the data.frame of amax flow, and returns a matrices of
#' Mann-Kendall test statistics and Theil-Sen estimates of trend for every choice
#' of start year and end year (with a minimum of 26 years of data)
#'
#' @section  Information about flags:
#' \enumerate{
#' \item for the years outside the range of data for each station give the value -100 \\
#' \item when there are less than 27 total years (NA included) give the value -200 \\
#' \item when there are: \itemize{
#'    \item more than 10% missing data or less than 27 actual data (NA excluded),\\
#'    \item or more than 2 missing values in the beginning of the subset,\\
#'    \item or more than 1 missing value in the end of the subset, then give the value -300 }\\
#' \item when end year is smaller than start year give the value -400
#' }
#'
#' @param ts a two column data.frame object of year and flow
#' @param startyr start year for triangle matrices
#' @param endyr end year for triangle matrices
#'
#' @return list of 2 matrices, MKZ values and TSE values.
#'
#' @export
triangleTrend <- function(ts, startyr, endyr){
  
  colnames(ts) <- c("year", "flow")
  ts$year0 <- ts$year - floor(stats::median(ts$year))
  dataSub <- zoo::na.trim(ts)
  # create empty array
  ND <- nrow(dataSub)
  Mat <- matrix(NA, ND, ND)
  Mat_TSArel <- matrix(NA, ND, ND)
  dimnames(Mat) <-  list(End=dataSub$year, Start=dataSub$year)
  dimnames(Mat_TSArel) <- list(End=dataSub$year, Start=dataSub$year)
  
  All_combs <- expand.grid(End=dataSub$year, Start=dataSub$year,
                           stringsAsFactors=FALSE)
  # loop through stations
  Used_Subsets <- vector('list', length = nrow(Mat)^2)
  # Get true data
  true.data <- dataSub$flow
  NonNAindex <- which(!is.na(true.data))
  firstNonNA <- min(NonNAindex)
  
  # relaxed criterion for start year, consistent with script 5
  firstNonNA <- ifelse(firstNonNA <= 2, 1, firstNonNA-2)
  lastNonNA <- max(NonNAindex)
  
  # relaxed criterion for end year, consistent with script 5
  if (lastNonNA < nrow(dataSub)) { lastNonNA <- lastNonNA + 1}
  
  for (yr_start in 1:dim(Mat)[2]){ # loop yr_start through starting years starts
    if (yr_start<firstNonNA){
      Mat[, yr_start] <- Mat_TSArel[, yr_start] <- -100
      Mat[yr_start, ] <- Mat_TSArel[yr_start, ] <- -100
    }else if (yr_start>lastNonNA){
      Mat[, yr_start] <- Mat_TSArel[, yr_start] <- -100
      Mat[yr_start, ] <- Mat_TSArel[yr_start, ] <- -100
    }else{
      for (yr_end in yr_start:dim(Mat)[1]){
        # loop yr_end through ending years starts
        if(yr_end - yr_start<(27-1)){
          # we need at least 27 years, meaning a difference of at least 26
          Mat[yr_end, yr_start] <- Mat_TSArel[yr_end, yr_start] <- -200
        }else{
          used_data <- true.data[yr_start:yr_end]
          # subset used data
          NA_percent <- sum(is.na(used_data))/length(used_data)
          # 10% missing data criterion
          NonNAindex_subset <- which(!is.na(used_data))
          # 2 years relaxation for start year
          firstNonNA_subset <- min(NonNAindex_subset)
          # 2 years relaxation for start year
          Length_used_data <- length(used_data[!is.na(used_data)])
          # 27-year of actual data criterion
          lastNonNA_subset <- max(NonNAindex_subset)
          # 1 year relaxation for end year
          CriterionPass <- (NA_percent<= 0.1 & Length_used_data>=27 &
                              firstNonNA_subset<=3 & lastNonNA_subset >= (length(used_data) - 1))
          # combined criteria
          if(CriterionPass){
            Mat_TSArel[yr_end, yr_start] <- relativeTheilSen(
              dataSub[yr_start:yr_end,3], used_data
            )
            Mat[yr_end, yr_start] <- KendallZScore(
              used_data
            )
            # assign used data to correct position within Used_Subsets
            i_row <- (which(All_combs$End==dataSub$year[yr_end] &
                              All_combs$Start==dataSub$year[yr_start]))
            Used_Subsets[[i_row]] <- used_data
          }else{
            Mat[yr_end, yr_start] <- Mat_TSArel[yr_end, yr_start] <- -300
          }
        }
      } # loop yr_end through ending years ends
      
      
    } # end criterion if tarting year falls outside range of data of the station
  } # loop yr_start through starting years ends
  
  # read the list with the used data sets and keep only the ones with actual data
  Used_Subset <- Used_Subsets
  # data analysis for keeping only the unique combinations
  Used_combs <- which(unlist(lapply(Used_Subset, is.null))==T)
  Used_Subset[Used_combs] <- NA
  # remove start/end NAs are they are not used on MKZ test
  Used_Subset <- lapply(Used_Subset, zoo::na.trim)
  # keep only the location of instances with unique time-series (w/ length>0)
  Used_Subsets <- which(duplicated(Used_Subset)==F &
                          unlist(lapply(Used_Subset, function(x)length(x)))>0 )
  
  Mat[is.na(Mat)] <- -400
  # all remaining NA's are places where end_year < start_year
  Mat_TSArel[is.na(Mat_TSArel)] <- -400
  
  return(list(MKZ=Mat, TSE=Mat_TSArel))
}


#' Triangle plotting of trends for all observed time windows
#'
#' This function uses the output from \code{triangleTrends} and plots the triangle
#' plot.
#'
#' @param MKZ matrix of Mann-Kendall Z-scores
#' @param ts time-series dataframe of year and flow
#'
#' @return triangle plot with start year along the side, end year along the bottom.
#'
#' Each gridsquare shows the Mann-Kendall statistic for that time period.
#' Points which are statistically significant at 95% are highlighted.
#'
#' @export
trianglePlot <- function(MKZ, ts){
  
  Sta.ts <- ts[, c('year', 'flow')]
  colnames(Sta.ts) <- c('Year', 'Q')
  Sta.ts <- zoo::na.trim(Sta.ts)
  Data_St <- reshape2::melt(MKZ, value.name="MKZ")
  colnames(Data_St)[3] <- 'MKZ'
  
  Pos_Values <- as.vector(MKZ[MKZ>0])
  Neg_Values <- as.vector(MKZ[MKZ>-100 & MKZ<0])
  
  Q_Pos <- stats::quantile(Pos_Values, c(1, 0.5, 0.25, 0))
  Q_Neg <- rev(-1*stats::quantile(-1*Neg_Values, c(1, 0.5, 0.25, 0)))
  
  # add a proper limit if no points go beyond +-1.96
  Q_Neg[4] <- ifelse(Q_Neg[4] < -1.96, Q_Neg[4], -2.0)
  Q_Pos[1] <- ifelse(Q_Pos[1] >  1.96, Q_Pos[1],  2.0)
  
  # adds a zero if there are only positive or negative values
  Q_Neg[1] <- ifelse(is.na(Q_Neg[1]), 0, Q_Neg[1])
  Q_Pos[4] <- ifelse(is.na(Q_Pos[4]), 0, Q_Pos[4])
  
  ALLNEG <- (length(Pos_Values)==0)
  ALLPOS <- (length(Neg_Values)==0)
  
  # Define breaks for classes for the values on the CUT function
  MAX_Q2.5 <- suppressWarnings(max(Neg_Values[Neg_Values< -1.96]))
  if(is.infinite(MAX_Q2.5)){MAX_Q2.5 <- -1.96}
  # The -1.96 is replaced with the maximum value that is still lower -1.96
  # othwrwise if there is value exactly -1.96 it will be counted on the
  # Q100--1.96 and not on the -1.96-Q50 class
  Color_Breaks <- c(Q_Neg[4], -1.96, Q_Neg[3:1],
                    Q_Pos[4:2], 1.96, Q_Pos[1])
  Color_Breaks[c(3,4,7,8)] <-
    trunc(Color_Breaks[c(3,4,7,8)]*100)/100 + c(-2e-5, -1e-5, 1e-5, 2e-5)
  Color_Breaks[c(5,6)] <- trunc(Color_Breaks[c(5,6)]*1000)/1000
  Color_Breaks[1] <- floor(Color_Breaks[1]*100)/100
  Color_Breaks[10] <- ceiling(Color_Breaks[10]*100)/100
  Color_Breaks_Lab <- round(Color_Breaks, 2)
  
  Labels <- rep(NA, 10)
  Labels[1] <- 'NA'
  Labels[2] <- '< -1.96'
  Labels[3] <- paste0('[-1.96,', Color_Breaks_Lab[3], ')')
  Labels[4] <- paste0('[', Color_Breaks_Lab[3], ", ", Color_Breaks_Lab[4], ")")
  Labels[5] <- paste0('[', Color_Breaks_Lab[4], ',0)')
  Labels[6] <- '0'
  Labels[7] <- paste0('(0,', Color_Breaks_Lab[7], ']')
  Labels[8] <- paste0('(', Color_Breaks_Lab[7], ", ", Color_Breaks_Lab[8], "]")
  Labels[9] <- paste0('(', Color_Breaks_Lab[8], ", 1.96]")
  Labels[10] <- '> 1.96'
  
  Colors <- c('black', rev(RColorBrewer::brewer.pal(n = 8, "Reds")[c(2,4,6,8)]),
              'white', RColorBrewer::brewer.pal(n = 8, "Blues")[c(2,4,6,8)])
  
  if(ALLNEG){
    # if all negative, remove all positive colors and breaks.
    Color_Breaks <- Color_Breaks[1:6]
    Color_Breaks_Lab <- Color_Breaks_Lab[1:6]
    Colors <- Colors[1:6]
    Labels <- Labels[1:6]
  }else if(ALLPOS){
    # if all positive, remove all negative colors and breaks.
    Color_Breaks <- Color_Breaks[5:10]
    Color_Breaks_Lab <- Color_Breaks_Lab[5:10]
    Colors <- Colors[c(1,6:10)]
    Labels <- Labels[c(1,6:10)]
  }
  
  AllData <- reshape2::melt(MKZ)
  AllData <- AllData[AllData$value == -300 | AllData$value > -100, ]
  AllData$value[AllData$value == -300] <- NA
  AllData$Discrete <- cut(AllData$value, breaks=Color_Breaks,
                          include.lowest = T, dig.lab = 7)
  
  Levels <- levels(AllData$Discrete)
  Levels <- c('NA', Levels) # Add the 'NA' as level
  
  # Remove any (a,a] type intervals from the plot
  if(ALLPOS | ALLNEG){
    if(Color_Breaks_Lab[3] == Color_Breaks_Lab[4]){
      Color_Breaks_Lab <- Color_Breaks_Lab[-3]
      Color_Breaks <- Color_Breaks[-3]
      Colors <- Colors[-4]
      Levels <- Levels[-4]
      Labels <- Labels[-4]
    }
  }else{
    if(Color_Breaks_Lab[7] == Color_Breaks_Lab[8]){
      Color_Breaks_Lab <- Color_Breaks_Lab[-7]
      Color_Breaks <- Color_Breaks[-7]
      Colors <- Colors[-8]
      Levels <- Levels[-8]
      Labels <- Labels[-8]
    }
    if(Color_Breaks_Lab[3] == Color_Breaks_Lab[4]){
      Color_Breaks_Lab <- Color_Breaks_Lab[-3]
      Color_Breaks <- Color_Breaks[-3]
      Colors <- Colors[-4]
      Levels <- Levels[-4]
      Labels <- Labels[-4]
    }
  }
  
  Data_St$Discrete <- cut(Data_St$MKZ, breaks=Color_Breaks,
                          include.lowest = T, dig.lab = 7)
  Data_St$Discrete <- factor(Data_St$Discrete, levels = Levels, ordered = T)
  Data_St$Discrete[is.na(Data_St$Discrete)] <- 'NA'
  
  Data_St$Significance <- 1*(Data_St$MKZ > -100 & abs(Data_St$MKZ) > 1.96)
  Data_St$Significance <- factor(Data_St$Significance)
  # convert to factor for better use at ggplot
  
  min_st  <- range(Data_St$Start[which(Data_St$MKZ > -99)])
  min_end <- range(Data_St$End[which(Data_St$MKZ > -99)])
  
  Data_St <-  Data_St[Data_St$Start >= min_st[1]  &
                        Data_St$Start <= min_st[2]  &
                        Data_St$End   >= min_end[1] &
                        Data_St$End   <= min_end[2] ,]
  
  pb_st <- function(x){pretty(x, min(diff(min_st)+1, 7))}
  pb_end <- function(x){pretty(x, min(diff(min_end)+1, 7))}
  
  Main_Actual <- ggplot2::ggplot(data = Data_St) +
    ggplot2::geom_tile(ggplot2::aes(x = Start, y = End, fill = Discrete)) +
    ggplot2::geom_point(
      data = Data_St[Data_St$Significance == 1,],
      size = 40 / diff(range(Data_St$Start)),
      # size of signif. indicator inversely proportional to number of years
      ggplot2::aes(x = Start, y = End, color = Significance)
    ) +
    ggplot2::labs(x = 'Start year', y = 'End year') +
    ggplot2::scale_fill_manual(
      name = 'MKZ',
      values = Colors,
      limits = Levels,
      labels = Labels
    ) +
    ggplot2::scale_color_manual(values = 'green',
                                limits = 1,
                                labels = 'significant \n(5% level)') +
    ggplot2::guides(
      fill = ggplot2::guide_legend('MKZ', reverse = T, order = 1),
      color = ggplot2::guide_legend(
        title.theme = ggplot2::element_blank(),
        order = 2,
        override.aes = list(size = 3) ) ) +
    ggplot2::theme(
      legend.box.background =
        ggplot2::element_rect(fill = 'grey95', colour = 'grey95'),
      legend.background = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(angle = 90, hjust = 0.5)
    ) +
    ggplot2::coord_fixed(ratio=1)+
    ggplot2::scale_y_continuous(expand = c(0, 0), breaks = pb_end) +
    ggplot2::scale_x_continuous(expand = c(0, 0), breaks = pb_st) +
    ggplot2::theme(legend.text=ggplot2::element_text(size=11))
  Main_Actual
}

#' Compute percentiles of flow within blocks of time
#'
#' This function takes daily flow and computes various
#' percentiles of daily flow, computed per month, year or decade. Currently uses
#' the British hydrological year which starts in October.
#'
#' @param daily dataframe of two columns, date and flow.
#' @param pc vector of percentiles (values between 0 and 1)
#' @param period string: "month", "year", or "decade" describing block of time 
#' to summarise over
#'
#' @return dataframe with timeblock name, date label, one column per percentile, plus block minimum and maximum.
#'
#' @export
periodPercentile <- function (daily, pc=c(0.5, 0.95), period="year") {
  
  exc <- c("MIN", "Q05%", "Q50%", "Q95%", "MAX")
  
  if(nrow(daily) < 2){
    stop("Daily flow data not found.")
  }
  colnames(daily) <- c("date", "flow")
  
  daily$year <- findHydrolYr(daily$date)$yr
  if(period == "month"){
    idv <- c("month", "year", "datelab")
    daily$month <- lubridate::month(daily$date)
    frm <- stats::as.formula(flow ~ month + year + datelab)
    daily$datelab <- as.Date(paste0(daily$year, "-", daily$month, "-01"))
  }else if(period == "decade"){
    idv <- c("decade", "datelab")
    daily$decade <- floor(daily$year/10)*10
    frm <- stats::as.formula(flow ~ decade + datelab)
    daily$datelab <- as.Date(paste0(daily$decade,"-01-01"))
  }else{
    idv <- c("year", "datelab")
    frm <- stats::as.formula(flow ~ year + datelab)
    daily$datelab <- as.Date(paste0(daily$year,"-01-01"))
  }
  
  quant_percentile <- stats::aggregate(frm, data=daily,
                             FUN=function(v){stats::quantile(v, probs=pc)})
  quant_range <- stats::aggregate(frm, data=daily,
                             FUN=range)
  
  dailysumm <- merge(quant_percentile, quant_range, by=idv)
  pnames <- paste0("P", formatC(floor(pc*100),
                                width = 2, format = "d", flag = "0"))
  dailysumm <- setNames(do.call(data.frame,dailysumm), c(idv, pnames, "PMIN", "PMAX"))

  dailysumm
}


MKSneyers <- function(x){
  N <- 300
  x <- rnorm(N,0,0.1) + 5*cos(1:N * pi/200)
  mi_fun <- function(y){
    mi <- sapply(1:N, \(i){sum(y[seq_len(i-1)]<y[i])})
    sk <- cumsum(mi)
    skbar <- mean(sk)#sapply(1:N, \(i){i*(i-1)/4})
    skvar <- var(sk)#sapply(1:N, \(i){i*(i-1)*(2*i-5)/72})
    return( (sk - skbar)/sqrt(skvar) )
  }
  xfwd <- mi_fun(x)
  xbkd <- -1*rev(mi_fun(rev(x)))
  plot(1:N, x, 'l', ylim=range(c(xfwd, xbkd,x), na.rm=T), lty=3)
  lines(1:N, xbkd, col=2)
  lines(1:N, xfwd, col=3)
  abline(h=1.96, col="grey50")
  abline(h=-1.96, col="grey50")
  crsses <- which(((xfwd[-1] > xbkd[-1]) & (xfwd[1:(length(xfwd)-1)] < xbkd[1:(length(xbkd)-1)]))|
                   (xfwd[-1] < xbkd[-1] & xfwd[1:(length(xfwd)-1)] > xbkd[1:(length(xbkd)-1)]))
  abline(v=crsses, col="grey70")
  
      
    
}

ripleysK <- function(x, lambda, r){
  N <- length(x)
  d <- dist(matrix(x, ncol=1))
  return(r/(N*(N-1))*sum(d <= r))
}

MKZs <- function(x) {
  
  S <- MannKendall(x)$S
  varS <- MannKendall(x)$varS
  
  #Calculate the Zs statistic
  if (S > 0) {
    Zs <- (S-1)/sqrt(varS)
  } else if (S < 0) {
    Zs <- (S+1)/sqrt(varS)
  } else {
    Zs <- 0
  }
  
  return(Zs)  # MkZs
  
}

MKZ_blockboot <- function(x, bootl=4){
  b4<-sort(tsboot(x, MKZs, R=10000,  sim="fixed", l=bootl)$t)
  ci <- b4[c(250,9750)]  # B4 lower
  truemk <- MKZs(x)
  sigp <- (truemk > ci[2] | truemk < ci[1])
  attributes(truemk) <- NULL
  return(list(mkz=truemk, mk_ciL=ci[1], mk_ciU=ci[2], sigp=sigp))
}


### DATA###

denclust <- function(dateline, bwi, daysNotSeconds=TRUE, M=500, plotnow=TRUE,
                     plotfilename="./FirstDensityExample.png"){
  
  daystamps <- as.numeric(dateline)
  if(!daysNotSeconds){daystamps <- daystamps/(24*60*60)}
  raw_dates <- dateline
  daterange <- seq(min(raw_dates),max(raw_dates),by="day")
  
  ND <- max(daystamps) - min(daystamps) + 1
  NE <- length(daystamps)
  daystamps_extended <- c(2*min(daystamps) - rev(daystamps[-1]),
                          daystamps,
                          2*max(daystamps) - rev(daystamps)[-1])
  kd_extended <- rep(0,ND)
  
  # Kernel rate density estimate [Merz et al 2016]
  for(t in 1:ND){
    kd_extended[t] <- 1/bwi*sum(dnorm((t - daystamps_extended)/bwi, 0, 1))
  }
  #### MONTE CARLO CONFIDENCE INTERVAL ####
  sq2pi <- sqrt(2*pi)
  f <- \(x){exp(-x^2/2)/sq2pi}
  kdmat <- matrix(0, nrow=M, ncol=ND)
  for(m in 1:M){
    if(m < 10 | m %% 50 == 0){message(paste0(100*m/M, "% MC complete."))}
    simm <- cumsum(rexp(NE, NE/ND))
    for(t in 1:ND){
      kdmat[m,t] <- 1/bwi*sum(f((t - simm)/bwi))
    }
  }
  # Monte Carlo 90% confidence intervals
  kdav <- apply(
    apply(kdmat[,floor(ND/2 - bwi):ceiling(ND/2 + bwi)], 2, 
          FUN=\(x){quantile(x,probs=c(0.05,0.95))}),
    1, mean)
  
  s1 <- sum(rle(kd_extended > kdav[2])$values)
  s2 <- sum(rle(kd_extended < kdav[1])$values)
  print(paste0(s1, " significant clusters and ", s2, " significant absences."))
  
  ### PLOT ###
  if(plotnow){
    g <- \(){
      par(mar=c(3,3,1,1), mgp=c(2,1,0))
      plot(daterange, 365*kd_extended, type='l',
           xlab=paste("Date, bw =",bwi), ylab="Rate estimate",
           ylim=c(0,1.2*max(365*kd_extended)))
      abline(h=365*NE/ND, col="red", lty=2)
      rug(raw_dates)
      abline(h=365*kdav, col="blue", lty=3)
    }
    g()
    png(plotfilename,
        width=100, height=90, units="mm", res=300, pointsize=10)
    g()
    dev.off()
  }
  return(list(dt=cbind(daterange, kd_extended),
              kdav=kdav)
  )
}
