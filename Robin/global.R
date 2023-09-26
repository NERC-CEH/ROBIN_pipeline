library(shiny)
library(tidyverse)
library(dplyr)
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

pers <- c(5, 70, 90, 95)

plot_list <- c("Peak flow"="peakflow",
               "Quantiles"="qxx",
               "High flow"="highflow",
               "Low flow"="lowflow",
               "Droughts"="droughts")

timestep_list <- c("Daily"="daily",
                   "Monthly"="month",
                   "Quarterly"="quart",
                   "Yearly"="yearly")

read_in_robin <- function(csv, partial=TRUE, ...){
  if(partial){
    nameVec <- c("Day", "Flow", "Flag")
  }else{
    nameVec <- c("Agency", "Station", "Day", "Flow", "Flag")
  }
  z <- setNames(read.csv(csv, col.names=nameVec, fill=T, ...), nameVec)
  z$Day <- lubridate::ymd_hms(z$Day, truncated=3)
  z$id <- seq(1, nrow(z))
  z$Flag[is.na(z$Flag)] <- ""
  #z$Flag <- factor(z$Flag)
  z
}

amax_prep <- function(data_in, min_days_per_year=300){
  data_in$year <- year(data_in$DAY)
  amax_raw <- data_in %>%
    group_by(year) %>%
    summarise(count=n(), amax=max(FLOW, na.rm=T), doy=which.max(FLOW),
              date=ymd(paste0(year,"-01-01"))) %>% 
    dplyr::filter(count > min_days_per_year)
  amax_raw
}

lfstat_prep <- function(data_raw, year_start_month=1){
  colnames(data_raw) <- c("Date","Flow")
  data_raw$day <- day(ymd(data_raw$Date))
  data_raw$month <- month(ymd(data_raw$Date))
  data_raw$year <- year(ymd(data_raw$Date))
  data <- data_raw[,c("day", "month", "year", "Flow")]
  colnames(data) <- c("day", "month", "year", "flow")
  data <- createlfobj(data, hyearstart=year_start_month)
  flowunit(data) <- "m^3/sec"
  data$date <- ymd(paste(data$year,data$month,data$day,sep="-"))
  data
}

pot_prep <- function(lf_flow){
  vmt_pot <- lfstat::vary_threshold(lf_flow, varying="monthly",
                fun=function(x){quantile(x, probs=c(0.986), na.rm=T)})
  lf_flow$threshold <- as.vector(vmt_pot)
  
  vmt_summary <- lf_flow %>% 
    group_by(month) %>% 
    summarise(threshold=mean(threshold, na.rm=T))
  
  peaks <- ilaprosUtils::extractPeaks(vecObs=lf_flow$flow, mintimeDiff=7)
  pot_raw <- lf_flow[((lf_flow$flow > lf_flow$threshold) & (peaks==1)), ]
  pot_raw
}

lf_seas_fun <- function(lf_flow, qxx=0.9, period="yearly"){
  if(period=="yearly"){
    z <- apply.yearly(lf_xts,FUN=\(x){quantile(x,qxx,na.rm=T)})
    names2 <- "Year"
    lfq <- as.data.frame(z) %>%
      mutate(date=ymd(rownames(.)), year=year(date), period=NA, Qxx=qxx)
  }else{
    if(period=="monthly"){
      z <- apply.monthly(lf_xts, FUN=\(x){quantile(x,qxx,na.rm=T)})
      names2 <- "Month"
      f <- lubridate::month
      lfq <- z %>%
        as.data.frame() %>%
        mutate(date=ymd(rownames(.)), year=year(date), period=f(date), Qxx=qxx)
    }
    if(period=="quart"){
     z <- apply.quarterly(lf_xts, FUN=\(x){quantile(x,qxx,na.rm=T)})
     names2 <- "Season"
     f <- \(x){paste0("Qu",lubridate::quarter(x))}
     lfq <- z %>%
       as.data.frame() %>%
       mutate(date=ymd(rownames(.)), year=year(date), period=f(date), Qxx=qxx)
    }

  }
  colnames(lfq) <- c("flow","date","year", "period","Qxx")
  lfq
}

quantile_prep <- function(lf_flow, period="yearly"){
  lf_xts <- as.xts(lf_flow)
  lf_seasonal <- list()
  lf_seasonal$Q95 <- lf_seas_fun(lf_xts, 0.95, period)
  lf_seasonal$Q90 <- lf_seas_fun(lf_xts, 0.90, period)
  lf_seasonal$Q70 <- lf_seas_fun(lf_xts, 0.70, period)
  lf_seasonal$Q05 <- lf_seas_fun(lf_xts, 0.05, period)
  
  lf_seasonal <- as.data.frame(do.call(rbind,lf_seasonal))
  if(period %in% c("quart", "monthly")){
  lf_seasonal <- lf_seasonal%>%
    pivot_wider(id_cols=c(year, period), names_from=Qxx, names_prefix="Q",
                values_from="flow")
  }
  lf_seasonal
}

channel <- RODBC::odbcConnect(dsn = "wla", uid = "swa2", pwd = "swa2",
                              believeNRows=FALSE)
meta_central <- sqlQuery(channel, "SELECT * from STETUR.ROBIN_METADATA",
                         believeNRows = FALSE)
country_list <- unique(meta_central$COUNTRY)
station_list0 <- unique(meta_central$NAME)
stationListRaw <- meta_central %>%
  dplyr::select(COUNTRY, NAME, LOCAL_NUMBER)
colnames(stationListRaw) <- c("country", "station", "localNumber")



