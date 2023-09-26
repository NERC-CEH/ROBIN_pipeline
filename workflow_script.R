#### ROBIN workflow script
# V0 2022-10-01 Adam Griffin
# V1 2023-03-15 Adam Griffin
# V2 2023-05-11 Adam Griffin

# This script runs the full pipeline needed to get data for the ROBIN dataset.
# It doesn't plot any of the figures, or print anything besides progress indicators.


#### SETUP ####
# Install all the needed packages for this analysis
packagesNeeded <- c("lfstat", "lmomco", "lubridate", "tidyverse", "circular",
                    "trend", "strucchange", "changepoint", "Kendall", "zyp",
                    "modifiedmk", "boot", "RODBC")
for(p in packagesNeeded){
  if(!(p %in% installed.packages()[,1])){
    install.packages(p)
  }
  library(p, character.only = T)
}
source("main_functions.R")

#### READ IN DATA ####
date_format <- "date" # state "date" or "datetime"

###                       ###
## KEY ARGUMENTS TO CHANGE ##
###                       ###
Agency <- "WSC"
Station <- "CA00191"
MIN_DAYS_PER_YEAR <- 300
data_in <- read_in_robin_db(Station, "swa2", "swa2")
data_in$Agency <- Agency
data_in$Station <- Station


#### LFSTAT SETUP ####
check_for_level(data_in, "M")

#data_in$id <- seq(1, nrow(data_in)) # does not account for missing rows in the data
data_in$Flag <- factor(data_in$Flag)

# Make the lfstat object
lf_flow_raw <- data_in
colnames(lf_flow_raw)[2] <- "Date"
lf_flow_raw$day <- lubridate::day(lf_flow_raw$Date)
lf_flow_raw$month <- lubridate::month(lf_flow_raw$Date)
lf_flow_raw$year <- lubridate::year(lf_flow_raw$Date)
lf_flow <- lf_flow_raw[,c("day", "month", "year", "Flow")]
colnames(lf_flow) <- c("day", "month", "year", "flow")
year_start_month <- 1
lf_flow <- lfstat::createlfobj(lf_flow, hyearstart=year_start_month)
flowunit(lf_flow) <- "m^3/sec"
lf_flow2 <- as.data.frame(lf_flow) %>%
  mutate(date=lubridate::ymd(paste0(year,"-",month,"-",day)))
lf_flow$date <- lf_flow2$date

#### AMAX STATS ####
print("AMAX stats")
amax_raw <- as.data.frame(lf_flow) %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(count=n(), amax=max(flow, na.rm=T),
            doy=which.max(flow), date=date[doy]) %>% 
  dplyr::filter(count > MIN_DAYS_PER_YEAR)

amax_out <- amax_raw[,c(1,3,5,4)]
colnames(amax_out) <- c("YEAR", "AMAX", "DATE", "DOY")
readr::write_csv(amax_out, paste0("./Data/AM_raw_", Station, "_", Agency, ".csv"))

#### POT STATS ####
print("POT stats")
# Varying threshold method for POT extraction
vmt_pot <- lfstat::vary_threshold(
  lf_flow, varying="monthly",
  fun=function(x){quantile(x, probs=c(0.992), na.rm=T)}
  )

lf_flow$threshold <- as.vector(vmt_pot)

vmt_summary <- lf_flow %>% 
  dplyr::group_by(month) %>% 
  dplyr::summarise(threshold=mean(threshold))

lf_flowPOT <- lf_flow
lf_flowPOT$flow[is.na(lf_flowPOT$flow)] <- -9999
peaks <- ilaprosUtils::extractPeaks(vecObs=lf_flowPOT$flow, mintimeDiff=7)
pot_raw <- lf_flow[((lf_flow$flow > lf_flow$threshold) & (peaks==1)), ]

readr::write_csv(pot_raw, paste0("./Data/POT_raw_", Station , "_", Agency, ".csv"))

#### OTHER PERCENTILES ####
print("Other Percentiles")
lf_xts <- xts::as.xts(lf_flow)

percentiles <- list(max=1, Q95=0.05, Q90=0.10, Q70=0.30, Med=0.5, Q05=0.95, min=0)
lf_year <- lapply(percentiles, \(x){lf_year_fun(lf_xts, x)})
lf_year_long <- as.data.frame(do.call(rbind,lf_year))
lf_year <- lf_year_long %>% 
  tidyr::pivot_wider(
    id_cols=c(year),
    names_from=Qxx,
    names_prefix="Q",
    values_from="discharge")
colnames(lf_year) <- 
  c("Year", "MAX", "Q95", "Q90", "Q70", "Median", "Q05", "MIN")
readr::write_csv(lf_year, paste0("./Data/yearlyPercentiles_", 
                          Station, "_", Agency, ".csv"))
lf_year_long$Qxx <- factor(lf_year_long$Qxx, levels=c(0,0.05,0.5,0.7,0.9,0.95,1),
                           labels=c("MAX","Q05", "Med", "Q70", "Q90", "Q95", "MIN"))

# Seasonal summaries
lf_seasonal <- lapply(percentiles, \(x){quarterly_summary(lf_xts, x)})
lf_seasonal_long <- as.data.frame(do.call(rbind,lf_seasonal))
lf_seasonal <- lf_seasonal_long %>% 
  tidyr::pivot_wider(
    id_cols=c(year, Season),
    names_from=Qxx,
    names_prefix="Q",
    values_from="Flow")
colnames(lf_seasonal) <- 
  c("Year", "Season", "MAX", "Q95", "Q90", "Q70", "Median", "Q05", "MIN")
readr::write_csv(lf_seasonal, paste0("./Data/seasonalPercentiles_", 
                              Station, "_", Agency, ".csv"))

# Monthly summaries
lf_monthly <- lapply(percentiles, \(x){monthly_summary(lf_xts, x)})
lf_monthly_long <- as.data.frame(do.call(rbind,lf_monthly))
lf_monthly <- lf_monthly_long %>% 
  tidyr::pivot_wider(
    id_cols=c(year, month),
    names_from=Qxx,
    names_prefix="Q",
    values_from="discharge")
colnames(lf_monthly) <- 
  c("Year", "Month", "MAX", "Q95", "Q90", "Q70", "Median", "Q05", "MIN")
readr::write_csv(lf_monthly, paste0("./Data/monthlyPercentiles_", 
                             Station, "_", Agency, ".csv"))
monthly_mean <- lf_flow %>%
  dplyr::group_by(month, year) %>%
  dplyr::summarise(minflow = min(flow, na.rm=T),
                   maxflow = max(flow, na.rm=T),
                   meanflow = mean(flow, na.rm=T),
                   .groups="keep")





### MEAN FLOWS###
print("MAM flow")
mean_annual_min_7 <- lfstat::MAM(lf_flow, 7, yearly=TRUE)
mean_annual_min_30 <- lfstat::MAM(lf_flow, 30, yearly=TRUE)
mean_flow <- lfstat::meanflow(lf_flow, yearly=TRUE)

mean_lowflow <- data.frame(Year=min(lf_flow$year):max(lf_flow$year),
                           MAM7=mean_annual_min_7$MAn,
                           MAM30=mean_annual_min_30$MAn,
                           MAF=mean_flow$flow)

readr::write_csv(mean_lowflow,
          paste0("./Data/mean_lowflow_", Station, "_", Agency, ".csv"))


#### DROUGHT STATS ####
print("Drought stats")
vmt_drought <- lfstat::vary_threshold(lf_flow,
                      varying="monthly",
                      fun=\(x){quantile(x, probs=c(0.2), na.rm=T)})
droughts <- lfstat::find_droughts(lf_flow, vmt_drought, varying="monthly")
readr::write_csv(summary(droughts),
          file=paste0("./Data/droughts_", Station, "_", Agency, ".csv"))

summdrought <- summary(droughts)
drght_per_yr <- findHydrolYr(summdrought$time, starttime="2000-10-01") %>%
  dplyr::group_by(yr) %>% 
  dplyr::summarise(N=n())
lf_year <- dplyr::left_join(lf_year, drght_per_yr, by=c("Year"="yr"))

write_csv(lf_year, 
          file=paste0("./Data/yearlystats_", Station,"_", Agency, ".csv"))

#### TREND TESTS ####
print("Trend tests")
colnames(mean_annual_min_30) <- c("year", "MAn_30")
colnames(mean_annual_min_7) <- c("year", "MAn_7")
lf_year_temp <- lf_year
colnames(lf_year_temp)[1] <- "year"
lf_data <- plyr::join_all(
      dfs=list(lf_year_temp,
               amax_raw[,c(1,3)],
               mean_annual_min_30,
               mean_annual_min_7),
      type='full', by="year")


pttest0 <- apply(lf_data[,-1], 2, pettitt_tc)
pttest_table <- as.data.frame(t(sapply(pttest0, \(x) {
  c(lf_year$Year[floor(x$estimate)[1]], x$p.value)
})))
colnames(pttest_table) <- c("Possible_breakpoint_1", "pvalue")

chowtest <- lapply(colnames(lf_data)[-1],
                   \(x) {
                     sctest(as.formula(paste0(x, "~year")),
                            data = lf_data, type = "Chow")
                   })
chowtest_table <- as.data.frame(t(sapply(chowtest, \(x) {
  c("Fscore" = x$statistic,
    "p-value" = x$p.value)
})))
colnames(chowtest_table) <- c("F_score", "p_value")

meanvar_table <- sapply(lf_data[,-1], meanvar_tc)

# summary table of breakpoint tests
changepoint_table <- data.frame(
  "Pettitt bp 1"= pttest_table$Possible_breakpoint_1,
  "Pettitt p"= pttest_table$pvalue,
  "Chow test F" = chowtest_table$F_score,
  "Chow test p" = chowtest_table$p_value,
  "CPT max bp" = meanvar_table
)

#### AUTOCORRELATION PLOTS
print("Autocorrelation")
daily_autocor <- acf(lf_flow$flow, ci.type="ma", plot=F, na.action=na.pass)
amax_autocor <- acf(amax_raw$amax, ci.type="ma", plot=F, na.action=na.pass)
pot_autocor <- acf(pot_raw$flow, ci.type="ma", plot=F)

low_ac_quarterly <- lf_seasonal_long %>%
  dplyr::filter(Qxx == 0.95, !is.na(Flow)) %>%
  dplyr::select(Flow) %>%
  acf(ci.type = "ma", plot = F)
low_ac_q1 <- lf_seasonal_long %>%
  dplyr::filter(Qxx == 0.95, Season == "Qu1", !is.na(Flow)) %>%
  dplyr::select(Flow) %>%
  acf(ci.type = "ma", plot = F)
low_ac_q2 <-  lf_seasonal_long %>%
  dplyr::filter(Qxx == 0.95, Season == "Qu2", !is.na(Flow)) %>%
  dplyr::select(Flow) %>%
  acf(ci.type = "ma", plot = F)



#### MANN-KENDALL TREND TESTS ####
print("Mann Kendall tests")
mk_basic <- Kendall::MannKendall(lf_flow$flow)
meanflow_ts <- ts(monthly_mean$meanflow, frequency=12, start=c(1970,4))
mk_seasonal <- Kendall::SeasonalMannKendall(meanflow_ts)
mk_amax_sliding <- zoo::rollapply(amax_raw$amax, FUN=Kendall::MannKendall, width=30)

mk_yearly <- lapply(lf_data[,-1], MKZ_blockboot)
mk_yearly <- as.data.frame(do.call(rbind, mk_yearly))

mk_yearly$pwmkZ <- NA
mk_yearly$pwmkP <- NA
mk_yearly$pwmkTSE <- NA

for(i in 1:ncol(lf_data[,-1])){
  z <- pwmk(lf_data[,i])
  mk_yearly$pwmkZ[i] <- z[1]
  mk_yearly$pwmkP[i] <- z[4]
  mk_yearly$pwmkTSE[i] <- z[2]
}

colnames(mk_yearly) <- c("MKZ", "MKZ 2.5%ile", "MKZ 97.5%ile",
                         "MKZ significant", "prewhitened MKZ", 
                         "prewhitened MKZ pvalue", "prewhitened TSE")

readr::write_csv(as.data.frame(cbind(mk_yearly, changepoint_table)),
          paste0("./Data/MK_changepoint_tests_", Station, "_", Agency, ".csv"))



#### BOOTSTRAPPED TREND SIGNIFICANCE ####
lf_data <- plyr::join_all(
  dfs=list(lf_year_temp, amax_raw[,c(1,3)],
           mean_annual_min_30, mean_annual_min_7),
  type='full', by="year")


MKp <- t(apply(lf_data[,-1], 2 , \(x)unlist(Kendall::MannKendall(x))[1:2]))
TSEp <- t(sapply(colnames(lf_data)[-1],
                 \(x){ 
                   zyp::zyp.sen(as.formula(paste0(x,"~year")),
                                data=lf_data)$coefficients
                   }))

# summary table of p-values and TSE estimates
trend_table <- data.frame(
  quantile = rownames(MKp),
  MKZ = MKp[,1],
  MK_pvalue = MKp[,2],
  TSE = TSEp[,2],
  TSE_intercept = TSEp[,1]
)



#### KERNEL CLUSTERING ####
print("Kernal Clustering")

DC <- denclust(summdrought$time, bwi=365*4,
               plotfilename=paste0(
                 "./droughtclustering_", Station,"_", Agency, ".png")
        )

pot_per_year <- pot_raw %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(N=n())

DC_pot <- denclust(pot_raw$date, bwi=365*2,
                   plotfilename=paste0(
                     "./droughtclustering_", Station,"_", Agency, ".png")
                   )

#### CIRCULAR STATISTICS ####
print("Circular stats")

event_timing <- function(times){
  yday <- ((lubridate::yday(times)+180) %% 365) - 180
  circular_stats <- circular::circular(lubridate::yday(times)*2*pi/365)
  mean_DOY <- zoo::rollapply(circular_stats, 
               FUN=\(x){suppressWarnings(circular::mean.circular(x))}, width=50)
  trend_DOY <- zoo::rollmean(yday, 50)
  
  return(list(mean_DOY=mean_DOY, trend_DOY=trend_DOY))
}

POT_circular <- event_timing(pot_raw$date)

AM_circular <- event_timing(amax_raw$date)

drought_circular <- event_timing(summdrought$time)


