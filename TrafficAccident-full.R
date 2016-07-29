
# Seattle traffic accident model
# Stephen Snyder <derauma@gmail.com>

library(dplyr)
library(stringr)
library(sets)
library(logging)
library(Holidays)
library(reshape2)
library(zoo)
library(glmnet)

work_dir <- "~/Downloads/TrafficAccidents/"
source(paste0(work_dir,'weather_retrieval.R'))

# Globals


setwd(work_dir)

# 3 ~ 360 feet
# 4 ~ 36 feet
latlong_significant_digits <- 3

# add on a default time of 12:00 AM if time is missing from string
standardizeTime <- function(timestamp_string) {

  if (grepl("^\\d+/\\d+/\\d{4}$",timestamp_string,perl=T))
    timestamp_string <- paste(timestamp_string,'12:00:00 AM')
  timestamp_string
}


parseTimestamp <- function(timestamp_string) {

  # improve: take off seconds
  #timestamp_string <- '12/31/2010 11:59:59 PM'
  as.POSIXct(timestamp_string,format="%m/%d/%Y %I:%M:%S %p")
}

analysis_start_date <- parseTimestamp('12/31/2011 11:59:59 PM')
analysis_end_date <- parseTimestamp('01/01/2016 12:00:00 AM')


toDate <- function(POSIXct_timestamp) {
   #as.POSIXct(round(POSIXct_timestamp, units="days"))
  as.Date(POSIXct_timestamp,tz=attributes(POSIXct_timestamp)$tzone)
}


floorToHour <- function(POSIXct_timestamp) {
  
  #POSIXct_timestamp <- parseTimestamp("12/31/2010 11:59:50 PM")
  d1 <- toDate(POSIXct_timestamp)
  h1 <- strftime(POSIXct_timestamp,format="%H")
  as.POSIXct(paste0(d1,' ',h1,':00:00'),tz=attributes(POSIXct_timestamp)$tzone)
}

# return a character tuple of (latitude,longitude) from a string such as (47.51432466800003, -122.37618650399997)
parseLatLong <- function(lat_long_string) {
  
  #lat_long_string <- '(47.63077, -122.29725)'
  latlong_pat <- paste0('(-?\\d+\\.(\\d{1,',latlong_significant_digits,'}))\\d*')  # truncate to 5 significant digits
  lati_match <- str_match(lat_long_string,paste0(latlong_pat,','))
  longi_match <- str_match(lat_long_string,paste0(', ',latlong_pat,'\\)'))
  tuple(lati=lati_match[,2],
       lati_granularity=nchar(lati_match[,3]),
       longi=longi_match[,2],
       longi_granularity=nchar(longi_match[,3])
  )
}

# filter out items that don't have the requires granularity of lat/long
# for example, if lat=47.3 and granularity=2, the item is filtered out

filterByLatLongGranularity <- function(df,df_name,granularity=latlong_significant_digits) {

  # df=data.permit
  skip_row <- as.vector(which(df$lati_granularity != latlong_significant_digits |
                      df$longi_granularity != latlong_significant_digits))
  
  if ( length(skip_row) ) {
    logwarn("Skipping %i %s with less than %i significant digits in location lat or long",
            length(skip_row),df_name,latlong_significant_digits
    )
    if (getLogger()$level <= loglevels['DEBUG']) {
      df[skip_row,c('lati','longi')]
    }
    df[-skip_row,]
  }
  else df
}

# Deprecated - use getCollisions() instead
# 911 incident response data for accidents, via data.seattle.gov
get911Accidents <- function() {

# this file is based on dataset Seattle Police Depart 911 Incident Response
# filtered on Event Clearance SubGroup = TRAFFIC RELATED

  data.911.filename <- "Seattle_Police_Department_911_Incident_Response.csv"
  data.911 <- read.csv(paste0(work_dir,data.911.filename))
  
  # prep 911 data
  
  # skip records with no timestamp (very few)
  data.911 <- data.911[data.911$Event.Clearance.Date!='',]
  data.911$timestamp <- parseTimestamp(data.911$Event.Clearance.Date)
  
  # filter source data
  data.911 <- data.911[data.911$timestamp > analysis_start_date &
                         data.911$timestamp < analysis_end_date,]

  data.911 <- data.911[data.911$Event.Clearance.Description=='MOTOR VEHICLE COLLISION' |
                         data.911$Event.Clearance.Description=='ACCIDENT INVESTIGATION',]

  # parse out incident lat/long and ignore Latitude / Longitude fields as they are different (caller location??)

  latlong <- parseLatLong(data.911$Incident.Location)
  data.911$lati <- latlong[[1]]
  data.911$lati_granularity <- latlong[[2]]
  data.911$longi <- latlong[[3]]
  data.911$longi_granularity <- latlong[[4]]

  data.911 <- filterByLatLongGranularity(data.911,'accidents')

  data.frame(accident_id=data.911$CAD.CDW.ID,
             acc_date=as.Date(data.911$timestamp),
             timestamp=data.911$timestamp,
             lati=data.911$lati,
             longi=data.911$longi,
             stringsAsFactors = FALSE)
}

# Seattle Street Collisions data from WA SDOT, via data.seattle.gov

getCollisions <- function() {

  coll.filename <- "Seattle_Collision_Data__SDOT_.csv"
  coll <- read.csv(paste0(work_dir,coll.filename),stringsAsFactors = F)
  
  coll$timestamp <- as.vector(sapply(coll$INCDTTM,standardizeTime))
  coll$timestamp <- parseTimestamp(coll$timestamp)

  # filter source data
  coll <- coll[coll$timestamp > analysis_start_date &
                         coll$timestamp < analysis_end_date,]

  coll$datehour <- floorToHour(coll$timestamp)

  latlong <- parseLatLong(coll$Shape)
  coll$lati <- latlong[[1]]
  coll$lati_granularity <- latlong[[2]]
  coll$longi <- latlong[[3]]
  coll$longi_granularity <- latlong[[4]]

  coll <- filterByLatLongGranularity(coll,'accidents')
  
  coll_frame <- data.frame(accident_id=coll$OBJECTID,
             Date=toDate(coll$datehour),
             Hour=as.numeric(strftime(coll$datehour,format="%H")),
             timestamp=coll$timestamp,
             #loc=coll$Shape,
             lati=coll$lati,
             longi=coll$longi,
             stringsAsFactors = FALSE)
  coll_frame[order(coll_frame$timestamp),]
}

# Street Use permit data
# https://data.seattle.gov/Transportation/SDOT-Street-Use-Permit-by-Address-includes-Impacts/czy3-hkh9

getPermitsAffectingRightOfWay <- function() {

  data.permit.filename <- "SDOT_-_Street_Use_Permit_-_by_Address_includes_Impacts_to_the_Right-of-Way.csv"
  data.permit <- read.csv(paste0(work_dir,data.permit.filename))

  # require a lane closure during the analysis period
  data.permit <- data.permit[data.permit$LANE_CLOSE_START_DATE!='' &
                               data.permit$LANE_CLOSE_END_DATE!='',]
  data.permit$start_timestamp <- parseTimestamp(data.permit$LANE_CLOSE_START_DATE)
  data.permit$end_timestamp <- parseTimestamp(data.permit$LANE_CLOSE_END_DATE)
  
  # filter out those with illogical dates
  data.permit <- data.permit[data.permit$start_timestamp < data.permit$end_timestamp,]
  data.permit <- data.permit[data.permit$start_timestamp > parseTimestamp('1/1/2000 12:00:00 AM'),]
  data.permit <- data.permit[data.permit$end_timestamp < parseTimestamp('1/1/2020 12:00:00 AM'),]
  
  # either the lane closure start or end dates must be in the analysis period
  ## I'm not trusting the 6 permits that show lane closure throughout the analysis period, as it looks like a date entry error
  data.permit <- data.permit[
      (data.permit$start_timestamp >= analysis_start_date & data.permit$start_timestamp < analysis_end_date)
                                          |
      (data.permit$end_timestamp > analysis_start_date & data.permit$end_timestamp <= analysis_end_date)        
        ,]

  # for lane closures that start or end outside of the analysis period, clip them
  #data.permit$start_timestamp <- max(data.permit$start_timestamp,analysis_start_date)
  #data.permit$end_timestamp <- min(data.permit$end_timestamp,analysis_end_date)
  
  # filter out if did not move to Issued or Complete state (still in Applied state)
  data.permit <- data.permit[data.permit$PERMIT_STATUS %in% c('Issued','Complete'),]
  
  # data problem: many permits with LANE_CLOSE START/END DATEs have LANE_CLOSED_FLAG=N
  # but with PARKING_LANE_CLOSED_FLAG=Y and no PARKING_LANE_CLOSE START/END DATEs

  latlong <- parseLatLong(data.permit$Shape)

  data.permit$lati <- latlong[[1]]
  data.permit$lati_granularity <- latlong[[2]]
  data.permit$longi <- latlong[[3]]
  data.permit$longi_granularity <- latlong[[4]]
  
  data.permit <- filterByLatLongGranularity(data.permit,'permits')

  data.permit2 <-
        select(data.permit,lati,longi,start_timestamp,end_timestamp,OBJECTID)
  data.permit2 <-
      summarize(group_by(data.permit2,lati,longi,start_timestamp,end_timestamp),
                OBJECTID=min(OBJECTID),
                duration=first(end_timestamp)-first(start_timestamp),
                workzone_cnt=n())

  data.frame(permit_obj_id=data.permit2$OBJECTID,
             started=data.permit2$start_timestamp,
             startedDate=as.Date(data.permit2$start_timestamp,tz=attributes(data.permit2$start_timestamp)$tzone),
             ended=data.permit2$end_timestamp,
             endedDate=as.Date(data.permit2$end_timestamp,tz=attributes(data.permit2$end_timestamp)$tzone),

             duration=as.numeric(data.permit2$duration),
             lati=data.permit2$lati,
             longi=data.permit2$longi,
             workzone_cnt=data.permit2$workzone_cnt,
             stringsAsFactors = FALSE)
}

# find the id of the first workzone that matches an accident lat/long and timestamp, else NA

getWorkZoneForAccident <- function(lati,longi,timestamp) {
  
  #lati=47.49
  #longi=-122.26
  #timestamp=parseTimestamp('9/12/2014 1:00:00 PM')
  wz_i <- which(workzone$lati==lati &
                  workzone$longi==longi &
                  workzone$started <= timestamp &
                  workzone$ended > timestamp)
  if (length(wz_i))
    workzone$permit_obj_id[wz_i[1]]
  else NA
}

countWorkzones <- function(date) {
  
  wz <- workzone[workzone$startedDate<= date & workzone$endedDate > date,]
  sum(wz$workzone_cnt)
}

# return the count of accidents that occurred in the workzone

countAccidentsForWorkzone <- function(lati,longi,started,ended) {
  
  #lati='47.63'
  #longi='-122.37'
  #started=parseTimestamp('11/10/2015 08:00:00 AM')
  #ended=parseTimestamp('12/31/2015 08:00:00 AM')
  nrow(accident[accident$lati==lati &
                  accident$longi==longi &
                  accident$timestamp >= started &
                  accident$timestamp < ended,])
}

countAccidentsBeforeWorkzone <- function(lati,longi,started,ended) {
  
  #lati='47.523'
  #longi='-122.264'
  #started=parseTimestamp('11/10/2015 08:00:00 AM')
  #ended=parseTimestamp('12/31/2015 08:00:00 AM')
  
  dur <- ended-started
  started1 <- started - 2*dur
  ended1 <- ended - 2*dur
  workzone_cnt <- 1

  while ( workzone_cnt & started1 >= analysis_start_date ) {

    workzones_before_this <- workzone[workzone$lati==lati &
                               workzone$longi==longi &
                               workzone$started >= started1 &
                               workzone$ended < ended1,]
    
    workzone_cnt <- nrow(workzones_before_this)
    
    if ( workzone_cnt ) {
      started1 <- started1- dur
      ended1 <- ended - dur
    }
  }
  
  if ( workzone_cnt ) return(NA)  # could not find a duration without earlier workzone

  nrow(accident[accident$lati==lati &
                  accident$longi==longi &
                  accident$timestamp >= started1 &
                  accident$timestamp < ended1,])
}

# adapted Weather Underground retrieval script from Nick McClure <nickmc@uw.edu>
getWeather <- function() {

      # KBFI = Boeing Field
  airport_code <- 'KBFI'
  weather_file_name <- paste0(airport_code,'_hourly_weather.csv')

    # use cached data if present
  if (weather_file_name %in% list.files()){
    weather_data = read.csv(weather_file_name, stringsAsFactors = FALSE)

  } else {
      # retrieve from weather underground
    dates = seq(from=as.Date(analysis_start_date),to=as.Date(analysis_end_date),by=1)
    weather_data <- get_weather_data(airport_code, dates)
    weather_data$X <- NULL
  }

  weather_data$dateUTC <- as.POSIXct(weather_data$dateUTC,tz='UTC')
  weather_data$timestamp <- weather_data$dateUTC
  attributes(weather_data$timestamp)$tzone <- 'America/Los_Angeles'
  weather_data$dateUTC <- NULL
  weather_data$time <- NULL

  weather_data <- weather_data[weather_data$timestamp >= analysis_start_date &
                                 weather_data$timestamp < analysis_end_date,]

  weather_data$datehour <- floorToHour(weather_data$timestamp)
  weather_data$Date=toDate(weather_data$datehour)
  weather_data$Hour=as.numeric(strftime(weather_data$datehour,format="%H"))

  weather_data = weather_data[!duplicated(weather_data[c("Date", 'Hour')]),]

  # impute missing values
  weather_data$events[weather_data$events=='']='None'
  
  WU_MISSING_VALUE = -9999  # Weather Underground missing value indicator
  weather_data$temp[weather_data$temp==WU_MISSING_VALUE] <- NA
  weather_data$temp <- na.approx(weather_data$temp)
  weather_data$dew_pt[weather_data$dew_pt==WU_MISSING_VALUE] <- NA
  weather_data$dew_pt <- na.approx(weather_data$dew_pt)
  weather_data$humidity[weather_data$humidity==WU_MISSING_VALUE] <- NA
    # humidity==0 is likely from imputation of '--' -> 0
  weather_data$humidity[weather_data$humidity==0] <- NA
  weather_data$humidity <- na.approx(weather_data$humidity)
  weather_data$pressure[weather_data$pressure==WU_MISSING_VALUE] <- NA
  weather_data$pressure <- na.approx(weather_data$pressure)
  weather_data$visibility[weather_data$visibility==WU_MISSING_VALUE] <- NA
  weather_data$visibility <- na.approx(weather_data$visibility)

  weather_data[order(weather_data$timestamp),]
}

# get sunrise and sunset times
# from http://www.r-bloggers.com/approximate-sunrise-and-sunset-times/

suncalc<-function(d,Lat,Long){
  ## d is the day of year
  ## Lat is latitude in decimal degrees
  ## Long is longitude in decimal degrees (negative == West)
  
  ##This method is copied from:
  ##Teets, D.A. 2003. Predicting sunrise and sunset times.
  ##  The College Mathematics Journal 34(4):317-321.
  
  ## At the default location the estimates of sunrise and sunset are within
  ## seven minutes of the correct times (http://aa.usno.navy.mil/data/docs/RS_OneYear.php)
  ## with a mean of 2.4 minutes error.
  
  ## Function to convert degrees to radians
  rad<-function(x)pi*x/180
  
  ##Radius of the earth (km)
  R=6378
  
  ##Radians between the xy-plane and the ecliptic plane
  epsilon=rad(23.45)
  
  ##Convert observer's latitude to radians
  L=rad(Lat)
  
  ## Calculate offset of sunrise based on longitude (min)
  ## If Long is negative, then the mod represents degrees West of
  ## a standard time meridian, so timing of sunrise and sunset should
  ## be made later.
  timezone = -4*(abs(Long)%%15)*sign(Long)
  
  ## The earth's mean distance from the sun (km)
  r = 149598000
  
  theta = 2*pi/365.25*(d-80)
  
  z.s = r*sin(theta)*sin(epsilon)
  r.p = sqrt(r^2-z.s^2)
  
  t0 = 1440/(2*pi)*acos((R-z.s*sin(L))/(r.p*cos(L)))
  
  ##a kludge adjustment for the radius of the sun
  that = t0+5 
  
  ## Adjust "noon" for the fact that the earth's orbit is not circular:
  n = 720-10*sin(4*pi*(d-80)/365.25)+8*sin(2*pi*d/365.25)
  
  ## now sunrise and sunset are:
  sunrise = (n-that+timezone)/60
  sunset = (n+that+timezone)/60
  
  return(list("sunrise" = sunrise,"sunset" = sunset))
}


isDaytime <- function(sunrise,sunset,datetime1) {
  th <- as.numeric(format(datetime1,format ="%H"))
  tm <- as.numeric(format(datetime1,format ="%M"))
  tt <- th + (tm/60)
  daytime <- (tt > sunrise & tt < sunset)
}

addTimeFeatures <- function(weather_df) {

  weather_df$DayNumber = as.numeric(weather_df$Date - min(weather_df$Date))
  weather_df$week_count = floor(weather_df$DayNumber/7.0)
  weather_df$month_count = floor(weather_df$DayNumber/30.5)
  
  weather_df$month = as.numeric(format(weather_df$Date, format="%m"))
  weather_df$season = floor((weather_df$month-1) / 3)
  weather_df$DayOfWeek = as.numeric(format(weather_df$Date,"%w"))
  weather_df$isSat = weather_df$DayOfWeek==6
  weather_df$isSun = weather_df$DayOfWeek==0
  weather_df$isWeekend = (weather_df$DayOfWeek %in% c(0,6))
  weather_df$tomorrowIsWeekend = (weather_df$DayOfWeek==5 | weather_df$DayOfWeek==6)
    # shift DayOfWeek so that Saturday=0 to make a better linear relationship with accident count
  weather_df$DayOfWeek <- weather_df$DayOfWeek + 1
  weather_df$DayOfWeek[weather_df$DayOfWeek==7] = 0
  weather_df$isFedHoliday = isHoliday(weather_df$Date,'USFed')
  weather_df$tomorrowIsFedHoliday = isHoliday(weather_df$Date+1,'USFed')

  sunriseset <- suncalc(as.numeric(format(weather_df$datehour,format="%j")),
                        mean(as.numeric(accident$lati)),
                        mean(as.numeric(accident$longi)))
  sunrise <- sunriseset[[1]]
  sunset <- sunriseset[[2]]
  weather_df$isDaytime <- isDaytime(sunrise,sunset,weather_df$datehour)
  weather_df$daytime_hours <- sunset-sunrise

  weather_df
}

# Hourly traffic volume counted at WA state road 520 mile marker 4 (the 520 bridge)
# Requested from http://www.wsdot.wa.gov/mapsdata/travel/trafficdatarequest.htm

getTrafficVolume520D10 <- function() {
  
  traffic_filename <- "D10 2010 2015PTR Hourly Volume Report.csv"
  traffic <- read.csv(paste0(work_dir,traffic_filename),stringsAsFactors = F,strip.white = T)
  traffic$Date <- as.Date(traffic$Date)
  traffic$Weekday <- NULL
  traffic <- melt(traffic,id=c('Direction','Date'))
  traffic$Hour <- as.numeric(sub("Hour_","",traffic$variable))-1
  traffic$variable <- NULL
  traffic$value <- as.numeric(sub(',','',traffic$value))
  
  eastbound <- traffic[traffic$Direction=='East Bound',]
  eastbound <- data.frame(Date=eastbound$Date,
                          Hour=eastbound$Hour,
                          traffic_east=eastbound$value)
  westbound <- traffic[traffic$Direction=='West Bound',]
  westbound <- data.frame(Date=westbound$Date,
                          Hour=westbound$Hour,
                          traffic_west=westbound$value)
  all <- merge(eastbound,westbound,all=T)
  all <- all[all$Date >= as.Date(analysis_start_date) &
               all$Date < as.Date(analysis_end_date),]
  all$traffic_east[is.na(all$traffic_east)] <- 0
  all$traffic_west[is.na(all$traffic_west)] <- 0
  all
  
  # note that there are gaps of several days in the data
}

if (interactive() ) {

  basicConfig()
  setLevel('FINEST')
  # gather

  #accident <- get911Accidents()  DO NOT USE: getCollisions is better
  accident <- getCollisions()
  
  weather <- getWeather()

  # add calendar / time features
  
  cal <- addTimeFeatures(weather)
  
  workzone <- getPermitsAffectingRightOfWay()

  cal$workzone_cnt <- sapply(cal$Date,countWorkzones)
  
  traffic <- getTrafficVolume520D10()

    # inner join to not introduce NAs due to gaps of several days in the traffic data
  full <- merge(cal,traffic)

  # join to dependent var:  accidents count
  accidentsByHour <- summarize(group_by(accident,Date,Hour),
                               accident_cnt=n())

  full <- merge(full,accidentsByHour,all.x=T)
  full$accident_cnt[is.na(full$accident_cnt)] <- 0

  # time-correlated features (lagging accident counts)
  # ensure sorted in time order first
  full <- full[order(full$Date,full$Hour),]

  # improve: deal with gaps in the hourly data
  full$accident_cntLag1Hour <- lag(full$accident_cnt,1,default=round(mean(full$accident_cnt)))
  full$accident_cntLag1Day <- lag(full$accident_cnt,24,default=round(mean(full$accident_cnt)))
  full$accident_cntLag1Week <- lag(full$accident_cnt,24*7,default=round(mean(full$accident_cnt)))
  
  fullday <- summarize(group_by(full,Date),
                       min_temp=min(temp),
                       max_temp=max(temp),
                       temp=mean(temp),
                       min_dew_pt=min(dew_pt),
                       max_dew_pt=max(dew_pt),
                       dew_pt=mean(dew_pt),
                       humidity=mean(humidity),
                       pressure=mean(pressure),
                       visibility=mean(visibility),
                       wind_speed=mean(wind_speed),
                       max_gust_speed=max(gust_speed),
                       precipitation=sum(precipitation),
                       DayNumber=first(DayNumber),
                       week_count=first(week_count),
                       month_count=first(month_count),
                       month=first(month),
                       season=first(season),
                       DayOfWeek=first(DayOfWeek),
                       isSat=first(isSat),
                       isSun=first(isSun),
                       isWeekend=first(isWeekend),
                       tomorrowIsWeekend=first(tomorrowIsWeekend),
                       isFedHoliday=first(isFedHoliday),
                       daytime_hours=first(daytime_hours),
                       traffic_east=sum(traffic_east),
                       traffic_west=sum(traffic_west),
                       workzone_cnt=first(workzone_cnt),
                       accident_cntLag1Day=sum(accident_cntLag1Day),
                       accident_cntLag1Week=sum(accident_cntLag1Week),
                       accident_cnt=sum(accident_cnt)
                       )

  # Explore accident distributions

  par(mfrow=c(1,2))
  
  hist(full$accident_cnt,breaks=50,
       main="Distribution of \nHourly Accident Counts",
       xlab="Hourly Accident Counts")

  daily_hist <- hist(fullday$accident_cnt,breaks=50,
                 main="Distribution of \nDaily Accident Counts",
                 xlab="Daily Accident Counts")

  multiplier <- daily_hist$counts / daily_hist$density
  daily_density <- density(fullday$accident_cnt)
  daily_density$y <- daily_density$y * multiplier[1]

  lines(daily_density, col="blue",lwd=2)
  
  # add theoretical normal distribution
  xs <- seq(0,70,length.out=100)
  ys <- dnorm(xs,mean=mean(fullday$accident_cnt),sd=sd(fullday$accident_cnt))
  lines(xs,ys * multiplier[1], col="red",lwd=2)

  legend(43,110,c("Observed","Theoretical"),
        lty=c(1,1),
        lwd=c(2,2),
        cex=0.6,
        col=c("blue","red"))
  
  # Use lasso to find a good set of predictors
  xfactors = model.matrix(accident_cnt ~ . , data = fullday[,-1])[,-1]
  
  lm_lasso =glmnet(xfactors,fullday$accident_cnt,alpha=1,family='gaussian')
 
  plot(lm_lasso, xvar="lambda")
  
  lasso_cv = cv.glmnet(xfactors,fullday$accident_cnt,alpha=1,family='gaussian')
  plot(lasso_cv)
  best_lambda = lasso_cv$lambda.min
  
  best_coef = coef(lm_lasso)[,lm_lasso$lambda == best_lambda]
  best_coef = best_coef[abs(best_coef) > 1e-10]

  # daily model using all predictors
  lm_day <- lm(accident_cnt~. + min_temp*precipitation,data=fullday[,-1])
  
  # daily model using lasso's best subset of predictors
  lm_day_lasso_best <- lm(accident_cnt~
                            temp+min_dew_pt+
                            humidity+pressure+visibility+
                            wind_speed+max_gust_speed+
                            precipitation+
                            DayNumber+week_count+
                            season+DayOfWeek+isSun+
                            tomorrowIsWeekend+isFedHoliday+
                            traffic_east+traffic_west+
                            workzone_cnt+
                            accident_cntLag1Day+accident_cntLag1Week,
                            data=fullday[,-1])
  
  # Final Model:  daily model picking out lasso's predictors with p-value < 0.1
  lm_day_lasso_reduced <- lm(accident_cnt~
                            temp+min_dew_pt+
                            wind_speed+max_gust_speed+
                            precipitation+
                            season+isSun+
                            tomorrowIsWeekend+isFedHoliday+
                            traffic_east+traffic_west+
                            workzone_cnt+
                            accident_cntLag1Day,
                          data=fullday[,-1])
  
  # check normality of residuals
  plot(lm_day_lasso_reduced)

}
