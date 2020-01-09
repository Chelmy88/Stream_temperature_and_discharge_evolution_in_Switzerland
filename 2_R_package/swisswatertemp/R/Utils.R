###################################################################################
# This content is related to the paper:
#
# Stream temperature and discharge evolution in Switzerland over the last 50 years:
# annual and seasonal behaviour
#
# Adrien Michel, Tristan Brauchli, Michael Lehning, Bettina Schaefli,
# and Hendrik Huwald
#
# Hydrol. Earth Syst. Sci., 2020
#
#
# Any use of the material (code or data) presented here should clearly reference
# to this paper and to the providers of the data mentioned in the documentation.
#
# This material is distributed under the GPLv3 license
# (https://www.gnu.org/licenses/gpl-3.0.html)
#
# Author: Adrien Michel, adrien.michel@epfl.ch, 01.2020
###################################################################################


#' swisswatertemp: A package to produce results presented in 'Stream temperature evolution in Switzerland over the last 50 years, Adrien Michel, Tristan Brauchli, Michael Lehning, Bettina Schaefli, and Hendrik Huwald, 2019'
#'
#' The swisswatertemp package is divided in two main parts: one is responsible to
#' generate the dataset, and one to perform the analysis and produce plots.
#'
#' @section Produce the data sets:
#' The data set can be produced from raw data in a SMET fromat. Raw data are not
#' provided here. The details about how to get the raw data and the scripts to
#' transform them in the SMET format are given in the directory 1_Obtain_raw_data.
#' Once the raw data are in the correct SMET format, the dataset can be generated
#' by running Preprocessing.R in the 3_Produce_data directory. These steps are
#' not mandatory, the datasets are indeed already available in
#' 4_Run_analysis/data/rds_data. Metedata can be found in the excel table
#' 3_Produce_data/data/discharge_gauging_station.xlsx
#'
#' @section Description of data sets:
#' Produced data sets have the general structure described below. Some data sets
#' produced have only part of it.
#' Structure of the data set:
#' \preformatted{
#' ["station name"]
#' |--header
#'   |--station_id = station number
#'   |--station_name = station name
#'   |--latitude: WG94 latitude
#'   |--longitude: WG94 longitude
#'   |--easting: CH1903 easting
#'   |--northing: CH1903 nothing
#'   |--altitude: altitude of the station
#'   |--operator: source of the data
#'   |--river: name of the river
#'   |--area: area of the catchment at the station
#'   |--mean_elevation: mean elevation of the catchment
#'   |--glacier_percent: percentage of the catchment glacier covered
#'   |--regime1: hydrological regime (classical)
#'   |--regime2: hydrological regime with regards to location
#'   |--regime3:Hydrological regime (following Aschwanden 1985, different from HADES 5.2)
#'   |--nodata: no data value used
#'   |--tz: timezone
#'   |--fields: variables in the [data] table
#' |--data: raw data
#'   |--timestamp: timestamp of the measurement as R date
#'   |--T: measured temperature (°C)
#'   |--Q: measured discharge (m3/s)
#' |--[monthly, yearly, DJF, MAM, JJA or SON]: data averaged over the given period
#'   |--[T Q]
#'     |--timestamp: timestamp in decimal year
#'     |--values: raw data averaged over the indicated period
#'     |--lm: output from linear model applied to trend + remainder
#'       |--["1999-2018" "1979-1998" "1979-2018" "1970-2018"] Periods over which
#'          trend is calculated, not necessarily all available
#'         |--timestamp: timestamp over the used period
#'         |--values: raw data over the given period
#'         |--trend: slope from linear model
#'         |--trend_std: std error of the trend value
#'         |--trend_p: p_value of the trend value
#'         |--intercept: intercept value from linear model
#'         |--intercept_std: std error of the intercept value
#'         |--intercept_p: p_value the of intercept value
#'         |--r_squared: r^2
#'         |--adj_r_squared: adjusted r^2
#'         |--printable:
#'           |--[trend, trend_std, trend_p, intercept, intercept_std, intercept_p,
#'              r_squared, adj_r_squared]: Same value as above but as string in "e" notation for display
#' |--hysteresis:
#'   |--[daily_mean or daily_mean_smoothed]: daily decadal mean, with or without
#'      smoothing (smoothed data is used in QT plots)
#'     |--["from_to" in years, e.g. "2009_1018"]
#'       |--T: temperature values(°C), 365 values
#'       |--Q: discharge values(m3/s), 365 values
#' |--meteo: attached meteo data
#'   |--[[station name]]
#'     |--header:
#'       |--station_id = station ID
#'       |--station_name = station name, same as ID
#'       |--latitude: WG94 latitude
#'       |--longitude: WG94 longitude
#'       |--easting: CH1903 easting
#'       |--northing: CH1903 nothing
#'       |--altitude: altitude of the station
#'       |--nodata: no data value used
#'       |--source: source of the meteodata
#'       |--tz: timezone
#'       |--fields: variables in the [data] table
#'     |--data: raw meteo data
#'       |--timestamp: timestamp of the measurement as R date
#'       |--[TA, P, TA_HOM, P_HOM, HS6, HS18, HSAUTO6, HSAUTO18]: available meteo variables
#'     |--[monthly, yearly, DJF, MAM, JJA or SON]: data averaged over the given period
#'       |--[TA,P]
#'         |--timestamp: timestamp in decimal year
#'         |--values: raw data averaged over the indicated period
#'         |--lm: output from linear model applied to trend + remainder
#'           |--["1999-2018" "1979-1998" "1979-2018" "1970-2018"] Periods over which
#'              trend is calculated, not necessarily all available
#'             |--timestamp: timestamps over the used period
#'             |--values: raw data over the given period
#'             |--trend: slope from linear model
#'             |--trend_std: std error of the trend value
#'             |--trend_p: p_value of the trend value
#'             |--intercept: intercept value from linear model
#'             |--intercept_std: std error of the intercept value
#'             |--intercept_p: p_value the of intercept value
#'             |--r_squared: r^2
#'             |--adj_r_squared: adjusted r^2
#'             |--printable:
#'               |--[trend, trend_std, trend_p, intercept, intercept_std, intercept_p,
#'                  r_squared, adj_r_squared]: Same value as above but as string in "e" notation for display
#' |--STL
#'   |--[T or Q]
#'     |--timestamp: date, in decimal years
#'     |--seasonal: seasonal component from STL
#'     |--trend: trend from STL
#'     |--remainder: remainders from STL
#'     |--raw: raw data used for STL
#'     |--acf: acf analysis as R acf object
#'     |--pacf: pacf analysis as R pacf object
#'     |--lm: output from linear model applied to trend + remainder
#'       |--["1999-2018" "1979-1998" "1979-2018" "1970-2018"] Periods over which
#'          trend is calculated, not necessarily all available
#'         |--timestamp: timestamp over the used period
#'         |--values: raw data over the given period
#'         |--trend: slope from linear model
#'         |--trend_std: std error of the trend value
#'         |--trend_p: p_value of the trend value
#'         |--intercept: intercept value from linear model
#'         |--intercept_std: std error of the intercept value
#'         |--intercept_p: p_value the of intercept value
#'         |--r_squared: r^2
#'         |--adj_r_squared: adjusted r^2
#'         |--printable:
#'           |--[trend, trend_std, trend_p, intercept, intercept_std, intercept_p,
#'              r_squared, adj_r_squared]: Same value as above but as string in "e" notation for display
#'   |--meteo
#'     |--[station name]
#'       |--[TA or P]
#'         |--timestamp: date, in decimal years
#'         |--seasonal: seasonal component from STL
#'         |--trend: trend from STL
#'         |--remainder: remainders from STL
#'         |--raw: raw data used for STL
#'         |--acf: acf analysis as R acf object
#'         |--pacf: pacf analysis as R pacf object
#'         |--ccf: ccf analysis (between meteo and river data T-TA and Q-P) as R ccf object
#'         |--lm: output from linear model applied to trend + remainder
#'           |--["1999-2018" "1979-1998" "1979-2018" "1970-2018"] Periods over
#'              which trend is calculated, not necessarily all available
#'             |--timestamp: timestamp over the used period
#'             |--values: raw data over the given period
#'             |--trend: slope from linear model
#'             |--trend_std: std error of the trend value
#'             |--trend_p: p_value of the trend value
#'             |--intercept: intercept value from linear model
#'             |--intercept_std: std error of the intercept value
#'             |--intercept_p: p_value the of intercept value
#'             |--r_squared: r^2
#'             |--adj_r_squared: adjusted r^2
#'             |--printable:
#'               |--[trend, trend_std, trend_p, intercept, intercept_std,
#'                  intercept_p, r_squared, adj_r_squared]: Same value as above
#'                  but as string in "e" notation for display
#' }
#'
#' @section Usage of the data sets:
#' \preformatted{
#' Entries can be accessed following the structure describes above
#' and with double brackets [["entry name here"]] (the name shoulb be
#' between quote marks), or with the "$" signe (in this case no quote
#' mark is needed except in the names contains special character).
#'
#' If a variable containing the name of the entry to be accesses is
#' used, double brakets shoudl be used [[var]], note that $var will
#' not work (text after $ is taken as string, i.e. variable will not
#' be accessed).
#'
#' Examples:
#' 1) rivers_data[["Aare-Brienzwiler"]][["STL"]][["meteo"]][["GRH"]][["TA"]][["trend"]]
#'    or
#'    rivers_data$"Aare-Brienzwiler"$STL$meteo$GRH$TA$trend
#'    are quivalent and return the trend component of the meteo station GRH linked
#'    to the Aare-Brienzwiler water station.
#' 2) Note that the function "names" is useful to retrieve the next entries at a
#'    given entry level. E.g. names(rivers_data$"Aare-Brienzwiler"$meteo) returns
#'    a list of the names of meteo station attached to the Aare-Brienzwiler river
#'    station.
#' 3) for (river_station in names(rivers_data))
#'    will loop over all stations names which are stored in river_station. Data can
#'    be thus accessed through: rivers_data[[river_station]]$...
#'    For example rivers_data[[river_station]]$header$mean_elevation, if in the above
#'    loop, will return the mean elevation for each catchment.
#'}
#' @docType package
#' @name swisswatertemp
NULL




#' Trim spaces in string
#' @param x A string
#'
#' @return Input string with leading or trailing spaces removed
#' @source Function taken from \url{https://stackoverflow.com/questions/2261079/how-to-trim-leading-and-trailing-whitespace}
#' @export
#'
trim <- function (x) {
  gsub("^\\s+|\\s+$", "", x)
}

#'
#' Returns year from a PosixCT (or vector of PosixCT)
#' @param timestamp A PosixCT or vector of PosixCT
#' @return The corresponding year (as numeric)
#' @export
get_year <- function(timestamp)
{
  return(as.numeric(format.Date(timestamp,"%Y")))
}


#' Returns month from a PosixCT (or vector of PosixCT)
#' @param timestamp A PosixCT or vector of PosixCT
#' @return The corresponding month (as numeric)
#' @export
get_month <- function(timestamp)
{
  return (as.numeric(format.Date(timestamp,"%m")))
}


#' Returns day from a PosixCT (or vector of PosixCT)
#' @param timestamp A PosixCT or vector of PosixCT
#' @return The corresponding day (as numeric)
#' @export
get_day <- function(timestamp)
{
  return (as.numeric(format.Date(timestamp,"%d")))
}

#' Compute the daily mean over a year
#'
#' Compute the daily mean over a year for all raw variables in SMET_OBJECT
#' obtained through the function \code{\link{get_file_data}} (raw variables are stored
#' in the \code{$data} part of theSMET_OBJECT).
#'
#' @param data A SMET_OBJECT for a given station
#' @param yr The year over which the data should be daily averaged
#'
#' @return A \code{\link[base]{data.frame}} the daily mean of each variables
#' (the timestamp is removed).
#'
daily_mean_for_year <- function(data,yr)
{
  dat=data$data[which(get_year(data$data$timestamp)==yr),]
  len=length(dat$data$timestamp)
  dat["DOY"]=as.numeric(strftime(dat$timestamp, format = "%j",tz="GMT"))
  mean <- aggregate(x = dat[,c(2:length(dat))],
                    by = list(unique.values = dat$DOY),
                    FUN = mean)
  return(mean[,2:(length(mean)-1)])
}

#'
#' Return the summary of \code{\link[stats]{lm}} model in a list of strings
#'
#' The values retuned are obtained by using \code{\link[base]{summary}}
#' on the \code{\link[stats]{lm}} object
#' @param lm An object generated by \code{\link[stats]{lm}}
#'
#' @return A list with the folowing entries, all as strings and rounded
#' to signifigant digits:
#' \item{intercept}{The intercept value}
#' \item{trend}{The trend value}
#' \item{intercept_std}{The std error of the intercept value}
#' \item{trend_std}{The std error of the trend value}
#' \item{intercept_p}{The p-value of the intercept value}
#' \item{trend_p}{The p-value of the trend value}
#' \item{r_squared}{The \eqn{R^{2}} value}
#' \item{adj_r_squared}{Teh adjusted R \eqn{R^{2}} value}
#' @export
get_lm_summary_printable<-function(lm)
{
  lm_sum=list()
  lm_sum[["intercept"]]=formatC(coef(summary(lm))[1,1], format = "e", digits = 2)
  lm_sum[["trend"]]=formatC(coef(summary(lm))[2,1], format = "e", digits = 2)
  lm_sum[["intercept_std"]]=formatC(coef(summary(lm))[1,2], format = "e", digits = 2)
  lm_sum[["trend_std"]]=formatC(coef(summary(lm))[2,2], format = "e", digits = 2)
  lm_sum[["intercept_p"]]=formatC(coef(summary(lm))[1,4], format = "e", digits = 2)
  lm_sum[["trend_p"]]=formatC(coef(summary(lm))[1,4], format = "e", digits = 2)
  lm_sum[["r_squared"]]=formatC(summary(lm)$r.squared, digits = 4)
  lm_sum[["adj_r_squared"]]=formatC(summary(lm)$adj.r.squared,digits = 4)
  return(lm_sum)
}

#'
#' Return the summary of \code{\link[stats]{lm}} model in a list of numeric values
#'
#' The values retuned are obtained by using \code{\link[base]{summary}}
#' on the \code{\link[stats]{lm}} object
#' @param lm An object generated by \code{\link[stats]{lm}}
#'
#' @return A list with the folowing entries, all as numeric:
#' \item{intercept}{The intercept value}
#' \item{trend}{The trend value}
#' \item{intercept_std}{The std error of the intercept value}
#' \item{trend_std}{The std error of the trend value}
#' \item{intercept_p}{The p-value of the intercept value}
#' \item{trend_p}{The p-value of the trend value}
#' \item{r_squared}{The \eqn{R^{2}} value}
#' \item{adj_r_squared}{Teh adjusted R \eqn{R^{2}} value}
#' @export
get_lm_summary<-function(lm)
{
  lm_sum=list()
  lm_sum[["intercept"]]=coef(summary(lm))[1,1]
  lm_sum[["trend"]]=coef(summary(lm))[2,1]
  lm_sum[["intercept_std"]]=coef(summary(lm))[1,2]
  lm_sum[["trend_std"]]=coef(summary(lm))[2,2]
  lm_sum[["intercept_p"]]=coef(summary(lm))[1,4]
  lm_sum[["trend_p"]]=coef(summary(lm))[1,4]
  lm_sum[["r_squared"]]=summary(lm)$r.squared
  lm_sum[["adj_r_squared"]]=summary(lm)$adj.r.squared
  return(lm_sum)
}


#' Plot a map with the location of the river station and meteostation used
#'
#' @param rivers_data A list of SMET_OBJECT obtained through the function
#' \code{\link{get_file_data}} containing
#' the data on the rivers stations
#' @param output_type Either \code{"NONE"} (default), \code{"PDF"} or \code{"PNG"}.
#'
#' \code{output_type = "NONE"} creates the plot in a normal plot window,
#'
#' \code{output_type = "PDF"} saves the plot as pdf under plots/General_situation.pdf,
#'
#' \code{output_type = "PNG"} saves the plot as png under plots/General_situation.png
#'
#' @section Requirements:
#' This functions needs the following files to be abailable:
#' maps/processed_maps/swiss_map.tif, maps/processed_maps/lakes.shp,
#' maps/processed_maps/rivers.shp, maps/processed_maps/borders.shp,
#' meteo/MeteoSwiss_StationList.txt. In addition, the plot directory must exist.
#'
#' @export
plot_general_situation <- function(rivers_data,output_type="NONE")
{

  # Load data
  map <- brick("data/maps/swiss_map.tif")
  lakes <- readOGR("data/maps/lakes.shp")
  rivers <- readOGR("data/maps/rivers.shp")
  borders <- readOGR("data/maps/borders.shp")
  meteo_suisse_stations=data.frame(fread("../3_Produce_data/meteo/MeteoSwiss_StationList.txt"))
  #Count number of catchment of each regimes to generate colors
  HYP=0
  SPJ=0
  DSL=0
  ALP=0
  for(river in names(rivers_data)){
    if(rivers_data[[river]]$header$regime2=="Strong influence of Hydropeaking")
    {
      HYP=HYP+1
    }
    else if(rivers_data[[river]]$header$regime2=="Regime from Plateau and Jura")
    {
      SPJ=SPJ+1
    }
    else if(rivers_data[[river]]$header$regime2=="After lakes"){DSL=DSL+1}
    else if(rivers_data[[river]]$header$regime2=="Alpine regime"){ALP=ALP+1}
  }
  col_ALP=colorRampPalette(brewer.pal(9,"Greys"))(ALP+2)[2:(2+SPJ)]
  col_DSL=colorRampPalette(brewer.pal(9,"Greens"))(DSL+10)[10:(10+DSL)]
  col_HYP=colorRampPalette(brewer.pal(9,"Blues"))(HYP+5)[5:(5+HYP)]
  col_SPJ=colorRampPalette(brewer.pal(9,"Reds"))(SPJ+5)[5:(5+SPJ)]

  # Plot map, rivers, lakes and borders
  if(output_type=="PDF"){
    pdf(paste0("plots/General_situation.pdf"),width=12,height=7)
  }
  else if(output_type=="PNG"){
    png(paste0("plots/General_situation.png"),width=2048,height=768, res = 150)
  }

  par(mfrow=c(1,1),oma=c(1,1,1,1),mar=c(0,0,0,0))
  layout(matrix(c(1,1,1,1,1,2), ncol=6, byrow=TRUE))

  m=map
  m$swiss_map.1= map$swiss_map.1
  m$swiss_map.2= map$swiss_map.1
  m$swiss_map.3= map$swiss_map.1

  plotRGB(m,alpha=130)

  plot(rivers,add=TRUE,col="cornflowerblue",lwd=2.5)

  plot(lakes,add=TRUE,col="cornflowerblue")

  plot(borders,add=TRUE,col="grey30",border=NA)

  north.arrow(xb=820000, yb=275000, len=2000, lab="N",cex=2,col=1)

  text(785000,90000,
       "Map, borders, rivers and lakes source:\n Swiss Federal Office of Topography",
       pos=1,col=1,font=2,cex=1.4)

  map.scale(xc=785000, yc=110000,len=50000,subdiv=50,ndivs=1,units="km")

  #Add BAFU stations
  legend_bafu=c()
  for (i in 1:length(rivers_data))
  {
    station=rivers_data[[i]]

    if(station$header$regime2=="Strong influence of Hydropeaking")
    {
      col=col_HYP[8]
    }
    else if(station$header$regime2=="Regime from Plateau and Jura")
    {
      col=col_SPJ[10]
    }
    else if(station$header$regime2=="After lakes"){col=col_DSL[10]}
    else if(station$header$regime2=="Alpine regime"){col=col_ALP[3]}

    points(station$header$easting,station$header$northing,pch=15,col=col,cex=2)
    points(station$header$easting,station$header$northing,pch=22,col=1,cex=2)
    text(station$header$easting,station$header$northing,i,pos=4,col=1,font=2,
         cex=1.2)
    legend_bafu=c(legend_bafu,paste(i,station$header$abbr))
  }

  #Read a list of used meteSwiss stations
  used_meteosuisse_stations=c()
  for (river in rivers_data){
    used_meteosuisse_stations=c(used_meteosuisse_stations,names(river$meteo))
  }
  used_meteosuisse_stations=unique(used_meteosuisse_stations)

  #List of stations offering homogenous data
  complete_hom_meteosuisse_stations=c("ALT","ANT","BAS","BER","CDF","CHM","DAV",
                                      "ELM", "ENG","GRC","GRH","GSB","GVE","LUG",
                                      "LUZ","MER","NEU","OTL","SAE","SBE","SIA",
                                      "SIO","STG","SMA")
  uncomplete_hom_meteosuisse_stations=c()
  #Add MeteoSuisse stations
  legend_meteosuisse=c()
  cex=1.3
  for (i in 1:length(meteo_suisse_stations[,1]))
  {
    station=meteo_suisse_stations[i,]
    color=5
    if(station$abreviation %in% complete_hom_meteosuisse_stations)
    {
      color=6
      points(station$easting,station$northing,pch=17,col=color,cex=cex)
      points(station$easting,station$northing,pch=24,col=1,cex=cex)
      text(station$easting,station$northing,station$abreviation,pos=2,col=1,
           font=2,cex=1.2)
    }
    else if(station$abreviation %in% uncomplete_hom_meteosuisse_stations){
      color=2
      points(station$easting,station$northing,pch=17,col=color,cex=cex)
      points(station$easting,station$northing,pch=24,col=1,cex=cex)
      text(station$easting,station$northing,station$abreviation,pos=2,col=1,
           font=2,cex=1.2)
    }
    if(paste0(station$abreviation,"_HOM") %in% used_meteosuisse_stations |
       station$abreviation %in% used_meteosuisse_stations)
    {
      points(station$easting,station$northing,pch=17,col=color,cex=cex)
      points(station$easting,station$northing,pch=24,col=1,cex=cex)
      text(station$easting,station$northing,station$abreviation,pos=2,col=1,
           font=2,cex=1.2)
    }
    else{
    }
    legend_meteosuisse=c(legend,paste(i,station))
  }

  paste(c(1:52))
  # Add BAFU station legend on the right
  plot.new()
  legend(x=-0.05,y=0.95,legend=legend_bafu, title="Water station abbreviation",cex=1.1,ncol=2,bty="n")
  # Add MeteoSuisse station legend below
  legend(x=0.25,y=0.15,legend=c("Homog. station","Normal station"),
        pch=c(17,17),col=c(6,5), title="Meteo Stations",inset=0,pt.cex=c(1),cex=1.1,bty="n")

  legend(x=0.28,y=0.26,legend=c("DLA","ALP","SPJ","HYP"),col=c(col_DSL[10],col_ALP[3],col_SPJ[10],col_HYP[8]),
         pch=15,bty="n",ncol=2,pt.cex=1.5,cex=1.1,title="Regime")

  if(output_type=="PDF" || output_type=="PNG"){
    dev.off()
  }
}
