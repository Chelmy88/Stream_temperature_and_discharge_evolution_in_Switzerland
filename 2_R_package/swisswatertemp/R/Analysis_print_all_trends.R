# This script is related to the paper:
# "Stream temperature evolution in Switzerland over the last 50 years, Adrien
# Michel, Tristan Brauchli, Michael Lehning, Bettina Schaefli, and Hendrik Huwald,
# HESS, 2019"

# This material is distributed under the GPLv3 license
# (https://www.gnu.org/licenses/gpl-3.0.html)

#' Print trends table in Latex format
#'
#' Tihs function prints to the console the trends table shown in Tables A1 and
#' A2 in appendix and in Tables S3 and S4 in supplementary. The table are
#' printed in latex format
#'
#' @param rivers_data The dataset of rivers data
#'
#' @param meteo_data The dataset of homegenous MeteoSwiss data
#'
#' @export
print_all_trends<-function(rivers_data,meteo_data)
{
  period="1999-2018"
  str=""
  for (river_station in names(rivers_data)){
    print(rivers_data[[river_station]]$header)
    str= paste0(str,rivers_data[[river_station]]$header$abbr," & & ")

    dat=rivers_data[[river_station]]$STL$T
    if(period %in% names(dat$lm))
    {
      str=paste0(str,format(round(dat$lm[[period]]$trend*10,2),nsmall=2) ,
                 " (", round(dat$lm[[period]]$trend_std*10,2),")")
    }
    else{
      str=paste0(str,"-")
    }
    str=paste0(str," & ")

    dat=rivers_data[[river_station]]$DJF$T
    if(period %in% names(dat$lm))
    {
      str=paste0(str,format(round(dat$lm[[period]]$trend*10,2),nsmall=2) ,
                 " (", format(round(dat$lm[[period]]$trend_std*10,2),nsmall=2),")")
    }
    else{
      str=paste0(str,"-")
    }
    str=paste0(str," & ")

    dat=rivers_data[[river_station]]$MAM$T
    if(period %in% names(dat$lm))
    {
      str=paste0(str,format(round(dat$lm[[period]]$trend*10,2),nsmall=2) ,
                 " (", format(round(dat$lm[[period]]$trend_std*10,2),nsmall=2),")")
    }
    else{
      str=paste0(str,"-")
    }
    str=paste0(str," & ")

    dat=rivers_data[[river_station]]$JJA$T
    if(period %in% names(dat$lm))
    {
      str=paste0(str,format(round(dat$lm[[period]]$trend*10,2),nsmall=2) ,
                 " (", format(round(dat$lm[[period]]$trend_std*10,2),nsmall=2),")")
    }
    else{
      str=paste0(str,"-")
    }
    str=paste0(str," & ")

    dat=rivers_data[[river_station]]$SON$T
    if(period %in% names(dat$lm))
    {
      str=paste0(str,format(round(dat$lm[[period]]$trend*10,2),nsmall=2) ,
                 " (", format(round(dat$lm[[period]]$trend_std*10,2),nsmall=2),")")
    }
    else{
      str=paste0(str,"-")
    }
    str=paste0(str," & ")
    str=paste0(str," & ")

    dat=rivers_data[[river_station]]$STL$Q
    if(period %in% names(dat$lm))
    {
      str=paste0(str,format(round(dat$lm[[period]]$trend/
                                    mean(dat$lm[[period]]$values,na.rm=TRUE)*100*10,1),nsmall=1),
                 " (", round(dat$lm[[period]]$trend_std/
                               mean(dat$lm[[period]]$values,na.rm=TRUE)*100*10,1),")")
      }
    else{
      str=paste0(str,"-")
    }
    str=paste0(str," & ")

    dat=rivers_data[[river_station]]$DJF$Q
    if(period %in% names(dat$lm))
    {
      str=paste0(str,format(round(dat$lm[[period]]$trend/
                                    mean(dat$lm[[period]]$values,na.rm=TRUE)*100*10,1),nsmall=1),
                 " (", round(dat$lm[[period]]$trend_std/
                               mean(dat$lm[[period]]$values,na.rm=TRUE)*100*10,1),")")
    }
    else{
      str=paste0(str,"-")
    }
    str=paste0(str," & ")

    dat=rivers_data[[river_station]]$MAM$Q
    if(period %in% names(dat$lm))
    {
      str=paste0(str,format(round(dat$lm[[period]]$trend/
                                    mean(dat$lm[[period]]$values,na.rm=TRUE)*100*10,1),nsmall=1),
                 " (", round(dat$lm[[period]]$trend_std/
                               mean(dat$lm[[period]]$values,na.rm=TRUE)*100*10,1),")")
    }
    else{
      str=paste0(str,"-")
    }
    str=paste0(str," & ")

    dat=rivers_data[[river_station]]$JJA$Q
    if(period %in% names(dat$lm))
    {
      str=paste0(str,format(round(dat$lm[[period]]$trend/
                                    mean(dat$lm[[period]]$values,na.rm=TRUE)*100*10,1),nsmall=1),
                 " (", round(dat$lm[[period]]$trend_std/
                               mean(dat$lm[[period]]$values,na.rm=TRUE)*100*10,1),")")
    }
    else{
      str=paste0(str,"-")
    }
    str=paste0(str," & ")

    dat=rivers_data[[river_station]]$SON$Q
    if(period %in% names(dat$lm))
    {
      str=paste0(str,format(round(dat$lm[[period]]$trend/
                                    mean(dat$lm[[period]]$values,na.rm=TRUE)*100*10,1),nsmall=1),
                 " (", round(dat$lm[[period]]$trend_std/
                               mean(dat$lm[[period]]$values,na.rm=TRUE)*100*10,1),")")
    }
    else{
      str=paste0(str,"-")
    }

    str=paste0(str,"\\\\\n")
      }

  cat(str)

  print("-------------")

  period="1979-2018"
  str=""
  for (river_station in names(rivers_data)){
    str= paste0(str,rivers_data[[river_station]]$header$abbr," & & ")

    dat=rivers_data[[river_station]]$STL$T
    if(period %in% names(dat$lm))
    {
      str=paste0(str,format(round(dat$lm[[period]]$trend*10,2),nsmall=2) ,
                 " (", round(dat$lm[[period]]$trend_std*10,2),")")
    }
    else{
      str=paste0(str,"-")
    }
    str=paste0(str," & ")

    dat=rivers_data[[river_station]]$DJF$T
    if(period %in% names(dat$lm))
    {
      str=paste0(str,format(round(dat$lm[[period]]$trend*10,2),nsmall=2) ,
                 " (", format(round(dat$lm[[period]]$trend_std*10,2),nsmall=2),")")
    }
    else{
      str=paste0(str,"-")
    }
    str=paste0(str," & ")

    dat=rivers_data[[river_station]]$MAM$T
    if(period %in% names(dat$lm))
    {
      str=paste0(str,format(round(dat$lm[[period]]$trend*10,2),nsmall=2) ,
                 " (", format(round(dat$lm[[period]]$trend_std*10,2),nsmall=2),")")
    }
    else{
      str=paste0(str,"-")
    }
    str=paste0(str," & ")

    dat=rivers_data[[river_station]]$JJA$T
    if(period %in% names(dat$lm))
    {
      str=paste0(str,format(round(dat$lm[[period]]$trend*10,2),nsmall=2) ,
                 " (", format(round(dat$lm[[period]]$trend_std*10,2),nsmall=2),")")
    }
    else{
      str=paste0(str,"-")
    }
    str=paste0(str," & ")

    dat=rivers_data[[river_station]]$SON$T
    if(period %in% names(dat$lm))
    {
      str=paste0(str,format(round(dat$lm[[period]]$trend*10,2),nsmall=2) ,
                 " (", format(round(dat$lm[[period]]$trend_std*10,2),nsmall=2),")")
    }
    else{
      str=paste0(str,"-")
    }
    str=paste0(str," & ")
    str=paste0(str," & ")

    dat=rivers_data[[river_station]]$STL$Q
    if(period %in% names(dat$lm))
    {
      str=paste0(str,format(round(dat$lm[[period]]$trend/
                                    mean(dat$lm[[period]]$values,na.rm=TRUE)*100*10,1),nsmall=1),
                 " (", round(dat$lm[[period]]$trend_std/
                               mean(dat$lm[[period]]$values,na.rm=TRUE)*100*10,1),")")
    }
    else{
      str=paste0(str,"-")
    }
    str=paste0(str," & ")

    dat=rivers_data[[river_station]]$DJF$Q
    if(period %in% names(dat$lm))
    {
      str=paste0(str,format(round(dat$lm[[period]]$trend/
                                    mean(dat$lm[[period]]$values,na.rm=TRUE)*100*10,1),nsmall=1),
                 " (", round(dat$lm[[period]]$trend_std/
                               mean(dat$lm[[period]]$values,na.rm=TRUE)*100*10,1),")")
    }
    else{
      str=paste0(str,"-")
    }
    str=paste0(str," & ")

    dat=rivers_data[[river_station]]$MAM$Q
    if(period %in% names(dat$lm))
    {
      str=paste0(str,format(round(dat$lm[[period]]$trend/
                                    mean(dat$lm[[period]]$values,na.rm=TRUE)*100*10,1),nsmall=1),
                 " (", round(dat$lm[[period]]$trend_std/
                               mean(dat$lm[[period]]$values,na.rm=TRUE)*100*10,1),")")
    }
    else{
      str=paste0(str,"-")
    }
    str=paste0(str," & ")

    dat=rivers_data[[river_station]]$JJA$Q
    if(period %in% names(dat$lm))
    {
      str=paste0(str,format(round(dat$lm[[period]]$trend/
                                    mean(dat$lm[[period]]$values,na.rm=TRUE)*100*10,1),nsmall=1),
                 " (", round(dat$lm[[period]]$trend_std/
                               mean(dat$lm[[period]]$values,na.rm=TRUE)*100*10,1),")")
    }
    else{
      str=paste0(str,"-")
    }
    str=paste0(str," & ")

    dat=rivers_data[[river_station]]$SON$Q
    if(period %in% names(dat$lm))
    {
      str=paste0(str,format(round(dat$lm[[period]]$trend/
                                    mean(dat$lm[[period]]$values,na.rm=TRUE)*100*10,1),nsmall=1),
                 " (", round(dat$lm[[period]]$trend_std/
                               mean(dat$lm[[period]]$values,na.rm=TRUE)*100*10,1),")")
    }
    else{
      str=paste0(str,"-")
    }

    str=paste0(str,"\\\\\n")
  }
  cat(str)


  meteo=list()
  for (river_station in names(rivers_data)){
    for (met in names(rivers_data[[river_station]]$meteo)){
      meteo[[met]]=rivers_data[[river_station]]$meteo[[met]]
    }
  }

  for (meteo_station in names(meteo_data)){
    print(meteo_station)
      meteo[[met]]=meteo_data[[meteo_station]]
  }

  length(names(meteo_data))

  str=""
  for (met in sort(names(meteo)))
  {
    str=paste0(str,meteo[[met]]$header$station_name)
    str=paste0(str," & ")
    str=paste0(str,met)
    str=paste0(str," & ")
    str=paste0(str,meteo[[met]]$header$easting)
    str=paste0(str," & ")
    str=paste0(str,meteo[[met]]$header$northing)
    str=paste0(str," & ")
    str=paste0(str,meteo[[met]]$header$altitude)
    str=paste0(str," & ")
    str=paste0(str,
               strftime(meteo[[met]]$data$timestamp[min(which(!is.nan(meteo[[met]]$data$TA)))],format="%Y"),
               "-",
               strftime(meteo[[met]]$data$timestamp[max(which(!is.nan(meteo[[met]]$data$TA)))],format="%Y"))
    str=paste0(str," & ")
    if("TA_HOM" %in% names(meteo[[met]]$data)){
      str=paste0(str,
                 strftime(meteo[[met]]$data$timestamp[min(which(!is.nan(meteo[[met]]$data$TA)))],format="%Y"),
                 "-",
                 strftime(meteo[[met]]$data$timestamp[max(which(!is.nan(meteo[[met]]$data$TA)))],format="%Y"))
    }
    else{
      str=paste0(str," - ")

    }
    str=paste0(str," & ")

    str=paste0(str,
               strftime(meteo[[met]]$data$timestamp[min(which(!is.nan(meteo[[met]]$data$P)))],format="%Y"),
               "-",
               strftime(meteo[[met]]$data$timestamp[max(which(!is.nan(meteo[[met]]$data$P)))],format="%Y"))

    str=paste0(str," & ")

    if("P_HOM" %in% names(meteo[[met]]$data)){
      str=paste0(str,
                 strftime(meteo[[met]]$data$timestamp[min(which(!is.nan(meteo[[met]]$data$TA)))],format="%Y"),
                 "-",
                 strftime(meteo[[met]]$data$timestamp[max(which(!is.nan(meteo[[met]]$data$TA)))],format="%Y"))
    }
    else{
      str=paste0(str," - ")

    }
    str=paste0(str,"\\\\\n")


  }



  meteo=list()
  stat=list()
  for (river_station in names(rivers_data)){
    for (met in names(rivers_data[[river_station]]$meteo)){
      meteo[[met]]=rivers_data[[river_station]]$meteo[[met]]
      stat[[met]]=river_station
    }
  }

  period="1999-2018"
  str=""

  for (river_station in names(meteo)){
    str= paste0(str,meteo[[river_station]]$header$station_id," & & ")

    dat=rivers_data[[(stat[[river_station]])]]$STL$meteo[[river_station]]$TA

    if(period %in% names(dat$lm))
    {
      str=paste0(str,format(round(dat$lm[[period]]$trend*10,2),nsmall=2) ,
                 " (", round(dat$lm[[period]]$trend_std*10,2),")")
    }
    else{
      str=paste0(str,"-")
    }
    str=paste0(str," & ")

    dat=meteo[[river_station]]$DJF$TA
    if(period %in% names(dat$lm))
    {
      str=paste0(str,format(round(dat$lm[[period]]$trend*10,2),nsmall=2) ,
                 " (", format(round(dat$lm[[period]]$trend_std*10,2),nsmall=2),")")
    }
    else{
      str=paste0(str,"-")
    }
    str=paste0(str," & ")

    dat=meteo[[river_station]]$MAM$TA
    if(period %in% names(dat$lm))
    {
      str=paste0(str,format(round(dat$lm[[period]]$trend*10,2),nsmall=2) ,
                 " (", format(round(dat$lm[[period]]$trend_std*10,2),nsmall=2),")")
    }
    else{
      str=paste0(str,"-")
    }
    str=paste0(str," & ")

    dat=meteo[[river_station]]$JJA$TA
    if(period %in% names(dat$lm))
    {
      str=paste0(str,format(round(dat$lm[[period]]$trend*10,2),nsmall=2) ,
                 " (", format(round(dat$lm[[period]]$trend_std*10,2),nsmall=2),")")
    }
    else{
      str=paste0(str,"-")
    }
    str=paste0(str," & ")

    dat=meteo[[river_station]]$SON$TA
    if(period %in% names(dat$lm))
    {
      str=paste0(str,format(round(dat$lm[[period]]$trend*10,2),nsmall=2) ,
                 " (", format(round(dat$lm[[period]]$trend_std*10,2),nsmall=2),")")
    }
    else{
      str=paste0(str,"-")
    }
    str=paste0(str," & ")
    str=paste0(str," & ")

    dat=rivers_data[[(stat[[river_station]])]]$STL$meteo[[river_station]]$P

    if(period %in% names(dat$lm))
    {
      str=paste0(str,format(round(dat$lm[[period]]$trend/
                                    mean(dat$lm[[period]]$values,na.rm=TRUE)*100*10,1),nsmall=1),
                 " (", round(dat$lm[[period]]$trend_std/
                               mean(dat$lm[[period]]$values,na.rm=TRUE)*100*10,1),")")
    }
    else{
      str=paste0(str,"-")
    }
    str=paste0(str," & ")

    dat=meteo[[river_station]]$DJF$P
    if(period %in% names(dat$lm))
    {
      str=paste0(str,format(round(dat$lm[[period]]$trend/
                                    mean(dat$lm[[period]]$values,na.rm=TRUE)*100*10,1),nsmall=1),
                 " (", round(dat$lm[[period]]$trend_std/
                               mean(dat$lm[[period]]$values,na.rm=TRUE)*100*10,1),")")
    }
    else{
      str=paste0(str,"-")
    }
    str=paste0(str," & ")

    dat=meteo[[river_station]]$MAM$P
    if(period %in% names(dat$lm))
    {
      str=paste0(str,format(round(dat$lm[[period]]$trend/
                                    mean(dat$lm[[period]]$values,na.rm=TRUE)*100*10,1),nsmall=1),
                 " (", round(dat$lm[[period]]$trend_std/
                               mean(dat$lm[[period]]$values,na.rm=TRUE)*100*10,1),")")
    }
    else{
      str=paste0(str,"-")
    }
    str=paste0(str," & ")

    dat=meteo[[river_station]]$JJA$P
    if(period %in% names(dat$lm))
    {
      str=paste0(str,format(round(dat$lm[[period]]$trend/
                                    mean(dat$lm[[period]]$values,na.rm=TRUE)*100*10,1),nsmall=1),
                 " (", round(dat$lm[[period]]$trend_std/
                               mean(dat$lm[[period]]$values,na.rm=TRUE)*100*10,1),")")
    }
    else{
      str=paste0(str,"-")
    }
    str=paste0(str," & ")

    dat=meteo[[river_station]]$SON$P
    if(period %in% names(dat$lm))
    {
      str=paste0(str,format(round(dat$lm[[period]]$trend/
                                    mean(dat$lm[[period]]$values,na.rm=TRUE)*100*10,1),nsmall=1),
                 " (", round(dat$lm[[period]]$trend_std/
                               mean(dat$lm[[period]]$values,na.rm=TRUE)*100*10,1),")")
    }
    else{
      str=paste0(str,"-")
    }

    str=paste0(str,"\\\\\n")
  }

  cat(str)

  print("----")

  period="1979-2018"
  str=""

  for (river_station in names(meteo)){
    str= paste0(str,meteo[[river_station]]$header$station_id," & & ")

    dat=rivers_data[[(stat[[river_station]])]]$STL$meteo[[river_station]]$TA

    if(period %in% names(dat$lm))
    {
      str=paste0(str,format(round(dat$lm[[period]]$trend*10,2),nsmall=2) ,
                 " (", round(dat$lm[[period]]$trend_std*10,2),")")
    }
    else{
      str=paste0(str,"-")
    }
    str=paste0(str," & ")

    dat=meteo[[river_station]]$DJF$TA
    if(period %in% names(dat$lm))
    {
      str=paste0(str,format(round(dat$lm[[period]]$trend*10,2),nsmall=2) ,
                 " (", format(round(dat$lm[[period]]$trend_std*10,2),nsmall=2),")")
    }
    else{
      str=paste0(str,"-")
    }
    str=paste0(str," & ")

    dat=meteo[[river_station]]$MAM$TA
    if(period %in% names(dat$lm))
    {
      str=paste0(str,format(round(dat$lm[[period]]$trend*10,2),nsmall=2) ,
                 " (", format(round(dat$lm[[period]]$trend_std*10,2),nsmall=2),")")
    }
    else{
      str=paste0(str,"-")
    }
    str=paste0(str," & ")

    dat=meteo[[river_station]]$JJA$TA
    if(period %in% names(dat$lm))
    {
      str=paste0(str,format(round(dat$lm[[period]]$trend*10,2),nsmall=2) ,
                 " (", format(round(dat$lm[[period]]$trend_std*10,2),nsmall=2),")")
    }
    else{
      str=paste0(str,"-")
    }
    str=paste0(str," & ")

    dat=meteo[[river_station]]$SON$TA
    if(period %in% names(dat$lm))
    {
      str=paste0(str,format(round(dat$lm[[period]]$trend*10,2),nsmall=2) ,
                 " (", format(round(dat$lm[[period]]$trend_std*10,2),nsmall=2),")")
    }
    else{
      str=paste0(str,"-")
    }
    str=paste0(str," & ")
    str=paste0(str," & ")

    dat=rivers_data[[(stat[[river_station]])]]$STL$meteo[[river_station]]$P

    if(period %in% names(dat$lm))
    {
      str=paste0(str,format(round(dat$lm[[period]]$trend/
                                    mean(dat$lm[[period]]$values,na.rm=TRUE)*100*10,1),nsmall=1),
                 " (", round(dat$lm[[period]]$trend_std/
                               mean(dat$lm[[period]]$values,na.rm=TRUE)*100*10,1),")")
    }
    else{
      str=paste0(str,"-")
    }
    str=paste0(str," & ")

    dat=meteo[[river_station]]$DJF$P
    if(period %in% names(dat$lm))
    {
      str=paste0(str,format(round(dat$lm[[period]]$trend/
                                    mean(dat$lm[[period]]$values,na.rm=TRUE)*100*10,1),nsmall=1),
                 " (", round(dat$lm[[period]]$trend_std/
                               mean(dat$lm[[period]]$values,na.rm=TRUE)*100*10,1),")")
    }
    else{
      str=paste0(str,"-")
    }
    str=paste0(str," & ")

    dat=meteo[[river_station]]$MAM$P
    if(period %in% names(dat$lm))
    {
      str=paste0(str,format(round(dat$lm[[period]]$trend/
                                    mean(dat$lm[[period]]$values,na.rm=TRUE)*100*10,1),nsmall=1),
                 " (", round(dat$lm[[period]]$trend_std/
                               mean(dat$lm[[period]]$values,na.rm=TRUE)*100*10,1),")")
    }
    else{
      str=paste0(str,"-")
    }
    str=paste0(str," & ")

    dat=meteo[[river_station]]$JJA$P
    if(period %in% names(dat$lm))
    {
      str=paste0(str,format(round(dat$lm[[period]]$trend/
                                    mean(dat$lm[[period]]$values,na.rm=TRUE)*100*10,1),nsmall=1),
                 " (", round(dat$lm[[period]]$trend_std/
                               mean(dat$lm[[period]]$values,na.rm=TRUE)*100*10,1),")")
    }
    else{
      str=paste0(str,"-")
    }
    str=paste0(str," & ")

    dat=meteo[[river_station]]$SON$P
    if(period %in% names(dat$lm))
    {
      str=paste0(str,format(round(dat$lm[[period]]$trend/
                                    mean(dat$lm[[period]]$values,na.rm=TRUE)*100*10,1),nsmall=1),
                 " (", round(dat$lm[[period]]$trend_std/
                               mean(dat$lm[[period]]$values,na.rm=TRUE)*100*10,1),")")
    }
    else{
      str=paste0(str,"-")
    }

    str=paste0(str,"\\\\\n")
  }

  cat(str)
}
