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


#' Check that variable and cut them to the same length
#'
#' Check that both water temperature (T) and discharge (Q) are provided and
#' cut the daily means of the two variables to have the same start and end
#' @param data A SMET_OBJECT obtained through the function
#' \code{\link{get_file_data}} containing the data of one river station
#'
#' @return A SMET_OBJECT with only the \code{$data} part filled with the cut
#' daily means T and Q time series.
#' @export
#'
check_and_cut_variables <- function(data)
{
  if(!("T" %in% names(data$daily) && "Q" %in% names(data$daily)))
  {
    stop("[E] Data must contain T and Q cloumn")
  }
  start=(max(min(data$daily$T$timestamp),min(data$daily$Q$timestamp)))
  stop=(min(max(data$daily$T$timestamp),max(data$daily$Q$timestamp)))
  dat=list()
  dat[["data"]]=list()
  dat$data=data.frame("timestamp"=data$daily$T$timestamp[which(data$daily$T$timestamp>=start & data$daily$T$timestamp<=stop)],
  "T"=data$daily$T$values[which(data$daily$T$timestamp>=start & data$daily$T$timestamp<=stop)],
  "Q"=data$daily$Q$values[which(data$daily$Q$timestamp>=start & data$daily$Q$timestamp<=stop)])

  return(dat)
}


#' Compute the mean of Julian days value over a period
#'
#' Compute the day-of-the-yaer mean over a period of a given length in year.
#' The day-of-the-yaer mean is the mean between each first of jannuary, each
#' second of jannuary, and-so-on. The periods are tkaen from the end of the available
#' data and as many periods as possible are taken.
#' @param data A SMET_OBJECT obtained through the function
#' \code{\link{get_file_data}} containing the data of one river station
#' @param period An integer representing the number of year over whihch
#' the average should be computed
#'
#' @return A list with the period as key. Periods have the format "AAAA-BBBB" where
#' AAAA is the starting year and BBBB the ending year. Each list element contains
#' a \code{\link[base]{data.frame}} with columns T and Q, which are  the day-of-the-yaer mean over the period
#' for the water temperature and the discharge.
#'
#' @export

compute_daily_means_over_period <- function(data,period)
{
  years=unique(get_year(data$data$timestamp))
  daily_means=list()
  num_decades=floor(length(years)/period-1)
  for(i in c(0:num_decades)){
    end_year=years[length(years)]-i*period
    start_year=end_year-period+1
    period_name=paste0(start_year,"_",end_year)
    daily_means_temp_T=matrix(0, nrow=365,ncol=period)
    daily_means_temp_Q=matrix(0, nrow=365,ncol=period)
    for(j in c(1:(period)))
    {
      year_mean=daily_mean_for_year(data,start_year+j-1)
      daily_means_temp_T[,j]=year_mean[,1]
      daily_means_temp_Q[,j]=year_mean[,2]
    }
    daily_means[[period_name]]=data.frame(T=rowMeans(daily_means_temp_T,na.rm = T),Q=rowMeans(daily_means_temp_Q,na.rm = T))
  }
  return(daily_means)
}


#' Compute a circular moving window average over all columns of a
#' \code{\link[base]{data.frame}} containing daily data over a year (365 data)
#'
#' @param year_daily_means A \code{\link[base]{data.frame}} with numeric values
#' containing daily data over a year (365 data)
#' @param smooth_time The window to be used for the moving average
#'
#' @return The \code{\link[base]{data.frame}} smoothed
smooth_circular <- function(year_daily_means,smooth_time)
{
  half_time=smooth_time/2
  smoothed_year=year_daily_means
  for (i in 1:365)
  {
    if((i-half_time)<1){
      j_start_1=365-(half_time-i)
      j_end_1=365
      j_start_2=1
      j_end_2=i+half_time
      mean=colMeans(rbind(year_daily_means[j_start_1:j_end_1,],year_daily_means[j_start_2:j_end_2,]))
    }
    else if((i+half_time)>365){
      j_start_1=i-half_time
      j_end_1=365
      j_start_2=1
      j_end_2=(half_time+i)-365
      mean=colMeans(rbind(year_daily_means[j_start_1:j_end_1,],year_daily_means[j_start_2:j_end_2,]))
    }
    else
    {
      mean=colMeans(year_daily_means[(i-half_time):(i+half_time),])
    }
    smoothed_year[i,]=mean
  }
  return(smoothed_year)
}


#' Function wrapper for \code{\link{smooth_circular}}
#'
#' Wrapper for \code{\link{smooth_circular}} to call it over all the periods
#' defined in \code{\link{compute_daily_means_over_period}}
#'
#' @param daily_means A list of \code{\link[base]{data.frame}}.
#' Each \code{\link[base]{data.frame}} is made of numeric values and
#' contains daily data over a year (365 data)
#' @param smooth_time The window to be used for the moving average
#'
#' @return A list where each key is one time period. List emntries are
#' \code{\link[base]{data.frame}} of daily mean data circularly smoothed
#' over the period used as key. Periods have the format "AAAA-BBBB" where
#' AAAA is the starting year and BBBB the ending year
#' @export
smooth_daily_means_over_period <- function(daily_means,smooth_time)
{
  smoothed_daily_mean=list()
  for (period in names(daily_means))
  {
    smoothed_daily_mean[[period]]=smooth_circular(daily_means[[period]],smooth_time)
  }
  return(smoothed_daily_mean)
}

#' Produce hysteresis data
#'
#' Read data from a list of SMET_OBJECT, which are obtained through the function
#' \code{\link{get_file_data}} and contain the data of one river station. It keeps
#' only subsequent years and compute necessary values for hysteresis plots
#' (daily means over years and smoothed daily means over years)
#
#' @param rivers_data A list of SMET_OBJECT, which are obtained through the function
#' \code{\link{get_file_data}} and contain the data of one river station.
#' @param period The lenght (in year) of the periods over which the hystheresis
#' data should be computed
#' @param smoothing The length (in day) of the moving average window to
#' be applied
#'
#' @return The input SMET_OBJECT whih a new list entry, "hysteresis", containing
#' the hysteresis data. The hysteresyis data are discharge and temperature values,
#' averaged for each day of the year separately over various periods and smoothed
#' with a circular moving average window. The new "hysteresis" antry of the
#' SMET_OBJECT contains a list where the keys are the periods (in the format
#' "AAAA-BBBB" where AAAA is the starting year and BBBB the ending year) and the
#' associated data are a \code{\link[base]{data.frame}} containing discharge and
#' temperature values.
#' @export
#'

get_hysteresis_data <- function(rivers_data,period,smoothing)
{
  for (i in c(1:length(rivers_data)))
  {
    station=rivers_data[[i]]
    name=names(rivers_data)[[i]]
    print(paste("[I] ***** Processing station:",station$header$station_name,"*****"))
    data_cut_var=check_and_cut_variables(station)
    daily_means_over_period=compute_daily_means_over_period(data_cut_var,period)
    daily_means_smoothed_over_period=smooth_daily_means_over_period(daily_means_over_period,smoothing)
    hysteresis=(list("daily_mean"=daily_means_over_period,"daily_mean_smoothed"=daily_means_smoothed_over_period))
    rivers_data[[i]][["hysteresis"]]=hysteresis
  }
  return(rivers_data)
}

