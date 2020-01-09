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


# This material is distributed under the GPLv3 license
# (https://www.gnu.org/licenses/gpl-3.0.html)

#' Read SMET file, returns a SMET object (a list)
#'
#' Read SMET file, returns a SMET object (a list). The SMET_OBJECT has
#' the following structure:
#' \describe{
#' \item{\code{$header}}{The header information, based on smet header value}
#' \item{\code{$data}}{A \code{\link[base]{data.frame}} containing read data, column names from SMET header,
#' timestamp as PosixCT}
#' }
#' @param file_name String containing the path th file to read
#'
#' @return A SMET_OBJECT containing the data
#' @export
read_smet <- function(file_name)
{
  print(paste("[I] Reading header data for:",file_name))
  header_file = file(file_name, "r")
  lines = readLines(header_file, n = 40)
  header=list()
  for (line in lines){
    if (startsWith(line,"SMET") || startsWith(line,"[HEADER]"))
    {
      next
    }
    else if (startsWith(line,"[DATA]"))
    {
      break
    }
    l<-strsplit(line,"=")[[1]]
    suppressWarnings(header[[trim(l[1])]] <- ifelse(is.na(as.numeric(trim(l[2]))),trim(l[2]),as.numeric(trim(l[2]))))
  }
  close(header_file)

  print(paste("[I] Reading content data for:",file_name))
  data=data.frame(fread(file_name,header=FALSE))
  fields=strsplit(header$fields,' ')[[1]]
  if(length(fields)<2)
  {
      fields=strsplit(header$fields,'\t')[[1]]
  }
  colnames(data)=fields
  data[data==-999] <- NaN
  data$timestamp = as.POSIXct(strptime(data$timestamp, "%Y-%m-%dT%H:%M"), tz="GMT")
  return (list(header=header, data=data))
}


#' Cut SMET_OBJECT to only keep complete years
#'
#' Cut SMET_OBJECT to only keep complete years (partial years at the beggining
#' or at the end of the time series are removed)
#' @param data SMET_OBJECT
#'
#' @return SMET_OBJECT
#' @export
cut_full_year <- function(data)
{
  ###Remove uncomplete year
  time_step<-get_timestep(data)
  years<-unique(get_year(data$data$timestamp))
  for(y in years)
  {
    l=length(which(get_year(data$data$timestamp)==y))
    if(l!=365*24*3600/time_step && l!=366*24*3600/time_step)
    {
      print(paste("[W]  Year:",y,"is incomplete, it will be removed"))
      data$data=data$data[which(get_year(data$data$timestamp)!=y),]
    }
  }

  return(data)
}


#' Remove 29th of february from a SMET_OBJECT
#'
#' @param data SMET_OBJECT
#'
#' @return SMET_OBJECT with 29th of February removed
#' @export
remove_bissextile <- function(data)
{
  data$data=data$data[which(get_month(data$data$timestamp)!=2 | get_day(data$data$timestamp)!=29),]
  return(data)
}

#' Filter tiem series for complete years
#'
#' Remove years starting or ending with NaN, kepps only subsequent years,
#' starting from the end of the timeseries.
#'
#' @param data A SMET_OBJECT
#'
#' @return A SMET_OBJECT
#' @export
keep_subs_years_preprocessing <- function(data){
  ###
  years<-unique(get_year(data$data$timestamp))
  data$cut=list()

  for(n in names(data$data)[2:length(names(data$data))])
  {
    threshold=20
    if(get_timestep(data)==86400)
    {
      threshold=20
    }
    else if(get_timestep(data)==3600)
    {
      threshold=20*24
    }

    yrs=years
    for(y in years)
    {
      yr=data$data[which(get_year(data$data$timestamp)==y),]

      if(length(which(is.nan(yr[[n]])))>0)
      {
        if(length(which(is.nan(yr[[n]][1:threshold])))==threshold){
          print(paste("[W] Year:",y," is starting with ",threshold," or more NaNs for variable",n,", it will be removed"))
          yrs=yrs[which(yrs!=y)]
        }
        else if(length(which(is.nan(yr[[n]][(length(yr[[n]])-(threshold-1)):length(yr[[n]])])))==threshold )
        {
          print(paste("[W] Year:",y," is ending with ",threshold," or more NaNs for variable",n,", it will be removed"))
          yrs=yrs[which(yrs!=y)]
        }
      }
    }
    if(length(yrs)>=10){
      ### Check for series of subsequent years
      series=list()
      i=1
      prev_year=yrs[1]
      current_seq=c(yrs[1])
      for (y in yrs[2:length(yrs)])
      {
        if(y!=prev_year+1)
        {
          series[[i]]=list(length=length(current_seq),seq=current_seq)
          i=i+1
          current_seq=c(y)
        }
        else
        {
          current_seq=c(current_seq,y)
        }

        prev_year=y
      }
      series[[i]]=list(length=length(current_seq),seq=current_seq)
      imax=0
      lenmax=0
      for (i in 1:length(series))
      {
        if(series[[i]]$length>lenmax)
        {
          lenmax=series[[i]]$length
          imax=i
        }
      }
      imax=length(series)
      ### Keep only the last sequence of subsequent years
      if(length(series[[imax]]$seq)<length(years)){
        print(paste("[W] Years gap in the time serie for var",n,", the year kept are: "))
        print(series[[imax]]$seq)
      }
      if(length(series[[imax]]$seq)>=10){
        values=data$data[[n]][which(get_year(data$data$timestamp) %in% series[[imax]]$seq)]
        timestamp=data$data$timestamp[which(get_year(data$data$timestamp) %in% series[[imax]]$seq)]
        data$cut[[n]]=data.frame(timestamp, values)
      }
      else{
        print(paste("[W] Less than 10 years available for var",n,",it will be removed"))
      }
    }
    else{
      print(paste("[W] Less than 10 years available for var",n,",it will be removed"))
    }

  }
  return(data)
}



#' Returns the timestep of the timeseries in a SMET_OBJECT
#'
#' Returns the timestep of the timeseries in a SMET_OBJECT. An error is thrown
#' if the timestep is not constant
#' @param data SMET_OBJECT
#'
#' @return Timestep of the timeseries
#' @export
get_timestep <- function(data)
{
  len=length(data$data$timestamp)
  t_s=as.numeric(data$data$timestamp[2:len]-data$data$timestamp[1:len-1],units="secs")
  time_step=sort(unique(t_s))
  if( (length(time_step)==2 && (time_step[2]!=2*time_step[1] && time_step[2]!=25*time_step[1]) ) || length(time_step)>2)
  {
    stop("[E] Time step not constant in input file! For now, this software only works with constant time step!")
  }
  return(time_step[1])
}


#' Return a SMET_OBJECT with daily, monthly, seasonnal, and yerly means added
#'
#' Return a SMET_OBJECT with daily, monthly, seasonnal and yearly means added.
#' The\code{\link[base]{data.frame}} added are \code{$daily} (daily means),
#' \code{$monthly} (monthly means), \code{$yearly} (yearly means),
#' \code{$DJF} (winter means), \code{$MAM} (spring means),
#' \code{$JJA} (summer means), and  \code{$SON} (fall means)
#'
#' @param data SMET_OBJECT
#'
#' @return SMET_OBJECT
#' @export
add_means <- function(data)
{
  #Locally store data to average
    for (var in names(data$cut)){
    #Store colname to put it to newly created data.frame
      dat=data$cut[[var]]

      c_names=colnames(dat)

      start_year=get_year(dat$timestamp[1])
      end_year=get_year(dat$timestamp[length(dat$timestamp)])
      start_date=as.Date(paste0(start_year,"-1-1"),tz="GMT")
      #Add one year to prepare dates for the last year
      end_date=as.Date(paste0(end_year+1,"-1-1"),tz="GMT")

      #Creates a sequence of daily dates at witch means have to be computed
      day_seq=as.POSIXct(strptime(seq(start_date, end_date,by="days"), "%Y-%m-%d"),
                         tz="GMT")
      #Remove bisextil
      day_seq=day_seq[which(get_month(day_seq)!=2 | get_day(day_seq)!=29)]
      day_seq=day_seq[1:length(day_seq)-1] #Remove the added year
      #Creates a sequence of montly dates at wich means have to be computed
      month_seq=as.POSIXct(strptime(seq(start_date, end_date,by="months"), "%Y-%m-%d"),
                           tz="GMT")+15*24*60*60
      month_seq=month_seq[1:length(month_seq)-1] #Remove the added year
      #Creates a sequence of yearly dates at wich means have to be computed
      year_seq=as.POSIXct(strptime(seq(start_date, end_date,by="years"), "%Y-%m-%d"),
                          tz="GMT")+183*24*60*60
      year_seq=year_seq[1:length(year_seq)-1] #Remove the added year

      #Prepare set of unique field for aggregation
      dat[["DOY"]]=as.vector(as.character(strftime(dat$timestamp, format = "%j",tz="GMT")))
      dat[["YR"]]=as.character(strftime(dat$timestamp, format = "%Y",tz="GMT"))
      dat[["MON"]]=as.character(strftime(dat$timestamp, format = "%m",tz="GMT"))
      dat[["DOY"]]=paste0(dat[["YR"]],dat[["MON"]],dat[["DOY"]])
      dat[["MON"]]=paste0(dat[["YR"]],dat[["MON"]])

      #Aggregate data (ignore added column for the aggregated output)
      day_means <- aggregate(x = dat[,c(2:(length(dat)-3))],
                        by = list(unique.values = dat[["DOY"]]),
                        FUN = mean,na.rm=TRUE, na.action=NULL)
      month_means <- aggregate(x = dat[,c(2:(length(dat)-3))],
                             by = list(unique.values = dat[["MON"]]),
                             FUN = mean,na.rm=TRUE, na.action=NULL)
      year_means <- aggregate(x = dat[,c(2:(length(dat)-3))],
                             by = list(unique.values = dat[["YR"]]),
                             FUN = mean,na.rm=TRUE, na.action=NULL)

      #Add new data.frame and set correct column names
      data[["daily"]][[var]]=data.frame(timestamp=day_seq,
                                        day_means[,2:(length(day_means))])
      colnames(data[["daily"]][[var]])=c_names
      data[["monthly"]][[var]]=data.frame(timestamp=month_seq
                                          ,month_means[,2:(length(month_means))])
      colnames(data[["monthly"]][[var]])=c_names
      data[["yearly"]][[var]]=data.frame(timestamp=year_seq,
                                         year_means[,2:(length(year_means))])
      colnames(data[["yearly"]][[var]])=c_names

      #ADD seasonnal mean
      seasons=c("DJF","DJF","MAM","MAM","MAM","JJA","JJA","JJA","SON","SON","SON","DJF")
      dat=data$monthly[[var]]
      dat[["YR"]]=as.character(as.numeric(
        strftime(dat$timestamp, format = "%Y",tz="GMT")) +
          ifelse(as.numeric(strftime(dat$timestamp, format = "%m",tz="GMT"))==12,1,0))
      dat[["SEAS"]]=paste0(seasons[as.numeric(strftime(dat$timestamp,
                                                       format = "%m",tz="GMT"))],dat[["YR"]])
      seas_means <- aggregate(x = dat[,c(1:(length(dat)-2))],
                              by = list(unique.values = dat[["SEAS"]]),
                              FUN = mean,na.rm=TRUE, na.action=NULL)

      data[["DJF"]][[var]]=data.frame(seas_means[grepl("DJF", seas_means$unique.values),
                                                 2:(length(seas_means))])
      data[["DJF"]][[var]]=data[["DJF"]][[var]][c(1:length(data[["DJF"]][[var]]$timestamp)-1),]
      colnames(data[["DJF"]][[var]])=c_names

      data[["MAM"]][[var]]=data.frame(seas_means[grepl("MAM", seas_means$unique.values),
                                                 2:(length(seas_means))])
      colnames(data[["MAM"]][[var]])=c_names

      data[["JJA"]][[var]]=data.frame(seas_means[grepl("JJA", seas_means$unique.values),
                                                 2:(length(seas_means))])
      colnames(data[["JJA"]][[var]])=c_names

      data[["SON"]][[var]]=data.frame(seas_means[grepl("SON", seas_means$unique.values),
                                                 2:(length(seas_means))])
      colnames(data[["SON"]][[var]])=c_names
    }
    return(data)
}


#' Read a SMET_OBJECT object, returns a SMET cut between the years indicated by start and end
#'
#' @param data A SMET_OBJECT
#' @param start Starting year to keep
#' @param end Ending year to keep
#'
#' @return A SMET_OBJECT where timeseries are cut between start and end
cut_data <- function(data,start,end)
{
  years=get_year(data$data$timestamp)
  selection=which(years>=start & years<=end)
  data$data=data$data[selection,]
  return(data)
}


#' Read data from a SMET file, returns a SMET_OBJECT
#'
#' Read data from a SMET file, returns a SMET_OBJECT with only full years,
#' bisssextile days removed, daily, monthly, seasonal, and yearly means computed
#' optionally cut between indicated years, and optoptionally associated with a list
#' of meteo stations.
#'
#' The inner data are \code{$header} (SMET header),\code{$data} (raw data),
#'\code{$daily} (daily means), \code{$monthly} (monthly means), \code{$yearly} (yearly means),
#' \code{$DJF} (winter means), \code{$MAM} (spring means),
#' \code{$JJA} (summer means), and\code{$SON} (fall means). The meteo station
#' list is accissible through \code{$meteo}.
#'
#' @param file Path of the SMET file to be loaded
#' @param start Starting year to keep (default \code{NULL})
#' @param end Ending year to keep(default \code{NULL})
#' @param meteo Three capital letters apreviation of the associated meteoSwiss
#' stations (default \code{NULL})
#'
#' @return SMET_OBJECT
#' @export
get_file_data <- function(file,start=NULL,end=NULL,meteo=NULL)
{

  raw_data<-read_smet(file)
  raw_data_cut<-cut_full_year(raw_data)
  if(!is.null(start) & !is.null(end))
  {
    raw_data_cut<-cut_data(raw_data_cut,start,end)
  }
  raw_data_no_bis<-remove_bissextile(raw_data_cut)
  raw_data_no_bis_cut<-keep_subs_years_preprocessing(raw_data_no_bis)
  raw_data_no_bis_means<-add_means(raw_data_no_bis_cut)
  if(!is.null(meteo))
  {
    raw_data_no_bis_means[["meteo"]]<-meteo
  }

  return(raw_data_no_bis_means)
}

#' Read data from a SMET file, returns a SMET_OBJECT without means computed
#'
#' Same as \code{\link{get_file_data}} but does not add the mean values.
#' @param file Path of the SMET file to be loaded
#' @param start Starting year to keep (default \code{NULL})
#' @param end Ending year to keep (default \code{NULL})
#' @param meteo Three capital letters apreviation of the associated meteoSwiss
#' stations (default \code{NULL})
#'
#' @return A list of SMET object continning the data. The keys are the input files
#' names (without extension).
#' @export
get_file_data_only <- function(file,start=NULL,end=NULL,meteo=NULL)
{
  raw_data<-read_smet(file)
  raw_data_cut<-cut_full_year(raw_data)
  if(!is.null(start) & !is.null(end))
  {
    raw_data_cut<-cut_data(raw_data_cut,start,end)
  }
  raw_data_no_bis<-remove_bissextile(raw_data_cut)
  raw_data_no_bis_cut<-keep_subs_years_preprocessing(raw_data_no_bis)
  return(raw_data_no_bis_cut)
}



#' Read raw of river or meteo data
#'
#' Read raw of river or meteo data. Data can be read from files or from RDS data.
#' All files to be read must be SMET files (see
#' \url{https://models.slf.ch/docserver/meteoio/SMET_specifications.pdf}).
#' Returns a list of SMET_OBJECT (see \code{\link{get_file_data}}), each entry
#' corresponding to one station.
#'
#' @param list A list of lists. Each inner list contains the name of the file
#' where the remperature and discharge data, or the meteoSwiss data, are located.
#' If data to load are temperature and discharge data, a vector containing the
#' three capital letters apreviation of the associated meteoSwiss stations must
#' also be in the inner lists for each station.
#' @param path The relative path the prepend to the files to be read.
#' @param type Either \code{"WATER"} or \code{"METEO"} to specify if the data to be
#' loaded are meteoSwiss data
#' @param RData A \code{RDS} objec tto be loaded. If provided data won't be loaded
#' from SMET files
#'
#' @return A list of SMET object continning the data. The keys are the input files
#' names (without extension).
#' @export
get_data <- function(list,path,type,RData=FALSE)
{
  if(RData)
  {
    if(type=="WATER"){
      data=readRDS("rds_data/rivers.RDS")
    }
    else if(type=="METEO"){
     data=readRDS("rds_data/meteo.RDS")
    }
    else{
      stop(paste0("[E] Data type must me either 'WATER' ot 'METEO': '",type,"' was provided."))
    }
    return(data)
  }

  data=list()
  for (station in list)
  {
    if(type=="WATER"){
      name=gsub(".smet","",station$name)
      data[[name]]<-get_file_data(paste0(path,station$name),1900,2019,station$meteo)
    }
    else if(type=="METEO"){
      name=gsub(".smet","",station$name)
      data[[name]]<-get_file_data(paste0(path,station$name))
    }
    else{
      stop(paste0("[E] Data type must me either 'WATER' ot 'METEO': '",type,"' was provided."))
    }
  }



  return(data)
}
