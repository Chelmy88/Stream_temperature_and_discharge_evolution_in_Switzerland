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


#' Add meteo data to SMET_OBJECT
#'
#'Read SMET_OBJECT , returns a SMET_OBJECT with meteo stations indicated in
#' \code{$meteo} added as a SMET_OBJECT object list in \code{$meteo_ts}.
#'
#' @param data SMET_OBJECT containing river data
#' @param meteo_data SMET_OBJECT continning meteo data
#'
#' @return SMET_OBJECT  with meteo added as a SMET_OBJECT object list \code{$meteo_ts}
#' @export
add_meteo <- function(data,meteo_data)
{
  for (station_name in data$meteo){
    for (period in c("daily","monthly","yearly"))
    {
      for(var in names(meteo_data[[station_name]][[period]]))
      {
        data$meteo_ts[[station_name]][[period]][[var]]=meteo_data[[station_name]][[period]][[var]]
        data$meteo_ts[[station_name]]$header=meteo_data[[station_name]]$header
      }
    }
    data$meteo_ts[[station_name]]$data=meteo_data[[station_name]]$data

  }
  return(data)
}


#' Cut timeseries in a SMET_OBJECT
#'
#' Read a SMET_OBJECT, returns a SMET cut between the years indicated by start and end
#' for the inner \code{\link[base]{data.frame}} \code{$data},\code{$daily},
#' \code{$monthly}, and \code{$yearly}
#' @param data SMET_OBJECT
#' @param start Starting year to keep
#' @param end Ending year to keep
#'
#' @return SMET_OBJECT
#' @export

cut_all_data <- function(data,start,end)
{
  years=get_year(data$data$timestamp)
  if(!(start %in% years))
  {
    print("[W] Choosen years to start if before the first complete year")
  }
  if(!(end %in% years))
  {
    print("[W] Choosen years to end if after the last complete year")
  }
  for (period in c("data","daily","monthly","yearly"))
  {
    years=get_year(data[[period]]$timestamp)
    selection=which(years>=start & years<=end)
    data[[period]]=data[[period]][selection,]
  }
  return(data)
}


#' Convert data from SMET_OBJECT to ts objects
#'
#' Read a SMET_OBJECT, returns a list of \code{\link[base]{data.frame}} containing
#' data as \code{\link[stats]{ts}} objects. Input data.frames are split
#' into list of times series, each list entry being one variable.
#' @param data A SMET_OBJECT
#' @return A SMET_OBJECT with all inner \code{\link[base]{data.frame}} transformed
#' into lists of \code{\link[stats]{ts}} objects
#' @export
get_time_series<- function(data)
{
  period=c("daily","monthly","yearly")
  frequency=c(365,12,1)
  data_stl=list()
  for (i in c(1:3))
  {
    data_stl[[period[i]]]=list()
    for(var in names(data[[period[i]]]))
    {
      years=get_year(data[[period[i]]][[var]]$timestamp)
      start=min(years)
      current_data=data[[period[i]]][[var]]$values
      #Get variables, ignore timestamp
      data_stl[[period[i]]][[var]]=ts(current_data,start=c(start,1),frequency=frequency[i])
    }
    for(station_name in names(data$meteo_ts))
    {
      data_stl$meteo_ts[[station_name]][[period[i]]]=list()
      for(var in names(data$meteo_ts[[station_name]][[period[i]]]))
      {
        years=get_year(data$meteo_ts[[station_name]][[period[i]]][[var]]$timestamp)
        start=min(years)
        current_data=data$meteo_ts[[station_name]][[period[i]]][[var]]$values
        #Get variables, ignore timestamp
        data_stl$meteo_ts[[station_name]][[period[i]]][[var]]=ts(current_data,start=c(start,1),frequency=frequency[i])
      }
    }
  }
  return(data_stl)
}


#' Wrapper function
#'
#' Wrapper for \code{\link{cut_all_data}} and \code{\link{add_meteo}} functions
#'
#' @param data A SMET_OBJECT with river data
#' @param meteo_data A SMET_OBJECT with meteo data
#' @param start Starting year to keep
#' @param end Ending year to keep
#'
#' @return SMET_OBJECT cut to the given dates with meteo data added
#' @export
add_meteo_and_trim <- function(data,meteo_data,start,end)
{
  if(!missing(start) & !missing(end) & !is.null(end) & !is.null(start))
  {
    #cut_data from Input_preprocessing.R
    data=cut_all_data(data,start,end)
  }
  if(length(data$meteo)>0)
  {
    data=add_meteo(data,meteo_data)
  }
  return(data)
}



#' Do an additional Loess fitting on the STL analysis
#'
#' Do an additional Loess fitting on the STL analysis seasonnal signal as suggested
#' in R. B. Cleveland, W. S. Cleveland, J.E. McRae, and I. Terpenning (1990)
#' STL: A Seasonal-Trend Decomposition Procedure Based on Loess. Journal
#' of Official Statistics, 6, 3–73
#'
#' @param stl_input List of stl analysis output, keys being the used \code{s.window}
#' values
#' @param s_win Vector of \code{s.window} values for the stl analysis
#' (see \code{link[stats]{stl}})
#'
#' @return List of stl analysis output, keys being the used \code{s.window}
#' values
#' @export

post_season_loess <- function(stl_input,s_win)
{
  stl_output=stl_input
  for (s in s_win)
  {
    x_serie=c(1:length(stl_output[[toString(s)]]$time.series[,"seasonal"]))
    span=7/length(stl_output[[toString(s)]]$time.series[,"seasonal"])
    loess_reg=suppressWarnings(loess(stl_output[[toString(s)]]$time.series[,"seasonal"]~x_serie,span=span,degree=2))
    stl_output[[toString(s)]]$time.series[,"seasonal"]=predict(loess_reg,newdata=x_serie)
    stl_output[[toString(s)]]$time.series[,"remainder"]=stl_output[[toString(s)]]$time.series[,"raw"]-stl_output[[toString(s)]]$time.series[,"seasonal"]-stl_output[[toString(s)]]$time.series[,"trend"]
    for(station_name in names(stl_output$meteo))
    {
      x_serie=c(1:length(stl_output$meteo[[station_name]][[toString(s)]]$time.series[,"seasonal"]))
      loess_reg=suppressWarnings(loess(stl_output$meteo[[station_name]][[toString(s)]]$time.series[,"seasonal"]~x_serie,span=span,degree=2))
      stl_output$meteo[[station_name]][[toString(s)]]$time.series[,"seasonal"]=predict(loess_reg,newdata=x_serie)
      stl_output$meteo[[station_name]][[toString(s)]]$time.series[,"remainder"] = (stl_output$meteo[[station_name]][[toString(s)]]$time.series[,"raw"]
                                                           - stl_output$meteo[[station_name]][[toString(s)]]$time.series[,"seasonal"]
                                                           - stl_output$meteo[[station_name]][[toString(s)]]$time.series[,"trend"])
    }
  }
  return(stl_output)
}



####################################################
################ STL APPLICATION ###################
####################################################

#' Perform STL analysis
#'
#' Perform STL analysis for the given variable and the associated mete variable
#' (air temperature if the variable is water temperature and precipitation if
#' the river variable is discharge)
#' @param station_data A SMET_OBJECT with river data
#' @param meteo_data A SMET_OBJECT with meteo data
#' @param variable Variable on which the STL analysis should be performed
#' @param s_win  vector of \code{s.window} values for the stl analysis
#' (see \code{link[stats]{stl}})
#' @param frequency Frequency of data do be used. "monthly" or "monthly".
#'
#' @return A list where each key is a value of \code{s.window}. The content in
#' each list entry is the output of the STL analysis. In addiditon, the entry
#' \code{$meteo} gives access to a list where keys are the meteo station three
#' capital letters abbreviation and the content is a list with keys \code{s.window}
#' containing the STL analysis for the meteorological variable.
#' @export
get_STL_output <- function(station_data,meteo_data,variable,s_win,frequency)
{
  #Get STL data in time series object
  data_stl<-get_time_series(station_data)
  #Prepare empty data container
  stl_output=list()
  stl_output$meteo=list()

  if (variable=="T")
  {
    meteo_var1="TA_HOM"
    meteo_var2="TA"
  }
  else if (variable=="Q")
  {
    meteo_var1="P_HOM"
    meteo_var2="P"
  }
  else{
    {stop("[E] Wrong variable provided, only Q and T are accepted")}
  }

  for(station_name in station_data$meteo)
  {
    stl_output$meteo[[station_name]]=list()
  }
  # Get STL data
  for (s in s_win)
  {
    stl_output[[toString(s)]]=stl(data_stl[[frequency]][[variable]],s.window=s,na.action = na.approx,robust=TRUE)
    stl_output[[toString(s)]]$time.series=cbind(stl_output[[toString(s)]]$time.series,data_stl[[frequency]][[variable]])
    colnames(stl_output[[toString(s)]]$time.series)=c("seasonal","trend","remainder","raw")

    for(station_name in station_data$meteo)
    {
      meteo_station=data_stl$meteo_ts[[station_name]]
      if(!is.null(meteo_station[[frequency]][[meteo_var1]])){
        stl_output$meteo[[station_name]][[toString(s)]]=stl(meteo_station[[frequency]][[meteo_var1]],s.window=s,na.action = na.approx,robust=TRUE)
        stl_output$meteo[[station_name]][[toString(s)]]$time.series=cbind(stl_output$meteo[[station_name]][[toString(s)]]$time.series,
                                                                          meteo_station[[frequency]][[meteo_var1]])
        colnames(stl_output$meteo[[station_name]][[toString(s)]]$time.series)=c("seasonal","trend","remainder","raw")
      }
      else if(!is.null(meteo_station[[frequency]][[meteo_var2]]))
      {
        stl_output$meteo[[station_name]][[toString(s)]]=stl(meteo_station[[frequency]][[meteo_var2]],s.window=s,na.action = na.approx,robust=TRUE)
        stl_output$meteo[[station_name]][[toString(s)]]$time.series=cbind(stl_output$meteo[[station_name]][[toString(s)]]$time.series, meteo_station[[frequency]][[meteo_var2]])
        colnames(stl_output$meteo[[station_name]][[toString(s)]]$time.series)=c("seasonal","trend","remainder","raw")
      }
      else
      {
        stl_output$meteo[[station_name]]=NULL
        print(paste0("[W] Meteo for station: ",station_name," do not contain the variable: ",meteo_var1," or ",meteo_var2,", it will be removed from STL analysis."))
      }
    }
  }
  return(stl_output)
}


#' Genenral call to perform STL analysis.
#'
#' @param rivers_data A SMET_OBJECT with river data
#' @param meteo_data A SMET_OBJECT with meteo data
#' @param variable Variable on which the STL analysis should be performed
#' @param s_win Vector of \code{s.window} values for the stl analysis
#' (see \code{link[stats]{stl}})
#' @param frequency Frequency of data do be used. "monthly" or "monthly".
#' @param start Year to start the STL analysis (years before are cut).
#' Default \code{NULL}.
#' @param end Year to end the STL analysis (years after are cut).
#' Default \code{NULL}.
#' @return A list where each key the station name and each value is a list.
#' In this list each key is a value of \code{s.window}. The content in
#' each list entry is the output of the STL analysis. In addiditon, the entry
#' \code{$meteo} gives access to a list where keys are the meteo station three
#' capital letters abbreviation and the content is a list with keys \code{s.window}
#' containing the STL analysis for the meteorological variable.
#' @export
get_STL_analysis <- function(rivers_data,meteo_data,variable,s_win,frequency,start=NULL,end=NULL)
{
  stl_output=list()
  if(missing(frequency))
  {stop("Frequency is missing in call to get_STL_output")
  }
  if(frequency!="daily" & frequency!="monthly")
  {stop("Frequency value in call to STL_analysis_explore is:",frequency,". Only 'daily' and 'monthly' are acepted")}

  output=list()
  for (station_name in names(rivers_data))
  {
    print(paste("[I] *** Processing station",station_name,"***"))
    print(paste("[I]    Removing uncomplete or empty years"))
    station_data <- add_meteo_and_trim(rivers_data[[station_name]],meteo_data,start,end)
    title=station_data$header$station_name
    print(paste("[I]    Getting STL"))
    stl_output[["raw"]]<-get_STL_output(station_data,meteo_data,variable,s_win,frequency)
    if(frequency=="daily"){
      print(paste("[I]    Post season loess"))
      stl_output[["post_seasons_loess"]]<-post_season_loess(stl_output[["raw"]],s_win)
    }
    output[[station_name]]=stl_output
  }
  return (output)
}






####################################################
############## REMAINDER ANALYSIS ##################
####################################################

#' Perform analysis of the remainder of the STL
#'
#' Get the ACF and PACF for the remainder of the STL analysis. For meterological
#' stations, the CCF between the hydrological variable and the meteorological
#' variable is also computed.
#'
#' @param STL_output List of output from the STL analysis, where keys are
#' \code{s.window} values.
#' @param s_windows Vector of \code{s.window} values for the stl analysis
#' (see \code{link[stats]{stl}})
#' @param version "raw" or "post_seasons_loess" depending if a "post_seasons_loess"
#' has be applied (whihc is the case when \code{\link{get_STL_analysis}}) is run
#' at daily time scale.
#'
#' @return  aa
#' @export
get_remainder_analysis <- function(STL_output,s_windows,version) {

  if(version!="raw" & version!="post_seasons_loess")
  {stop("[E] Version value in call to get_remainder_analysis is:",version,". Only 'raw' and 'post_seasons_loess' are acepted")}

  for (station_name in names(STL_output))
  {
    print(paste("[I] Doing remainder analysis for station:",station_name))
    if(is.null(STL_output[[station_name]][["remainder_analysis"]])){
      STL_output[[station_name]][["remainder_analysis"]]=list()
    }
    STL_output[[station_name]][["remainder_analysis"]][[version]]=list()

    for (s_value in paste(s_windows)){

      STL_output[[station_name]][["remainder_analysis"]][[version]][[s_value]]=list()

      STL_output[[station_name]][["remainder_analysis"]][[version]][[s_value]][["pacf"]] =
        pacf(as.vector(STL_output[[station_name]][[version]][[s_value]][["time.series"]][,"remainder"]),lag.max = 400,na.action = na.pass,plot=FALSE)

      STL_output[[station_name]][["remainder_analysis"]][[version]][[s_value]][["pacf_mean"]] =
        signif(mean(abs(STL_output[[station_name]][["remainder_analysis"]][[version]][[s_value]][["pacf"]][360:370]$acf)),3)

      STL_output[[station_name]][["remainder_analysis"]][[version]][[s_value]][["acf"]] =
        acf(as.vector(STL_output[[station_name]][[version]][[s_value]][["time.series"]][,"remainder"]),lag.max = 400,na.action = na.pass,plot=FALSE)

      STL_output[[station_name]][["remainder_analysis"]][[version]][[s_value]][["acf_mean"]] =
        signif(mean(abs(STL_output[[station_name]][["remainder_analysis"]][[version]][[s_value]][["acf"]][360:370]$acf)),3)

      STL_output[[station_name]][["remainder_analysis"]][[version]][[s_value]][["meteo"]]=list()

      for (meteo_station in names(STL_output[[station_name]][[version]][["meteo"]]))
      {
        STL_output[[station_name]][["remainder_analysis"]][[version]][[s_value]][["meteo"]][[meteo_station]]=list()

        STL_output[[station_name]][["remainder_analysis"]][[version]][[s_value]][["meteo"]][[meteo_station]][["pacf"]] =
          pacf( as.vector(STL_output[[station_name]][[version]][["meteo"]][[meteo_station]][[s_value]][["time.series"]][,"remainder"]),
                na.action = na.pass,lag.max = 400,plot=FALSE)

        STL_output[[station_name]][["remainder_analysis"]][[version]][[s_value]][["meteo"]][[meteo_station]][["pacf_mean"]] =
          signif(mean(abs(STL_output[[station_name]][["remainder_analysis"]][[version]][[s_value]][["meteo"]][[meteo_station]][["pacf"]][360:370]$acf)),3)

        STL_output[[station_name]][["remainder_analysis"]][[version]][[s_value]][["meteo"]][[meteo_station]][["acf"]] =
          acf( as.vector(STL_output[[station_name]][[version]][["meteo"]][[meteo_station]][[s_value]][["time.series"]][,"remainder"]),
               na.action = na.pass,lag.max = 400,plot=FALSE)

        STL_output[[station_name]][["remainder_analysis"]][[version]][[s_value]][["meteo"]][[meteo_station]][["acf_mean"]] =
          signif(mean(abs(STL_output[[station_name]][["remainder_analysis"]][[version]][[s_value]][["meteo"]][[meteo_station]][["acf"]][360:370]$acf)),3)

        STL_output[[station_name]][["remainder_analysis"]][[version]][[s_value]][["meteo"]][[meteo_station]][["ccf"]] =
          ccf(as.vector(STL_output[[station_name]][[version]][[s_value]][["time.series"]][,"remainder"]),
              as.vector(STL_output[[station_name]][[version]][["meteo"]][[meteo_station]][[s_value]][["time.series"]][,"remainder"]),
              na.action = na.pass,lag.max = 400, plot=FALSE)

        STL_output[[station_name]][["remainder_analysis"]][[version]][[s_value]][["meteo"]][[meteo_station]][["ccf_mean"]] =
          signif(mean(abs(STL_output[[station_name]][["remainder_analysis"]][[version]][[s_value]][["meteo"]][[meteo_station]][["ccf"]][360:370]$acf)),3)
      }
    }
  }
  return(STL_output)
}

#' Title
#'
#' @param STL_output a
#' @param s_windows a
#' @param version a
#' @param name a
#'
#' @return a
#' @export
plot_remainder_analysis <- function(STL_output,s_windows,version,name)
{

  if(version!="raw" & version!="post_seasons_loess")
  {stop("[E] Version value in call to plot_remainder_analysis is:",version,". Only 'raw' and 'post_seasons_loess' are acepted")}

  dir.create("plots/remainder_analysis/", showWarnings = FALSE)

  for (station_name in names(STL_output))
  {
    print(paste("[I] Plotting remainder analysis for station:",station_name))
    pdf(paste0("plots/remainder_analysis/STL_remainder_analysis_",name,"_",version,"_",station_name,".pdf"),width=10,height=6)


    coeffs=data.frame(s_window=s_windows)

    acf=c()
    pacf=c()
    #Get data for rivers station
    for (s_value in paste(s_windows))
    {
      acf=c(acf,STL_output[[station_name]][["remainder_analysis"]][[version]][[s_value]][["acf_mean"]])
      pacf=c(pacf,STL_output[[station_name]][["remainder_analysis"]][[version]][[s_value]][["pacf_mean"]])
    }
    coeffs$acf=acf
    coeffs$pacf=pacf

    #Get data for meteo stations
    for (meteo_station in names(STL_output[[station_name]][[version]][["meteo"]]))
    {
      acf=c()
      pacf=c()
      ccf=c()
      for (s_value in paste(s_windows))
      {
        acf=c(acf, STL_output[[station_name]][["remainder_analysis"]][[version]][[s_value]][["meteo"]][[meteo_station]][["acf_mean"]])
        pacf=c(pacf, STL_output[[station_name]][["remainder_analysis"]][[version]][[s_value]][["meteo"]][[meteo_station]][["pacf_mean"]])
        ccf=c(ccf, STL_output[[station_name]][["remainder_analysis"]][[version]][[s_value]][["meteo"]][[meteo_station]][["ccf_mean"]])
      }

      coeffs[,paste0(meteo_station,"_acf")]<-acf
      coeffs[,paste0(meteo_station,"_pacf")]<-pacf
    }

    #Plot coeffs data
    col=1
    par(mfrow=c(1,1),mar=c(3,3,3,1),oma=c(0,0,0,0),mgp=c(1.7,0.5,0))
    plot(coeffs$s_window,coeffs$acf,type='n',main=paste("Summary:", station_name),xlab="s value",ylab="PACF/ACF",ylim=c(0,max(coeffs[,2:length(coeffs)])))
    for (i in c(2:length(coeffs)))
    {
      lines(coeffs$s_window,coeffs[,i],col=i-1)
    }
    legend("top",legend=colnames(coeffs)[2:length(coeffs)],col=c(1:(length(coeffs)-1)),bty='n',ncol=4,lty=1)
    abline(h=1.96/sqrt(length(STL_output[[station_name]][[version]][[s_value]][["time.series"]][,"remainder"])-365))

    #Required to print the table before plots
    plot.new()
    grid.table(coeffs,rows=NULL)
    mtext(paste("Summary:", station_name))

    #De separate eplot for each station and s_value
    for (s_value in paste(s_windows))
    {
      par(mfrow=c(2,1),mar=c(1.5,1.5,3,1),oma=c(0,0,2,0),mgp=c(1.7,0.5,0))
      title=paste(station_name,"S-value:",s_value)
      plot(STL_output[[station_name]][[version]][[s_value]][["time.series"]],main=title)

      plot(STL_output[[station_name]][["remainder_analysis"]][[version]][[s_value]][["pacf"]],
           main=paste("PACF",STL_output[[station_name]][["remainder_analysis"]][[version]][[s_value]][["pacf_mean"]]))
      rect(360, -1, 370, 1, density = NULL, angle = 45,col = rgb(0.5,0.5,0.5,0.5), border = NA, lty = par("lty"), lwd = par("lwd"))
      mtext(title,outer=TRUE)

      plot(STL_output[[station_name]][["remainder_analysis"]][[version]][[s_value]][["acf"]],
           main=paste("ACF",STL_output[[station_name]][["remainder_analysis"]][[version]][[s_value]][["acf_mean"]]))
      rect(360, -1, 370, 1, density = NULL, angle = 45,col = rgb(0.5,0.5,0.5,0.5), border = NA, lty = par("lty"), lwd = par("lwd"))
      mtext(title,outer=TRUE)

      for (meteo_station in names(STL_output[[station_name]][[version]][["meteo"]]))
      {
        par(mfrow=c(3,1),mar=c(1.5,1.5,3,1),oma=c(0,0,2,0),mgp=c(1.7,0.5,0))
        title=paste(station_name,"-",meteo_station,"S-value:",s_value)
        plot(STL_output[[station_name]][[version]][["meteo"]][[meteo_station]][[s_value]][["time.series"]],main=title)

        plot(STL_output[[station_name]][["remainder_analysis"]][[version]][[s_value]][["meteo"]][[meteo_station]][["pacf"]],
             main=paste("PACF",STL_output[[station_name]][["remainder_analysis"]][[version]][[s_value]][["meteo"]][[meteo_station]][["pacf_mean"]]))

        plot(STL_output[[station_name]][["remainder_analysis"]][[version]][[s_value]][["meteo"]][[meteo_station]][["acf"]],
             main=paste("ACF",STL_output[[station_name]][["remainder_analysis"]][[version]][[s_value]][["meteo"]][[meteo_station]][["acf_mean"]]))

        plot(STL_output[[station_name]][["remainder_analysis"]][[version]][[s_value]][["meteo"]][[meteo_station]][["ccf"]],
             main=paste("CCF",STL_output[[station_name]][["remainder_analysis"]][[version]][[s_value]][["meteo"]][[meteo_station]][["ccf_mean"]]))

        mtext(title,outer=TRUE)
      }
    }
    dev.off()
  }
}

