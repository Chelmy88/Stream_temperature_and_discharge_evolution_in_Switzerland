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


#' Produce tend robustness plots for T, Q, TA and P.
#'
#' This function produces the trend robustness plots by using the robust liner model
#' function (rlm) from the MASS package (Figures S9 and S10 in supplementary) and
#' by using trends when removing one year at the begining or end of the time series
#' (Figures S11 and S12 in supplementary).
#' Figures are saved in plots/trend_robustness.pdf
#'
#' @param rivers_data The dataset of rivers data
#'
#' @export

plot_trends_robustness<-function(rivers_data)
{
  #Periods for the analysis
  periods=list(c(1999,2018),c(1999,2017),c(2000,2018),
               c(1979,2018),c(1979,2017),c(1980,2018))

  # List to store the newly computed trends
  rivers_data_tmp=list()

  # Compute the trends gor the given periods
  get_lm_STL <- function(station)
  {
    lm=list()
    for(p in periods){
      period=paste0(p[1],"-",p[2])
      sel=which(station$timestamp>=p[1] & station$timestamp<(p[2]+1))
      len=(p[2]-p[1]+1)
      if(length(sel)==len*365|| length(sel)==(len-1)*365 || length(sel)==(len-2)*365 ){
        lin_mod=lm(station$trend[sel] +station$remainder[sel] ~ station$timestamp[sel])
        lm[[period]]=get_lm_summary(lin_mod)
        lm[[period]][["timestamp"]]=station$timestamp[sel]
        lm[[period]][["values"]]=station$trend[sel] +station$remainder[sel]
        lm[[period]][["printable"]]=get_lm_summary_printable(lin_mod)
      }
    }
    return(lm)
  }

  get_rlm_STL <- function(station)
  {
    lm=list()
    for(p in periods){
      period=paste0(p[1],"-",p[2])
      lm[[period]]=list()
      sel=which(station$timestamp>=p[1] & station$timestamp<(p[2]+1))
      len=(p[2]-p[1]+1)
      if(length(sel)==len*365|| length(sel)==(len-1)*365 || length(sel)==(len-2)*365 ){
        lin_mod=rlm(station$trend[sel] +station$remainder[sel] ~ station$timestamp[sel])
        lm[[period]][["intercept"]]=coef(summary(lin_mod))[1,1]
        lm[[period]][["trend"]]=coef(summary(lin_mod))[2,1]
        lm[[period]][["timestamp"]]=station$timestamp[sel]
        lm[[period]][["values"]]=station$trend[sel] +station$remainder[sel]
      }
    }
    return(lm)
  }

  for (station in names(rivers_data))
  {
    print(paste("Processing station:",station))
    rivers_data_tmp[[station]][["STL"]][["T"]][["lm"]]=get_lm_STL(rivers_data[[station]][["STL"]][["T"]])
    rivers_data_tmp[[station]][["STL"]][["Q"]][["lm"]]=get_lm_STL(rivers_data[[station]][["STL"]][["Q"]])
    rivers_data_tmp[[station]][["STL"]][["T"]][["rlm"]]=get_rlm_STL(rivers_data[[station]][["STL"]][["T"]])
    rivers_data_tmp[[station]][["STL"]][["Q"]][["rlm"]]=get_rlm_STL(rivers_data[[station]][["STL"]][["Q"]])
    for (meteo_station in names(rivers_data[[station]][["meteo"]]))
    {
      rivers_data_tmp[[station]][["STL"]][["meteo"]][[meteo_station]][["TA"]][["lm"]]=
        get_lm_STL(rivers_data[[station]][["STL"]][["meteo"]][[meteo_station]][["TA"]])
      rivers_data_tmp[[station]][["STL"]][["meteo"]][[meteo_station]][["P"]][["lm"]]=
        get_lm_STL(rivers_data[[station]][["STL"]][["meteo"]][[meteo_station]][["P"]])
      rivers_data_tmp[[station]][["STL"]][["meteo"]][[meteo_station]][["TA"]][["rlm"]]=
        get_rlm_STL(rivers_data[[station]][["STL"]][["meteo"]][[meteo_station]][["TA"]])
      #rivers_data_tmp[[station]][["STL"]][["meteo"]][[meteo_station]][["P"]][["rlm"]]=
      #  get_rlm_STL(rivers_data[[station]][["STL"]][["meteo"]][[meteo_station]][["P"]])
    }
  }


  # Group trends values for the two main periods
  periods=list(c("1999-2018"),c("1999-2017"),c("2000-2018"))
  lm_p1=list()
  lm_p1[["T"]]=list()
  lm_p1[["TA"]]=list()
  lm_p1[["P"]]=list()
  lm_p1[["Q"]]=list()
  rlm_p1=list()
  rlm_p1[["T"]]=list()
  rlm_p1[["TA"]]=list()
  rlm_p1[["P"]]=list()
  rlm_p1[["Q"]]=list()
  for (p in periods)
  {
    lm_p1[["T"]][[p]]=c()
    lm_p1[["TA"]][[p]]=c()
    lm_p1[["P"]][[p]]=c()
    lm_p1[["Q"]][[p]]=c()
    rlm_p1[["T"]][[p]]=c()
    rlm_p1[["TA"]][[p]]=c()
    rlm_p1[["P"]][[p]]=c()
    rlm_p1[["Q"]][[p]]=c()
  }
  for (station in names(rivers_data))
  {
    for (p in periods)
    {
      if(p %in% names(rivers_data_tmp[[station]][["STL"]][["T"]][["lm"]]) &&
         p %in% names(rivers_data_tmp[[station]][["STL"]][["Q"]][["lm"]]))
      {
        lm_p1[["T"]][[p]]=c(lm_p1[["T"]][[p]],rivers_data_tmp[[station]][["STL"]][["T"]][["lm"]][[p]]$trend*10)
        lm_p1[["Q"]][[p]]=c(lm_p1[["Q"]][[p]],rivers_data_tmp[[station]][["STL"]][["Q"]][["lm"]][[p]]$trend/
                                              mean(rivers_data_tmp[[station]][["STL"]][["Q"]][["lm"]][[p]]$values,na.rm=TRUE)*10*100)
        rlm_p1[["T"]][[p]]=c(rlm_p1[["T"]][[p]],rivers_data_tmp[[station]][["STL"]][["T"]][["rlm"]][[p]]$trend*10)
        rlm_p1[["Q"]][[p]]=c(rlm_p1[["Q"]][[p]],rivers_data_tmp[[station]][["STL"]][["Q"]][["rlm"]][[p]]$trend/
                              mean(rivers_data_tmp[[station]][["STL"]][["Q"]][["rlm"]][[p]]$values,na.rm=TRUE)*10*100)
        T=c()
        P=c()
        rT=c()
       # rP=c()
        for (meteo_station in names(rivers_data[[station]][["meteo"]]))
        {
          T=c(T,rivers_data_tmp[[station]][["STL"]][["meteo"]][[meteo_station]][["TA"]][["lm"]][[p]]$trend*10)
          P=c(P,rivers_data_tmp[[station]][["STL"]][["meteo"]][[meteo_station]][["P"]][["lm"]][[p]]$trend/
                mean(rivers_data_tmp[[station]][["STL"]][["meteo"]][[meteo_station]][["P"]][["lm"]][[p]]$values,na.rm=TRUE)*10*100)
          rT=c(rT,rivers_data_tmp[[station]][["STL"]][["meteo"]][[meteo_station]][["TA"]][["rlm"]][[p]]$trend*10)
         # rP=c(rP,rivers_data_tmp[[station]][["STL"]][["meteo"]][[meteo_station]][["P"]][["rlm"]][[p]]$trend/
        #        mean(rivers_data_tmp[[station]][["STL"]][["meteo"]][[meteo_station]][["P"]][["rlm"]][[p]]$values,na.rm=TRUE)*10*100)
        }
        lm_p1[["TA"]][[p]]=c(lm_p1[["TA"]][[p]],mean(T))
        lm_p1[["P"]][[p]]=c(lm_p1[["P"]][[p]],mean(P))
        rlm_p1[["TA"]][[p]]=c(rlm_p1[["TA"]][[p]],mean(rT))
       # rlm_p1[["P"]][[p]]=c(rlm_p1[["P"]][[p]],mean(rP))
      }
    }
  }
  periods=list(c("1979-2018"),c("1979-2017"),c("1980-2018"))
  lm_p2=list()
  lm_p2[["T"]]=list()
  lm_p2[["TA"]]=list()
  lm_p2[["P"]]=list()
  lm_p2[["Q"]]=list()
  rlm_p2=list()
  rlm_p2[["T"]]=list()
  rlm_p2[["TA"]]=list()
  rlm_p2[["P"]]=list()
  rlm_p2[["Q"]]=list()
  for (p in periods)
  {
    lm_p2[["T"]][[p]]=c()
    lm_p2[["TA"]][[p]]=c()
    lm_p2[["P"]][[p]]=c()
    lm_p2[["Q"]][[p]]=c()
    rlm_p2[["T"]][[p]]=c()
    rlm_p2[["TA"]][[p]]=c()
    rlm_p2[["P"]][[p]]=c()
    rlm_p2[["Q"]][[p]]=c()
  }
  for (station in names(rivers_data))
  {
    for (p in periods)
    {
      if(p %in% names(rivers_data_tmp[[station]][["STL"]][["T"]][["lm"]]) &&
         p %in% names(rivers_data_tmp[[station]][["STL"]][["Q"]][["lm"]]))
      {
        lm_p2[["T"]][[p]]=c(lm_p2[["T"]][[p]],rivers_data_tmp[[station]][["STL"]][["T"]][["lm"]][[p]]$trend*10)
        lm_p2[["Q"]][[p]]=c(lm_p2[["Q"]][[p]],rivers_data_tmp[[station]][["STL"]][["Q"]][["lm"]][[p]]$trend/
                              mean(rivers_data_tmp[[station]][["STL"]][["Q"]][["lm"]][[p]]$values,na.rm=TRUE)*10*100)
        rlm_p2[["T"]][[p]]=c(rlm_p2[["T"]][[p]],rivers_data_tmp[[station]][["STL"]][["T"]][["rlm"]][[p]]$trend*10)
        rlm_p2[["Q"]][[p]]=c(rlm_p2[["Q"]][[p]],rivers_data_tmp[[station]][["STL"]][["Q"]][["rlm"]][[p]]$trend/
                              mean(rivers_data_tmp[[station]][["STL"]][["Q"]][["rlm"]][[p]]$values,na.rm=TRUE)*10*100)
        P=c()
        T=c()
        rT=c()
        rP=c()
        for (meteo_station in names(rivers_data[[station]][["meteo"]]))
        {
          T=c(T,rivers_data_tmp[[station]][["STL"]][["meteo"]][[meteo_station]][["TA"]][["lm"]][[p]]$trend*10)
          P=c(P,rivers_data_tmp[[station]][["STL"]][["meteo"]][[meteo_station]][["P"]][["lm"]][[p]]$trend/
                mean(rivers_data_tmp[[station]][["STL"]][["meteo"]][[meteo_station]][["P"]][["lm"]][[p]]$values,na.rm=TRUE)*10*100)
          rT=c(rT,rivers_data_tmp[[station]][["STL"]][["meteo"]][[meteo_station]][["TA"]][["rlm"]][[p]]$trend*10)
          # rP=c(rP,rivers_data_tmp[[station]][["STL"]][["meteo"]][[meteo_station]][["P"]][["rlm"]][[p]]$trend/
          #        mean(rivers_data_tmp[[station]][["STL"]][["meteo"]][[meteo_station]][["P"]][["rlm"]][[p]]$values,na.rm=TRUE)*10*100)
        }
        lm_p2[["TA"]][[p]]=c(lm_p2[["TA"]][[p]],mean(T))
        lm_p2[["P"]][[p]]=c(lm_p2[["P"]][[p]],mean(P))
        rlm_p2[["TA"]][[p]]=c(rlm_p2[["TA"]][[p]],mean(rT))
        # rlm_p2[["P"]][[p]]=c(rlm_p2[["P"]][[p]],mean(rP))
      }
    }
  }

  ###############################################
  # Do the trend analysis 1999-2018 standard lm #
  ###############################################

  pdf("plots/trend_robustness.pdf",width=10,height=10)
  par(mfrow=c(2,2),mar=c(3.5,3.5,2,1),oma=c(0,0,2,0))

  plot(lm_p1[["T"]][["1999-2018"]],lm_p1[["T"]][["2000-2018"]],cex=0.7,pch=16,col=2,xlim=c(0,1),ylim=c(0,1),xlab="",ylab="",main="Water temperature")
  points(lm_p1[["T"]][["1999-2018"]],lm_p1[["T"]][["1999-2017"]],cex=0.7,pch=16,col=3)
  points(mean(lm_p1[["T"]][["1999-2018"]]),mean(lm_p1[["T"]][["2000-2018"]]),cex=1.3,pch=15,col=2)
  points(mean(lm_p1[["T"]][["1999-2018"]]),mean(lm_p1[["T"]][["2000-2018"]]),cex=1.3,pch=22,col=1)
  points(mean(lm_p1[["T"]][["1999-2018"]]),mean(lm_p1[["T"]][["1999-2017"]]),cex=1.3,pch=15,col=3)
  points(mean(lm_p1[["T"]][["1999-2018"]]),mean(lm_p1[["T"]][["1999-2017"]]),cex=1.3,pch=22,col=1)
  mtext(side=1,text=expression(paste("Control period trend (",degree,"C per decade)")),line=2.4,cex=0.9)
  mtext(side=2,text=expression(paste("Modified periods trend (",degree,"C per decade)")),line=2.4,cex=0.9)
  lines(c(0,10),c(0,10))
  rmse1=sqrt(sum((lm_p1[["T"]][["1999-2018"]]-lm_p1[["T"]][["2000-2018"]])^2)/length(lm_p1[["T"]][["1999-2018"]]))
  rmse2=sqrt(sum((lm_p1[["T"]][["1999-2018"]]-lm_p1[["T"]][["1999-2017"]])^2)/length(lm_p1[["T"]][["1999-2018"]]))
  legend("topleft",c(paste("Period 2000-2018, RMSE:",round(rmse1,3)),paste("Period 1999-2017, RMSE:",round(rmse2,3)),
                     "Sample mean"),col=c(2,3,1),pch=c(16,16,22), bty="n")

  plot(lm_p1[["TA"]][["1999-2018"]],lm_p1[["TA"]][["2000-2018"]],cex=0.7,pch=16,col=2,xlim=c(0,1),ylim=c(0,1),xlab="",ylab="",main="Air temperature")
  points(lm_p1[["TA"]][["1999-2018"]],lm_p1[["TA"]][["1999-2017"]],cex=0.7,pch=16,col=3)
  points(mean(lm_p1[["TA"]][["1999-2018"]]),mean(lm_p1[["TA"]][["2000-2018"]]),cex=1.3,pch=15,col=2)
  points(mean(lm_p1[["TA"]][["1999-2018"]]),mean(lm_p1[["TA"]][["2000-2018"]]),cex=1.3,pch=22,col=1)
  points(mean(lm_p1[["TA"]][["1999-2018"]]),mean(lm_p1[["TA"]][["1999-2017"]]),cex=1.3,pch=15,col=3)
  points(mean(lm_p1[["TA"]][["1999-2018"]]),mean(lm_p1[["TA"]][["1999-2017"]]),cex=1.3,pch=22,col=1)
  mtext(side=1,text=expression(paste("Control period trend (",degree,"C per decade)")),line=2.4,cex=0.9)
  mtext(side=2,text=expression(paste("Modified periods trend (",degree,"C per decade)")),line=2.4,cex=0.9)
  lines(c(0,1),c(0,1))
  rmse1=sqrt(sum((lm_p1[["TA"]][["1999-2018"]]-lm_p1[["TA"]][["2000-2018"]])^2)/length(lm_p1[["TA"]][["1999-2018"]]))
  rmse2=sqrt(sum((lm_p1[["TA"]][["1999-2018"]]-lm_p1[["TA"]][["1999-2017"]])^2)/length(lm_p1[["TA"]][["1999-2018"]]))
  legend("topleft",c(paste("Period 2000-2018, RMSE:",round(rmse1,3)),paste("Period 1999-2017, RMSE:",round(rmse2,3)),
                     "Sample mean"),col=c(2,3,1),pch=c(16,16,22), bty="n")

  plot(lm_p1[["Q"]][["1999-2018"]],lm_p1[["Q"]][["2000-2018"]],cex=0.7,pch=16,col=2,ylim=c(-25,10),xlim=c(-25,10),xlab="",ylab="",main="Discharge")
  points(lm_p1[["Q"]][["1999-2018"]],lm_p1[["Q"]][["1999-2017"]],cex=0.7,pch=16,col=3)
  points(mean(lm_p1[["Q"]][["1999-2018"]]),mean(lm_p1[["Q"]][["2000-2018"]]),cex=1.3,pch=15,col=2)
  points(mean(lm_p1[["Q"]][["1999-2018"]]),mean(lm_p1[["Q"]][["2000-2018"]]),cex=1.3,pch=22,col=1)
  points(mean(lm_p1[["Q"]][["1999-2018"]]),mean(lm_p1[["Q"]][["1999-2017"]]),cex=1.3,pch=15,col=3)
  points(mean(lm_p1[["Q"]][["1999-2018"]]),mean(lm_p1[["Q"]][["1999-2017"]]),cex=1.3,pch=22,col=1)
  mtext(side=1,text="Control period trend (% per decade)",line=2.4,cex=0.9)
  mtext(side=2,text="Modified periods trend (% per decade)",line=2.4,cex=0.9)
  lines(c(-100,100),c(-100,100))
  rmse1=sqrt(sum((lm_p1[["Q"]][["1999-2018"]]-lm_p1[["Q"]][["2000-2018"]])^2)/length(lm_p1[["Q"]][["1999-2018"]]))
  rmse2=sqrt(sum((lm_p1[["Q"]][["1999-2018"]]-lm_p1[["Q"]][["1999-2017"]])^2)/length(lm_p1[["Q"]][["1999-2018"]]))
  legend("topleft",c(paste("Period 2000-2018, RMSE:",round(rmse1,3)),paste("Period 1999-2017, RMSE:",round(rmse2,3)),
                     "Sample mean"),col=c(2,3,1),pch=c(16,16,22), bty="n")

  plot(lm_p1[["P"]][["1999-2018"]],lm_p1[["P"]][["2000-2018"]],cex=0.7,pch=16,col=2,ylim=c(-25,10),xlim=c(-25,10),xlab="",ylab="",main="Precipitation")
  points(lm_p1[["P"]][["1999-2018"]],lm_p1[["P"]][["1999-2017"]],cex=0.7,pch=16,col=3)
  points(mean(lm_p1[["P"]][["1999-2018"]]),mean(lm_p1[["P"]][["2000-2018"]]),cex=1.3,pch=15,col=2)
  points(mean(lm_p1[["P"]][["1999-2018"]]),mean(lm_p1[["P"]][["2000-2018"]]),cex=1.3,pch=22,col=1)
  points(mean(lm_p1[["P"]][["1999-2018"]]),mean(lm_p1[["P"]][["1999-2017"]]),cex=1.3,pch=15,col=3)
  points(mean(lm_p1[["P"]][["1999-2018"]]),mean(lm_p1[["P"]][["1999-2017"]]),cex=1.3,pch=22,col=1)
  mtext(side=1,text="Control period trend (% per decade)",line=2.4,cex=0.9)
  mtext(side=2,text="Modified periods trend (% per decade)",line=2.4,cex=0.9)
  lines(c(-100,100),c(-100,100))
  rmse1=sqrt(sum((lm_p1[["P"]][["1999-2018"]]-lm_p1[["P"]][["2000-2018"]])^2)/length(lm_p1[["P"]][["1999-2018"]]))
  rmse2=sqrt(sum((lm_p1[["P"]][["1999-2018"]]-lm_p1[["P"]][["1999-2017"]])^2)/length(lm_p1[["P"]][["1999-2018"]]))
  legend("topleft",c(paste("Period 2000-2018, RMSE:",round(rmse1,3)),paste("Period 1999-2017, RMSE:",round(rmse2,3)),
                     "Sample mean"),col=c(2,3,1),pch=c(16,16,22), bty="n")

  title("Period 1999-2018, normal linear regression", outer=TRUE)

  #############################################
  # Do the trend analysis 1979-2018 normal lm #
  #############################################

  par(mfrow=c(2,2),mar=c(3.5,3.5,2,1),oma=c(0,0,2,0))

  plot(lm_p2[["T"]][["1979-2018"]],lm_p2[["T"]][["1980-2018"]],cex=0.7,pch=16,col=2,xlim=c(0,1),ylim=c(0,1),xlab="",ylab="",main="Water temperature")
  points(lm_p2[["T"]][["1979-2018"]],lm_p2[["T"]][["1979-2017"]],cex=0.7,pch=16,col=3)
  points(mean(lm_p2[["T"]][["1979-2018"]]),mean(lm_p2[["T"]][["1980-2018"]]),cex=1.3,pch=15,col=2)
  points(mean(lm_p2[["T"]][["1979-2018"]]),mean(lm_p2[["T"]][["1980-2018"]]),cex=1.3,pch=22,col=1)
  points(mean(lm_p2[["T"]][["1979-2018"]]),mean(lm_p2[["T"]][["1979-2017"]]),cex=1.3,pch=15,col=3)
  points(mean(lm_p2[["T"]][["1979-2018"]]),mean(lm_p2[["T"]][["1979-2017"]]),cex=1.3,pch=22,col=1)
  mtext(side=1,text=expression(paste("Control period trend (",degree,"C per decade)")),line=2.4,cex=0.9)
  mtext(side=2,text=expression(paste("Modified periods trend (",degree,"C per decade)")),line=2.4,cex=0.9)
  lines(c(0,10),c(0,10))
  rmse1=sqrt(sum((lm_p2[["T"]][["1979-2018"]]-lm_p2[["T"]][["1980-2018"]])^2)/length(lm_p2[["T"]][["1979-2018"]]))
  rmse2=sqrt(sum((lm_p2[["T"]][["1979-2018"]]-lm_p2[["T"]][["1979-2017"]])^2)/length(lm_p2[["T"]][["1979-2018"]]))
  legend("topleft",c(paste("Period 1980-2018, RMSE:",round(rmse1,3)),paste("Period 1979-2017, RMSE:",round(rmse2,3)),
         "Sample mean"),col=c(2,3,1),pch=c(16,16,22), bty="n")


  plot(lm_p2[["TA"]][["1979-2018"]],lm_p2[["TA"]][["1980-2018"]],cex=0.7,pch=16,col=2,xlim=c(0,1),ylim=c(0,1),xlab="",ylab="",main="Air temperature")
  points(lm_p2[["TA"]][["1979-2018"]],lm_p2[["TA"]][["1979-2017"]],cex=0.7,pch=16,col=3)
  points(mean(lm_p2[["TA"]][["1979-2018"]]),mean(lm_p2[["TA"]][["1980-2018"]]),cex=1.3,pch=15,col=2)
  points(mean(lm_p2[["TA"]][["1979-2018"]]),mean(lm_p2[["TA"]][["1980-2018"]]),cex=1.3,pch=22,col=1)
  points(mean(lm_p2[["TA"]][["1979-2018"]]),mean(lm_p2[["TA"]][["1979-2017"]]),cex=1.3,pch=15,col=3)
  points(mean(lm_p2[["TA"]][["1979-2018"]]),mean(lm_p2[["TA"]][["1979-2017"]]),cex=1.3,pch=22,col=1)
  mtext(side=1,text=expression(paste("Control period trend (",degree,"C per decade)")),line=2.4,cex=0.9)
  mtext(side=2,text=expression(paste("Modified periods trend (",degree,"C per decade)")),line=2.4,cex=0.9)
  lines(c(0,1),c(0,1))
  rmse1=sqrt(sum((lm_p2[["TA"]][["1979-2018"]]-lm_p2[["TA"]][["1980-2018"]])^2)/length(lm_p2[["TA"]][["1979-2018"]]))
  rmse2=sqrt(sum((lm_p2[["TA"]][["1979-2018"]]-lm_p2[["TA"]][["1979-2017"]])^2)/length(lm_p2[["TA"]][["1979-2018"]]))
  legend("topleft",c(paste("Period 1980-2018, RMSE:",round(rmse1,3)),paste("Period 1979-2017, RMSE:",round(rmse2,3)),
         "Sample mean"),col=c(2,3,1),pch=c(16,16,22), bty="n")


  plot(lm_p2[["Q"]][["1979-2018"]],lm_p2[["Q"]][["1980-2018"]],cex=0.7,pch=16,col=2,ylim=c(-25,10),xlim=c(-25,10),xlab="",ylab="",main="Discharge")
  points(lm_p2[["Q"]][["1979-2018"]],lm_p2[["Q"]][["1979-2017"]],cex=0.7,pch=16,col=3)
  points(mean(lm_p2[["Q"]][["1979-2018"]]),mean(lm_p2[["Q"]][["1980-2018"]]),cex=1.3,pch=15,col=2)
  points(mean(lm_p2[["Q"]][["1979-2018"]]),mean(lm_p2[["Q"]][["1980-2018"]]),cex=1.3,pch=22,col=1)
  points(mean(lm_p2[["Q"]][["1979-2018"]]),mean(lm_p2[["Q"]][["1979-2017"]]),cex=1.3,pch=15,col=3)
  points(mean(lm_p2[["Q"]][["1979-2018"]]),mean(lm_p2[["Q"]][["1979-2017"]]),cex=1.3,pch=22,col=1)
  mtext(side=1,text="Control period trend (% per decade)",line=2.4,cex=0.9)
  mtext(side=2,text="Modified periods trend (% per decade)",line=2.4,cex=0.9)
  lines(c(-100,100),c(-100,100))
  rmse1=sqrt(sum((lm_p2[["Q"]][["1979-2018"]]-lm_p2[["Q"]][["1980-2018"]])^2)/length(lm_p2[["Q"]][["1979-2018"]]))
  rmse2=sqrt(sum((lm_p2[["Q"]][["1979-2018"]]-lm_p2[["Q"]][["1979-2017"]])^2)/length(lm_p2[["Q"]][["1979-2018"]]))
  legend("topleft",c(paste("Period 1980-2018, RMSE:",round(rmse1,3)),paste("Period 1979-2017, RMSE:",round(rmse2,3)),
         "Sample mean"),col=c(2,3,1),pch=c(16,16,22), bty="n")

  plot(lm_p2[["P"]][["1979-2018"]],lm_p2[["P"]][["1980-2018"]],cex=0.7,pch=16,col=2,ylim=c(-25,10),xlim=c(-25,10),xlab="",ylab="",main="Precipitation")
  points(lm_p2[["P"]][["1979-2018"]],lm_p2[["P"]][["1979-2017"]],cex=0.7,pch=16,col=3)
  points(lm_p2[["Q"]][["1979-2018"]],lm_p2[["Q"]][["1979-2017"]],cex=0.7,pch=16,col=3)
  points(mean(lm_p2[["P"]][["1979-2018"]]),mean(lm_p2[["P"]][["1980-2018"]]),cex=1.3,pch=15,col=2)
  points(mean(lm_p2[["P"]][["1979-2018"]]),mean(lm_p2[["P"]][["1980-2018"]]),cex=1.3,pch=22,col=1)
  points(mean(lm_p2[["P"]][["1979-2018"]]),mean(lm_p2[["P"]][["1979-2017"]]),cex=1.3,pch=15,col=3)
  points(mean(lm_p2[["P"]][["1979-2018"]]),mean(lm_p2[["P"]][["1979-2017"]]),cex=1.3,pch=22,col=1)
  mtext(side=1,text="Control period trend (% per decade)",line=2.4,cex=0.9)
  mtext(side=2,text="Modified periods trend (% per decade)",line=2.4,cex=0.9)
  lines(c(-100,100),c(-100,100))
  rmse1=sqrt(sum((lm_p2[["P"]][["1979-2018"]]-lm_p2[["P"]][["1980-2018"]])^2)/length(lm_p2[["P"]][["1979-2018"]]))
  rmse2=sqrt(sum((lm_p2[["P"]][["1979-2018"]]-lm_p2[["P"]][["1979-2017"]])^2)/length(lm_p2[["P"]][["1979-2018"]]))
  legend("topleft",c(paste("Period 1980-2018, RMSE:",round(rmse1,3)),paste("Period 1979-2017, RMSE:",round(rmse2,3)),
                     "Sample mean"),col=c(2,3,1),pch=c(16,16,22), bty="n")

  title("Period 1979-2018, normal linear regression", outer=TRUE)

  ##############################################
  # Do the trend analysis 1999-2018 compare lm #
  ##############################################

  par(mfrow=c(2,2),mar=c(3.5,3.5,2,1),oma=c(0,0,2,0))

  plot(lm_p1[["T"]][["1999-2018"]],rlm_p1[["T"]][["1999-2018"]],cex=0.7,pch=16,col=3,xlim=c(0,1),ylim=c(0,1),xlab="",ylab="",main="Water temperature")
  points(mean(lm_p1[["T"]][["1999-2018"]]),mean(rlm_p1[["T"]][["1999-2018"]]),cex=1.3,pch=15,col=3)
  points(mean(lm_p1[["T"]][["1999-2018"]]),mean(rlm_p1[["T"]][["1999-2018"]]),cex=1.3,pch=22,col=1)
  mtext(side=1,text=expression(paste("Normal lm trend (",degree,"C per decade)")),line=2.4,cex=0.9)
  mtext(side=2,text=expression(paste("Robust lm trend (",degree,"C per decade)")),line=2.4,cex=0.9)
  lines(c(0,10),c(0,10))
  rmse1=sqrt(sum((lm_p1[["T"]][["1999-2018"]]-rlm_p1[["T"]][["1999-2018"]])^2)/length(lm_p1[["T"]][["1999-2018"]]))
  legend("topleft",c(paste("RMSE:",round(rmse1,3)),"Sample mean"),col=c(3,1),pch=c(16,22), bty="n")

  plot(lm_p1[["TA"]][["1999-2018"]],rlm_p1[["TA"]][["1999-2018"]],cex=0.7,pch=16,col=3,xlim=c(0,1),ylim=c(0,1),xlab="",ylab="",main="Air temperature")
  points(mean(lm_p1[["TA"]][["1999-2018"]]),mean(rlm_p1[["TA"]][["1999-2018"]]),cex=1.3,pch=15,col=3)
  points(mean(lm_p1[["TA"]][["1999-2018"]]),mean(rlm_p1[["TA"]][["1999-2018"]]),cex=1.3,pch=22,col=1)
  mtext(side=1,text=expression(paste("Normal lm trend (",degree,"C per decade)")),line=2.4,cex=0.9)
  mtext(side=2,text=expression(paste("Robust lm trend (",degree,"C per decade)")),line=2.4,cex=0.9)
  lines(c(0,1),c(0,1))
  rmse1=sqrt(sum((lm_p1[["TA"]][["1999-2018"]]-rlm_p1[["TA"]][["1999-2018"]])^2)/length(lm_p1[["TA"]][["1999-2018"]]))
  legend("topleft",c(paste("RMSE:",round(rmse1,3)),"Sample mean"),col=c(3,1),pch=c(16,22), bty="n")

  plot(lm_p1[["Q"]][["1999-2018"]],rlm_p1[["Q"]][["1999-2018"]],cex=0.7,pch=16,col=3,ylim=c(-25,10),xlim=c(-25,10),xlab="",ylab="",main="Discharge")
  points(mean(lm_p1[["Q"]][["1999-2018"]]),mean(rlm_p1[["Q"]][["1999-2018"]]),cex=1.3,pch=15,col=3)
  points(mean(lm_p1[["Q"]][["1999-2018"]]),mean(rlm_p1[["Q"]][["1999-2018"]]),cex=1.3,pch=22,col=1)
  mtext(side=1,text="Normal lm trend (% per decade)",line=2.4,cex=0.9)
  mtext(side=2,text="Robust lm trend (% per decade)",line=2.4,cex=0.9)
  lines(c(-100,100),c(-100,100))
  rmse1=sqrt(sum((lm_p1[["Q"]][["1999-2018"]]-rlm_p1[["Q"]][["1999-2018"]])^2)/length(lm_p1[["Q"]][["1999-2018"]]))
  legend("topleft",c(paste("RMSE:",round(rmse1,3)),"Sample mean"),col=c(3,1),pch=c(16,22), bty="n")

  title("Period 1999-2018, regression method comparison", outer=TRUE)

  ##############################################
  # Do the trend analysis 1979-2018 compare lm #
  ##############################################

  par(mfrow=c(2,2),mar=c(3.5,3.5,2,1),oma=c(0,0,2,0))

  plot(lm_p2[["T"]][["1979-2018"]],rlm_p2[["T"]][["1979-2018"]],cex=0.7,pch=16,col=3,xlim=c(0,1),ylim=c(0,1),xlab="",ylab="",main="Water temperature")
  points(mean(lm_p2[["T"]][["1979-2018"]]),mean(rlm_p2[["T"]][["1979-2018"]]),cex=1.3,pch=15,col=3)
  points(mean(lm_p2[["T"]][["1979-2018"]]),mean(rlm_p2[["T"]][["1979-2018"]]),cex=1.3,pch=22,col=1)
  mtext(side=1,text=expression(paste("Normal lm trend (",degree,"C per decade)")),line=2.4,cex=0.9)
  mtext(side=2,text=expression(paste("Robust lm trend (",degree,"C per decade)")),line=2.4,cex=0.9)
  lines(c(0,10),c(0,10))
  rmse1=sqrt(sum((lm_p2[["T"]][["1979-2018"]]-rlm_p2[["T"]][["1979-2018"]])^2)/length(lm_p2[["T"]][["1979-2018"]]))
  legend("topleft",c(paste("RMSE:",round(rmse1,3)),"Sample mean"),col=c(3,1),pch=c(16,22), bty="n")

  plot(lm_p2[["TA"]][["1979-2018"]],rlm_p2[["TA"]][["1979-2018"]],cex=0.7,pch=16,col=3,xlim=c(0,1),ylim=c(0,1),xlab="",ylab="",main="Air temperature")
  points(mean(lm_p2[["TA"]][["1979-2018"]]),mean(rlm_p2[["TA"]][["1979-2018"]]),cex=1.3,pch=15,col=3)
  points(mean(lm_p2[["TA"]][["1979-2018"]]),mean(rlm_p2[["TA"]][["1979-2018"]]),cex=1.3,pch=22,col=1)
  mtext(side=1,text=expression(paste("Normal lm trend (",degree,"C per decade)")),line=2.4,cex=0.9)
  mtext(side=2,text=expression(paste("Robust lm trend (",degree,"C per decade)")),line=2.4,cex=0.9)
  lines(c(0,1),c(0,1))
  rmse1=sqrt(sum((lm_p2[["TA"]][["1979-2018"]]-rlm_p2[["TA"]][["1979-2018"]])^2)/length(lm_p2[["TA"]][["1979-2018"]]))
  legend("topleft",c(paste("RMSE:",round(rmse1,3)),"Sample mean"),col=c(3,1),pch=c(16,22), bty="n")

  plot(lm_p2[["Q"]][["1979-2018"]],rlm_p2[["Q"]][["1979-2018"]],cex=0.7,pch=16,col=3,ylim=c(-25,10),xlim=c(-25,10),xlab="",ylab="",main="Discharge")
  points(mean(lm_p2[["Q"]][["1979-2018"]]),mean(rlm_p2[["Q"]][["1979-2018"]]),cex=1.3,pch=15,col=3)
  points(mean(lm_p2[["Q"]][["1979-2018"]]),mean(rlm_p2[["Q"]][["1979-2018"]]),cex=1.3,pch=22,col=1)
  mtext(side=1,text="Normal lm trend (% per decade)",line=2.4,cex=0.9)
  mtext(side=2,text="Robust lm trend (% per decade)",line=2.4,cex=0.9)
  lines(c(-100,100),c(-100,100))
  rmse1=sqrt(sum((lm_p2[["Q"]][["1979-2018"]]-rlm_p2[["Q"]][["1979-2018"]])^2)/length(lm_p2[["Q"]][["1979-2018"]]))
  legend("topleft",c(paste("RMSE:",round(rmse1,3)),"Sample mean"),col=c(3,1),pch=c(16,22), bty="n")

  title("Period 1979-2018, regression method comparison", outer=TRUE)



  #############################################
  # Do the trend analysis 1999-2018 robust lm #
  #############################################

  par(mfrow=c(2,2),mar=c(3.5,3.5,2,1))

  plot(rlm_p1[["T"]][["1999-2018"]],rlm_p1[["T"]][["2000-2018"]],cex=0.7,pch=16,col=2,xlim=c(0,1),ylim=c(0,1),xlab="",ylab="",main="Water temperature")
  points(rlm_p1[["T"]][["1999-2018"]],rlm_p1[["T"]][["1999-2017"]],cex=0.7,pch=16,col=3)
  points(mean(rlm_p1[["T"]][["1999-2018"]]),mean(rlm_p1[["T"]][["2000-2018"]]),cex=1.3,pch=15,col=2)
  points(mean(rlm_p1[["T"]][["1999-2018"]]),mean(rlm_p1[["T"]][["2000-2018"]]),cex=1.3,pch=22,col=1)
  points(mean(rlm_p1[["T"]][["1999-2018"]]),mean(rlm_p1[["T"]][["1999-2017"]]),cex=1.3,pch=15,col=3)
  points(mean(rlm_p1[["T"]][["1999-2018"]]),mean(rlm_p1[["T"]][["1999-2017"]]),cex=1.3,pch=22,col=1)
  mtext(side=1,text=expression(paste("Control period trend (",degree,"C per decade)")),line=2.4,cex=0.9)
  mtext(side=2,text=expression(paste("Modified periods trend (",degree,"C per decade)")),line=2.4,cex=0.9)
  lines(c(0,10),c(0,10))
  rmse1=sqrt(sum((rlm_p1[["T"]][["1999-2018"]]-rlm_p1[["T"]][["2000-2018"]])^2)/length(rlm_p1[["T"]][["1999-2018"]]))
  rmse2=sqrt(sum((rlm_p1[["T"]][["1999-2018"]]-rlm_p1[["T"]][["1999-2017"]])^2)/length(rlm_p1[["T"]][["1999-2018"]]))
  legend("topleft",c(paste("Period 1980-2018, RMSE:",round(rmse1,3)),paste("Period 1979-2017, RMSE:",round(rmse2,3)),
                     "Sample mean"),col=c(2,3,1),pch=c(16,16,22), bty="n")

  plot(rlm_p1[["TA"]][["1999-2018"]],rlm_p1[["TA"]][["2000-2018"]],cex=0.7,pch=16,col=2,xlim=c(0,1),ylim=c(0,1),xlab="",ylab="",main="Air temperature")
  points(rlm_p1[["TA"]][["1999-2018"]],rlm_p1[["TA"]][["1999-2017"]],cex=0.7,pch=16,col=3)
  points(mean(rlm_p1[["TA"]][["1999-2018"]]),mean(rlm_p1[["TA"]][["2000-2018"]]),cex=1.3,pch=15,col=2)
  points(mean(rlm_p1[["TA"]][["1999-2018"]]),mean(rlm_p1[["TA"]][["2000-2018"]]),cex=1.3,pch=22,col=1)
  points(mean(rlm_p1[["TA"]][["1999-2018"]]),mean(rlm_p1[["TA"]][["1999-2017"]]),cex=1.3,pch=15,col=3)
  points(mean(rlm_p1[["TA"]][["1999-2018"]]),mean(rlm_p1[["TA"]][["1999-2017"]]),cex=1.3,pch=22,col=1)
  mtext(side=1,text=expression(paste("Control period trend (",degree,"C per decade)")),line=2.4,cex=0.9)
  mtext(side=2,text=expression(paste("Modified periods trend (",degree,"C per decade)")),line=2.4,cex=0.9)
  lines(c(0,1),c(0,1))
  rmse1=sqrt(sum((rlm_p1[["TA"]][["1999-2018"]]-rlm_p1[["TA"]][["2000-2018"]])^2)/length(rlm_p1[["TA"]][["1999-2018"]]))
  rmse2=sqrt(sum((rlm_p1[["TA"]][["1999-2018"]]-rlm_p1[["TA"]][["1999-2017"]])^2)/length(rlm_p1[["TA"]][["1999-2018"]]))
  legend("topleft",c(paste("Period 1980-2018, RMSE:",round(rmse1,3)),paste("Period 1979-2017, RMSE:",round(rmse2,3)),
                     "Sample mean"),col=c(2,3,1),pch=c(16,16,22), bty="n")

  plot(rlm_p1[["Q"]][["1999-2018"]],rlm_p1[["Q"]][["2000-2018"]],cex=0.7,pch=16,col=2,ylim=c(-25,10),xlim=c(-25,10),xlab="",ylab="",main="Discharge")
  points(rlm_p1[["Q"]][["1999-2018"]],rlm_p1[["Q"]][["1999-2017"]],cex=0.7,pch=16,col=3)
  points(mean(rlm_p1[["Q"]][["1999-2018"]]),mean(rlm_p1[["Q"]][["2000-2018"]]),cex=1.3,pch=15,col=2)
  points(mean(rlm_p1[["Q"]][["1999-2018"]]),mean(rlm_p1[["Q"]][["2000-2018"]]),cex=1.3,pch=22,col=1)
  points(mean(rlm_p1[["Q"]][["1999-2018"]]),mean(rlm_p1[["Q"]][["1999-2017"]]),cex=1.3,pch=15,col=3)
  points(mean(rlm_p1[["Q"]][["1999-2018"]]),mean(rlm_p1[["Q"]][["1999-2017"]]),cex=1.3,pch=22,col=1)
  mtext(side=1,text="Control period trend (% per decade)",line=2.4,cex=0.9)
  mtext(side=2,text="Modified periods trend (% per decade)",line=2.4,cex=0.9)
  lines(c(-100,100),c(-100,100))
  rmse1=sqrt(sum((rlm_p1[["Q"]][["1999-2018"]]-rlm_p1[["Q"]][["2000-2018"]])^2)/length(rlm_p1[["Q"]][["1999-2018"]]))
  rmse2=sqrt(sum((rlm_p1[["Q"]][["1999-2018"]]-rlm_p1[["Q"]][["1999-2017"]])^2)/length(rlm_p1[["Q"]][["1999-2018"]]))
  legend("topleft",c(paste("Period 1980-2018, RMSE:",round(rmse1,3)),paste("Period 1979-2017, RMSE:",round(rmse2,3)),
                     "Sample mean"),col=c(2,3,1),pch=c(16,16,22), bty="n")

  # plot(rlm_p1[["P"]][["1999-2018"]],rlm_p1[["P"]][["2000-2018"]],cex=0.7,pch=16,col=2,ylim=c(-25,10),xlim=c(-25,10),xlab="",ylab="",main="P")
  # points(rlm_p1[["P"]][["1999-2018"]],rlm_p1[["P"]][["1999-2017"]],cex=0.7,pch=16,col=3)
  # mtext(side=1,text="Control period trend (% per decade)",line=2.4,cex=0.9)
  # mtext(side=2,text="Modified periods trend (% per decade)",line=2.4,cex=0.9)
  # lines(c(-100,100),c(-100,100))
  # rmse1=sqrt(sum((rlm_p1[["P"]][["1999-2018"]]-rlm_p1[["P"]][["2000-2018"]])^2)/length(rlm_p1[["P"]][["1999-2018"]]))
  # rmse2=sqrt(sum((rlm_p1[["P"]][["1999-2018"]]-rlm_p1[["P"]][["1999-2017"]])^2)/length(rlm_p1[["P"]][["1999-2018"]]))
  # legend("topleft",c(paste("Period 1980-2018, RMSE:",round(rmse1,3)),paste("Period 1979-2017, RMSE:",round(rmse2,3))),col=c(2,3),pch=16, bty="n")
  #
  title("Period 1999-2018, robust linear regression", outer=TRUE)


  #############################################
  # Do the trend analysis 1979-2018 robust lm #
  #############################################

  par(mfrow=c(2,2),mar=c(3.5,3.5,2,1),oma=c(0,0,2,0))

  plot(rlm_p2[["T"]][["1979-2018"]],rlm_p2[["T"]][["1980-2018"]],cex=0.7,pch=16,col=2,xlim=c(0,1),ylim=c(0,1),xlab="",ylab="",main="T")
  points(rlm_p2[["T"]][["1979-2018"]],rlm_p2[["T"]][["1979-2017"]],cex=0.7,pch=16,col=3)
  points(mean(rlm_p2[["T"]][["1979-2018"]]),mean(rlm_p2[["T"]][["1980-2018"]]),cex=1.3,pch=15,col=2)
  points(mean(rlm_p2[["T"]][["1979-2018"]]),mean(rlm_p2[["T"]][["1980-2018"]]),cex=1.3,pch=22,col=1)
  points(mean(rlm_p2[["T"]][["1979-2018"]]),mean(rlm_p2[["T"]][["1979-2017"]]),cex=1.3,pch=15,col=3)
  points(mean(rlm_p2[["T"]][["1979-2018"]]),mean(rlm_p2[["T"]][["1979-2017"]]),cex=1.3,pch=22,col=1)
  mtext(side=1,text=expression(paste("Control period trend (",degree,"C per decade)")),line=2.4,cex=0.9)
  mtext(side=2,text=expression(paste("Modified periods trend (",degree,"C per decade)")),line=2.4,cex=0.9)
  lines(c(0,10),c(0,10))
  rmse1=sqrt(sum((rlm_p2[["T"]][["1979-2018"]]-rlm_p2[["T"]][["1980-2018"]])^2)/length(rlm_p2[["T"]][["1979-2018"]]))
  rmse2=sqrt(sum((rlm_p2[["T"]][["1979-2018"]]-rlm_p2[["T"]][["1979-2017"]])^2)/length(rlm_p2[["T"]][["1979-2018"]]))
  legend("topleft",c(paste("Period 1980-2018, RMSE:",round(rmse1,3)),paste("Period 1979-2017, RMSE:",round(rmse2,3)),
                     "Sample mean"),col=c(2,3),pch=16, bty="n")


  plot(rlm_p2[["TA"]][["1979-2018"]],rlm_p2[["TA"]][["1980-2018"]],cex=0.7,pch=16,col=2,xlim=c(0,1),ylim=c(0,1),xlab="",ylab="",main="TA")
  points(rlm_p2[["TA"]][["1979-2018"]],rlm_p2[["TA"]][["1979-2017"]],cex=0.7,pch=16,col=3)
  points(mean(rlm_p2[["TA"]][["1979-2018"]]),mean(rlm_p2[["TA"]][["1980-2018"]]),cex=1.3,pch=15,col=2)
  points(mean(rlm_p2[["TA"]][["1979-2018"]]),mean(rlm_p2[["TA"]][["1980-2018"]]),cex=1.3,pch=22,col=1)
  points(mean(rlm_p2[["TA"]][["1979-2018"]]),mean(rlm_p2[["TA"]][["1979-2017"]]),cex=1.3,pch=15,col=3)
  points(mean(rlm_p2[["TA"]][["1979-2018"]]),mean(rlm_p2[["TA"]][["1979-2017"]]),cex=1.3,pch=22,col=1)
  mtext(side=1,text=expression(paste("Control period trend (",degree,"C per decade)")),line=2.4,cex=0.9)
  mtext(side=2,text=expression(paste("Modified periods trend (",degree,"C per decade)")),line=2.4,cex=0.9)
  lines(c(0,1),c(0,1))
  rmse1=sqrt(sum((rlm_p2[["TA"]][["1979-2018"]]-rlm_p2[["TA"]][["1980-2018"]])^2)/length(rlm_p2[["TA"]][["1979-2018"]]))
  rmse2=sqrt(sum((rlm_p2[["TA"]][["1979-2018"]]-rlm_p2[["TA"]][["1979-2017"]])^2)/length(rlm_p2[["TA"]][["1979-2018"]]))
  legend("topleft",c(paste("Period 1980-2018, RMSE:",round(rmse1,3)),paste("Period 1979-2017, RMSE:",round(rmse2,3)),
                     "Sample mean"),col=c(2,3),pch=16, bty="n")


  plot(rlm_p2[["Q"]][["1979-2018"]],rlm_p2[["Q"]][["1980-2018"]],cex=0.7,pch=16,col=2,ylim=c(-25,10),xlim=c(-25,10),xlab="",ylab="",main="Q")
  points(rlm_p2[["Q"]][["1979-2018"]],rlm_p2[["Q"]][["1979-2017"]],cex=0.7,pch=16,col=3)
  points(mean(rlm_p2[["Q"]][["1979-2018"]]),mean(rlm_p2[["Q"]][["1980-2018"]]),cex=1.3,pch=15,col=2)
  points(mean(rlm_p2[["Q"]][["1979-2018"]]),mean(rlm_p2[["Q"]][["1980-2018"]]),cex=1.3,pch=22,col=1)
  points(mean(rlm_p2[["Q"]][["1979-2018"]]),mean(rlm_p2[["Q"]][["1979-2017"]]),cex=1.3,pch=15,col=3)
  points(mean(rlm_p2[["Q"]][["1979-2018"]]),mean(rlm_p2[["Q"]][["1979-2017"]]),cex=1.3,pch=22,col=1)
  mtext(side=1,text="Control period trend (% per decade)",line=2.4,cex=0.9)
  mtext(side=2,text="Modified periods trend (% per decade)",line=2.4,cex=0.9)
  lines(c(-100,100),c(-100,100))
  rmse1=sqrt(sum((rlm_p2[["Q"]][["1979-2018"]]-rlm_p2[["Q"]][["1980-2018"]])^2)/length(rlm_p2[["Q"]][["1979-2018"]]))
  rmse2=sqrt(sum((rlm_p2[["Q"]][["1979-2018"]]-rlm_p2[["Q"]][["1979-2017"]])^2)/length(rlm_p2[["Q"]][["1979-2018"]]))
  legend("topleft",c(paste("Period 1980-2018, RMSE:",round(rmse1,3)),paste("Period 1979-2017, RMSE:",round(rmse2,3)),
                     "Sample mean"),col=c(2,3),pch=16, bty="n")

  # plot(lm_p2[["P"]][["1979-2018"]],lm_p2[["P"]][["1980-2018"]],cex=0.7,pch=16,col=2,ylim=c(-25,10),xlim=c(-25,10),xlab="",ylab="",main="P")
  # points(lm_p2[["P"]][["1979-2018"]],lm_p2[["P"]][["1979-2017"]],cex=0.7,pch=16,col=3)
  # points(lm_p2[["Q"]][["1979-2018"]],lm_p2[["Q"]][["1979-2017"]],cex=0.7,pch=16,col=3)
  # mtext(side=1,text="Control period trend (% per decade)",line=2.4,cex=0.9)
  # mtext(side=2,text="Modified periods trend (% per decade)",line=2.4,cex=0.9)
  # lines(c(-100,100),c(-100,100))
  # rmse1=sqrt(sum((lm_p2[["P"]][["1979-2018"]]-lm_p2[["P"]][["1980-2018"]])^2)/length(lm_p2[["P"]][["1979-2018"]]))
  # rmse2=sqrt(sum((lm_p2[["P"]][["1979-2018"]]-lm_p2[["P"]][["1979-2017"]])^2)/length(lm_p2[["P"]][["1979-2018"]]))
  # legend("topleft",c(paste("Period 1980-2018, RMSE:",round(rmse1,3)),paste("Period 1979-2017, RMSE:",round(rmse2,3))),col=c(2,3),pch=16, bty="n")

  title("Period 1979-2018, robust linear regression", outer=TRUE)
  dev.off()

}

