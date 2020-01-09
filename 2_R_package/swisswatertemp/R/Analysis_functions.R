# This script is related to the paper:
# "Stream temperature evolution in Switzerland over the last 50 years, Adrien
# Michel, Tristan Brauchli, Michael Lehning, Bettina Schaefli, and Hendrik Huwald,
# HESS, 2019"

# This material is distributed under the GPLv3 license
# (https://www.gnu.org/licenses/gpl-3.0.html)

#' Plot time series and trends
#'
#' This function produces plots for time series and trends for the given station
#' and for the variables T, Q, TA, P. These plots are not used in the paper.
#'
#' @param station A SMET object containing the data for one station
#' @param output_type Either \code{"NONE"} (default), \code{"PDF"} or \code{"PNG"}.
#'
#' \code{output_type = "NONE"} creates the plot in a normal plot window (default),
#'
#' \code{output_type = "PDF"} saves the plot as pdf in plots/analysis/'station_name'/,
#'
#' \code{output_type = "PNG"} saves the plot as png in plots/analysis/'station_name'/,
#' @export
plot_trends <- function(station,output_type="NONE")
{
  name=station$header$station_name
  print(paste("[I] *** Plotting trends for station",name,"***"))

  # Colors for meteo stations
  positions=c("bottomleft","bottomright","center","center")
  cols=c("red","green","blue")
  lightcols=c(rgb(0.5,0,0,0.5),
              rgb(0,0.5,0,0.5),
              rgb(0,0,0.5,0.5))

  ############ T - TA ############

  #Prepare output
  if(output_type=="PDF"){
    pdf(paste0("plots/analysis/",name,"/Trends_T_",name,".pdf"),width=12,height=8)
  }
  else if(output_type=="PNG"){
    png(paste0("plots/analysis/",name,"/Trends_T_",name,".png"),width=2048,
        height=1536, res = 150)
  }

  par(mar=c(2,3,3,1),mgp=c(1.7,0.5,0))
  par(mfrow=c(3,1))
  # Plot trend for T
  plot(station$STL$T$timestamp, station$STL$T$remainder + station$STL$T$trend,
       xlab="",ylab="T (C)",type="l",col="grey",main="Water temp trend")

  i=1
  leg1=c()
  leg2=c()

  for(period in names(station$STL$T$lm))
  {
  lines(station$STL$T$lm[[period]]$timestamp,
        station$STL$T$lm[[period]]$trend*station$STL$T$lm[[period]]$timestamp +
        station$STL$T$lm[[period]]$intercept,lwd=2,col=i)
    i=i+1

    leg1=c(leg1,paste0("Trend ",period,": ",round(station$STL$T$lm[[period]]$trend*10,digits=2),
                                    "+/-",round(station$STL$T$lm[[period]]$trend_std*10,digits=3),
                       " (C/dec)"))
    leg2=c(leg2,paste0("r^2: ",station$STL$T$lm[[period]]$printable$adj_r_squared))
  }

  lines(station$yearly$T$timestamp,station$yearly$T$values,type="b")

  legend("bottom",legend=c(leg1,leg2),bty='n',ncol=2,cex=0.7)
  legend("top",legend=c("Deseasonalyzed ts","Annual means", "trend"),
         lty=c(1,1,1),pch=c(NA,1,NA),col=c("grey",1,1), bty='n',ncol=3,cex=0.7)

  #Plot trend for TA
  i=1
  # Find minumun and maximum of meteo times series to adjust plots
  minimum=1000
  maximum=-1000
  for (met_station in names(station$meteo))
  {
   minimum=min(minimum,station$STL$meteo[[met_station]]$TA$remainder+
                 station$STL$meteo[[met_station]]$TA$trend,na.rm=TRUE)
   maximum=max(maximum,station$STL$meteo[[met_station]]$TA$remainder+
                 station$STL$meteo[[met_station]]$TA$trend,na.rm=TRUE)
  }

  plot(station$STL$meteo[[1]]$TA$timestamp,
       station$STL$meteo[[1]]$TA$remainder +
       station$STL$meteo[[1]]$TA$trend,
       xlab="",ylab="T (C)",type="n",main="Air temp trend",ylim=c(minimum,maximum))

  for (met_station in names(station$meteo))
  {
    lines(station$STL$meteo[[met_station]]$TA$timestamp,
          station$STL$meteo[[met_station]]$TA$remainder +
          station$STL$meteo[[met_station]]$TA$trend,
          type="l",col=lightcols[i])
    i=i+1
  }
  i=1
  for (met_station in names(station$meteo))
  {
    for(period in names(station$STL$meteo[[met_station]]$TA$lm))
    {
      lines(station$STL$meteo[[met_station]]$TA$lm[[period]]$timestamp,
           station$STL$meteo[[met_station]]$TA$lm[[period]]$trend*
             station$STL$meteo[[met_station]]$TA$lm[[period]]$timestamp +
             station$STL$meteo[[met_station]]$TA$lm[[period]]$intercept,lwd=2,
           col=cols[i],ylab="")
    }
    lines(station$meteo[[met_station]]$yearly$TA$timestamp,
          station$meteo[[met_station]]$yearly$TA$values,type="b",col=cols[i])
    i=i+1
  }


  i=1
  for (met_station in names(station$meteo))
  {
    leg1=c()
    leg2=c()
    leg3=c()
    for(period in names(station$STL$meteo[[met_station]]$TA$lm))
    {

      leg1=c(leg1,paste(met_station,period))
      leg2=c(leg2,paste0("Trend: ",round(station$STL$meteo[[met_station]]$TA$lm[[period]]$trend*10,
                                         digits=2),
                      "+/-",round(station$STL$meteo[[met_station]]$TA$lm[[period]]$trend_std*10,
                                  digits=3)," (C/dec)"))
      leg3=c(leg3,paste0("r^2: ",station$STL$meteo[[met_station]]$TA$lm[[period]]$printable$adj_r_squared))
    }
    n=length(names(station$STL$meteo[[met_station]]$TA$lm))
    lty=rep(NA,times=3*n)
    lty[1:n]=1
    col=lty
    col[1:n]=cols[i]
    legend(positions[i],legend=c(leg1,leg2,leg3),
           lty=lty,col=col,bty='n',ncol=3,cex=0.7,x.intersp=0.1)
    i=i+1
  }

  legend("top",legend=c("Deseasonalyzed ts","Annual means", "trend"),
         lty=c(1,1,1),pch=c(NA,1,NA),col=c("grey",1,1), bty='n',ncol=3,cex=0.7)


  plot(station$STL$T$timestamp,
       station$STL$T$timestamp,type="n",ylim=c(-2,2),ylab="Delta T (C)",xlab="",
       main="Trends and annual means, 0 centered")

  st_mean=mean(station$yearly$T$values)

  for(period in names(station$STL$T$lm))
  {
    lines(station$STL$T$lm[[period]]$timestamp,
          station$STL$T$lm[[period]]$trend*station$STL$T$lm[[period]]$timestamp +
          station$STL$T$lm[[period]]$intercept-st_mean,lwd=2)
  }
  lines(station$yearly$T$timestamp,station$yearly$T$values-st_mean,type="b")

  i=1
  for (met_station in names(station$meteo))
  {
    st_mean=mean(station$meteo[[met_station]]$yearly$T$values,na.rm=TRUE)
    lines(station$meteo[[met_station]]$yearly$TA$timestamp,
          station$meteo[[met_station]]$yearly$TA$values-st_mean,type="b",col=cols[i])

    for(period in names(station$STL$meteo[[met_station]]$TA$lm))
    {

    lines(station$STL$meteo[[met_station]]$TA$lm[[period]]$timestamp,
          station$STL$meteo[[met_station]]$TA$lm[[period]]$trend*
            station$STL$meteo[[met_station]]$TA$lm[[period]]$timestamp +
            station$STL$meteo[[met_station]]$TA$lm[[period]]$intercept-st_mean,
          lwd=2,col=cols[i])
    }
    i=i+1
  }

  if(output_type=="PDF" || output_type=="PNG"){
    dev.off()
  }

  ############ Q - P ############

  # Prepare output
  if(output_type=="PDF"){
    pdf(paste0("plots/analysis/",name,"/Trends_Q_",name,".pdf"),width=12,height=8)
  }
  else if(output_type=="PNG"){
    png(paste0("plots/analysis/",name,"/Trends_Q_",name,".png"),width=2048,
        height=1536, res = 150)
  }

  par(mar=c(2,3,3,1),mgp=c(1.7,0.5,0))
  par(mfrow=c(3,1))
  # Plot trend for Q
  plot(station$STL$Q$timestamp, station$STL$Q$remainder + station$STL$Q$trend,
       xlab="",ylab="Q (mm/h)",type="l",col="grey",main="Water discharge trend")

  i=1
  leg1=c()
  leg2=c()

  for(period in names(station$STL$Q$lm))
  {
    lines(station$STL$Q$lm[[period]]$timestamp,
          station$STL$Q$lm[[period]]$trend*station$STL$Q$lm[[period]]$timestamp +
            station$STL$Q$lm[[period]]$intercept,lwd=2,col=i)
    i=i+1

    leg1=c(leg1,paste0("Trend ",period,": ",
                       round(station$STL$Q$lm[[period]]$trend*10,digits=2),
                       "+/-",round(station$STL$Q$lm[[period]]$trend_std*10,digits=3),
                       " (mm/h//dec)"))
    leg2=c(leg2,paste0("r^2: ",station$STL$Q$lm[[period]]$printable$adj_r_squared))
  }

  lines(station$yearly$Q$timestamp,station$yearly$Q$values,type="b")

  legend("bottom",legend=c(leg1,leg2),bty='n',ncol=2,cex=0.7)
  legend("top",legend=c("Deseasonalyzed ts","Annual means", "trend"),
         lty=c(1,1,1),pch=c(NA,1,NA),col=c("grey",1,1), bty='n',ncol=3,cex=0.7)

  #Plot trend for P
  i=1
  # Find minumun and maximum of meteo times series to adjust plots
  minimum=1000
  maximum=-1000
  for (met_station in names(station$meteo))
  {
    minimum=min(minimum,station$STL$meteo[[met_station]]$P$remainder+
                  station$STL$meteo[[met_station]]$P$trend,na.rm=TRUE)
    maximum=max(maximum,station$STL$meteo[[met_station]]$P$remainder+
                  station$STL$meteo[[met_station]]$P$trend,na.rm=TRUE)
  }

  plot(station$STL$meteo[[1]]$P$timestamp,
       station$STL$meteo[[1]]$P$remainder +
         station$STL$meteo[[1]]$P$trend,
       xlab="",ylab="P (mm/h)",type="n",main="Precipitation trend",
       ylim=c(minimum,maximum))

  for (met_station in names(station$meteo))
  {
    lines(station$STL$meteo[[met_station]]$P$timestamp,
          station$STL$meteo[[met_station]]$P$remainder +
            station$STL$meteo[[met_station]]$P$trend,
          type="l",col=lightcols[i])
    i=i+1
  }
  i=1
  for (met_station in names(station$meteo))
  {
    for(period in names(station$STL$meteo[[met_station]]$P$lm))
    {
      lines(station$STL$meteo[[met_station]]$P$lm[[period]]$timestamp,
            station$STL$meteo[[met_station]]$P$lm[[period]]$trend*
              station$STL$meteo[[met_station]]$P$lm[[period]]$timestamp +
              station$STL$meteo[[met_station]]$P$lm[[period]]$intercept,lwd=2,
            col=cols[i],ylab="")
    }
    lines(station$meteo[[met_station]]$yearly$P$timestamp,
          station$meteo[[met_station]]$yearly$P$values,type="b",col=cols[i])
    i=i+1
  }


  i=1
  for (met_station in names(station$meteo))
  {
    leg1=c()
    leg2=c()
    leg3=c()
    for(period in names(station$STL$meteo[[met_station]]$P$lm))
    {
      leg1=c(leg1,paste(met_station,period))
      leg2=c(leg2,paste0("Trend: ",round(station$STL$meteo[[met_station]]$P$lm[[period]]$trend*10,
                                         digits=2),
                         "+/-",round(station$STL$meteo[[met_station]]$P$lm[[period]]$trend_std*10,
                                     digits=3)," (C/dec)"))
      leg3=c(leg3,paste0("r^2: ",station$STL$meteo[[met_station]]$P$lm[[period]]$printable$adj_r_squared))
    }
    n=length(names(station$STL$meteo[[met_station]]$P$lm))
    lty=rep(NA,times=3*n)
    lty[1:n]=1
    col=lty
    col[1:n]=cols[i]
    legend(positions[i],legend=c(leg1,leg2,leg3),
           lty=lty,col=col,bty='n',ncol=3,cex=0.7,x.intersp=0.1)
    i=i+1
  }

  legend("top",legend=c("Deseasonalyzed ts","Annual means", "trend"),
         lty=c(1,1,1),pch=c(NA,1,NA),col=c("grey",1,1), bty='n',ncol=3,cex=0.7)


  plot(station$STL$Q$timestamp,
       station$STL$Q$timestamp,type="n",ylim=c(0.5,1.5),ylab="Delta Q-P",xlab="",
       main="Trends and annual means, normalized and 0 centered")

  st_mean=mean(station$yearly$Q$values)

  for(period in names(station$STL$Q$lm))
  {
    lines(station$STL$Q$lm[[period]]$timestamp,
          (station$STL$Q$lm[[period]]$trend*station$STL$Q$lm[[period]]$timestamp +
            station$STL$Q$lm[[period]]$intercept)/st_mean,lwd=2)
  }
  lines(station$yearly$Q$timestamp,(station$yearly$Q$values/st_mean),type="b")

  i=1
  for (met_station in names(station$meteo))
  {
    st_mean=mean(station$meteo[[met_station]]$yearly$P$values,na.rm=TRUE)
    lines(station$meteo[[met_station]]$yearly$P$timestamp,
          (station$meteo[[met_station]]$yearly$P$values)/st_mean,type="b",col=cols[i])

    for(period in names(station$STL$meteo[[met_station]]$P$lm))
    {

      lines(station$STL$meteo[[met_station]]$P$lm[[period]]$timestamp,
            (station$STL$meteo[[met_station]]$P$lm[[period]]$trend*
               station$STL$meteo[[met_station]]$P$lm[[period]]$timestamp +
              station$STL$meteo[[met_station]]$P$lm[[period]]$intercept)/st_mean,
            lwd=2,col=cols[i])
    }
    i=i+1
  }
  #
  if(output_type=="PDF" || output_type=="PNG"){
   dev.off()
  }
}


#########################################################
#########################################################
#########################################################


#' Plot time series and means
#'
#' This function produces plots for time series and monthly and annual means
#' for the given station and for the variables T, Q, TA, P. These plots are
#' not used in the paper.
#'
#' @param station A SMET object containing the data for one station
#' @param output_type Either \code{"NONE"} (default), \code{"PDF"} or \code{"PNG"}.
#'
#' \code{output_type = "NONE"} creates the plot in a normal plot window (default),
#'
#' \code{output_type = "PDF"} saves the plot as pdf in plots/analysis/'station_name'/,
#'
#' \code{output_type = "PNG"} saves the plot as png in plots/analysis/'station_name'/,
#' @export
plot_time_series <- function(station,output_type="NONE")
{
  name=station$header$station_name
  print(paste("[I] *** Plotting time series for station",name,"***"))

  #Prepare output
  if(output_type=="PDF"){
    pdf(paste0("plots/analysis/",name,"/Time_series_",name,".pdf"),
        width=12,height=8)
  }
  else if(output_type=="PNG"){
    png(paste0("plots/analysis/",name,"/Time_series_",name,".png"),
        width=2048,height=768, res = 150)
  }

  #Plot Q for the whole TS
  par(mfrow=c(3,1),mar=c(2,3,0,1),mgp=c(1.7,0.5,0),oma = c(0, 0, 2, 0))
  #Plot T for the whole TS
  plot(decimal_date(station$data$timestamp[!is.na(station$data$T)]),
       station$data$T[!is.na(station$data$T)],xlab="",ylab="T (C)",type="l",ylim=c(0,25))
  lines(station$monthly$T$timestamp,station$monthly$T$values,type="b",col=3,pch=20)
  lines(station$yearly$T$timestamp,station$yearly$T$values,type="b",col=4,pch=20,lwd=2)
  mtext(name, outer = TRUE, cex = 1.2,line=0.4)
  # Add meteo TS
  plot_meteo=c()
  col_meteo=c()
  pch_meteo=c()

  c=6
  for (met_station in names(station$meteo))
  {
    lines(station$meteo[[met_station]]$yearly$TA$timestamp,
          station$meteo[[met_station]]$yearly$TA$values,type="b",col=c,pch=3,lwd=1)
    if(length(station$meteo[[met_station]]$yearly$TA_HOM)>0){
      lines(station$meteo[[met_station]]$yearly$TA_HOM$imestamp,
            station$meteo[[met_station]]$yearly$TA_HOM$values,type="b",col=c,pch=8,lwd=1)
      plot_meteo=c(plot_meteo,paste(met_station,"homogeneous T"))
      col_meteo=c(col_meteo,c)
      pch_meteo=c(pch_meteo,8)
    }
    plot_meteo=c(plot_meteo,paste(met_station,"non homogeneous T"))
    col_meteo=c(col_meteo,c)
    pch_meteo=c(pch_meteo,3)
    c=c+1
  }

  # Add legend
  legend("topleft",legend=c("RAW","Monthly mean","Yearly mean",plot_meteo),
         col=c(1,3,4,col_meteo),pch=c(NA,20,20,pch_meteo),lty=1,bty='n',ncol=2)


  plot(decimal_date(station$data$timestamp[!is.na(station$data$Q)]),
       station$data$Q[!is.na(station$data$Q)],xlab="",ylab="Q (m3/s)",type="l")
  lines(station$monthly$Q$timestamp,station$monthly$Q$values,type="b",col=3,pch=20)
  lines(station$yearly$Q$timestamp,station$yearly$Q$values,type="b",col=4,pch=20,lwd=2)
  legend("topleft",legend=c("RAW","Monthly mean","Yearly mean"),
         col=c(1,3,4),pch=c(NA,20,20),lty=1,bty='n',ncol=2)

  plot_meteo=c()
  col_meteo=c()
  pch_meteo=c()
  c=6
  plot(decimal_date(station$data$timestamp),station$data$T,type="n",col=c,
       pch=3,lwd=1,ylim=c(0,10),xlab="",ylab="mean P (mm/hr)")
  for (met_station in names(station$meteo))
  {
    lines(station$meteo[[met_station]]$yearly$P$timestamp,
          station$meteo[[met_station]]$yearly$P$values,type="b",col=c,pch=3,lwd=1)
    if(length(station$meteo[[met_station]]$yearly$P_HOM$raw)>0){
      lines(station$meteo[[met_station]]$yearly$P_HOM$timestamp,
            station$meteo[[met_station]]$yearly$P_HOM$values,type="b",col=c,pch=8,lwd=1)
      plot_meteo=c(plot_meteo,paste(met_station,"homogeneous PSUM"))
      col_meteo=c(col_meteo,c)
      pch_meteo=c(pch_meteo,8)
    }
    plot_meteo=c(plot_meteo,paste(met_station,"non homogeneous PSUM"))
    col_meteo=c(col_meteo,c)
    pch_meteo=c(pch_meteo,3)
    c=c+1
  }

  legend("topleft",legend=c(plot_meteo),
         col=c(col_meteo),pch=c(pch_meteo),lty=1,bty='n',ncol=2)

  if(output_type=="PDF" || output_type=="PNG"){
    dev.off()
  }
}


#########################################################
#########################################################
#########################################################


#' Plot component of STL for one station
#'
#' This function plots the component of the STL for the four variables T, Q, TA
#' and P for the water station passed in parameters and the associated meteoSwiss
#' stations. This produces the plots shown in Figures S1 to S4 in supplementary.
#'
#' @param station A SMET object containing the data for one station
#' @param output_type Either \code{"NONE"} (default), \code{"PDF"} or \code{"PNG"}.
#'
#' \code{output_type = "NONE"} creates the plot in a normal plot window,
#'
#' \code{output_type = "PDF"} saves the plot as pdf under plots/General_situation.pdf,
#'
#' \code{output_type = "PNG"} saves the plot as png under plots/General_situation.png

#' @export
plot_stl <- function(station,output_type="NONE")
{

  name=station$header$station_name
  print(paste("[I] *** Plotting STL for station",name,"***"))

  if(output_type=="PDF"){
    pdf(paste0("plots/analysis/",name,"/STL_T_",name,".pdf"),width=12,height=8)
  }
  else if(output_type=="PNG"){
    png(paste0("plots/analysis/",name,"/STL_T_",name,".png"),width=2048,
        height=1600, res = 150)
  }

  par(mfrow=c(4,1))
  par(mar=c(3,3.8,2,1),mgp=c(1.7,0.7,0),cex.axis=1.6,cex.lab=1.6,cex.main=1.3)
  plot(station$STL$T$timestamp,station$STL$T$raw,type="l",
       ylab=expression(paste("Temperature (",degree,"C)")),
       xlab="",main="Raw water temperature")
  plot(station$STL$T$timestamp,station$STL$T$seasonal,type="l",
       ylab=expression(paste("Temperature (",degree,"C)")),
       xlab="",main="Seasonnal water temperature")
  plot(station$STL$T$timestamp,station$STL$T$trend,type="l",
       ylab=expression(paste("Temperature (",degree,"C)")),
       xlab="",main="Trend water temperature")
  plot(station$STL$T$timestamp,station$STL$T$remainder,type="l",
       ylab=expression(paste("Temperature (",degree,"C)")),xlab="",
       main="Remainder water temperature")

  if(output_type=="PDF" || output_type=="PNG"){
    dev.off()
  }

  if(output_type=="PDF"){
    pdf(paste0("plots/analysis/",name,"/STL_Q_",name,".pdf"),width=12,height=8)
  }
  else if(output_type=="PNG"){
    png(paste0("plots/analysis/",name,"/STL_Q_",name,".png"),width=2048,
        height=1600, res = 150)
  }

  par(mfrow=c(4,1))
  par(mar=c(3,3.8,2,1),mgp=c(1.7,0.6,0),cex.axis=1.6,cex.lab=1.6,cex.main=1.3)
  plot(station$STL$Q$timestamp,station$STL$Q$raw,type="l",
       ylab=expression(paste("Discharge (m"^"3","s"^"-1",")")),xlab="",
       main="Raw discharge")
  plot(station$STL$Q$timestamp,station$STL$Q$seasonal,type="l",
       ylab=expression(paste("Discharge (m"^"3","s"^"-1",")")),xlab="",
       main="Seasonnal discharge")
  plot(station$STL$Q$timestamp,station$STL$Q$trend,type="l",
       ylab=expression(paste("Discharge (m"^"3","s"^"-1",")")),xlab="",
       main="Trend discharge")
  plot(station$STL$Q$timestamp,station$STL$Q$remainder,type="l",
       ylab=expression(paste("Discharge (m"^"3","s"^"-1",")")),xlab="",
       main="Remainder discharge")


  if(output_type=="PDF" || output_type=="PNG"){
    dev.off()
  }

  for (meteo_station in names(station$meteo))
  {

    if(output_type=="PDF"){
      pdf(paste0("plots/analysis/",name,"/STL_TA_",meteo_station,".pdf"),
          width=12,height=8)
    }
    else if(output_type=="PNG"){
      png(paste0("plots/analysis/",name,"/STL_TA_",meteo_station,".png"),
          width=2048,height=1600, res = 150)
    }
    par(mfrow=c(4,1))
    par(mar=c(3,3.8,2,1),mgp=c(1.7,0.6,0),cex.axis=1.6,cex.lab=1.6,cex.main=1.3)
    plot(station$STL$meteo[[meteo_station]]$TA$timestamp,
         station$STL$meteo[[meteo_station]]$TA$raw,type="l",
         ylab=expression(paste("Temperature (",degree,"C)")),xlab="",
         main="Raw air temperature")
    plot(station$STL$meteo[[meteo_station]]$TA$timestamp,
         station$STL$meteo[[meteo_station]]$TA$seasonal,type="l",
         ylab=expression(paste("Temperature (",degree,"C)")),xlab="",
         main="Seasonnal air temperature")
    plot(station$STL$meteo[[meteo_station]]$TA$timestamp,
         station$STL$meteo[[meteo_station]]$TA$trend,type="l",
         ylab=expression(paste("Temperature (",degree,"C)")),xlab="",
         main="Trend air temperature")
    plot(station$STL$meteo[[meteo_station]]$TA$timestamp,
         station$STL$meteo[[meteo_station]]$TA$remainder,type="l",
         ylab=expression(paste("Temperature (",degree,"C)")),xlab="",
         main="Remainder air temperature")
    if(output_type=="PDF" || output_type=="PNG"){
      dev.off()
    }

    if(output_type=="PDF"){
      pdf(paste0("plots/analysis/",name,"/STL_P_",meteo_station,".pdf"),
          width=12,height=8)
    }
    else if(output_type=="PNG"){
      png(paste0("plots/analysis/",name,"/STL_P_",meteo_station,".png"),
          width=2048,height=1600, res = 150)
    }
    par(mfrow=c(4,1))
    par(mar=c(3,3.8,2,1),mgp=c(1.7,0.6,0),cex.axis=1.6,cex.lab=1.6,cex.main=1.3)
    plot(station$STL$meteo[[meteo_station]]$P$timestamp,
         station$STL$meteo[[meteo_station]]$P$raw,type="l",
         ylab=expression(paste("P (mm"," h"^"-1",")")), xlab="",
         main="Raw precipitation")
    plot(station$STL$meteo[[meteo_station]]$P$timestamp,
         station$STL$meteo[[meteo_station]]$P$seasonal,type="l",
         ylab=expression(paste("P (mm"," h"^"-1",")")), xlab="",
         main="Seasonnal precipitation")
    plot(station$STL$meteo[[meteo_station]]$P$timestamp,
         station$STL$meteo[[meteo_station]]$P$trend,type="l",
         ylab=expression(paste("P (mm"," h"^"-1",")")), xlab="",
         main="Trend precipitation")
    plot(station$STL$meteo[[meteo_station]]$P$timestamp,
         station$STL$meteo[[meteo_station]]$P$remainder, type="l",
         ylab=expression(paste("P (mm"," h"^"-1",")")), xlab="",
         main="Remainder precipitation")
    if(output_type=="PDF" || output_type=="PNG"){
      dev.off()
    }
  }
}


#########################################################
#########################################################
#########################################################


#' Plot acf and pacf of the residuals of the STL analysis
#'
#' This function plots the acf and pacf of the residuals of the STL analysis
#' for the four variables T, Q, TA and P for the water station passed in
#' parameters and the associated meteoSwiss stations. This produces the plots
#' shown in Figures S7 and S8 in supplementary.
#'
#' @param station A SMET object containing the data for one station
#' @param output_type Either \code{"NONE"} (default), \code{"PDF"} or \code{"PNG"}.
#'
#' \code{output_type = "NONE"} creates the plot in a normal plot window (default),
#'
#' \code{output_type = "PDF"} saves the plot as pdf in plots/analysis/'station_name'/,
#'
#' \code{output_type = "PNG"} saves the plot as png in plots/analysis/'station_name'/,
#' @export

plot_acf <- function(station,output_type="NONE")
{

  name=station$header$station_name
  print(paste("[I] *** Plotting ACF for station",name,"***"))

  if(output_type=="PDF"){
    pdf(paste0("plots/analysis/",name,"/ACF_",name,".pdf"),width=12,height=8)
  }
  else if(output_type=="PNG"){
    png(paste0("plots/analysis/",name,"/ACF_",name,".png"),width=2048,
        height=1600, res = 150)
  }

  par(mfrow=c(2,2))
  par(mar=c(3,3.5,2,1),mgp=c(1.7,0.6,0),cex.axis=1.3,cex.lab=1.4,cex.main=1.2)
  plot(station$STL$T$acf,main="", ylim=c(0,1))
  title("ACF water temperature")
  plot(station$STL$T$pacf,main="", ylim=c(0,1))
  title("PACF water temperature")
  plot(station$STL$Q$acf,main="", ylim=c(0,1))
  title("ACF discharge")
  plot(station$STL$Q$pacf,main="", ylim=c(0,1))
  title("PACF discharge")

  if(output_type=="PDF" || output_type=="PNG"){
    dev.off()
  }

  for (meteo_station in names(station$meteo))
  {
    if(output_type=="PDF"){
      pdf(paste0("plots/analysis/",name,"/ACF_",meteo_station,".pdf"),width=12,height=8)
    }
    else if(output_type=="PNG"){
      png(paste0("plots/analysis/",name,"/ACF_",meteo_station,".png"),width=2048,height=1600, res = 150)
    }

    par(mfrow=c(2,2))
    par(mar=c(3,3.5,2,1),mgp=c(1.7,0.6,0),cex.axis=1.3,cex.lab=1.4,cex.main=1.2)
    plot(station$STL$meteo[[meteo_station]]$TA$acf,main="", ylim=c(0,1))
    title("ACF air temperature")
    plot(station$STL$meteo[[meteo_station]]$TA$pacf,main="", ylim=c(0,1))
    title("PACF air temperature")
    #plot(station$STL$meteo[[meteo_station]]$TA$ccf,main="", ylim=c(0,1))
    #title("CCF air temp")
    plot(station$STL$meteo[[meteo_station]]$P$acf,main="", ylim=c(0,1))
    title("ACF precipitation")
    plot(station$STL$meteo[[meteo_station]]$P$pacf,main="", ylim=c(0,1))
    title("PACF precipitation")
    #plot(station$STL$meteo[[meteo_station]]$P$ccf,main="", ylim=c(0,1))
    #title("CCF air temp")

    if(output_type=="PDF" || output_type=="PNG"){
      dev.off()
    }
  }
}


#########################################################
#########################################################
#########################################################


#' Plot hysteresis
#'
#' This function plots the day-of-the -year decadal mean of Q and T for the
#' given station along wiht the Q-T hysteresis plot. This function produces the
#' plot shown in Figure 15
#'
#' @param station A SMET object containing the data for one station
#' @param output_type Either \code{"NONE"} (default), \code{"PDF"} or \code{"PNG"}.
#'
#' \code{output_type = "NONE"} creates the plot in a normal plot window (default),
#'
#' \code{output_type = "PDF"} saves the plot as pdf in plots/analysis/'station_name'/,
#'
#' \code{output_type = "PNG"} saves the plot as png in plots/analysis/'station_name'/,
#' @export

plot_hysteresis <- function(station,output_type="NONE")
{

  name=station$header$station_name
  print(paste("[I] *** Plotting Hysteresis for station",name,"***"))

  if(output_type=="PDF"){
    pdf(paste0("plots/analysis/",name,"/Hysteresis_",name,".pdf"),width=12,height=8)
  }
  else if(output_type=="PNG"){
    png(paste0("plots/analysis/",name,"/Hysteresis_",name,".png"),width=2048,height=1600, res = 150)
  }

  layout(matrix(c(1,1,3,3,2,2,3,3), nrow = 2, ncol = 4, byrow = TRUE))
  par(mar=c(4,4,0.5,1),mgp=c(2,0.8,0),oma = c(0, 0, 0, 0),cex.lab=1.5,cex.axis=1.5)

  seasons=list(winter1=list(period=c(1:59),color="blue"),
               spring=list(period=c(60:150),color="green"),
               summer=list(period=c(151:242),color="orange"),
               autumn=list(period=c(243:334),color="brown"),
               winter2=list(period=c(335:365),color="blue")
  )
  names(station$hysteresis$daily_mean_smoothed[[1]])
  Q_lim=c()
  T_lim=c()
  for(i in 1:length(station$hysteresis$daily_mean_smoothed)){
    Q_lim=c(Q_lim,floor(min(station$hysteresis$daily_mean_smoothed[[i]]$Q,na.rm = T)),
            ceiling(max(station$hysteresis$daily_mean_smoothed[[i]]$Q,na.rm = T)))
    T_lim=c(T_lim,floor(min(station$hysteresis$daily_mean_smoothed[[i]]$T,na.rm = T)),
            ceiling(max(station$hysteresis$daily_mean_smoothed[[i]]$T,na.rm = T)))
  }

  lty_table=c(3,4,6,5,1)
  ## Plot all periods together ##
  Q_lim=c(min(Q_lim),max(Q_lim))
  T_lim=c(min(T_lim),max(T_lim))
  plot(1, xlab="",type="l",ylab=expression(paste("Discharge (m"^"3"," s"^"-1",")")),ylim=Q_lim,xlim=c(0,365))
  for(i in 1:length(station$hysteresis$daily_mean_smoothed)){
    lty = lty_table[length(station$hysteresis$daily_mean_smoothed) -i + 1]
    lines(seasons[["winter1"]]$period,station$hysteresis$daily_mean_smoothed[[i]]$Q[seasons[["winter1"]]$period],
          type="l", col=seasons[["winter1"]]$color,lty=lty)
    lines(seasons[["spring"]]$period,station$hysteresis$daily_mean_smoothed[[i]]$Q[seasons[["spring"]]$period],
          type="l", col=seasons[["spring"]]$color,lty=lty)
    lines(seasons[["summer"]]$period,station$hysteresis$daily_mean_smoothed[[i]]$Q[seasons[["summer"]]$period],
          type="l", col=seasons[["summer"]]$color,lty=lty)
    lines(seasons[["autumn"]]$period,station$hysteresis$daily_mean_smoothed[[i]]$Q[seasons[["autumn"]]$period],
          type="l", col=seasons[["autumn"]]$color,lty=lty)
    lines(seasons[["winter2"]]$period,station$hysteresis$daily_mean_smoothed[[i]]$Q[seasons[["winter2"]]$period],
          type="l", col=seasons[["winter2"]]$color,lty=lty)

  }
  mtext(side=1,text="DOY",line=2.3)

  plot(0, type="n",xlab="",ylab=expression(paste("Water temperature (",degree,"C)")),ylim=T_lim,xlim=c(0,365))
  for(i in 1:length(station$hysteresis$daily_mean_smoothed)){
    lty = lty_table[length(station$hysteresis$daily_mean_smoothed) -i + 1]
    lines(seasons[["winter1"]]$period,station$hysteresis$daily_mean_smoothed[[i]]$T[seasons[["winter1"]]$period],
          type="l", col=seasons[["winter1"]]$color,lty=lty)
    lines(seasons[["spring"]]$period,station$hysteresis$daily_mean_smoothed[[i]]$T[seasons[["spring"]]$period],
          type="l", col=seasons[["spring"]]$color,lty=lty)
    lines(seasons[["summer"]]$period,station$hysteresis$daily_mean_smoothed[[i]]$T[seasons[["summer"]]$period],
          type="l", col=seasons[["summer"]]$color,lty=lty)
    lines(seasons[["autumn"]]$period,station$hysteresis$daily_mean_smoothed[[i]]$T[seasons[["autumn"]]$period],
          type="l", col=seasons[["autumn"]]$color,lty=lty)
    lines(seasons[["winter2"]]$period,station$hysteresis$daily_mean_smoothed[[i]]$T[seasons[["winter2"]]$period],
          type="l", col=seasons[["winter2"]]$color,lty=lty)
  }
  mtext(side=1,text="DOY",line=2.3)

  plot(0, type="l",xlab="",ylab=expression(paste("Water temperature (",degree,"C)")),xlim=Q_lim,ylim=T_lim)
  lty_legend=c()
  period_legend=c()
  for(i in 1:length(station$hysteresis$daily_mean_smoothed)){
    lty = lty_table[length(station$hysteresis$daily_mean_smoothed) -i + 1]
    lty_legend=c(lty_legend,lty)
    period_legend=c(period_legend,gsub("_","-",names(station$hysteresis$daily_mean_smoothed)[i]))
    lines(station$hysteresis$daily_mean_smoothed[[i]]$Q[seasons[["winter1"]]$period],
          station$hysteresis$daily_mean_smoothed[[i]]$T[seasons[["winter1"]]$period],
          type="l", col=seasons[["winter1"]]$color,lty=lty)
    lines(station$hysteresis$daily_mean_smoothed[[i]]$Q[seasons[["spring"]]$period],
          station$hysteresis$daily_mean_smoothed[[i]]$T[seasons[["spring"]]$period],
          type="l", col=seasons[["spring"]]$color,lty=lty)
    lines(station$hysteresis$daily_mean_smoothed[[i]]$Q[seasons[["summer"]]$period],
          station$hysteresis$daily_mean_smoothed[[i]]$T[seasons[["summer"]]$period],
          type="l", col=seasons[["summer"]]$color,lty=lty)
    lines(station$hysteresis$daily_mean_smoothed[[i]]$Q[seasons[["autumn"]]$period],
          station$hysteresis$daily_mean_smoothed[[i]]$T[seasons[["autumn"]]$period],
          type="l", col=seasons[["autumn"]]$color,lty=lty)
    lines(station$hysteresis$daily_mean_smoothed[[i]]$Q[seasons[["winter2"]]$period],
          station$hysteresis$daily_mean_smoothed[[i]]$T[seasons[["winter2"]]$period],
          type="l", col=seasons[["winter2"]]$color,lty=lty)
  }
  mtext(side=1,text=expression(paste("Discharge (m"^"3"," s"^"-1",")")),line=2.6)

  legend("topleft",legend=c("DJF","MAM","JJA","SON"),col=c("blue","green","orange","brown"),lty=1,bty='n',cex=1.3,title="Season")
  legend("topright",legend=period_legend,col=1,lty=lty_legend,bty='n',cex=1.3,title="Decade")
  if(output_type=="PDF" || output_type=="PNG"){
    dev.off()
  }
}


#########################################################
#########################################################
#########################################################


#' Plot variable distributions and compute t-tests
#'
#' This function produces the plots  variable distributions as shown in Figures
#' 5, 6, and 10, and in Figures S14, and S16 to S21 in supplementary. It also computes
#' wilcox test shown in Table S3 in supplementary and print the results to the console.

#' @param period A string, either "1999-2018", "1979-1998", "1979-2018", or
#' "1970-2018" defining the period over which the plots and analysis should be
#' produced
#' @param rivers_data Either \code{"NONE"} (default), \code{"PDF"} or \code{"PNG"}.
#'
#' @export

genreal_analysis_plots <- function(period,rivers_data)
{
  get_regimes2_colors <- function(regimes2){
    colors=c()
    for (value in regimes2)
    {
      if(is.na(value))
        colors=c(colors,1)
      else if(value==" Undefined")
        colors=c(colors,1)
      else if(value=="Strong influence of Hydropeaking")
        colors=c(colors,2)
      else if(value=="After lakes")
        colors=c(colors,3)
      else if(value=="Regime from Plateau and Jura")
        colors=c(colors,4)
      else
        colors=c(colors,5)
    }
    return(colors)
  }

  get_glacier_colors <- function(glacier){
    colors=c()
    for (value in suppressWarnings(as.integer(glacier)))
    {
      if(!is.na(value))
      {
        if(value==0)
          colors=c(colors,1)
        else if(value<5)
          colors=c(colors,2)
        else if(value<10)
          colors=c(colors,3)
        else
          colors=c(colors,4)
      }
      else
      {
        colors=c(colors,0)
      }
    }
    return(colors)
  }

  get_glacier_labels <- function(glacier){
    labels=c()
    for (value in suppressWarnings(as.integer(glacier)))
    {
      if(!is.na(value))
      {
        if(value==0)
          labels=c(labels,"< 0.1 %")
        else if(value<5)
          labels=c(labels,"< 5%")
        else if(value<10)
          labels=c(labels,"<10 %")
        else
          labels=c(labels,">= 10%")
      }
      else
      {
        labels=c(labels,"< 0.1 %")
      }
    }
    return(labels)
  }

  get_area_colors <- function(area){
    colors=c()
    for (value in as.integer(area))
    {
      if(!is.na(value))
      {
        if(value<100)
          colors=c(colors,1)
        else if(value<1000)
          colors=c(colors,2)
        else if(value<10000)
          colors=c(colors,3)
        else
          colors=c(colors,4)
      }
      else
      {
        colors=c(colors,0)
      }
    }
    return(colors)
  }
  get_area_labels <- function(area){
    labels=c()
    for (value in as.integer(area))
    {
      if(!is.na(value))
      {
        if(value<100)
          labels=c(labels,"< 100 km2")
        else if(value<1000)
          labels=c(labels,"< 1000 km2")
        else if(value<10000)
          labels=c(labels,"< 10'000 km2")
        else
          labels=c(labels,">= 10'000 km2")
      }
      else
      {
        labels=c(labels,0)
      }
    }
    return(labels)
  }

  get_elevation_colors <- function(elevation){
    colors=c()
    for (value in as.integer(elevation))
    {
      if(!is.na(value))
      {
        if(value<700)
          colors=c(colors,1)
        else if(value<1200)
          colors=c(colors,2)
        else if(value<1800)
          colors=c(colors,3)
        else
          colors=c(colors,4)
      }
      else
      {
        colors=c(colors,0)
      }
    }
    return(colors)
  }
  get_elevation_labels <- function(elevation){
    labels=c()
    for (value in as.integer(elevation))
    {
      if(!is.na(value))
      {
        if(value<700)
          labels=c(labels,"< 700m")
        else if(value<1200)
          labels=c(labels,"< 1200m")
        else if(value<1800)
          labels=c(labels,"< 1800m")
        else
          labels=c(labels,">= 1800m")
      }
      else
      {
        labels=c(labels,"")
      }
    }
    return(labels)
  }

  print_correlation <-function(a,b,outer=TRUE){
    if(outer)
    {
      mtext(side=3, outer=TRUE,paste0("cor1=",round(cor(a,b,use="complete.obs"),3),", spearman cor=",round(cor(a,b,method="spearman",use="complete.obs"),3)),line=-1)
    }
    else{
      legend("top",legend=paste0("cor=",round(cor(a,b,use="complete.obs"),3),", spearman cor=",round(cor(a,b,method="spearman",use="complete.obs"),3)),bty='n')
    }
  }
  #Create data
  name=c()
  ts_length=c()
  regimes1=c()
  regimes2=c()
  elevation=c()
  area=c()
  glacier=c()
  t=c()
  q=c()
  q_norm=c()
  ta=c()
  p=c()
  p_norm=c()
  summer_t=c()
  summer_ta=c()
  summer_q_norm=c()
  summer_p_norm=c()
  winter_t=c()
  winter_ta=c()
  winter_q_norm=c()
  winter_p_norm=c()
  spring_t=c()
  spring_ta=c()
  spring_q_norm=c()
  spring_p_norm=c()
  fall_t=c()
  fall_ta=c()
  fall_q_norm=c()
  fall_p_norm=c()

    for (river_station in names(rivers_data)){
    name=c(name,rivers_data[[river_station]]$header$station_name)
    ts_length=c(ts_length,length(unique(floor(rivers_data[[river_station]]$STL$T$timestamp))))
    regimes1=c(regimes1,rivers_data[[river_station]]$header$regime1)
    regimes2=c(regimes2,rivers_data[[river_station]]$header$regime2)
    elevation=c(elevation,rivers_data[[river_station]]$header$mean_elevation)
    area=c(area,rivers_data[[river_station]]$header$area)
    glacier=c(glacier,rivers_data[[river_station]]$header$glacier_percent)

    t=c(t,ifelse(period %in% names(rivers_data[[river_station]]$STL$T$lm),
                 rivers_data[[river_station]]$STL$T$lm[[period]]$trend*10,NA))

    q=c(q,ifelse(period %in% names(rivers_data[[river_station]]$STL$Q$lm),
                 rivers_data[[river_station]]$STL$Q$lm[[period]]$trend*100*10,NA))
    q_norm=c(q_norm,ifelse(period %in% names(rivers_data[[river_station]]$STL$Q$lm),
              rivers_data[[river_station]]$STL$Q$lm[[period]]$trend/
               mean(rivers_data[[river_station]]$STL$Q$lm[[period]]$values,na.rm=TRUE)*100*10,NA))

    summer_t=c(summer_t,ifelse(period %in% names(rivers_data[[river_station]]$JJA$T$lm),
               rivers_data[[river_station]]$JJA$T$lm[[period]]$trend*10,NA))
    summer_q_norm=c(summer_q_norm,ifelse(period %in% names(rivers_data[[river_station]]$JJA$Q$lm),
                    rivers_data[[river_station]]$JJA$Q$lm[[period]]$trend/
                    mean(rivers_data[[river_station]]$JJA$Q$lm[[period]]$values,na.rm=TRUE)*100*10,NA))

    winter_t=c(winter_t,ifelse(period %in% names(rivers_data[[river_station]]$DJF$T$lm),
               rivers_data[[river_station]]$DJF$T$lm[[period]]$trend*10,NA))
    winter_q_norm=c(winter_q_norm,ifelse(period %in% names(rivers_data[[river_station]]$DJF$Q$lm),
                    rivers_data[[river_station]]$DJF$Q$lm[[period]]$trend/
                    mean(rivers_data[[river_station]]$DJF$Q$lm[[period]]$values,na.rm=TRUE)*100*10,NA))
    spring_t=c(spring_t,ifelse(period %in% names(rivers_data[[river_station]]$MAM$T$lm),
               rivers_data[[river_station]]$MAM$T$lm[[period]]$trend*10,NA))
    spring_q_norm=c(spring_q_norm,ifelse(period %in% names(rivers_data[[river_station]]$MAM$Q$lm),
                    rivers_data[[river_station]]$MAM$Q$lm[[period]]$trend/
                    mean(rivers_data[[river_station]]$MAM$Q$lm[[period]]$values,na.rm=TRUE)*100*10,NA))
    fall_t=c(fall_t,ifelse(period %in% names(rivers_data[[river_station]]$SON$T$lm),
             rivers_data[[river_station]]$SON$T$lm[[period]]$trend*10,NA))
    fall_q_norm=c(fall_q_norm,ifelse(period %in% names(rivers_data[[river_station]]$SON$Q$lm),
                  rivers_data[[river_station]]$SON$Q$lm[[period]]$trend/
                  mean(rivers_data[[river_station]]$SON$Q$lm[[period]]$values,na.rm=TRUE)*100*10,NA))
    ta_mean=c()
    p_mean=c()
    p_norm_mean=c()
    summer_ta_mean=c()
    winter_ta_mean=c()
    summer_p_norm_mean=c()
    winter_p_norm_mean=c()
    spring_ta_mean=c()
    spring_p_norm_mean=c()
    fall_ta_mean=c()
    fall_p_norm_mean=c()

    for (meteo_station in rivers_data[[river_station]]$STL$meteo)
    {

      ta_mean=c(ta_mean,ifelse(period %in% names(meteo_station$TA$lm) &
                               period %in% names(rivers_data[[river_station]]$STL$T$lm),
                               meteo_station$TA$lm[[period]]$trend*10,NA))
      p_mean=c(p_mean,ifelse(period %in% names(meteo_station$P$lm),meteo_station$P$lm[[period]]$trend*
            as.numeric(rivers_data[[river_station]]$header$area)*1000000/1000/3600*100,NA))
      p_norm_mean=c(p_norm_mean,ifelse(period %in% names(meteo_station$P$lm),meteo_station$P$lm[[period]]$trend/
                 mean(meteo_station$P$lm[[period]]$values,na.rm=TRUE)*100*10,NA))
    }

    for (meteo_station in rivers_data[[river_station]]$meteo)
    {

      summer_ta_mean=c(summer_ta_mean,ifelse((period %in% names(meteo_station$JJA$TA$lm)) &
                                            (period %in% names(rivers_data[[river_station]]$JJA$T$lm)),
                                            meteo_station$JJA$TA$lm[[period]]$trend*10,NA))

      winter_ta_mean=c(winter_ta_mean,ifelse(period %in% names(meteo_station$DJF$TA$lm) &
                                             period %in% names(rivers_data[[river_station]]$DJF$T$lm),
                                             meteo_station$DJF$TA$lm[[period]]$trend*10,NA))

      summer_p_norm_mean=c(summer_p_norm_mean,ifelse(period %in% names(meteo_station$JJA$P$lm) &
                                                     period %in% names(rivers_data[[river_station]]$JJA$Q$lm),
                                                     meteo_station$JJA$P$lm[[period]]$trend/
                                                     mean(meteo_station$JJA$P$lm[[period]]$values,na.rm=TRUE)*100*10,NA))

      winter_p_norm_mean=c(winter_p_norm_mean,ifelse(period %in% names(meteo_station$DJF$P$lm) &
                                                    period %in% names(rivers_data[[river_station]]$DJF$Q$lm),
                                                     meteo_station$DJF$P$lm[[period]]$trend/
                                                     mean(meteo_station$DJF$P$lm[[period]]$values,na.rm=TRUE)*100*10,NA))

      spring_ta_mean=c(spring_ta_mean,ifelse(period %in% names(meteo_station$MAM$TA$lm) &
                                             period %in% names(rivers_data[[river_station]]$MAM$T$lm),
                                             meteo_station$MAM$TA$lm[[period]]$trend*10,NA))

      spring_p_norm_mean=c(spring_p_norm_mean,ifelse(period %in% names(meteo_station$MAM$P$lm) &
                                                     period %in% names(rivers_data[[river_station]]$MAM$Q$lm),
                                                     meteo_station$MAM$P$lm[[period]]$trend/
                                                     mean(meteo_station$MAM$P$lm[[period]]$values,na.rm=TRUE)*10*100,NA))

      fall_ta_mean=c(fall_ta_mean,ifelse(period %in% names(meteo_station$SON$TA$lm) &
                                         period %in% names(rivers_data[[river_station]]$SON$T$lm),
                                         meteo_station$SON$TA$lm[[period]]$trend*10,NA))

      fall_p_norm_mean=c(fall_p_norm_mean,ifelse(period %in% names(meteo_station$SON$P$lm)&
                                                 period %in% names(rivers_data[[river_station]]$SON$Q$lm),
                                                 meteo_station$SON$P$lm[[period]]$trend/
                                                 mean(meteo_station$SON$P$lm[[period]]$values,na.rm=TRUE)*100*10,NA))
    }

    ta=c(ta,mean(ta_mean))
    p=c(p,mean(p_mean))
    p_norm=c(p_norm,mean(p_norm_mean))
    summer_ta=c(summer_ta,mean(summer_ta_mean))
    winter_ta=c(winter_ta,mean(winter_ta_mean))
    summer_p_norm=c(summer_p_norm,mean(summer_p_norm_mean))
    winter_p_norm=c(winter_p_norm,mean(winter_p_norm_mean))
    spring_ta=c(spring_ta,mean(spring_ta_mean))
    spring_p_norm=c(spring_p_norm,mean(spring_p_norm_mean))
    fall_ta=c(fall_ta,mean(fall_ta_mean))
    fall_p_norm=c(fall_p_norm,mean(fall_p_norm_mean))
  }

  length(which(!is.na(q_norm)))

  mean(q_norm,na.rm=TRUE)
  nans=which(is.na(t) | is.na(q))
  ts_length[nans]=NA
  name[nans]=NA
  regimes1[nans]=NA
  regimes2[nans]=NA
  elevation[nans]=NA
  area[nans]=NA
  glacier[nans]=NA
  t[nans]=NA
  q[nans]=NA
  q_norm[nans]=NA
  ta[nans]=NA
  p[nans]=NA
  p_norm[nans]=NA
  summer_t[nans]=NA
  summer_ta[nans]=NA
  summer_q_norm[nans]=NA
  summer_p_norm[nans]=NA
  winter_t[nans]=NA
  winter_ta[nans]=NA
  winter_q_norm[nans]=NA
  winter_p_norm[nans]=NA
  spring_t[nans]=NA
  spring_ta[nans]=NA
  spring_q_norm[nans]=NA
  spring_p_norm[nans]=NA
  fall_t[nans]=NA
  fall_ta[nans]=NA
  fall_q_norm[nans]=NA
  fall_p_norm[nans]=NA

  mean(t,na.rm=TRUE)
  mean(q_norm,na.rm=TRUE)
  mean(ta,na.rm=TRUE)
  mean(p_norm,na.rm=TRUE)

  ### HISTOGRAMS ###
  par(mfrow=c(2,2))
  par(mar=c(3,3,1,1),mgp=c(1.7,0.5,0),oma=c(0,0,2,0),cex.lab=1.4,cex.axis=1.3)
  c(t,ta)
  lab=c(rep("1water",length(t)),rep("2air",length(ta)))
  boxplot(c(t,ta)~lab,names=c("",""),ylab=expression(paste("Trend (",degree,"C per decade)")))
  #points(1:2, c(mean(t),mean(ta)), col = "red",pch=16)
  mtext(text=c("Water temperature\ntrends", "Air temperature\ntrends"),
        side = 1, line = 1.7, at = c(1,2),cex=1)

  lab=c(rep("1discharge",length(t)),rep("2precipitation",length(ta)))
  boxplot(c(q_norm,p_norm)~lab,names=c("",""),ylab="Trend (% per decade)")
  mtext(text=c("Discharge\ntrends", "Precipitation\ntrends"),
        side = 1, line = 1.7, at = c(1,2),cex=1)

  ### BOXPLOTS ###

  boxplots<-function(variable,legend,ylimt,ylimq)
  {

    data=data.frame(t,variable)
    if(legend=="elevation"){
      data[,2]<-factor(data[,2], levels=c("< 700m", "< 1200m", "< 1800m",">= 1800m"))
      boxplot(data[,1] ~ data[,2],las=2,xlab="",ylab="",
              names=c("< 700", "< 1200", "< 1800",">= 1800"),ylim=ylimt)
      mtext(side = 1, text = expression(paste("Mean Elevation (m)")), line = 5, cex=1)
    }
    else if(legend=="area"){
      data[,2]<-factor(data[,2], levels=c("< 100 km2", "< 1000 km2",
                                          "< 10'000 km2",">= 10'000 km2"))
      boxplot(data[,1] ~ data[,2],las=2,xlab="",ylab="",
              names=c("< 100", "< 1000", "<10'000",">= 10'000"),ylim=ylimt)
      mtext(side = 1, text = expression(paste("Area (km"^2,")")), line = 6 ,cex=1)
    }
    else if(legend=="regime"){
      boxplot(data[,1] ~ data[,2],las=2,
              names=c("DLA", "ALP", "SPJ","HYP"),xlab="",ylab="",ylim=ylimt)
      mtext(side = 1, text = expression(paste("Regime")), line = 4.5, cex=1)
    }
    else if(legend=="glaciers"){
      boxplot(data[,1] ~ data[,2],las=2,xlab="",ylab="",
              names=c("< 0.1", "< 5", "< 10",">= 10"),ylim=ylimt)
      mtext(side = 1, text = expression(paste("Glacier coverage (%)")), line = 4.5, cex=1)
    }
    mtext(side = 2, text = expression(paste("Temperature trend (",degree,"C per decade)")), line = 2.8)

    if(legend=="regime"){
      means <- aggregate(data[,1] ,
                         by = list(data[,2]),
                         FUN = mean,  na.rm=TRUE)

      points(1:4, means$x, col = "red",pch=16)
    }

    for(i in 1:length(table(data[,2])))
    {
      text(i,  ylimt[1]+0.01,table(data[which(!is.na(t)),2])[[i]],cex=1.1)
    }

    data=data.frame(q_norm,variable)

    if(legend=="elevation"){
      data[,2]<-factor(data[,2], levels=c("< 700m", "< 1200m", "< 1800m",">= 1800m"))
      boxplot(data[,1] ~ data[,2],las=2,xlab="",ylab="",
              names=c("< 700", "< 1200", "< 1800",">= 1800"),ylim=ylimq)
      mtext(side = 1, text = expression(paste("Mean Elevation (m)")), line = 5, cex=1)
    }
    else if(legend=="area"){
      data[,2]<-factor(data[,2], levels=c("< 100 km2", "< 1000 km2",
                                          "< 10'000 km2",">= 10'000 km2"))
      boxplot(data[,1] ~ data[,2],las=2,xlab="",ylab="",
              names=c("< 100", "< 1000", "<10'000",">= 10'000"),ylim=ylimq)
      mtext(side = 1, text = expression(paste("Area (km"^2,")")), line = 6 ,cex=1)
    }
    else if(legend=="regime"){
      boxplot(data[,1] ~ data[,2],las=2,
              names=c("DLA", "ALP", "SPJ","HYP"),xlab="",ylab="",ylim=ylimq)
      mtext(side = 1, text = expression(paste("Regime")), line = 4.5, cex=1)
    }
    else if(legend=="glaciers"){
      boxplot(data[,1] ~ data[,2],las=2,xlab="",ylab="",
              names=c("< 0.1", "< 5", "< 10",">= 10"),ylim=ylimq)
      mtext(side = 1, text = expression(paste("Glacier coverage (%)")), line = 4.5, cex=1)
    }
    mtext(side = 2, text = "Discharge trend (% per decade)", line = 3)

    for(i in 1:length(table(data[,2])))
    {
      text(i, ylimq[1]+0.2,table(data[which(!is.na(q_norm)),2])[[i]],cex=1.1)
    }
  }

  boxplots_meteo<-function(variable,legend, ylimt, ylimq)
  {

    data=data.frame(ta,variable)
    if(legend=="elevation"){
      data[,2]<-factor(data[,2], levels=c("< 700m", "< 1200m", "< 1800m",">= 1800m"))
      boxplot(data[,1] ~ data[,2],las=2,xlab="",ylab="",
              names=c("< 700", "< 1200", "< 1800",">= 1800"), ylim=ylimt)
      mtext(side = 1, text = expression(paste("Mean Elevation (m)")), line = 5, cex=1)
    }
    else if(legend=="area"){
      data[,2]<-factor(data[,2], levels=c("< 100 km2", "< 1000 km2",
                                          "< 10'000 km2",">= 10'000 km2"))
      boxplot(data[,1] ~ data[,2],las=2,xlab="",ylab="",
              names=c("< 100", "< 1000", "<10'000",">= 10'000"), ylim=ylimt)
      mtext(side = 1, text = expression(paste("Area (km"^2,")")), line = 6 ,cex=1)
    }
    else if(legend=="regime"){
      boxplot(data[,1] ~ data[,2],las=2,
              names=c("DLA", "ALP", "SPJ","HYP"),xlab="",ylab="", ylim=ylimt)
      mtext(side = 1, text = expression(paste("Regime")), line = 4.5, cex=1)
    }
    else if(legend=="glaciers"){
      boxplot(data[,1] ~ data[,2],las=2,xlab="",ylab="",
              names=c("< 0.1", "< 5", "< 10",">= 10"), ylim=ylimt)
      mtext(side = 1, text = expression(paste("Glacier coverage (%)")), line = 4.5, cex=1)
    }
    mtext(side = 2, text = expression(paste("Air temperature trend (",degree,"C per decade)")), line = 2.8)

    for(i in 1:length(table(data[,2])))
    {
      text(i,  ylimt[1]+0.01,table(data[which(!is.na(t)),2])[[i]],cex=1.1)
    }

    data=data.frame(p_norm,variable)

    if(legend=="elevation"){
      data[,2]<-factor(data[,2], levels=c("< 700m", "< 1200m", "< 1800m",">= 1800m"))
      boxplot(data[,1] ~ data[,2],las=2,xlab="",ylab="",
              names=c("< 700", "< 1200", "< 1800",">= 1800"), ylim=ylimq)
      mtext(side = 1, text = expression(paste("Mean Elevation (m)")), line = 5, cex=1)
    }
    else if(legend=="area"){
      data[,2]<-factor(data[,2], levels=c("< 100 km2", "< 1000 km2",
                                          "< 10'000 km2",">= 10'000 km2"))
      boxplot(data[,1] ~ data[,2],las=2,xlab="",ylab="",
              names=c("< 100", "< 1000", "<10'000",">= 10'000"), ylim=ylimq)
      mtext(side = 1, text = expression(paste("Area (km"^2,")")), line = 6 ,cex=1)
    }
    else if(legend=="regime"){
      boxplot(data[,1] ~ data[,2],las=2,
              names=c("DLA", "ALP", "SPJ","HYP"),xlab="",ylab="", ylim=ylimq)
      mtext(side = 1, text = expression(paste("Regime")), line = 4.5, cex=1)
    }
    else if(legend=="glaciers"){
      boxplot(data[,1] ~ data[,2],las=2,xlab="",ylab="",
              names=c("< 0.1", "< 5", "< 10",">= 10"), ylim=ylimq)
      mtext(side = 1, text = expression(paste("Glacier coverage (%)")), line = 4.5, cex=1)
    }
    mtext(side = 2, text = "Precipitation trend (% per decade)", line = 3)

    for(i in 1:length(table(data[,2])))
    {
      text(i, ylimq[1]+0.2,table(data[which(!is.na(q_norm)),2])[[i]],cex=1.1)
    }
  }

  par(mfrow=c(2,4))
  par(mar=c(7.5,5,1,1),mgp=c(2,1,0),oma=c(0,0,0,0),cex.lab=1.2,cex.axis=1.2,
      cex.main=1.2)
  ylimt=c(0,0.9)
  ylimq=c(-25,10)

  boxplots(regimes2,"regime", ylimt, ylimq)
  boxplots(get_area_labels(area),"area", ylimt, ylimq)
  boxplots(get_elevation_labels(elevation),"elevation", ylimt, ylimq)
  boxplots(get_glacier_labels(glacier),"glaciers", ylimt, ylimq)


  boxplots_meteo(regimes2,"regime", ylimt, ylimq)
  boxplots_meteo(get_area_labels(area),"area", ylimt, ylimq)
  boxplots_meteo(get_elevation_labels(elevation),"elevation", ylimt, ylimq)
  boxplots_meteo(get_glacier_labels(glacier),"glaciers", ylimt, ylimq)



  t_hp=t[which(regimes2=="Strong influence of Hydropeaking")]
  t_al=t[which(regimes2=="After lakes")]
  t_pj=t[which(regimes2=="Regime from Plateau and Jura")]
  t_ar=t[which(regimes2=="Alpine regime")]

  print(wilcox.test(t_ar, t_al))
  print(wilcox.test(t_pj, t_al))
  print(wilcox.test(t_hp, t_al))

  print(wilcox.test(t_hp, t_ar))
  print(wilcox.test(t_pj, t_ar))
  print(wilcox.test(t_hp, t_pj))

  q_hp=q_norm[which(regimes2=="Strong influence of Hydropeaking")]
  q_al=q_norm[which(regimes2=="After lakes")]
  q_pj=q_norm[which(regimes2=="Regime from Plateau and Jura")]
  q_ar=q_norm[which(regimes2=="Alpine regime")]

  print(wilcox.test(q_hp, q_al))
  print(wilcox.test(q_pj, q_al))
  print(wilcox.test(q_ar, q_al))
  print(wilcox.test(q_hp, q_ar))
  print(wilcox.test(q_pj, q_ar))
  print(wilcox.test(q_hp, q_pj))

  par(mfrow=c(1,1))
  # empty plot to keep the same pagination in the PDF than odler versions
  plot.new()
  plot.new()

  par(mfrow=c(1,2))
  par(mar=c(18,4,2,1),mgp=c(2.5,0.6,0),oma=c(0,0,0,0),cex.lab=1.3,cex.axis=1.2)
  data=data.frame(as.numeric(elevation),regimes2)
  boxplot(data[,1] ~ data[,2],las=2,main=paste("Elevation per regimes"),
          ylab="Elevation (m)",names=c("AFL", "ALP", "SPJ","HP"))
  for(i in 1:length(table(data[,2])))
  {
    text(i,2500,table(data[which(!is.na(data[,1])),2])[[i]])
  }

  data=data.frame(suppressWarnings(as.numeric(glacier)),regimes2)
  boxplot(data[,1] ~ data[,2],las=2,main=paste("Glaciers coverage per regimes"),
          names=c("AFL", "ALP", "SPJ","HP"),ylab="Glacier coverage (%)")
  for(i in 1:length(table(data[,2])))
  {
    text(i,25,table(data[which(!is.na(data[,1])),2])[[i]])
  }

  par(mfrow=c(1,2))
  par(mar=c(18,4,2,1),mgp=c(2.5,0.6,0),oma=c(0,0,0,0),cex.lab=1.3,cex.axis=1.2)
  data=data.frame((as.numeric(area)),regimes2)
  boxplot(data[,1] ~ data[,2],las=2,main=paste("Area per regimes"),log="y",
          names=c("DLA", "ALP", "SPJ","HYP"),ylab=expression(paste("Area (km"^"2",")")),xlab="Regime")
  for(i in 1:length(table(data[,2])))
  {
    text(i,13,table(data[which(!is.na(data[,1])),2])[[i]])
  }

  sel=which(regimes2=="Regime from Plateau and Jura")
  plot((as.numeric(area))[sel],t[sel],main="Water temperature trend per area, SPJ only",
       xlab=expression(paste("Area (km"^"2",")")),log="x",
       ylab=expression(paste("Water temperature trend (",degree,"C per decade)")))

  par(mfrow=c(2,2))
  par(mar=c(3,4,2,1),mgp=c(2.5,0.6,0),oma=c(0,0,0,0),cex.lab=1.3,cex.axis=1.2)
  plot(elevation,area,main="Area for mean elevation",ylab="Log area (km2)",
       xlab="Mean elevation (m)",log="y")
  suppressWarnings(plot(elevation,glacier,main="Glacier percents per elevation",
                        xlab="Mean elevation (m)",ylab="Glacier (%)"))
  suppressWarnings(plot(area,glacier,main="Glacier percent per area",
                        xlab="Log area (km2)",ylab="Glacier (%)"))


  ####
  par(mfrow=c(2,2))
  par(mar=c(3,3,2,1),mgp=c(2.5,0.6,0),oma=c(0,0,0,0),cex.lab=1.3,cex.axis=1.2)
  plot(elevation,t,main="T trend per mean elevation",xlab="Mean elevation (m)")
  plot(elevation,q_norm,main="norm Q trend per mean elevation",
       xlab="Mean elevation (m)")

  plot((as.numeric(area)),t,main="T trend per log area",xlab="log Area (km2)",log="x")
  plot((as.numeric(area)),q_norm,main="norm Q trend per log area",
       xlab="log Area (km2)",log="x")

  suppressWarnings(plot(glacier,t,main="T trend per glacier %",xlab="Glacier (%)"))
  suppressWarnings(plot(glacier,q_norm,main="norm Q trend per glacier %",
                        xlab="Glacier (%)"))


  ### Q AND T RELATION ###

  par(mfrow=c(2,2))
  par(mar=c(2.5,3,2,1),mgp=c(1.7,0.5,0),oma=c(0,0,0,0),cex.lab=1.3,cex.axis=1.2)


  lab=c(rep("0",length(t)),rep("1",length(winter_t)),rep("2",length(spring_t)),
        rep("3",length(summer_t)),rep("4",length(fall_t)))
  boxplot(  c(t,winter_t, spring_t,summer_t,fall_t)~lab,names=c("","","","",""),
            ylab=expression(paste("Water temperature trend (",degree,"C per decade)")),
            ylim=c(-0.7,1.1),main="Water temperature",xlab="")
  points(1:5, c(mean(t,na.rm=TRUE),mean(winter_t,na.rm=TRUE),mean(spring_t,na.rm=TRUE),
                mean(summer_t,na.rm=TRUE),mean(fall_t,na.rm=TRUE)), col = "red",pch=16)
  mtext(text=c("Annual", "Winter","Spring", "Summer","Fall"),
        side = 1, line = 0.8, at = c(1:5),cex=0.8)
  text(c(1:5),-0.5,paste("mean:",round(c(mean(t,na.rm=TRUE),mean(winter_t,na.rm=TRUE),
                                         mean(spring_t,na.rm=TRUE),
                                         mean(summer_t,na.rm=TRUE),
                                         mean(fall_t,na.rm=TRUE)),digits=2)))
  abline(v=1.5,lty=3)


  lab=c(rep("0",length(ta)),rep("1",length(winter_ta)),rep("2",length(spring_ta)),
        rep("3",length(summer_ta)),rep("4",length(fall_ta)))
  boxplot(  c(t,winter_ta, spring_ta,summer_ta,fall_ta)~lab,names=c("","","","",""),
            ylab=expression(paste("Air temperature trend (",degree,"C per decade)")),
            ylim=c(-0.7,1.1),main="Air temperature",xlab="")
  points(1:5, c(mean(ta,na.rm=TRUE),mean(winter_ta,na.rm=TRUE),mean(spring_ta,na.rm=TRUE),
                mean(summer_ta,na.rm=TRUE),mean(fall_ta,na.rm=TRUE)), col = "red",pch=16)
  mtext(text=c("Annual", "Winter","Spring", "Summer","Fall"),
        side = 1, line = 0.8, at = c(1:5),cex=0.8)
  text(c(1:5),-0.5,paste("mean:",round(c(mean(ta,na.rm=TRUE),
                                        mean(winter_ta,na.rm=TRUE),
                                        mean(spring_ta,na.rm=TRUE),
                                        mean(summer_ta,na.rm=TRUE),
                                        mean(fall_ta,na.rm=TRUE)),digits=2)))
  abline(v=1.5,lty=3)


  lab=c(rep("0",length(q_norm)),rep("1",length(winter_q_norm)),
       rep("2",length(spring_q_norm)),
       rep("3",length(summer_q_norm)),rep("4",length(fall_q_norm)))
  boxplot(  c(q_norm,winter_q_norm, spring_q_norm,summer_q_norm,fall_q_norm)~lab,
           names=c("","","","",""),
           ylab=expression(paste("Discharge trend (% per decade)")),ylim=c(-60,25),
           main="Discharge",xlab="")
  points(1:5, c(mean(q_norm,na.rm=TRUE),mean(winter_q_norm,na.rm=TRUE),
               mean(spring_q_norm,na.rm=TRUE),
               mean(summer_q_norm,na.rm=TRUE),mean(fall_q_norm,na.rm=TRUE)),
        col = "red",pch=16)
  mtext(text=c("Annual", "Winter","Spring", "Summer","Fall"),
       side = 1, line = 0.8, at = c(1:5),cex=0.8)
  text(c(1:5),-50,paste("mean:",round(c(mean(q_norm,na.rm=TRUE),
                                       mean(winter_q_norm,na.rm=TRUE),
                                       mean(spring_q_norm,na.rm=TRUE),
                                       mean(summer_q_norm,na.rm=TRUE),
                                       mean(fall_q_norm,na.rm=TRUE)),digits=2)))
  abline(v=1.5,lty=3)


  lab=c(rep("0",length(p_norm)),rep("1",length(winter_p_norm)),
       rep("2",length(spring_p_norm)),
       rep("3",length(summer_p_norm)),rep("4",length(fall_p_norm)))
  boxplot(  c(q_norm,winter_p_norm, spring_p_norm,summer_p_norm,fall_p_norm)~lab,
           names=c("","","","",""),
           ylab=expression(paste("Precipitation trend (% per decade)")),
           ylim=c(-60,25),main="Precipitation",xlab="")
  points(1:5, c(mean(p_norm,na.rm=TRUE),mean(winter_p_norm,na.rm=TRUE),
               mean(spring_p_norm,na.rm=TRUE),mean(summer_p_norm,na.rm=TRUE),
               mean(fall_p_norm,na.rm=TRUE)), col = "red",pch=16)
  mtext(text=c("Annual", "Winter","Spring", "Summer","Fall"),
       side = 1, line = 0.8, at = c(1:5),cex=0.8)
  text(c(1:5),-50,paste("mean:",round(c(mean(p_norm,na.rm=TRUE),
                                       mean(winter_p_norm,na.rm=TRUE),
                                       mean(spring_p_norm,na.rm=TRUE),
                                       mean(summer_p_norm,na.rm=TRUE),
                                       mean(fall_p_norm,na.rm=TRUE)),digits=2)))
  abline(v=1.5,lty=3)



  par(mfrow=c(3,2))
  par(mar=c(2.5,3,2,1),mgp=c(1.7,0.5,0),oma=c(0,0,0,0))

  r2=which(regimes2=="Alpine regime")
  pc=16
  co=2:6


  plot(rep(1,length(r2)),t[r2],xlim=c(0.4,5.6),ylim=c(-0.7,1.1),pch=pc,col=co,xaxt="n",
      xlab="",ylab=expression(paste("Water temperature trend (",degree,"C per decade)")))
  axis(1, at=c(1:5), labels=c("Annual", "Winter","Spring", "Summer","Fall"))
  points(rep(2,length(r2)),winter_t[r2],pch=pc,col=co)
  points(rep(3,length(r2)),spring_t[r2],pch=pc,col=co)
  points(rep(4,length(r2)),summer_t[r2],pch=pc,col=co)
  points(rep(5,length(r2)),fall_t[r2],pch=pc,col=co)
  grid()
  points(1:5, c(mean(t,na.rm=TRUE),mean(winter_t,na.rm=TRUE),mean(spring_t,na.rm=TRUE),
                mean(summer_t,na.rm=TRUE),mean(fall_t,na.rm=TRUE)),
        col = "black",pch=15,cex=1)

  plot(rep(1,length(r2)),ta[r2],xlim=c(0.4,5.6),ylim=c(-0.7,1.1),pch=pc,col=co,xaxt="n",
      xlab="",ylab=expression(paste("Air temperature trend (",degree,"C per decade)")))
  axis(1, at=c(1:5), labels=c("Annual", "Winter","Spring", "Summer","Fall"))
  points(rep(2,length(r2)),winter_ta[r2],pch=pc,col=co)
  points(rep(3,length(r2)),spring_ta[r2],pch=pc,col=co)
  points(rep(4,length(r2)),summer_ta[r2],pch=pc,col=co)
  points(rep(5,length(r2)),fall_ta[r2],pch=pc,col=co)
  points(1:5, c(mean(ta,na.rm=TRUE),mean(winter_ta,na.rm=TRUE),mean(spring_ta,na.rm=TRUE),
                mean(summer_ta,na.rm=TRUE),mean(fall_ta,na.rm=TRUE)),
        col = "black",pch=15,cex=1)
  grid()

  plot(rep(1,length(r2)),q_norm[r2],xlim=c(0.4,5.6),ylim=c(-60,25),pch=pc,col=co,xaxt="n",
      xlab="",ylab=expression(paste("Air temperature trend (",degree,"C per decade)")))
  axis(1, at=c(1:5), labels=c("Annual", "Winter","Spring", "Summer","Fall"))
  points(rep(2,length(r2)),winter_q_norm[r2],pch=pc,col=co)
  points(rep(3,length(r2)),spring_q_norm[r2],pch=pc,col=co)
  points(rep(4,length(r2)),summer_q_norm[r2],pch=pc,col=co)
  points(rep(5,length(r2)),fall_q_norm[r2],pch=pc,col=co)
  points(1:5, c(mean(q_norm,na.rm=TRUE),mean(winter_q_norm,na.rm=TRUE),mean(spring_q_norm,na.rm=TRUE),
               mean(summer_q_norm,na.rm=TRUE),mean(fall_q_norm,na.rm=TRUE)), col = "black",pch=15,cex=1)
  grid()

  plot(rep(1,length(r2)),p_norm[r2],xlim=c(0.4,5.6),ylim=c(-60,25),pch=pc,col=co,xaxt="n",
      xlab="",ylab=expression(paste("Water temperature trend (",degree,"C per decade)")))
  axis(1, at=c(1:5), labels=c("Annual", "Winter","Spring", "Summer","Fall"))
  points(rep(2,length(r2)),winter_p_norm[r2],pch=pc,col=co)
  points(rep(3,length(r2)),spring_p_norm[r2],pch=pc,col=co)
  points(rep(4,length(r2)),summer_p_norm[r2],pch=pc,col=co)
  points(rep(5,length(r2)),fall_p_norm[r2],pch=pc,col=co)
  points(1:5, c(mean(p_norm,na.rm=TRUE),mean(winter_p_norm,na.rm=TRUE),mean(spring_p_norm,na.rm=TRUE),
               mean(summer_p_norm,na.rm=TRUE),mean(fall_p_norm,na.rm=TRUE)), col = "black",pch=15,cex=1)
  grid()

  extremes=c()
  seasons=c("DJF","MAM","JJA","SON")
  regimes=c()
  for (river_station in names(rivers_data))
  {
   if(rivers_data[[river_station]]$header$regime2=="Alpine regime"){

     T=list()
     Q=list()
     TA=list()
     P=list()
     for(s in seasons)
     {
       T[[s]]=rivers_data[[river_station]][[s]]$T$lm$`1999-2018`$values
       Q[[s]]=rivers_data[[river_station]][[s]]$Q$lm$`1999-2018`$values
       TA[[s]]=rivers_data[[river_station]]$meteo[[1]][[s]]$TA$lm$`1999-2018`$values
       P[[s]]=rivers_data[[river_station]]$meteo[[1]][[s]]$P$lm$`1999-2018`$values
       while(length(T[[s]])<length(TA[[s]]))
       {
         T[[s]]=c(T[[s]],NaN)
       }
       while(length(Q[[s]])<length(P[[s]]))
       {
         Q[[s]]=c(Q[[s]],NaN)
       }
     }
     extremes[[river_station]]=list("T"=T,"Q"=Q,"TA"=TA,"P"=P)
   }
  }

  years=c(1999:2018)
  Y_all=c()
  T_all=list()
  Q_all=list()
  TA_all=list()
  P_all=list()
  R_all=c()
  seasons=c("DJF","MAM","JJA","SON")
  i=0
  for (dat in extremes){

   for(y in (1:length(years)))
   {
     for(s in seasons)
     {
       T_all[[s]]=c(T_all[[s]],dat$T[[s]][y]-mean(dat$T[[s]],na.rm=TRUE))
       Q_all[[s]]=c(Q_all[[s]],(dat$Q[[s]][y]-mean(dat$Q[[s]],na.rm=TRUE))/
                      mean(dat$Q[[s]],na.rm=TRUE)*100)
       TA_all[[s]]=c(TA_all[[s]],dat$TA[[s]][y]-mean(dat$TA[[s]],na.rm=TRUE))
       P_all[[s]]=c(P_all[[s]],(dat$P[[s]][y]-mean(dat$P[[s]],na.rm=TRUE))/
                      mean(dat$P[[s]],na.rm=TRUE)*100)
     }
     Y_all=c(Y_all,years[y])
     R_all=c(R_all,regimes[i])
   }
   i=i+1
  }


  plot( Y_all, T_all$JJA, ylim=c(-4,5),
       ylab=expression(paste("Summer water temperature anomaly (",degree,"C)")))
  abline(h=c(-3:4),col="lightgrey",lty=3)
  abline(v=c(2,7,12,17),col="lightgrey",lty=3)
  abline(h=0)

  plot(Y_all,TA_all$JJA, ylim=c(-4,5),
      ylab=expression(paste("Summer air temperature anomaly (",degree,"C)")))
  abline(h=c(-3:4),col="lightgrey",lty=3)
  abline(v=c(2,7,12,17),col="lightgrey",lty=3)
  abline(h=0)

  plot(Y_all,Q_all$JJA,ylim=c(-100,270),ylab="Summer discharge anomaly (%)")
  abline(v=c(2,7,12,17),col="lightgrey",lty=3)
  abline(h=c(-100,-50,0,50,100,150,200,250),col="lightgrey",lty=3)
  abline(h=0)
  i=1; j=1; leg=c()

  plot(Y_all, P_all$JJA,ylim=c(-100,270),ylab="Summer precipitaion anomaly (%)")
  abline(v=c(2,7,12,17),col="lightgrey",lty=3)
  abline(h=c(-100,-50,0,50,100,150,200,250),col="lightgrey",lty=3)
  abline(h=0)


  lab=c(rep("0",length(t)),rep("1",length(winter_t)),rep("2",length(spring_t)),
       rep("3",length(summer_t)),rep("4",length(fall_t)))
  boxplot(  c(t,winter_t, spring_t,summer_t,fall_t)~lab,names=c("","","","",""),
           ylab=expression(paste("Water temperature trend (",degree,"C per decade)")),
           ylim=c(-0.7,1.1),main="Water temperature")

  points(1:5, c(mean(t),mean(winter_t),mean(spring_t),mean(summer_t),mean(fall_t)),
        col = "red",pch=16)
  mtext(text=c("Annual", "Winter","Spring", "Summer","Fall"),
       side = 1, line = 0.8, at = c(1:5),cex=0.8)
  grid()
  text(c(1:5),-0.5,paste("mean:",round(c(mean(t),mean(winter_t),mean(spring_t),
                                        mean(summer_t),mean(fall_t)),digits=2)))


  lab=c(rep("0",length(ta)),rep("1",length(winter_ta)),rep("2",length(spring_ta)),
       rep("3",length(summer_ta)),rep("4",length(fall_ta)))
  boxplot(  c(t,winter_ta, spring_ta,summer_ta,fall_ta)~lab,names=c("","","","",""),
           ylab=expression(paste("Air temperature trend (",degree,"C per decade)")),
           ylim=c(-0.7,1.1),main="Air temperature")
  points(1:5, c(mean(ta),mean(winter_ta),mean(spring_ta),mean(summer_ta),mean(fall_ta)),
        col = "red",pch=16)
  mtext(text=c("Annual", "Winter","Spring", "Summer","Fall"),
       side = 1, line = 0.8, at = c(1:5),cex=0.8)
  grid()
  text(c(1:5),-0.5,paste("mean:",round(c(mean(ta),mean(winter_ta),
                                        mean(spring_ta),mean(summer_ta),
                                        mean(fall_ta)),digits=2)))


  lab=c(rep("0",length(q_norm)),rep("1",length(winter_q_norm)),
       rep("2",length(spring_q_norm)),
       rep("3",length(summer_q_norm)),rep("4",length(fall_q_norm)))
  boxplot(  c(q_norm,winter_q_norm, spring_q_norm,summer_q_norm,fall_q_norm)~lab,
           names=c("","","","",""),
           ylab=expression(paste("Discharge trend (% per decade)"))
           ,ylim=c(-60,25),main="Discharge")
  points(1:5, c(mean(q_norm),mean(winter_q_norm),mean(spring_q_norm),
               mean(summer_q_norm),mean(fall_q_norm)), col = "red",pch=16)
  mtext(text=c("Annual", "Winter","Spring", "Summer","Fall"),
       side = 1, line = 0.8, at = c(1:5),cex=0.8)
  grid()
  text(c(1:5),-50,paste("mean:",round(c(mean(q_norm),mean(winter_q_norm),
                                       mean(spring_q_norm),mean(summer_q_norm),
                                       mean(fall_q_norm)),digits=2)))

  lab=c(rep("0",length(p_norm)),rep("1",length(winter_p_norm)),rep("2",length(spring_p_norm)),
       rep("3",length(summer_p_norm)),rep("4",length(fall_p_norm)))
  boxplot(  c(q_norm,winter_p_norm, spring_p_norm,summer_p_norm,fall_p_norm)~lab,
           names=c("","","","",""),
           ylab=expression(paste("Precipitation trend (% per decade)")),
           ylim=c(-60,25),main="Precipitation")
  points(1:5, c(mean(p_norm),mean(winter_p_norm),mean(spring_p_norm),
               mean(summer_p_norm),mean(fall_p_norm)), col = "red",pch=16)
  mtext(text=c("Annual", "Winter","Spring", "Summer","Fall"),
       side = 1, line = 0.8, at = c(1:5),cex=0.8)
  grid()
  text(c(1:5),-50,paste("mean:",round(c(mean(p_norm),mean(winter_p_norm),
                                       mean(spring_p_norm),mean(summer_p_norm),
                                       mean(fall_p_norm)),digits=2)))

  par(mfrow=c(2,5))
  hist(t,xlim=c(-1,1.5),breaks=seq(-1,1.5,by=0.1))
  abline(v=mean(t,na.rm=TRUE),col=2)
  text(x=-1,y=3,labels=paste("mean:",round(mean(t,na.rm=TRUE),3)),pos=4)
  hist(summer_t,xlim=c(-1,2),breaks=seq(-1,2,by=0.1))
  abline(v=mean(summer_t,na.rm=TRUE),col=2)
  text(x=-1,y=3,labels=paste("mean:",round(mean(summer_t,na.rm=TRUE),3)),pos=4)
  hist(winter_t,xlim=c(-1,1.5),breaks=seq(-1,1.5,by=0.1))
  abline(v=mean(winter_t,na.rm=TRUE),col=2)
  text(x=-1,y=3,labels=paste("mean:",round(mean(winter_t,na.rm=TRUE),3)),pos=4)
  hist(spring_t,xlim=c(-1,1.5),breaks=seq(-1,1.5,by=0.1))
  abline(v=mean(spring_t,na.rm=TRUE),col=2)
  text(x=-1,y=3,labels=paste("mean:",round(mean(spring_t,na.rm=TRUE),3)),pos=4)
  hist(fall_t,xlim=c(-1,1.5),breaks=seq(-1,1.5,by=0.1))
  abline(v=mean(fall_t,na.rm=TRUE),col=2)
  text(x=-1,y=3,labels=paste("mean:",round(mean(fall_t,na.rm=TRUE),3)),pos=4)

  hist(q_norm,xlim=c(-60,20),breaks=seq(-60,20,by=5))
  abline(v=mean(q_norm,na.rm=TRUE),col=2)
  text(x=-60,y=3,labels=paste("mean:",round(mean(q_norm,na.rm=TRUE),3)),pos=4)
  hist(summer_q_norm,xlim=c(-60,20),breaks=seq(-60,20,by=5))
  abline(v=mean(summer_q_norm,na.rm=TRUE),col=2)
  text(x=-60,y=3,labels=paste("mean:",round(mean(summer_q_norm,na.rm=TRUE),3)),pos=4)
  hist(winter_q_norm,xlim=c(-60,20),breaks=seq(-60,20,by=5))
  abline(v=mean(winter_q_norm,na.rm=TRUE),col=2)
  text(x=-60,y=3,labels=paste("mean:",round(mean(winter_q_norm,na.rm=TRUE),3)),pos=4)
  hist(spring_q_norm,xlim=c(-60,20),breaks=seq(-60,20,by=5))
  abline(v=mean(spring_q_norm,na.rm=TRUE),col=2)
  text(x=-60,y=3,labels=paste("mean:",round(mean(spring_q_norm,na.rm=TRUE),3)),pos=4)
  hist(fall_q_norm,xlim=c(-60,20),breaks=seq(-60,20,by=5))
  abline(v=mean(fall_q_norm,na.rm=TRUE),col=2)
  text(x=-60,y=3,labels=paste("mean:",round(mean(fall_q_norm,na.rm=TRUE),3)),pos=4)

  par(mfrow=c(2,2))
  par(mar=c(3,3,2,1),mgp=c(1.7,0.5,0),oma=c(0,0,2,0))
  plot(q_norm,t,main="Area",xlab="norm Q trend",ylab="T trend",
       col=get_area_colors(area))
  legend("bottom",legend=c("<100km2","100-1000km^2","1000-10000km^2",">10000km^2"),
         col=c(1,2,3,4),pch=16,bty='n',ncol=2)
  plot(q_norm,t,main="Glacier %",xlab="norm Q trend",ylab="T trend",
       col=get_glacier_colors(glacier))
  legend("bottom",legend=c("0%","0-5%","5-10%",">10%"),col=c(1,2,3,4),
         pch=16,bty='n',ncol=2)
  plot(q_norm,t,main="Elevation",xlab="norm Q trend",ylab="T trend",
       col=get_elevation_colors(elevation))
  legend("bottom",legend=c("<700m","700-1200m","1200-1800m",">1800m"),
         col=c(1,2,3,4),pch=16,bty='n',ncol=2)
  plot(q_norm,t,main="Regime",ylab="T trend",col=get_regimes2_colors(regimes2))
  legend("bottom",legend=c("None","Hydropeaking","After lakes","Plateau/Jura","Alpine"),
         col=c(1,2,3,4,5),pch=16,bty='n',ncol=4)
  mtext(side=3, outer=TRUE,"Annual T trend as function of annual Q trend",font=2)
  print_correlation(q_norm,t)

  plot(summer_q_norm,summer_t,main="Area",xlab="summer q norm trend",
       ylab="summer T trend",col=get_area_colors(area))
  legend("bottom",legend=c("<100km2","100-1000km^2","1000-10000km^2",">10000km^2"),
         col=c(1,2,3,4),pch=16,bty='n',ncol=2)
  plot(summer_q_norm,summer_t,main="Glacier %",xlab="summer q norm trend",
       ylab="summer T trend",col=get_glacier_colors(glacier))
  legend("bottom",legend=c("0%","0-5%","5-10%",">10%"),col=c(1,2,3,4),
         pch=16,bty='n',ncol=2)
  plot(summer_q_norm,summer_t,main="Elevation",xlab="summer q norm trend",
       ylab="summer T trend",col=get_elevation_colors(elevation))
  legend("bottom",legend=c("<700m","700-1200m","1200-1800m",">1800m"),
         col=c(1,2,3,4),pch=16,bty='n',ncol=2)
  plot(summer_q_norm,summer_t,main="Regime",xlab="summer q norm trend",
       ylab="summer T trend",
       col=get_regimes2_colors(regimes2))
  legend("bottom",legend=c("None","Hydropeaking","After lakes","Plateau/Jura","Alpine"),
         col=c(1,2,3,4,5),pch=16,bty='n',ncol=4)
  mtext(side=3, outer=TRUE,"Summer T trend as function of Summer Q trend",font=2)
  print_correlation(summer_q_norm,summer_t)

  plot(winter_q_norm,winter_t,main="Area",xlab="winter q norm trend"
       ,ylab="winter T trend", col=get_area_colors(area))
  legend("bottom",legend=c("<100km2","100-1000km^2","1000-10000km^2",">10000km^2"),
         col=c(1,2,3,4),pch=16,bty='n',ncol=2)
  plot(winter_q_norm,winter_t,main="Glacier %",xlab="winter q norm trend",
       ylab="winter T trend", col=get_glacier_colors(glacier))
  legend("bottom",legend=c("0%","0-5%","5-10%",">10%"),col=c(1,2,3,4),pch=16,bty='n',ncol=2)
  plot(winter_q_norm,winter_t,main="Elevation",xlab="winter q norm trend",ylab="winter T trend",
       col=get_elevation_colors(elevation))
  legend("bottom",legend=c("<700m","700-1200m","1200-1800m",">1800m"),
         col=c(1,2,3,4),pch=16,bty='n',ncol=2)
  plot(winter_q_norm,winter_t,main="Regime",xlab="winter q norm trend",
       ylab="winter T trend", col=get_regimes2_colors(regimes2))
  legend("bottom",legend=c("None","Hydropeaking","After lakes","Plateau/Jura","Alpine"),
         col=c(1,2,3,4,5),pch=16,bty='n',ncol=4)
  mtext(side=3, outer=TRUE,"Winter T trend as function of winter Q trend",font=2)
  print_correlation(winter_q_norm,winter_t)

  plot(spring_q_norm,spring_t,main="Area",xlab="spring q norm trend",ylab="spring T trend",
       col=get_area_colors(area))
  legend("bottom",legend=c("<100km2","100-1000km^2","1000-10000km^2",">10000km^2"),
         col=c(1,2,3,4),pch=16,bty='n',ncol=2)
  plot(spring_q_norm,spring_t,main="Glacier %",xlab="spring q norm trend",
       ylab="spring T trend",col=get_glacier_colors(glacier))
  legend("bottom",legend=c("0%","0-5%","5-10%",">10%"),col=c(1,2,3,4),pch=16,bty='n',ncol=2)
  plot(spring_q_norm,spring_t,main="Elevation",xlab="spring q norm trend",ylab="spring T trend",
       col=get_elevation_colors(elevation))
  legend("bottom",legend=c("<700m","700-1200m","1200-1800m",">1800m"),col=c(1,2,3,4),
         pch=16,bty='n',ncol=2)
  plot(spring_q_norm,spring_t,main="Regime",xlab="spring q norm trend",ylab="spring T trend",
       col=get_regimes2_colors(regimes2))
  legend("bottom",legend=c("None","Hydropeaking","After lakes","Plateau/Jura","Alpine"),
         col=c(1,2,3,4,5),pch=16,bty='n',ncol=4)
  mtext(side=3, outer=TRUE,"Spring T trend as function of spring Q trend",font=2)
  print_correlation(spring_q_norm,spring_t)

  plot(fall_q_norm,fall_t,main="Area",xlab="fall q norm trend",ylab="fall T trend",
       col=get_area_colors(area))
  legend("bottom",legend=c("<100km2","100-1000km^2","1000-10000km^2",">10000km^2"),
         col=c(1,2,3,4),pch=16,bty='n',ncol=2)
  plot(fall_q_norm,fall_t,main="Glacier %",xlab="fall q norm trend",ylab="fall T trend",
       col=get_glacier_colors(glacier))
  legend("bottom",legend=c("0%","0-5%","5-10%",">10%"),col=c(1,2,3,4),pch=16,bty='n',ncol=2)
  plot(fall_q_norm,fall_t,main="Elevation",xlab="fall q norm trend",ylab="fall T trend",
       col=get_elevation_colors(elevation))
  legend("bottom",legend=c("<700m","700-1200m","1200-1800m",">1800m"),col=c(1,2,3,4),
         pch=16,bty='n',ncol=2)
  plot(fall_q_norm,fall_t,main="Regime",xlab="fall q norm trend",ylab="fall T trend",
       col=get_regimes2_colors(regimes2))
  legend("bottom",legend=c("None","Hydropeaking","After lakes","Plateau/Jura","Alpine")
         ,col=c(1,2,3,4,5),pch=16,bty='n',ncol=4)
  mtext(side=3, outer=TRUE,"Fall T trend as function of fall Q trend",font=2)
  print_correlation(fall_q_norm,fall_t)

  ### WINTER-SUMMER T RELATION ###
  lims=c(min(winter_t,summer_t,na.rm=TRUE),max(winter_t,summer_t,na.rm=TRUE))
  plot(winter_t,summer_t,main="Area",xlab="winter T trend",ylab="summer T trend",
       ylim=lims,xlim=lims,col=get_area_colors(area))
  lines(c(-100,100),c(-100,100),lty=2)
  lines(c(-1,1),c(-2,2))
  legend("bottom",legend=c("<100km2","100-1000km^2","1000-10000km^2",">10000km^2"),
         col=c(1,2,3,4),pch=16,bty='n',ncol=2)
  plot(winter_t,summer_t,main="Glacier",xlab="winter T trend",ylab="summer T trend",
       ylim=lims,xlim=lims,col=get_glacier_colors(glacier))
  lines(c(-100,100),c(-100,100),lty=2)
  lines(c(-1,1),c(-2,2))
  legend("bottom",legend=c("0%","0-5%","5-10%",">10%"),col=c(1,2,3,4),pch=16,
         bty='n',ncol=2)
  plot(winter_t,summer_t,main="Elevation",xlab="winter T trend",ylab="summer T trend",
       ylim=lims,xlim=lims,col=get_elevation_colors(elevation))
  lines(c(-100,100),c(-100,100),lty=2)
  lines(c(-1,1),c(-2,2))
  legend("bottom",legend=c("<700m","700-1200m","1200-1800m",">1800m"),
         col=c(1,2,3,4),pch=16,bty='n',ncol=2)
  plot(winter_t,summer_t,main="Regime",xlab="winter T trend",ylab="summer T trend",
       ylim=lims,xlim=lims,col=get_regimes2_colors(regimes2))
  lines(c(-100,100),c(-100,100),lty=2)
  lines(c(-1,1),c(-2,2))
  legend("bottom",legend=c("None","Hydropeaking","After lakes","Plateau/Jura","Alpine"),
         col=c(1,2,3,4,5),pch=16,bty='n',ncol=4)
  mtext(side=3, outer=TRUE,"summer T trend as function of winter T trend",font=2)
  print_correlation(winter_t,summer_t)

  ### TA AND T RELATION ###
  par(mfrow=c(2,5))
  hist(t,xlim=c(-1,1.6),breaks=seq(-1,1.6,by=0.1))
  abline(v=mean(t,na.rm=TRUE),col=2)
  text(x=-1,y=3,labels=paste("mean:",round(mean(t,na.rm=TRUE),3)),pos=4)
  hist(summer_t,xlim=c(-1,1.6),breaks=seq(-1,1.6,by=0.1))
  abline(v=mean(summer_t,na.rm=TRUE),col=2)
  text(x=-1,y=3,labels=paste("mean:",round(mean(summer_t,na.rm=TRUE),3)),pos=4)
  hist(winter_t,xlim=c(-1,1.6),breaks=seq(-1,1.6,by=0.1))
  abline(v=mean(winter_t,na.rm=TRUE),col=2)
  text(x=-1,y=3,labels=paste("mean:",round(mean(winter_t,na.rm=TRUE),3)),pos=4)
  hist(spring_t,xlim=c(-1,1.6),breaks=seq(-1,1.6,by=0.1))
  abline(v=mean(spring_t,na.rm=TRUE),col=2)
  text(x=-1,y=3,labels=paste("mean:",round(mean(spring_t,na.rm=TRUE),3)),pos=4)
  hist(fall_t,xlim=c(-1,1.6),breaks=seq(-1,1.6,by=0.1))
  abline(v=mean(fall_t,na.rm=TRUE),col=2)
  text(x=-1,y=3,labels=paste("mean:",round(mean(fall_t,na.rm=TRUE),3)),pos=4)

  hist(ta,xlim=c(-1,1.6),breaks=seq(-1,1.6,by=0.1))
  abline(v=mean(ta,na.rm=TRUE),col=2)
  text(x=-1,y=3,labels=paste("mean:",round(mean(ta,na.rm=TRUE),3)),pos=4)
  hist(summer_ta,xlim=c(-1,1.6),breaks=seq(-1,1.6,by=0.1))
  abline(v=mean(summer_ta,na.rm=TRUE),col=2)
  text(x=-1,y=3,labels=paste("mean:",round(mean(summer_ta,na.rm=TRUE),3)),pos=4)
  hist(winter_ta,xlim=c(-1,1.6),breaks=seq(-1,1.6,by=0.1))
  abline(v=mean(winter_ta,na.rm=TRUE),col=2)
  text(x=-1,y=3,labels=paste("mean:",round(mean(winter_ta,na.rm=TRUE),3)),pos=4)
  hist(spring_ta,xlim=c(-1,1.6),breaks=seq(-1,1.6,by=0.1))
  abline(v=mean(spring_ta,na.rm=TRUE),col=2)
  text(x=-1,y=3,labels=paste("mean:",round(mean(spring_ta,na.rm=TRUE),3)),pos=4)
  hist(fall_ta,xlim=c(-1,1.6),breaks=seq(-1,1.6,by=0.1))
  abline(v=mean(fall_ta,na.rm=TRUE),col=2)
  text(x=-1,y=3,labels=paste("mean:",round(mean(fall_ta,na.rm=TRUE),3)),pos=4)
  mtext(side=3, outer=TRUE,"Distribution of trends for T and TA",font=2)

  par(mfrow=c(2,2))
  par(mar=c(4,4,2,1),mgp=c(2.5,1,0),oma=c(0,0,0,0))

  lims=c(min(ta,t,na.rm=TRUE),max(ta,t,na.rm=TRUE))
  plot(ta,t,main="Area",xlab=expression(paste("Air temperature trend (",degree,"C per decade)")),
       ylab=expression(paste("Water temperature trend (",degree,"C per decade)")),
       ylim=lims,xlim=lims,col=get_area_colors(area))
  lines(c(-100,100),c(-100,100),lty=2)
  legend("bottom",legend=c(expression(paste("<100 km"^2)),expression(paste("100-1000 km"^2)),
                           expression(paste("1000-10'000 km"^2)),expression(paste(">=10'000 km"^2))),
         col=c(1,2,3,4),pch=16,bty='n',ncol=2)
  plot(ta,t,main="Glacier",xlab=expression(paste("Air temperature trend (",degree,"C per decade)")),
       ylab=expression(paste("Water temperature trend (",degree,"C per decade)")),
       ylim=lims,xlim=lims,col=get_glacier_colors(glacier))
  lines(c(-100,100),c(-100,100),lty=2)
  legend("bottom",legend=c("0%","0-5%","5-10%",">=10%"),col=c(1,2,3,4),pch=16,
         bty='n',ncol=2)
  plot(ta,t,main="Elevation",xlab=expression(paste("Air temperature trend (",degree,"C per decade)")),
       ylab=expression(paste("Water temperature trend (",degree,"C per decade)")),
       ylim=lims,xlim=lims,col=get_elevation_colors(elevation))
  lines(c(-100,100),c(-100,100),lty=2)
  legend("bottom",legend=c("<700 m","700-1200 m","1200-1800 m",">=1800 m"),
         col=c(1,2,3,4),pch=16,bty='n',ncol=2)
  plot(ta,t,main="Regime",xlab=expression(paste("Air temperature trend (",degree,"C per decade)")),
       ylab=expression(paste("Water temperature trend (",degree,"C per decade)")),
       ylim=lims,xlim=lims,col=get_regimes2_colors(regimes2))
  lines(c(-100,100),c(-100,100),lty=2)
  legend("bottom",legend=c("HYP","DLA","SPJ","ALP"),
         col=c(2,3,4,5),pch=16,bty='n',ncol=4)
  #mtext(side=3, outer=TRUE,"Annual T trend as function of annual TA trend",font=2)
  #print_correlation(ta,t)





  lims=c(min(summer_ta,summer_t,na.rm=TRUE),max(summer_ta,summer_t,na.rm=TRUE))
  plot(summer_ta,summer_t,main="Area",xlab="summer TA trend",ylab="summer T trend",
       ylim=lims,xlim=lims,col=get_area_colors(area))
  lines(c(-100,100),c(-100,100),lty=2)
  legend("bottom",legend=c("<100km2","100-1000km^2","1000-10000km^2",">10000km^2"),
         col=c(1,2,3,4),pch=16,bty='n',ncol=2)
  plot(summer_ta,summer_t,main="Glacier",xlab="summer TA trend",ylab="summer T trend",
       ylim=lims,xlim=lims,col=get_glacier_colors(glacier))
  lines(c(-100,100),c(-100,100),lty=2)
  legend("bottom",legend=c("0%","0-5%","5-10%",">10%"),col=c(1,2,3,4),pch=16,
         bty='n',ncol=2)
  plot(summer_ta,summer_t,main="Elevation",xlab="summer TA trend",ylab="summer T trend",
       ylim=lims,xlim=lims,col=get_elevation_colors(elevation))
  lines(c(-100,100),c(-100,100),lty=2)
  legend("bottom",legend=c("<700m","700-1200m","1200-1800m",">1800m"),
         col=c(1,2,3,4),pch=16,bty='n',ncol=2)
  plot(summer_ta,summer_t,main="Regime",xlab="summer TA trend",ylab="summer T trend",
       ylim=lims,xlim=lims,col=get_regimes2_colors(regimes2))
  lines(c(-100,100),c(-100,100),lty=2)
  legend("bottom",legend=c("None","Hydropeaking","After lakes","Plateau/Jura","Alpine"),
         col=c(1,2,3,4,5),pch=16,bty='n',ncol=4)
  mtext(side=3, outer=TRUE,"Summer T trend as function of summer TA trend",font=2)
  print_correlation(summer_ta,summer_t)

  lims=c(min(winter_ta,winter_t,na.rm=TRUE),max(winter_ta,winter_t,na.rm=TRUE))
  plot(winter_ta,winter_t,main="Area",xlab="winter TA trend",ylab="winter T trend",
       ylim=lims,xlim=lims,col=get_area_colors(area))
  lines(c(-100,100),c(-100,100),lty=2)
  legend("bottom",legend=c("<100km2","100-1000km^2","1000-10000km^2",">10000km^2"),
         col=c(1,2,3,4),pch=16,bty='n',ncol=2)
  plot(winter_ta,winter_t,main="Glacier",xlab="winter TA trend",ylab="winter T trend",
       ylim=lims,xlim=lims,col=get_glacier_colors(glacier))
  lines(c(-100,100),c(-100,100),lty=2)
  legend("bottom",legend=c("0%","0-5%","5-10%",">10%"),col=c(1,2,3,4),
         pch=16,bty='n',ncol=2)
  plot(winter_ta,winter_t,main="Elevation",xlab="winter TA trend",ylab="winter T trend",
       ylim=lims,xlim=lims,col=get_elevation_colors(elevation))
  lines(c(-100,100),c(-100,100),lty=2)
  legend("bottom",legend=c("<700m","700-1200m","1200-1800m",">1800m"),col=c(1,2,3,4),
         pch=16,bty='n',ncol=2)
  plot(winter_ta,winter_t,main="Glacier",xlab="winter TA trend",ylab="winter T trend",
       ylim=lims,xlim=lims,col=get_regimes2_colors(regimes2))
  lines(c(-100,100),c(-100,100),lty=2)
  legend("bottom",legend=c("None","Hydropeaking","After lakes","Plateau/Jura","Alpine"),
         col=c(1,2,3,4,5),pch=16,bty='n',ncol=4)
  mtext(side=3, outer=TRUE,"Winter T trend as function of winter TA trend",font=2)
  print_correlation(winter_ta,winter_t)

  lims=c(min(spring_ta,spring_t,na.rm=TRUE),max(spring_ta,spring_t,na.rm=TRUE))
  plot(spring_ta,spring_t,main="Area",xlab="spring TA trend",ylab="spring T trend",
       ylim=lims,xlim=lims,col=get_area_colors(area))
  lines(c(-100,100),c(-100,100),lty=2)
  legend("bottom",legend=c("<100km2","100-1000km^2","1000-10000km^2",">10000km^2"),
         col=c(1,2,3,4),pch=16,bty='n',ncol=2)
  plot(spring_ta,spring_t,main="Glacier",xlab="spring TA trend",ylab="spring T trend",
       ylim=lims,xlim=lims,col=get_glacier_colors(glacier))
  lines(c(-100,100),c(-100,100),lty=2)
  legend("bottom",legend=c("0%","0-5%","5-10%",">10%"),col=c(1,2,3,4),pch=16,
         bty='n',ncol=2)
  plot(spring_ta,spring_t,main="Elevation",xlab="spring TA trend",ylab="spring T trend",
       ylim=lims,xlim=lims,col=get_elevation_colors(elevation))
  lines(c(-100,100),c(-100,100),lty=2)
  legend("bottom",legend=c("<700m","700-1200m","1200-1800m",">1800m"),col=c(1,2,3,4),
         pch=16,bty='n',ncol=2)
  plot(spring_ta,spring_t,main="Glacier",xlab="spring TA trend",ylab="spring T trend",
       ylim=lims,xlim=lims,col=get_regimes2_colors(regimes2))
  lines(c(-100,100),c(-100,100),lty=2)
  legend("bottom",legend=c("None","Hydropeaking","After lakes","Plateau/Jura","Alpine"),
         col=c(1,2,3,4,5),pch=16,bty='n',ncol=4)
  mtext(side=3, outer=TRUE,"Spring T trend as function of spring TA trend",font=2)
  print_correlation(spring_ta,spring_t)

  lims=c(min(fall_ta,fall_t,na.rm=TRUE),max(fall_ta,fall_t,na.rm=TRUE))
  plot(fall_ta,fall_t,main="Area",xlab="fall TA trend",ylab="fall T trend",
       ylim=lims,xlim=lims,col=get_area_colors(area))
  lines(c(-100,100),c(-100,100),lty=2)
  legend("bottom",legend=c("<100km2","100-1000km^2","1000-10000km^2",">10000km^2"),
         col=c(1,2,3,4),pch=16,bty='n',ncol=2)
  plot(fall_ta,fall_t,main="Glacier",xlab="fall TA trend",ylab="fall T trend",
       ylim=lims,xlim=lims,col=get_glacier_colors(glacier))
  lines(c(-100,100),c(-100,100),lty=2)
  legend("bottom",legend=c("0%","0-5%","5-10%",">10%"),col=c(1,2,3,4),pch=16,
         bty='n',ncol=2)
  plot(fall_ta,fall_t,main="Elevation",xlab="fall TA trend",ylab="fall T trend",
       ylim=lims,xlim=lims,col=get_elevation_colors(elevation))
  lines(c(-100,100),c(-100,100),lty=2)
  legend("bottom",legend=c("<700m","700-1200m","1200-1800m",">1800m"),col=c(1,2,3,4),
         pch=16,bty='n',ncol=2)
  plot(fall_ta,fall_t,main="Glacier",xlab="fall TA trend",ylab="fall T trend",
       ylim=lims,xlim=lims,col=get_regimes2_colors(regimes2))
  lines(c(-100,100),c(-100,100),lty=2)
  legend("bottom",legend=c("None","Hydropeaking","After lakes","Plateau/Jura","Alpine"),
         col=c(1,2,3,4,5),pch=16,bty='n',ncol=4)
  mtext(side=3, outer=TRUE,"Fall T trend as function of fall TA trend",font=2)
  print_correlation(fall_ta,fall_t)

  ### P AND Q RELATION ###
  par(mfrow=c(2,5))
  hist(q_norm,xlim=c(-60,20),breaks=seq(-50,50,by=5))
  abline(v=mean(q_norm,na.rm=TRUE),col=2)
  text(x=-60,y=3,labels=paste("mean:",round(mean(q_norm,na.rm=TRUE),3)),pos=4)
  hist(summer_q_norm,xlim=c(-60,20),breaks=seq(-50,50,by=5))
  abline(v=mean(summer_q_norm,na.rm=TRUE),col=2)
  text(x=-60,y=3,labels=paste("mean:",round(mean(summer_q_norm,na.rm=TRUE),3)),pos=4)
  hist(winter_q_norm,xlim=c(-60,20),breaks=seq(-50,50,by=5))
  abline(v=mean(winter_q_norm,na.rm=TRUE),col=2)
  text(x=-60,y=3,labels=paste("mean:",round(mean(winter_q_norm,na.rm=TRUE),3)),pos=4)
  hist(spring_q_norm,xlim=c(-60,20),breaks=seq(-50,50,by=5))
  abline(v=mean(spring_q_norm,na.rm=TRUE),col=2)
  text(x=-60,y=3,labels=paste("mean:",round(mean(spring_q_norm,na.rm=TRUE),3)),pos=4)
  hist(fall_q_norm,xlim=c(-60,20),breaks=seq(-60,50,by=5))
  abline(v=mean(fall_q_norm,na.rm=TRUE),col=2)
  text(x=-60,y=3,labels=paste("mean:",round(mean(fall_q_norm,na.rm=TRUE),3)),pos=4)
  hist(p_norm,xlim=c(-60,20),breaks=seq(-50,50,by=5))
  abline(v=mean(p_norm,na.rm=TRUE),col=2)
  text(x=-60,y=3,labels=paste("mean:",round(mean(p_norm,na.rm=TRUE),3)),pos=4)
  hist(summer_p_norm,xlim=c(-60,20),breaks=seq(-50,50,by=5))
  abline(v=mean(summer_p_norm,na.rm=TRUE),col=2)
  text(x=-60,y=3,labels=paste("mean:",round(mean(summer_p_norm,na.rm=TRUE),3)),pos=4)
  hist(winter_p_norm,xlim=c(-60,20),breaks=seq(-50,50,by=5))
  abline(v=mean(winter_p_norm,na.rm=TRUE),col=2)
  text(x=-60,y=3,labels=paste("mean:",round(mean(winter_p_norm,na.rm=TRUE),3)),pos=4)
  hist(spring_p_norm,xlim=c(-60,20),breaks=seq(-50,50,by=5))
  abline(v=mean(spring_p_norm,na.rm=TRUE),col=2)
  text(x=-60,y=3,labels=paste("mean:",round(mean(spring_p_norm,na.rm=TRUE),3)),pos=4)
  hist(fall_p_norm,xlim=c(-60,20),breaks=seq(-50,50,by=5))
  abline(v=mean(fall_p_norm,na.rm=TRUE),col=2)
  text(x=-60,y=3,labels=paste("mean:",round(mean(fall_p_norm,na.rm=TRUE),3)),pos=4)
  mtext(side=3, outer=TRUE,"Distribution of trends for Q and P",font=2)
  par(mfrow=c(2,2))

  lims=c(min(p_norm,q_norm,na.rm=TRUE),max(p_norm,q_norm,na.rm=TRUE))
  plot(p_norm,q_norm,main="Area",xlab="norm P trend",ylab="norm Q trend",
       ylim=lims,xlim=lims,col=get_area_colors(area))
  lines(c(-100,100),c(-100,100),lty=2)
  legend("bottom",legend=c("<100km2","100-1000km^2","1000-10000km^2",">10000km^2"),
         col=c(1,2,3,4),pch=16,bty='n',ncol=2)
  plot(p_norm,q_norm,main="Glacier",xlab="norm P trend",ylab="norm Q trend",
       ylim=lims,xlim=lims,col=get_glacier_colors(glacier))
  lines(c(-100,100),c(-100,100),lty=2)
  legend("bottom",legend=c("0%","0-5%","5-10%",">10%"),col=c(1,2,3,4),pch=16,
         bty='n',ncol=2)
  plot(p_norm,q_norm,main="Elevation",xlab="norm P trend",ylab="norm Q trend",
       ylim=lims,xlim=lims,col=get_elevation_colors(elevation))
  lines(c(-100,100),c(-100,100),lty=2)
  legend("bottom",legend=c("<700m","700-1200m","1200-1800m",">1800m"),col=c(1,2,3,4),
         pch=16,bty='n',ncol=2)
  plot(p_norm,q_norm,main="Regime",xlab="norm P trend",ylab="norm Q trend",
       ylim=lims,xlim=lims,col=get_regimes2_colors(regimes2))
  lines(c(-100,100),c(-100,100),lty=2)
  legend("bottom",legend=c("None","Hydropeaking","After lakes","Plateau/Jura","Alpine"),
         col=c(1,2,3,4,5),pch=16,bty='n',ncol=4)
  mtext(side=3, outer=TRUE,"Annual norm Q trend as function of norm P trend",font=2)
  print_correlation(p_norm,q_norm)

  lims=c(min(summer_p_norm,summer_q_norm,na.rm=TRUE),
         max(summer_p_norm,summer_q_norm,na.rm=TRUE))
  plot(summer_p_norm,summer_q_norm,main="Area",xlab="summer norm P trend",
       ylab="summer norm Q trend",ylim=lims,xlim=lims,col=get_area_colors(area))
  lines(c(-100,100),c(-100,100),lty=2)
  legend("bottom",legend=c("<100km2","100-1000km^2","1000-10000km^2",">10000km^2"),
         col=c(1,2,3,4),pch=16,bty='n',ncol=2)
  plot(summer_p_norm,summer_q_norm,main="Glacier",xlab="summer norm P trend",
       ylab="summer norm Q trend",ylim=lims,xlim=lims,col=get_glacier_colors(glacier))
  lines(c(-100,100),c(-100,100),lty=2)
  legend("bottom",legend=c("0%","0-5%","5-10%",">10%"),col=c(1,2,3,4),pch=16,bty='n',ncol=2)
  plot(summer_p_norm,summer_q_norm,main="Elevation",xlab="summer norm P trend",
       ylab="summer norm Q trend",ylim=lims,xlim=lims,col=get_elevation_colors(elevation))
  lines(c(-100,100),c(-100,100),lty=2)
  legend("bottom",legend=c("<700m","700-1200m","1200-1800m",">1800m"),col=c(1,2,3,4),
         pch=16,bty='n',ncol=2)
  plot(summer_p_norm,summer_q_norm,main="Regime",xlab="summer norm P trend",
       ylab="summer norm Q trend",ylim=lims,xlim=lims,col=get_regimes2_colors(regimes2))
  lines(c(-100,100),c(-100,100),lty=2)
  legend("bottom",legend=c("None","Hydropeaking","After lakes","Plateau/Jura","Alpine"),
         col=c(1,2,3,4,5),pch=16,bty='n',ncol=4)
  mtext(side=3, outer=TRUE,"Summer norm Q trend as function of summer norm P trend",font=2)
  print_correlation(summer_p_norm,summer_q_norm)

  lims=c(min(winter_p_norm,winter_q_norm,na.rm=TRUE),
         max(winter_p_norm,winter_q_norm,na.rm=TRUE))
  plot(winter_p_norm,winter_q_norm,main="Area",xlab="winter norm P trend",
       ylab="winter norm Q trend", ylim=lims,xlim=lims,col=get_area_colors(area))
  lines(c(-100,100),c(-100,100),lty=2)
  legend("bottom",legend=c("<100km2","100-1000km^2","1000-10000km^2",">10000km^2"),
         col=c(1,2,3,4),pch=16,bty='n',ncol=2)
  plot(winter_p_norm,winter_q_norm,main="Glacier",xlab="winter norm P trend",
       ylab="winter norm Q trend", ylim=lims,xlim=lims,col=get_glacier_colors(glacier))
  lines(c(-100,100),c(-100,100),lty=2)
  legend("bottom",legend=c("0%","0-5%","5-10%",">10%"),col=c(1,2,3,4),pch=16,bty='n',ncol=2)
  plot(winter_p_norm,winter_q_norm,main="Elevation",xlab="winter norm P trend",
       ylab="winter norm Q trend", ylim=lims,xlim=lims,col=get_elevation_colors(elevation))
  lines(c(-100,100),c(-100,100),lty=2)
  legend("bottom",legend=c("<700m","700-1200m","1200-1800m",">1800m"),col=c(1,2,3,4),pch=16,bty='n',ncol=2)
  plot(winter_p_norm,winter_q_norm,main="Regime",xlab="winter norm P trend",ylab="winter norm Q trend",
       ylim=lims,xlim=lims,col=get_regimes2_colors(regimes2))
  lines(c(-100,100),c(-100,100),lty=2)
  legend("bottom",legend=c("None","Hydropeaking","After lakes","Plateau/Jura","Alpine"),
         col=c(1,2,3,4,5),pch=16,bty='n',ncol=4)
  mtext(side=3, outer=TRUE,"Winter norm Q trend as function of winter norm P trend",font=2)
  print_correlation(winter_p_norm,winter_q_norm)

  lims=c(min(spring_p_norm,spring_q_norm,na.rm=TRUE),max(spring_p_norm,spring_q_norm,na.rm=TRUE))
  plot(spring_p_norm,spring_q_norm,main="Area",xlab="spring norm P trend",
       ylab="spring norm Q trend", ylim=lims,xlim=lims,col=get_area_colors(area))
  lines(c(-100,100),c(-100,100),lty=2)
  legend("bottom",legend=c("<100km2","100-1000km^2","1000-10000km^2",">10000km^2"),
         col=c(1,2,3,4),pch=16,bty='n',ncol=2)
  plot(spring_p_norm,spring_q_norm,main="Glacier",xlab="spring norm P trend",
       ylab="spring norm Q trend",ylim=lims,xlim=lims,col=get_glacier_colors(glacier))
  lines(c(-100,100),c(-100,100),lty=2)
  legend("bottom",legend=c("0%","0-5%","5-10%",">10%"),col=c(1,2,3,4),pch=16,bty='n',ncol=2)
  plot(spring_p_norm,spring_q_norm,main="Elevation",xlab="spring norm P trend",
       ylab="spring norm Q trend", ylim=lims,xlim=lims,col=get_elevation_colors(elevation))
  lines(c(-100,100),c(-100,100),lty=2)
  legend("bottom",legend=c("<700m","700-1200m","1200-1800m",">1800m"),
         col=c(1,2,3,4),pch=16,bty='n',ncol=2)
  plot(spring_p_norm,spring_q_norm,main="Regime",xlab="spring norm P trend",
       ylab="spring norm Q trend",ylim=lims,xlim=lims,col=get_regimes2_colors(regimes2))
  lines(c(-100,100),c(-100,100),lty=2)
  legend("bottom",legend=c("None","Hydropeaking","After lakes","Plateau/Jura","Alpine"),
         col=c(1,2,3,4,5),pch=16,bty='n',ncol=4)
  mtext(side=3, outer=TRUE,"Spring norm Q trend as function of spring norm P trend",font=2)
  print_correlation(spring_p_norm,spring_q_norm)

  lims=c(min(fall_p_norm,fall_q_norm,na.rm=TRUE),max(fall_p_norm,fall_q_norm,na.rm=TRUE))
  plot(fall_p_norm,fall_q_norm,main="Area",xlab="fall norm P trend",ylab="fall norm Q trend",
       ylim=lims,xlim=lims,col=get_area_colors(area))
  lines(c(-100,100),c(-100,100),lty=2)
  legend("bottom",legend=c("<100km2","100-1000km^2","1000-10000km^2",">10000km^2"),
         col=c(1,2,3,4),pch=16,bty='n',ncol=2)
  plot(fall_p_norm,fall_q_norm,main="Glacier",xlab="fall norm P trend",
       ylab="fall norm Q trend", ylim=lims,xlim=lims,col=get_glacier_colors(glacier))
  lines(c(-100,100),c(-100,100),lty=2)
  legend("bottom",legend=c("0%","0-5%","5-10%",">10%"),col=c(1,2,3,4),pch=16,
         bty='n',ncol=2)
  plot(fall_p_norm,fall_q_norm,main="Elevation",xlab="fall norm P trend",
       ylab="fall norm Q trend", ylim=lims,xlim=lims,col=get_elevation_colors(elevation))
  lines(c(-100,100),c(-100,100),lty=2)
  legend("bottom",legend=c("<700m","700-1200m","1200-1800m",">1800m"),
         col=c(1,2,3,4),pch=16,bty='n',ncol=2)
  plot(fall_p_norm,fall_q_norm,main="Regime",xlab="fall norm P trend",
       ylab="fall norm Q trend",ylim=lims,xlim=lims,col=get_regimes2_colors(regimes2))
  lines(c(-100,100),c(-100,100),lty=2)
  legend("bottom",legend=c("None","Hydropeaking","After lakes","Plateau/Jura","Alpine"),
         col=c(1,2,3,4,5),pch=16,bty='n',ncol=4)
  mtext(side=3, outer=TRUE,"Fall norm Q trend as function of fall norm P trend",font=2)
  print_correlation(fall_p_norm,fall_q_norm)
}
