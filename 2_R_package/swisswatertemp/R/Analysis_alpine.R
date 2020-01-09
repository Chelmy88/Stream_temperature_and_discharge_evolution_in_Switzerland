# This script is related to the paper:
# "Stream temperature evolution in Switzerland over the last 50 years, Adrien
# Michel, Tristan Brauchli, Michael Lehning, Bettina Schaefli, and Hendrik Huwald,
# HESS, 2019"

# This material is distributed under the GPLv3 license
# (https://www.gnu.org/licenses/gpl-3.0.html)

#' Produces plot for alpine catchemts
#'
#' This function produces plot for alpine catchments (Figure S35). The plots are
#' saved in plots/alpine.pdf
#'
#' @param rivers_data The dataset of rivers data
#' @export
plot_alpine <- function(rivers_data)
{
  #Create data
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
  period="1999-2018"
  for (river_station in names(rivers_data)){
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

  # Set NA to all variable if T or Q is NA
  nans=which(is.na(t) | is.na(q))
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


  extremes=c()
  seasons=c("DJF","MAM","JJA","SON")
  regimes=c()
  names=c()
  for (river_station in names(rivers_data))
  {
    regimes=c(regimes,rivers_data[[river_station]]$header$regime2)
    names=c(names,rivers_data[[river_station]]$header$abbr)
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


  years=c(1999:2018)
  Y_all=c()
  T_all=list()
  Q_all=list()
  TA_all=list()
  P_all=list()
  R_all=c()
  N_all=c()
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
      N_all=c(N_all,names[i])
    }
    i=i+1
  }



  pdf(paste0("plots/alpine.pdf"),width=12,height=8)


  par(mar=c(1.5,3.5,2,1),mgp=c(1.7,0.5,0),oma=c(0,0,0,0),cex.lab=1.2,cex.axis=1.2)
  layout(matrix(c(1,2,3,4,5,6,7,8), ncol=2, byrow=TRUE),heights=c(1,1,1,1))

  r2=which(regimes2=="Alpine regime")
  alp_abbr=c()
  for(i in r2){
    alp_abbr=c(alp_abbr,rivers_data[[i]]$header$abbr)
  }


  pc=16
  co=2:6
  plot(rep(1,5),t[r2],xlim=c(0.4,5.6),ylim=c(-0.05,0.7),pch=pc,col=co,xaxt="n",xlab="",ylab=expression(paste("Trend (",degree,"C per decade)")),main="Water temperature trend" )
  axis(1, at=c(1:5), labels=c("Annual", "Winter","Spring", "Summer","Fall"))
  points(rep(2,5),winter_t[r2],pch=pc,col=co)
  points(rep(3,5),spring_t[r2],pch=pc,col=co)
  points(rep(4,5),summer_t[r2],pch=pc,col=co)
  points(rep(5,5),fall_t[r2],pch=pc,col=co)
  points(1:5, c(median(t),median(winter_t),median(spring_t),median(summer_t),median(fall_t)), col = "black",pch=15,cex=1)
  legend("top",legend=c(alp_abbr,"Median all"),col=c(co,1),pch=c(pc,pc,pc,pc,pc,15),ncol=6,bty='n',cex=1.2)
  abline(h=0,lwd=0.5)

  plot(rep(1,5),ta[r2],xlim=c(0.4,5.6),ylim=c(-0.05,0.7),pch=pc,col=co,xaxt="n",xlab="",ylab=expression(paste("Trend (",degree,"C per decade)")),main="Air temperature trend ")
  axis(1, at=c(1:5), labels=c("Annual", "Winter","Spring", "Summer","Fall"))
  points(rep(2,5),winter_ta[r2],pch=pc,col=co)
  points(rep(3,5),spring_ta[r2],pch=pc,col=co)
  points(rep(4,5),summer_ta[r2],pch=pc,col=co)
  points(rep(5,5),fall_ta[r2],pch=pc,col=co)
  points(1:5, c(median(ta),median(winter_ta),median(spring_ta),median(summer_ta),median(fall_ta)), col = "black",pch=15,cex=1)
  abline(h=0,lwd=0.5)

  plot(rep(1,5),q_norm[r2],xlim=c(0.4,5.6),ylim=c(-25,15),pch=pc,col=co,xaxt="n",xlab="",ylab=expression(paste("Trend (% per decade)")),main="Discharge trend")
  axis(1, at=c(1:5), labels=c("Annual", "Winter","Spring", "Summer","Fall"))
  points(rep(2,5),winter_q_norm[r2],pch=pc,col=co)
  points(rep(3,5),spring_q_norm[r2],pch=pc,col=co)
  points(rep(4,5),summer_q_norm[r2],pch=pc,col=co)
  points(rep(5,5),fall_q_norm[r2],pch=pc,col=co)
  points(1:5, c(median(q_norm),median(winter_q_norm),median(spring_q_norm),median(summer_q_norm),median(fall_q_norm)), col = "black",pch=15,cex=1)
  abline(h=0,lwd=0.5)

  plot(rep(1,5),p_norm[r2],xlim=c(0.4,5.6),ylim=c(-25,15),pch=pc,col=co,xaxt="n",xlab="",ylab=expression(paste("Trend (% per decade)")),main="Precipitation trend")
  axis(1, at=c(1:5), labels=c("Annual", "Winter","Spring", "Summer","Fall"))
  points(rep(2,5),winter_p_norm[r2],pch=pc,col=co)
  points(rep(3,5),spring_p_norm[r2],pch=pc,col=co)
  points(rep(4,5),summer_p_norm[r2],pch=pc,col=co)
  points(rep(5,5),fall_p_norm[r2],pch=pc,col=co)
  points(1:5, c(median(p_norm),median(winter_p_norm),median(spring_p_norm),median(summer_p_norm),median(fall_p_norm)), col = "black",pch=15,cex=1)
  abline(h=0,lwd=0.5)

  alp=which(R_all=="Alpine regime")

  meansT <- aggregate(T_all, by = list(Y_all), FUN = median,  na.rm=TRUE)
  meansTA <- aggregate(TA_all, by = list(Y_all), FUN = median,  na.rm=TRUE)
  meansQ <- aggregate(Q_all, by = list(Y_all), FUN = median,  na.rm=TRUE)
  meansP <- aggregate(P_all, by = list(Y_all), FUN = median,  na.rm=TRUE)

  co=c(rep(2,20),rep(3,20),rep(4,20),rep(5,20),rep(6,20))

  plot(Y_all[alp], T_all$JJA[alp], ylim=c(-1.5,4),ylab=expression(paste("Anomaly (",degree,"C)")),col=co,pch=pc,xlab="",main="Summer water temperature anomaly")
  points(meansT$Group.1,meansT$JJA,col = "black",pch=15,cex=1)
  abline(h=0,lwd=0.5)

  plot(Y_all[alp],TA_all$JJA[alp], ylim=c(-1.5,4),ylab=expression(paste("Anomaly (",degree,"C)")),col=co,pch=pc,xlab="",main="Summer air temperature anomaly ")
  points(meansTA$Group.1,meansTA$JJA,col = "black",pch=15,cex=1)
  abline(h=0,lwd=0.5)

  plot(Y_all[alp],Q_all$JJA[alp],ylim=c(-50,100),ylab="Anomaly (%)",col=co,pch=pc,xlab="",main="Summer discharge anomaly")
  points(meansQ$Group.1,meansQ$JJA,col = "black",pch=15,cex=1)
  abline(h=0,lwd=0.5)

  plot(Y_all[alp], P_all$JJA[alp],ylim=c(-50,100),ylab="Anomaly (%)",col=co,pch=pc,xlab="",main="Summer precipitaion anomaly")
  points(meansP$Group.1,meansP$JJA,col = "black",pch=15,cex=1)
  abline(h=0,lwd=0.5)

  plot(Y_all[alp], T_all$DJF[alp], ylim=c(-4,5),ylab=expression(paste("Winter water temperature anomaly (",degree,"C)")),col=co,pch=pc)
  points(meansT$Group.1,meansT$DJF,col = "black",pch=15,cex=1)
  abline(h=c(-3:4),col="lightgrey",lty=3)
  abline(v=c(2,7,12,17),col="lightgrey",lty=3)

  plot(Y_all[alp],TA_all$DJF[alp], ylim=c(-4,5),ylab=expression(paste("Winter air temperature anomaly (",degree,"C)")),col=co,pch=pc)
  points(meansTA$Group.1,meansTA$DJF,col = "black",pch=15,cex=1)
  abline(h=c(-3:4),col="lightgrey",lty=3)
  abline(v=c(2,7,12,17),col="lightgrey",lty=3)

  plot(Y_all[alp],Q_all$DJF[alp],ylim=c(-100,270),ylab="Winter discharge anomaly (%)",col=co,pch=pc)
  points(meansQ$Group.1,meansQ$DJF,col = "black",pch=15,cex=1)
  abline(v=c(2,7,12,17),col="lightgrey",lty=3)
  abline(h=c(-100,-50,0,50,100,150,200,250),col="lightgrey",lty=3)

  plot(Y_all[alp], P_all$DJF[alp],ylim=c(-100,270),ylab="Winter precipitaion anomaly (%)",col=co,pch=pc)
  points(meansP$Group.1,meansP$DJF,col = "black",pch=15,cex=1)
  abline(v=c(2,7,12,17),col="lightgrey",lty=3)
  abline(h=c(-100,-50,0,50,100,150,200,250),col="lightgrey",lty=3)

  plot(Y_all[alp], T_all$MAM[alp], ylim=c(-4,5),ylab=expression(paste("Spring water temperature anomaly (",degree,"C)")),col=co,pch=pc)
  points(meansT$Group.1,meansT$MAM,col = "black",pch=15,cex=1)
  abline(h=c(-3:4),col="lightgrey",lty=3)
  abline(v=c(2,7,12,17),col="lightgrey",lty=3)

  plot(Y_all[alp],TA_all$MAM[alp], ylim=c(-4,5),ylab=expression(paste("Spring air temperature anomaly (",degree,"C)")),col=co,pch=pc)
  points(meansTA$Group.1,meansTA$MAM,col = "black",pch=15,cex=1)
  abline(h=c(-3:4),col="lightgrey",lty=3)
  abline(v=c(2,7,12,17),col="lightgrey",lty=3)

  plot(Y_all[alp],Q_all$MAM[alp],ylim=c(-100,270),ylab="Spring discharge anomaly (%)",col=co,pch=pc)
  points(meansQ$Group.1,meansQ$MAM,col = "black",pch=15,cex=1)
  abline(v=c(2,7,12,17),col="lightgrey",lty=3)
  abline(h=c(-100,-50,0,50,100,150,200,250),col="lightgrey",lty=3)

  plot(Y_all[alp], P_all$MAM[alp],ylim=c(-100,270),ylab="Spring precipitaion anomaly (%)",col=co,pch=pc)
  points(meansP$Group.1,meansP$MAM,col = "black",pch=15,cex=1)
  abline(v=c(2,7,12,17),col="lightgrey",lty=3)
  abline(h=c(-100,-50,0,50,100,150,200,250),col="lightgrey",lty=3)

  dev.off()
}
