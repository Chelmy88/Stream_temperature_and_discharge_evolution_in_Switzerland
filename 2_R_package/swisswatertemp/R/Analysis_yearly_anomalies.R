# This script is related to the paper:
# "Stream temperature evolution in Switzerland over the last 50 years, Adrien
# Michel, Tristan Brauchli, Michael Lehning, Bettina Schaefli, and Hendrik Huwald,
# HESS, 2019"

# This material is distributed under the GPLv3 license
# (https://www.gnu.org/licenses/gpl-3.0.html)

#' Plot yearyl seasonnal anomlaies
#'
#' This function plots the yearly seasonnal anomalies for T, Q, TA and P shown
#' in Figures 11 and 13 and in Figures S20 and S22 in supplementary. The figure
#' are saved under plots/summer_anomalies.pdf, plots/winter_anomalies.pdf,
#' plots/spring_anomalies.pdf, and plots/fall_anomalies.pdf
#'
#' @param rivers_data The dataset of rivers data
#'
#' @export
plot_yearly_anomalies<-function(rivers_data)
{

  get_regimes2_colors <- function(regimes2){
    colors=c()
    for (value in regimes2)
    {
      if(value==" Undefined")
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

  extremes=c()
  seasons=c("DJF","MAM","JJA","SON")
  regimes=c()
  for (river_station in names(rivers_data))
  {
    regimes=c(regimes,rivers_data[[river_station]]$header$regime2)

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

  pdf(paste0("plots/summer_anomalies.pdf"),width=12,height=8)

  par(mfrow=c(2,2))
  par(mar=c(2,3.5,1,1),mgp=c(1.7,0.6,0),oma=c(0,0,0,0),cex.lab=1.4,cex.axis=1.25)

  boxplot(T_all$JJA ~ Y_all, ylim=c(-3,4),ylab=expression(paste("Summer water temperature anomaly (",degree,"C)")))
  abline(h=c(-3:4),col="lightgrey",lty=3)
  abline(v=c(2,7,12,17),col="lightgrey",lty=3)
  abline(h=0)
  legend("topleft",title="Water temperature",legend="",bty="n",cex=1.3)

  boxplot(TA_all$JJA ~ Y_all, ylim=c(-3,4),ylab=expression(paste("Summer air temperature anomaly (",degree,"C)")))
  abline(h=c(-3:4),col="lightgrey",lty=3)
  abline(v=c(2,7,12,17),col="lightgrey",lty=3)
  abline(h=0)
  legend("topleft",title="Air temperature",legend="",bty="n",cex=1.3)

  boxplot(Q_all$JJA ~ Y_all,ylim=c(-100,270),ylab="Summer discharge anomaly (%)")
  abline(v=c(2,7,12,17),col="lightgrey",lty=3)
  abline(h=c(-100,-50,0,50,100,150,200,250),col="lightgrey",lty=3)
  abline(h=0)
  i=1; j=1; leg=c()
  legend("topleft",title="Dsicharge",legend="",bty="n",cex=1.3)

  boxplot(P_all$JJA ~ Y_all,ylim=c(-100,270),ylab="Summer precipitaion anomaly (%)")
  abline(v=c(2,7,12,17),col="lightgrey",lty=3)
  abline(h=c(-100,-50,0,50,100,150,200,250),col="lightgrey",lty=3)
  abline(h=0)
  legend("topleft",title="Precipitation",legend="",bty="n",cex=1.3)

  ######

  get_regimes2_colors <- function(regimes2){
    colors=c()
    for (value in regimes2)
    {
      if(value==" Undefined")
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

  cors_T_JJA_P_MAM=c()
  cors_T_JJA_T_MAM=c()
  cors_T_MAM_TA_MAM=c()
  cors_T_MAM_P_MAM=c()

  cors_T_JJA_P_JJA=c()
  cors_T_JJA_T_JJA=c()
  cors_T_JJA_TA_JJA=c()

  cors_T_JJA_T_DJF=c()
  cors_T_JJA_P_DJF=c()
  cors_T_DJF_TA_DJF=c()
  cors_T_DJF_P_DJF=c()

  cors_T_SON_T_JJA=c()
  cors_T_SON_P_JJA=c()
  cors_T_SON_TA_SON=c()
  cors_T_SON_P_SON=c()

  for (t in extremes){
    cors_T_JJA_P_MAM=c(cors_T_JJA_P_MAM,cor(t$T$JJA,t$P$MAM,use="complete.obs"))
    cors_T_JJA_T_MAM=c(cors_T_JJA_T_MAM,cor(t$T$JJA,t$T$MAM,use="complete.obs"))
    cors_T_MAM_TA_MAM=c(cors_T_MAM_TA_MAM,cor(t$T$MAM,t$TA$MAM,use="complete.obs"))
    cors_T_MAM_P_MAM=c(cors_T_MAM_P_MAM,cor(t$T$MAM,t$P$MAM,use="complete.obs"))

    cors_T_JJA_P_JJA=c(cors_T_JJA_P_JJA,cor(t$T$JJA,t$P$JJA,use="complete.obs"))
    cors_T_JJA_T_JJA=c(cors_T_JJA_T_JJA,cor(t$T$JJA,t$T$JJA,use="complete.obs"))
    cors_T_JJA_TA_JJA=c(cors_T_JJA_TA_JJA,cor(t$T$JJA,t$TA$JJA,use="complete.obs"))

    cors_T_JJA_T_DJF=c(cors_T_JJA_T_DJF,cor(t$T$JJA,t$T$DJF,use="complete.obs"))
    cors_T_JJA_P_DJF=c(cors_T_JJA_T_DJF,cor(t$T$JJA,t$P$DJF,use="complete.obs"))
    cors_T_DJF_TA_DJF=c(cors_T_DJF_TA_DJF,cor(t$T$DJF,t$TA$DJF,use="complete.obs"))
    cors_T_DJF_P_DJF=c(cors_T_DJF_P_DJF,cor(t$T$DJF,t$P$DJF,use="complete.obs"))

    cors_T_SON_T_JJA=c(cors_T_SON_T_JJA,cor(t$T$SON,t$T$JJA,use="complete.obs"))
    cors_T_SON_P_JJA=c(cors_T_SON_P_JJA,cor(t$T$SON,t$P$JJA,use="complete.obs"))
    cors_T_SON_TA_SON=c(cors_T_SON_TA_SON,cor(t$T$SON,t$TA$SON,use="complete.obs"))
    cors_T_SON_P_SON=c(cors_T_SON_P_SON,cor(t$T$SON,t$P$SON,use="complete.obs"))

  }
  dev.off()


  pdf(paste0("plots/winter_anomalies.pdf"),width=12,height=8)
  par(mfrow=c(2,2))
  par(mar=c(2,3.5,1,1),mgp=c(1.7,0.6,0),oma=c(0,0,0,0),cex.lab=1.4,cex.axis=1.25)

  boxplot(T_all$DJF ~ Y_all, ylim=c(-3,4),ylab=expression(paste("Winter water temperature anomaly (",degree,"C)")))
  abline(h=c(-3:4),col="lightgrey",lty=3)
  abline(v=c(1:20),col="lightgrey",lty=3)
  abline(h=0)
  legend("topleft",title="Water temperature",legend="",bty="n",cex=1.3)

  boxplot(TA_all$DJF ~ Y_all, ylim=c(-3,4),ylab=expression(paste("Winter air temperature anomaly (",degree,"C)")))
  abline(h=c(-3:4),col="lightgrey",lty=3)
  abline(v=c(1:20),col="lightgrey",lty=3)
  abline(h=0)
  legend("topleft",title="Air temperature",legend="",bty="n",cex=1.3)

  boxplot(Q_all$DJF ~ Y_all,ylim=c(-100,270), ylab="Winter discharge anomaly (%)")
  abline(v=c(1:20),col="lightgrey",lty=3)
  abline(h=c(-100,-50,0,50,100,150,200,250),col="lightgrey",lty=3)
  abline(h=0)
  legend("topleft",title="Discharge",legend="",bty="n",cex=1.3)

  boxplot(P_all$DJF ~ Y_all,ylim=c(-100,270) ,ylab="Winter precipitation anomaly (%)")
  abline(v=c(1:20),col="lightgrey",lty=3)
  abline(h=c(-100,-50,0,50,100,150,200,250),col="lightgrey",lty=3)
  abline(h=0)
  legend("topleft",title="Precipitation",legend="",bty="n",cex=1.3)

  dev.off()


  pdf(paste0("plots/spring_anomalies.pdf"),width=12,height=8)

  par(mfrow=c(2,2))
  par(mar=c(2,3.5,1,1),mgp=c(1.7,0.6,0),oma=c(0,0,0,0),cex.lab=1.4,cex.axis=1.25)

  boxplot(T_all$MAM ~ Y_all, ylim=c(-3,4),ylab=expression(paste("Spring water temperature anomaly (",degree,"C)")))
  abline(h=c(-3:4),col="lightgrey",lty=3)
  abline(v=c(2,7,12,17),col="lightgrey",lty=3)
  abline(h=0)
  legend("topleft",title="Water temperature",legend="",bty="n",cex=1.3)

  boxplot(TA_all$MAM ~ Y_all, ylim=c(-3,4), ylab=expression(paste("Spring air temperature anomaly (",degree,"C)")))
  abline(h=c(-3:4),col="lightgrey",lty=3)
  abline(v=c(2,7,12,17),col="lightgrey",lty=3)
  abline(h=0)
  legend("topleft",title="Air temperature",legend="",bty="n",cex=1.3)

  boxplot(Q_all$MAM ~ Y_all,ylim=c(-100,270), ylab="Spring discharge anomaly (%)")
  abline(v=c(2,7,12,17),col="lightgrey",lty=3)
  abline(h=c(-100,-50,0,50,100,150,200,250),col="lightgrey",lty=3)
  abline(h=0)
  legend("topleft",title="Discharge",legend="",bty="n",cex=1.3)

  boxplot(P_all$MAM ~ Y_all,ylim=c(-100,270), ylab="Spring precipitaion anomaly (%)")
  abline(v=c(2,7,12,17),col="lightgrey",lty=3)
  abline(h=c(-100,-50,0,50,100,150,200,250),col="lightgrey",lty=3)
  abline(h=0)
  legend("topleft",title="Precipitation",legend="",bty="n",cex=1.3)

  dev.off()


  pdf(paste0("plots/fall_anomalies.pdf"),width=12,height=8)

  par(mfrow=c(2,2))
  par(mar=c(2,3.5,1,1),mgp=c(1.7,0.6,0),oma=c(0,0,0,0),cex.lab=1.4,cex.axis=1.25)

  boxplot(T_all$SON ~ Y_all, ylim=c(-3,4), ylab=expression(paste("Fall water temperature anomaly (",degree,"C)")))
  abline(h=c(-3:4),col="lightgrey",lty=3)
  abline(v=c(2,7,12,17),col="lightgrey",lty=3)
  abline(h=0)
  legend("topleft",title="Water temperature",legend="",bty="n",cex=1.3)

  boxplot(TA_all$SON ~ Y_all, ylim=c(-3,4), ylab=expression(paste("Fall air temperature anomaly (",degree,"C)")))
  abline(h=c(-3:4),col="lightgrey",lty=3)
  abline(v=c(2,7,12,17),col="lightgrey",lty=3)
  abline(h=0)
  legend("topleft",title="Air temperature",legend="",bty="n",cex=1.3)

  boxplot(Q_all$SON ~ Y_all,ylim=c(-100,270),ylab="Fall discharge anomaly (%)")
  abline(v=c(2,7,12,17),col="lightgrey",lty=3)
  abline(h=c(-100,-50,0,50,100,150,200,250),col="lightgrey",lty=3)
  abline(h=0)
  legend("topleft",title="Discharge",legend="",bty="n",cex=1.3)

  boxplot(P_all$SON ~ Y_all,ylim=c(-100,270),ylab="Fall precipitaion anomaly (%)")
  abline(v=c(2,7,12,17),col="lightgrey",lty=3)
  abline(h=c(-100,-50,0,50,100,150,200,250),col="lightgrey",lty=3)
  abline(h=0)
  legend("topleft",title="Precipitation",legend="",bty="n",cex=1.3)

  dev.off()
}
