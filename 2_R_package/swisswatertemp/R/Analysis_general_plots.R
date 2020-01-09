# This script is related to the paper:
# "Stream temperature evolution in Switzerland over the last 50 years, Adrien
# Michel, Tristan Brauchli, Michael Lehning, Bettina Schaefli, and Hendrik Huwald,
# HESS, 2019"

# This material is distributed under the GPLv3 license
# (https://www.gnu.org/licenses/gpl-3.0.html)

#' Produce general T and Q plot and variance plot
#'
#' This function produces general T and Q plot (all the catchments, Figures 2 and 3).
#' The plot for T also contains a lower pannel showing the decadan anomalies (Figure 2).
#' This function also produces the plot of the evolution of the infra-annual
#' variability (Figure S34 in supplementary). Plots are written in the 'plots' directory.
#' The plots are saved in plots/general_plot.pdf, plots/general_plot_Q.pdf,
#' and plots/annual_var.pdf
#'
#' @param rivers_data The dataset of rivers data
#'
#' @export
plot_general<-function(rivers_data)
{
  #Moving window average function
  ma <- function(x,n=5){filter(x,rep(1/n,n), sides=2)}

  ###############
  #### T PLOT ###
  ###############

  pdf(paste0("plots/general_plot.pdf"),width=12,height=9)
  riverSet=names(rivers_data)
  par(mfrow=c(1,1),oma=c(1,5,0.5,1),mar=c(3,0,2,1),mgp=c(4,0.8,0),cex.lab=1.5, cex.axis=1.5)
  layout(matrix(c(1,1,1,1,2,3,3,3,3,4), ncol=5, byrow=TRUE),heights=c(3,1.5))
  plot(rivers_data[[1]]$yearly$T$timestamp,rivers_data[[1]]$yearly$T$values,type="n",ylim=c(2,14),xlim=c(1964,2018),
       xlab="",ylab="")
  rect(1987,0,1989,15,border=NA,col="lightgrey")
  mtext(side=2,text=expression(paste("Mean annual water temeprature (",degree,"C)")),outer=FALSE,font=1,line=2.7,cex=1.2)
  mtext(side=1,text="Year",outer=FALSE,font=1,line=2.7,cex=1.2)
  i=1
  n <- length(riverSet)

  # Create colors tables
  HYP=0
  SPJ=0
  DSL=0
  ALP=0
  for(river in riverSet){
    if(rivers_data[[river]]$header$regime2=="Strong influence of Hydropeaking"){HYP=HYP+1}
    else if(rivers_data[[river]]$header$regime2=="Regime from Plateau and Jura"){SPJ=SPJ+1}
    else if(rivers_data[[river]]$header$regime2=="After lakes"){DSL=DSL+1}
    else if(rivers_data[[river]]$header$regime2=="Alpine regime"){ALP=ALP+1}
  }
  col_ALP=colorRampPalette(brewer.pal(9,"Greys"))(ALP+2)[2:(2+SPJ)]
  col_DSL=colorRampPalette(brewer.pal(9,"Greens"))(DSL+10)[10:(10+DSL)]
  col_HYP=colorRampPalette(brewer.pal(9,"Blues"))(HYP+5)[5:(5+HYP)]
  col_SPJ=colorRampPalette(brewer.pal(9,"Reds"))(SPJ+5)[5:(5+SPJ)]

  # PLot individual rivers
  HYP=0
  SPJ=0
  DSL=0
  ALP=0
  rivers_abbr=c()
  leg_col=c()
  value=c()
  for(river in riverSet){
    reg=""
    if(rivers_data[[river]]$header$regime2=="Strong influence of Hydropeaking"){
      HYP=HYP+1;col=col_HYP[HYP]
      reg="HYP"
      }
    else if(rivers_data[[river]]$header$regime2=="Regime from Plateau and Jura"){
      SPJ=SPJ+1;col=col_SPJ[SPJ]
      reg="SPJ"
      }
    else if(rivers_data[[river]]$header$regime2=="After lakes"){
      DSL=DSL+1;col=col_DSL[DSL]
      reg="DLA"}
    else if(rivers_data[[river]]$header$regime2=="Alpine regime"){
      ALP=ALP+1;col=col_ALP[ALP]
      reg="ALP"}

    leg_col=c(leg_col,col)
    rivers_abbr=c(rivers_abbr,rivers_data[[river]]$header$abbr)
    value=c(value,ma(rivers_data[[river]]$yearly$T$values,5)[length(ma(rivers_data[[river]]$yearly$T$values,5))-2])
    points(rivers_data[[river]]$yearly$T$timestamp,rivers_data[[river]]$yearly$T$values,col=col,pch=20)
    lines(rivers_data[[river]]$yearly$T$timestamp,ma(rivers_data[[river]]$yearly$T$values,5),col=col)
    i=i+1
  }
  grid()
  plot.new()
  leg_value=data.frame(leg_col,rivers_abbr,value)
  leg_value$leg_col=as.character(leg_value$leg_col)
  leg_value=leg_value[with(leg_value, order(value,decreasing = TRUE)), ]
  legend("top",legend=leg_value$rivers_abbr,col=leg_value$leg_col,
         lty=1,bty="n",ncol=2,cex=1.2,title="Water station abbreviation")
  legend("bottom",legend=c("DLA","ALP","SPJ","HYP"),col=c(col_DSL[10],col_ALP[3],col_SPJ[10],col_HYP[8]),
         lty=1,bty="n",ncol=2,cex=1.2,title="Regime")

  # Anomalies plot
  all_mean_T=list()
  all_mean_T$yearly=list()
  for (season in c("DJF","MAM","JJA","SON"))
  {
    all_mean_T[[season]]=list()
  }
  for (river_station in names(rivers_data))
  {
    T=rivers_data[[river_station]]$STL$T
    if(T$timestamp[1]>1970){next}
    meant=c()
    yrs=c()
    periods=c(1970,1980,1990,2000,2010)
    dec_means=list()
    mean_period=mean(T$raw[which(T$timestamp>1970)],na.rm=TRUE)

    for (p in periods)
    {
      daily_means_temp_T=matrix(NA, nrow=365,ncol=10)
      for(j in c(1:10))
      {
        if(T$timestamp[length(T$timestamp)]<p+j-1){next}
        year_mean=T$raw[which(floor(T$timestamp)==p+j-1)]
        daily_means_temp_T[,j]=year_mean-mean_period
      }

      dec_means=rowMeans(daily_means_temp_T,na.rm = TRUE)
      meant=c(meant,mean(dec_means))
    }
    all_mean_T$yearly$T=c(all_mean_T$yearly$T,meant)
    all_mean_T$yearly$decade=c(all_mean_T$yearly$decade,periods)

    for (season in c("DJF","MAM","JJA","SON"))
    {
      T=rivers_data[[river_station]][[season]]$T
      if(T$timestamp[1]>1970){next}
      meant=c()
      yrs=c()
      periods=c(1970,1980,1990,2000,2010)
      dec_means=list()
      mean_period=mean(T$values[which(T$timestamp>1970)],na.rm=TRUE)
      for (p in periods)
      {
        daily_means_T=matrix(NA, nrow=365,ncol=10)
        for(j in c(1:10))
        {
          if(T$timestamp[length(T$timestamp)]<p+j-1){next}
          if(T$timestamp[1]>p+j-1){next}
          year_mean=T$values[which(floor(T$timestamp)==p+j-1)]
          daily_means_temp_T[,j]=year_mean-mean_period
        }
        dec_means=rowMeans(daily_means_temp_T,na.rm = TRUE)
        meant=c(meant,mean(dec_means))
      }
      all_mean_T[[season]]$T=c(all_mean_T[[season]]$T,meant)
      all_mean_T[[season]]$decade=c(all_mean_T[[season]]$decade,periods)
    }
  }

  par(mar=c(4,0,2,1),mgp=c(2,1,0))
  boxplot(all_mean_T$yearly$T ~ all_mean_T$yearly$decade,xlab="",ylab="")
  mtext(side=2,text=expression(paste("Water temeprature anomaly (",degree,"C)")),outer=FALSE,font=1,line=2.7,cex=1.2)
  mtext(side=1,text="Decade (starting year)",outer=FALSE,font=1,line=2.7,cex=1.2)
  means <- aggregate(all_mean_T$yearly$T ,
                     by = list(all_mean_T$yearly$decade),
                     FUN = mean,  na.rm=TRUE)

  points(1:5, means$x, col = "red",pch=16)

  dev.off()

  ###############
  #### Q PLOT ###
  ###############

  pdf(paste0("plots/general_plot_q.pdf"),width=12,height=6)
  riverSet=names(rivers_data)
  par(mfrow=c(1,1),oma=c(1,5,0.5,1),mar=c(4,0,2,1),mgp=c(4,0.8,0),cex.lab=1.5, cex.axis=1.5)
  layout(matrix(c(1,1,1,1,2), ncol=5, byrow=TRUE))
  # Create colors tables
  HYP=0
  SPJ=0
  DSL=0
  ALP=0
  for(river in riverSet){
    if(rivers_data[[river]]$header$regime2=="Strong influence of Hydropeaking"){HYP=HYP+1}
    else if(rivers_data[[river]]$header$regime2=="Regime from Plateau and Jura"){SPJ=SPJ+1}
    else if(rivers_data[[river]]$header$regime2=="After lakes"){DSL=DSL+1}
    else if(rivers_data[[river]]$header$regime2=="Alpine regime"){ALP=ALP+1}
  }

  col_ALP=colorRampPalette(brewer.pal(9,"Greys"))(ALP+2)[2:(2+SPJ)]
  col_DSL=colorRampPalette(brewer.pal(9,"Greens"))(DSL+10)[10:(10+DSL)]
  col_HYP=colorRampPalette(brewer.pal(9,"Blues"))(HYP+5)[5:(5+HYP)]
  col_SPJ=colorRampPalette(brewer.pal(9,"Reds"))(SPJ+5)[5:(5+SPJ)]

  plot(rivers_data[[1]]$yearly$Q$timestamp,rivers_data[[1]]$yearly$Q$values/
         as.numeric(rivers_data[[1]]$header$area),type="n",ylim=c(0,0.09),xlim=c(1900,2018),xlab="",ylab="")
  mtext(side=2,text=expression(paste("Mean annual specific discharge (m"^"3","s"^"-1","km"^"-2",")")),
        outer=FALSE,font=1,line=2.7,cex=1.2)
  mtext(side=1,text="Year",outer=FALSE,font=1,line=2.7,cex=1.3)

  # PLot individual rivers
  value=c()
  rivers_abbr=c()
  leg_col=c()
  i=1
  HYP=0
  SPJ=0
  DSL=0
  ALP=0
  for(river in riverSet){
    if(rivers_data[[river]]$header$regime2=="Strong influence of Hydropeaking"){HYP=HYP+1;col=col_HYP[HYP]}
    else if(rivers_data[[river]]$header$regime2=="Regime from Plateau and Jura"){SPJ=SPJ+1;col=col_SPJ[SPJ]}
    else if(rivers_data[[river]]$header$regime2=="After lakes"){DSL=DSL+1;col=col_DSL[DSL]}
    else if(rivers_data[[river]]$header$regime2=="Alpine regime"){ALP=ALP+1;col=col_ALP[ALP]}


    if(  rivers_data[[river]]$header$abbr=="Aar-Lys")
    {
      ma_ts=ma(rivers_data[[river]]$yearly$Q$values/
           as.numeric(rivers_data[[river]]$header$area)
         ,n=5)/4
      leg_col=c(leg_col,col)
      rivers_abbr=c(rivers_abbr,paste0(rivers_data[[river]]$header$abbr,"*"))
      value=c(value,ma_ts[length(ma_ts)-2])

      points(rivers_data[[river]]$yearly$Q$timestamp,rivers_data[[river]]$yearly$Q$values/
               as.numeric(rivers_data[[river]]$header$area)/4
             ,col=col,pch=20)
      lines(rivers_data[[river]]$yearly$Q$timestamp,ma_ts,col=col)
      i=i+1
    }
    else
    {
      ma_ts=ma(rivers_data[[river]]$yearly$Q$values/
                 as.numeric(rivers_data[[river]]$header$area)
               ,n=5)
      leg_col=c(leg_col,col)
      rivers_abbr=c(rivers_abbr,rivers_data[[river]]$header$abbr)
      value=c(value,ma_ts[length(ma_ts)-2])

      points(rivers_data[[river]]$yearly$Q$timestamp,rivers_data[[river]]$yearly$Q$values/
               as.numeric(rivers_data[[river]]$header$area)
             ,col=col,pch=20)
      lines(rivers_data[[river]]$yearly$Q$timestamp,ma_ts,col=col)
      i=i+1
    }
  }

  grid()
  plot.new()
  leg_value=data.frame(leg_col,rivers_abbr,value)
  leg_value$leg_col=as.character(leg_value$leg_col)
  leg_value=leg_value[with(leg_value, order(value,decreasing = TRUE)), ]

  leg_value
  legend("top",legend=leg_value$rivers_abbr,col=leg_value$leg_col,
         lty=1,bty="n",ncol=2,cex=1.2,title="Water station abbreviation")
  legend("bottom",legend=c("DLA","ALP","SPJ","HYP"),col=c(col_DSL[10],col_ALP[3],col_SPJ[10],col_HYP[8]),
         lty=1,bty="n",ncol=2,cex=1.2,title="Regime")
  dev.off()

  #################
  #### VAR PLOT ###
  #################

  pdf(paste0("plots/annual_var.pdf"),width=12,height=5)

  # Function to compute the time series of infra annual variability
  annual_diff <- function(station)
  {
    HYP=0
    SPJ=0
    DSL=0
    ALP=0
    for(river in names(rivers_data)){
      if(rivers_data[[river]]$header$regime2=="Strong influence of Hydropeaking"){HYP=HYP+1}
      else if(rivers_data[[river]]$header$regime2=="Regime from Plateau and Jura"){SPJ=SPJ+1}
      else if(rivers_data[[river]]$header$regime2=="After lakes"){DSL=DSL+1}
      else if(rivers_data[[river]]$header$regime2=="Alpine regime"){ALP=ALP+1}
    }

    col_ALP=colorRampPalette(brewer.pal(9,"Greys"))(ALP+2)[2:(2+SPJ)]
    col_DSL=colorRampPalette(brewer.pal(9,"Greens"))(DSL+10)[10:(10+DSL)]
    col_HYP=colorRampPalette(brewer.pal(9,"Blues"))(HYP+5)[5:(5+HYP)]
    col_SPJ=colorRampPalette(brewer.pal(9,"Reds"))(SPJ+5)[5:(5+SPJ)]
    col=0
    if(station$header$regime2=="Strong influence of Hydropeaking"){HYP=HYP+1;col=col_HYP[8]}
    else if(station$header$regime2=="Regime from Plateau and Jura"){SPJ=SPJ+1;col=col_SPJ[10]}
    else if(station$header$regime2=="After lakes"){DSL=DSL+1;col=col_DSL[10]}
    else if(station$header$regime2=="Alpine regime"){ALP=ALP+1;col=col_ALP[3]}

    t1=station$DJF$T$values[which(station$DJF$T$timestamp>=1981)]
    t2=station$JJA$T$values[which(station$DJF$T$timestamp>=1981)]
    return(cbind(station$DJF$T$timestamp[which(station$DJF$T$timestamp>=1981)],t2-t1,col))
  }

  # Compute infra annual difference for each river
  annual_diff=lapply(rivers_data,annual_diff)

  par(mfrow=c(1,1),oma=c(0,0,0,0),mar=c(3,3,1,1),mgp=c(1.7,0.5,0))

  # Put all data into one vector per variable of interest
  annual_diff
  years=c()
  diff=c()
  diff_ma=c()
  col=c()
  raw_diff=c()
  for(d in annual_diff)
  {
    if(d[1,1]==1981){
      years=c(years,as.numeric(d[,1]))
      diff_ma=c(diff_ma,ma(as.numeric(d[,2])-mean(as.numeric(d[,2]))))
      diff=c(diff,(as.numeric(d[,2])-mean(as.numeric(d[,2]))))

      raw_diff=c(raw_diff,(as.numeric(d[,2])))

      col=c(col,d[,3])
    }
  }

  # Do plot
  df=data.frame(years,diff)
  diff_mean=aggregate( df , by=list(years) , mean,na.rm=TRUE)
  lm=lm(diff_mean$diff ~ diff_mean$years)
  plot(years,diff,col=col,pch=16, ylab=expression(paste("Water temperature difference anomaly (",degree,"C)")),xlab="Year",cex=1.2)
  grid()
  lines(diff_mean$years,lm$coefficients[1]+lm$coefficients[2]*diff_mean$years)
  legend("topleft",legend=c("DLA","ALP","SPJ","HYP","Linear regression"),
         col=c("#0D58A1","#969696","#2A924A","#FB6A4A",1),lty=c(NA,NA,NA,NA,1),
         pch=c(16,16,16,16,NA),bty="n",cex=1.2)

  dev.off()
}
