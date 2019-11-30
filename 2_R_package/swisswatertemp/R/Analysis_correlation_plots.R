# This script is related to the paper:
# "Stream temperature evolution in Switzerland over the last 50 years, Adrien
# Michel, Tristan Brauchli, Michael Lehning, Bettina Schaefli, and Hendrik Huwald,
# HESS, 2019"

# This material is distributed under the GPLv3 license
# (https://www.gnu.org/licenses/gpl-3.0.html)

#' Print correlation matrices
#'
#' This function prints to the console the correlations matrices shown in
#' Tables 4 and 5 and in Table S5 in supplementary.
#'
#' Some additional plots nor present in the paper are also produced and
#' saveud under plots/correlations_plots.pdf
#'
#' @param rivers_data The dataset of rivers data
#' @export
plot_correlations<-function(rivers_data)
{
  pdf(paste0("plots/correlations_plots.pdf"),width=12,height=9)

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

  myImagePlot <- function(x, ...){
    min <- -1
    max <- 1

    yLabels <- rownames(x)
    xLabels <- colnames(x)
    ylab=NULL
    title <-c()
    # check for additional function arguments
    if( length(list(...)) ){
      Lst <- list(...)
      if( !is.null(Lst$zlim) ){
        min <- Lst$zlim[1]
        max <- Lst$zlim[2]
      }
      if( !is.null(Lst$yLabels) ){
        yLabels <- c(Lst$yLabels)
      }
      if( !is.null(Lst$xLabels) ){
        xLabels <- c(Lst$xLabels)
      }
      if( !is.null(Lst$title) ){
        title <- Lst$title
      }
      if( !is.null(Lst$ylab) ){
        ylab <- Lst$ylab
      }
    }
    # check for null values
    if( is.null(xLabels) ){
      xLabels <- c(1:ncol(x))
    }
    if( is.null(yLabels) ){
      yLabels <- c(1:nrow(x))
    }

    if( is.null(ylab) ){
      ylab =""
    }
    # Red and green range from 0 to 1 while Blue ranges from 1 to 0
    #
    ColorLevels <- seq(min, max, length=256)
    ColorRamp <-  colorRampPalette(brewer.pal(11,"PRGn"))(100)
    # Reverse Y axis
    reverse <- nrow(x) : 1
    yLabels <- yLabels[reverse]
    x <- x[reverse,]

    # Data Map
    par(mar = c(2,3,2,0))
    image(1:length(xLabels), 1:length(yLabels), t(x), col=ColorRamp, xlab="",
          ylab="", axes=FALSE, zlim=c(min,max))
    if( !is.null(title) ){
      title(main=title)
    }
    axis(BELOW<-1, at=1:length(xLabels), labels=xLabels, cex.axis=0.7)
    axis(LEFT <-2, at=1:length(yLabels), labels=yLabels, las= HORIZONTAL<-1,
         cex.axis=0.7)

    # Color Scale
    par(mar = c(2,3,2,1),mgp=c(1.7,0.5,0))
    image(1, ColorLevels,
          matrix(data=ColorLevels, ncol=length(ColorLevels),nrow=1),
          col=ColorRamp,
          xlab="",ylab=ylab,
          xaxt="n")
  }

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


  t_raw=list()
  ta_raw=list()
  p_raw=list()
  q_raw=list()

  t_trend=list()
  ta_trend=list()
  p_trend=list()
  q_trend=list()
  riv_abbr=c()
  col=c()
  regimes=c()
  periods=c("yearly","MAM","JJA","SON","DJF")
  for (p in periods)
  {
    t_trend[[p]]=c()
    ta_trend[[p]]=c()
    p_trend[[p]]=c()
    q_trend[[p]]=c()
  }
  for (river_station in names(rivers_data))
  {

    if(rivers_data[[river_station]]$header$regime2=="Strong influence of Hydropeaking"){col=c(col,col_HYP[8])}
    else if(rivers_data[[river_station]]$header$regime2=="Regime from Plateau and Jura"){col=c(col,col_SPJ[10])}
    else if(rivers_data[[river_station]]$header$regime2=="After lakes"){col=c(col,col_DSL[10])}
    else if(rivers_data[[river_station]]$header$regime2=="Alpine regime"){col=c(col,col_ALP[3])}

    riv_abbr=c(riv_abbr,rivers_data[[river_station]]$header$abbr)

    t_raw[[river_station]]=list()
    ta_raw[[river_station]]=list()
    p_raw[[river_station]]=list()
    q_raw[[river_station]]=list()
    for (p in periods)
    {
      t_trend[[p]]=c(t_trend[[p]],rivers_data[[river_station]][[p]]$T$lm$`1999-2018`$trend)
      q_trend[[p]]=c(q_trend[[p]],rivers_data[[river_station]][[p]]$Q$lm$`1999-2018`$trend/
                       mean(rivers_data[[river_station]][[p]]$Q$lm$`1999-2018`$values,na.rm=TRUE)*10)

      t_raw[[river_station]][[p]]=rivers_data[[river_station]][[p]]$T$lm$`1999-2018`$values
      q_raw[[river_station]][[p]]=rivers_data[[river_station]][[p]]$Q$lm$`1999-2018`$values

      ta_temp=0
      p_temp=0
      ta_temp_raw=0
      p_temp_raw=0
      for(station in rivers_data[[river_station]]$meteo){
        ta_temp=ta_temp+station[[p]]$T$lm$`1999-2018`$trend
        p_temp=station[[p]]$P$lm$`1999-2018`$trend/
                    mean(station[[p]]$P$lm$`1999-2018`$values,na.rm=TRUE)*10
        ta_temp_raw=ta_temp_raw+station[[p]]$T$lm$`1999-2018`$values
        p_temp_raw=p_temp_raw+station[[p]]$P$lm$`1999-2018`$values
      }
      ta_temp=ta_temp/length(names(rivers_data[[river_station]]$meteo))
      ta_trend[[p]]=c(ta_trend[[p]],ta_temp)
      p_temp=p_temp/length(names(rivers_data[[river_station]]$meteo))
      p_trend[[p]]=c(p_trend[[p]],p_temp)

      ta_raw[[river_station]][[p]]=ta_temp_raw/length(names(rivers_data[[river_station]]$meteo))
      p_raw[[river_station]][[p]]=p_temp_raw/length(names(rivers_data[[river_station]]$meteo))

    }
    regimes=c(regimes,rivers_data[[river_station]]$header$regime2)
  }
  station[[p]]$P$va
  rivers_data[[river_station]][[p]]$Q

  t_to_ta_trends=c(0,0,0,0,0)
  t_to_ta_trends_sig=c(0,0,0,0,0)
  for (i in c(1:length(t_trend))){
      t_to_ta_trends[i]=cor(t_trend[[i]],ta_trend[[i]],method="spearman")
      t_to_ta_trends_sig[i]=rcorr(t_trend[[i]],ta_trend[[i]],"spearman")[[3]][1,2]
  }

  round(t_to_ta_trends,2)

  round(t_to_ta_trends_sig,2)




  t_to_q_trends=c(0,0,0,0,0)
  t_to_q_trends_sig=c(0,0,0,0,0)
  for (i in c(1:length(t_trend))){
      t_to_q_trends[i]=cor(t_trend[[i]],q_trend[[i]],method="spearman")
      t_to_q_trends_sig[i]=rcorr(t_trend[[i]],q_trend[[i]],"spearman")[[3]][1,2]
  }
  round(t_to_q_trends,2)
  round(t_to_q_trends_sig,2)
  t_to_q_trends_sig

  q_to_p_trends=c(0,0,0,0,0)
  q_to_p_trends_sig=c(0,0,0,0,0)
  for (i in c(1:length(q_trend))){
      q_to_p_trends[i]=cor(q_trend[[i]],p_trend[[i]],method="spearman")
      q_to_p_trends_sig[i]=rcorr(q_trend[[i]],p_trend[[i]],"spearman")[[3]][1,2]
  }

  round(q_to_p_trends,2)
  round(q_to_p_trends_sig,2)

  par(mfrow=c(2,3))

  cors=c()
  pvalues=c()
  for (k in c(1:length(t_raw))){
    while(length(ta_raw[[k]]$yearly)>length(t_raw[[k]]$yearly)){
      t_raw[[k]]$yearly=c(t_raw[[k]]$yearly,NA)
    }
    cors=c(cors,cor(t_raw[[k]]$yearly,ta_raw[[k]]$yearly,method="spearman",use="complete"))
    pvalues=c(pvalues,rcorr(t_raw[[k]]$yearly,ta_raw[[k]]$yearly,"spearman")[[3]][1,2])
  }
  plot(cors,pch= as.numeric(pvalues>0.05)*2+16,xaxt='n',xlab="",main="yearly t ta",col=col)
  axis(side=1,at=c(1:length(names(rivers_data))), labels=riv_abbr,las=2,cex.axis=0.5)
  abline(v=c(1:length(names(rivers_data))),col=8,lty=3)
  print(paste("yearly t ta:",mean(cors)))
  print(paste("yearly t ta:",min(pvalues),length(which(pvalues>0.05))))

  cors=c()
  pvalues=c()
  for (k in c(1:length(t_raw))){
      while(length(ta_raw[[k]]$DJF)>length(t_raw[[k]]$DJF)){
        t_raw[[k]]$DJF=c(t_raw[[k]]$DJF,NA)
      }
    cors=c(cors,cor(t_raw[[k]]$DJF,ta_raw[[k]]$DJF,method="spearman",use="complete"))
    pvalues=c(pvalues,rcorr(t_raw[[k]]$DJF,ta_raw[[k]]$DJF,"spearman")[[3]][1,2])
  }
  plot(cors,pch= as.numeric(pvalues>0.05)*2+16,xaxt='n',xlab="",main="DJF t ta",col=col)
  axis(side=1,at=c(1:length(names(rivers_data))), labels=riv_abbr,las=2,cex.axis=0.5)
  abline(v=c(1:length(names(rivers_data))),col=8,lty=3)
  print(paste("DJF t ta:",mean(cors)))
  print(paste("DJF t ta:",min(pvalues),length(which(pvalues>0.05))))

  cors=c()
  pvalues=c()
  for (k in c(1:length(t_raw))){
    while(length(ta_raw[[k]]$MAM)>length(t_raw[[k]]$MAM)){
      t_raw[[k]]$MAM=c(t_raw[[k]]$MAM,NA)
    }
    cors=c(cors,cor(t_raw[[k]]$MAM,ta_raw[[k]]$MAM,method="spearman",use="complete"))
    pvalues=c(pvalues,rcorr(t_raw[[k]]$MAM,ta_raw[[k]]$MAM,"spearman")[[3]][1,2])
  }
  plot(cors,pch= as.numeric(pvalues>0.05)*2+16,xaxt='n',xlab="",main="MAM t ta",col=col)
  axis(side=1,at=c(1:length(names(rivers_data))), labels=riv_abbr,las=2,cex.axis=0.5)
  abline(v=c(1:length(names(rivers_data))),col=8,lty=3)
  print(paste("MAM t ta:",mean(cors)))
  print(paste("MAM t ta:",min(pvalues),length(which(pvalues>0.05))))


  cors=c()
  pvalues=c()
  for (k in c(1:length(t_raw))){
    while(length(ta_raw[[k]]$JJA)>length(t_raw[[k]]$JJA)){
      t_raw[[k]]$JJA=c(t_raw[[k]]$JJA,NA)
    }
    cors=c(cors,cor(t_raw[[k]]$JJA,ta_raw[[k]]$JJA,method="spearman",use="complete"))
    pvalues=c(pvalues,rcorr(t_raw[[k]]$JJA,ta_raw[[k]]$JJA,"spearman")[[3]][1,2])
  }
  plot(cors,pch= as.numeric(pvalues>0.05)*2+16,xaxt='n',xlab="",main="JJA t ta",col=col)
  axis(side=1,at=c(1:length(names(rivers_data))), labels=riv_abbr,las=2,cex.axis=0.5)
  abline(v=c(1:length(names(rivers_data))),col=8,lty=3)
  print(paste("JJA t ta:",mean(cors)))
  print(paste("JJA t ta:",min(pvalues),length(which(pvalues>0.05))))

  cors=c()
  pvalues=c()
  for (k in c(1:length(t_raw))){
    while(length(ta_raw[[k]]$SON)>length(t_raw[[k]]$SON)){
      t_raw[[k]]$SON=c(t_raw[[k]]$SON,NA)
    }
    cors=c(cors,cor(t_raw[[k]]$SON,ta_raw[[k]]$SON,method="spearman",use="complete"))
    pvalues=c(pvalues,rcorr(t_raw[[k]]$SON,ta_raw[[k]]$SON,"spearman")[[3]][1,2])
  }
  plot(cors,pch= as.numeric(pvalues>0.05)*2+16,xaxt='n',xlab="",main="SON t ta",col=col)
  axis(side=1,at=c(1:length(names(rivers_data))), labels=riv_abbr,las=2,cex.axis=0.5)
  abline(v=c(1:length(names(rivers_data))),col=8,lty=3)
  print(paste("SON t ta:",mean(cors)))
  print(paste("SON t ta:",min(pvalues),length(which(pvalues>0.05))))


  plot.new()
  cors=c()
  pvalues=c()
  for (k in c(1:length(t_raw))){
    while(length(q_raw[[k]]$yearly)>length(t_raw[[k]]$yearly)){
      t_raw[[k]]$yearly=c(t_raw[[k]]$yearly,NA)
    }
    while(length(t_raw[[k]]$yearly)>length(q_raw[[k]]$yearly)){
      q_raw[[k]]$yearly=c(q_raw[[k]]$yearly,NA)
    }
    cors=c(cors,cor(t_raw[[k]]$yearly,q_raw[[k]]$yearly,method="spearman",use="complete"))
    pvalues=c(pvalues,rcorr(t_raw[[k]]$yearly,q_raw[[k]]$yearly,"spearman")[[3]][1,2])
  }
  print(paste("yearly t q:",mean(cors)))
  print(paste("yearly t q:",min(pvalues),length(which(pvalues>0.05))))
  plot(cors,pch= as.numeric(pvalues>0.05)*2+16,xaxt='n',xlab="",main="yearly t q",col=col)
  axis(side=1,at=c(1:length(names(rivers_data))), labels=riv_abbr,las=2,cex.axis=0.5)
  abline(v=c(1:length(names(rivers_data))),col=8,lty=3)

  cors=c()
  pvalues=c()
  for (k in c(1:length(t_raw))){
    while(length(q_raw[[k]]$DJF)>length(t_raw[[k]]$DJF)){
      t_raw[[k]]$DJF=c(t_raw[[k]]$DJF,NA)
    }
    while(length(t_raw[[k]]$DJF)>length(q_raw[[k]]$DJF)){
      q_raw[[k]]$DJF=c(q_raw[[k]]$DJF,NA)
    }
    cors=c(cors,cor(t_raw[[k]]$DJF,q_raw[[k]]$DJF,method="spearman",use="complete"))
    pvalues=c(pvalues,rcorr(t_raw[[k]]$DJF,q_raw[[k]]$DJF,"spearman")[[3]][1,2])
  }
  print(paste("DJF t q:",mean(cors)))
  print(paste("DJF t q:",min(pvalues),length(which(pvalues>0.05))))
  plot(cors,pch= as.numeric(pvalues>0.05)*2+16,xaxt='n',xlab="",main="DJF t q",col=col)
  axis(side=1,at=c(1:length(names(rivers_data))), labels=riv_abbr,las=2,cex.axis=0.5)
  abline(v=c(1:length(names(rivers_data))),col=8,lty=3)

  cors=c()
  pvalues=c()
  for (k in c(1:length(t_raw))){
    while(length(q_raw[[k]]$MAM)>length(t_raw[[k]]$MAM)){
      t_raw[[k]]$MAM=c(t_raw[[k]]$MAM,NA)
    }
    while(length(t_raw[[k]]$MAM)>length(q_raw[[k]]$MAM)){
      q_raw[[k]]$MAM=c(q_raw[[k]]$MAM,NA)
    }
    cors=c(cors,cor(t_raw[[k]]$MAM,q_raw[[k]]$MAM,method="spearman",use="complete"))
    pvalues=c(pvalues,rcorr(t_raw[[k]]$MAM,q_raw[[k]]$MAM,"spearman")[[3]][1,2])
  }
  print(paste("MAM t q:",mean(cors)))
  print(paste("MAM t q:",min(pvalues),length(which(pvalues>0.05))))
  plot(cors,pch= as.numeric(pvalues>0.05)*2+16,xaxt='n',xlab="",main="MAM t q",col=col)
  axis(side=1,at=c(1:length(names(rivers_data))), labels=riv_abbr,las=2,cex.axis=0.5)
  abline(v=c(1:length(names(rivers_data))),col=8,lty=3)

  cors=c()
  pvalues=c()
  for (k in c(1:length(t_raw))){
    while(length(q_raw[[k]]$JJA)>length(t_raw[[k]]$JJA)){
      t_raw[[k]]$JJA=c(t_raw[[k]]$JJA,NA)
    }
    while(length(t_raw[[k]]$JJA)>length(q_raw[[k]]$JJA)){
      q_raw[[k]]$JJA=c(q_raw[[k]]$JJA,NA)
    }
    cors=c(cors,cor(t_raw[[k]]$JJA,q_raw[[k]]$JJA,method="spearman",use="complete"))
    pvalues=c(pvalues,rcorr(t_raw[[k]]$JJA,q_raw[[k]]$JJA,"spearman")[[3]][1,2])
  }
  print(paste("JJA t q:",mean(cors)))
  print(paste("JJA t q:",min(pvalues),length(which(pvalues>0.05))))
  plot(cors,pch= as.numeric(pvalues>0.05)*2+16,xaxt='n',xlab="",main="JJA t q",col=col)
  axis(side=1,at=c(1:length(names(rivers_data))), labels=riv_abbr,las=2,cex.axis=0.5)
  abline(v=c(1:length(names(rivers_data))),col=8,lty=3)

  cors=c()
  pvalues=c()
  for (k in c(1:length(t_raw))){
    while(length(q_raw[[k]]$SON)>length(t_raw[[k]]$SON)){
      t_raw[[k]]$SON=c(t_raw[[k]]$SON,NA)
    }
    while(length(t_raw[[k]]$SON)>length(q_raw[[k]]$SON)){
      q_raw[[k]]$SON=c(q_raw[[k]]$SON,NA)
    }
    cors=c(cors,cor(t_raw[[k]]$SON,q_raw[[k]]$SON,method="spearman",use="complete"))
    pvalues=c(pvalues,rcorr(t_raw[[k]]$SON,q_raw[[k]]$SON,"spearman")[[3]][1,2])
  }
  print(paste("SON t q:",mean(cors)))
  print(paste("SON t q:",min(pvalues),length(which(pvalues>0.05))))
  plot(cors,pch= as.numeric(pvalues>0.05)*2+16,xaxt='n',xlab="",main="SON t q",col=col)
  axis(side=1,at=c(1:length(names(rivers_data))), labels=riv_abbr,las=2,cex.axis=0.5)
  abline(v=c(1:length(names(rivers_data))),col=8,lty=3)


  plot.new()
  cors=c()
  pvalues=c()
  for (k in c(1:length(t_raw))){
    while(length(q_raw[[k]]$yearly)>length(p_raw[[k]]$yearly)){
      p_raw[[k]]$yearly=c(t_raw[[k]]$yearly,NA)
    }
    while(length(p_raw[[k]]$yearly)>length(q_raw[[k]]$yearly)){
      q_raw[[k]]$yearly=c(q_raw[[k]]$yearly,NA)
    }
    cors=c(cors,cor(p_raw[[k]]$yearly,q_raw[[k]]$yearly,method="spearman",use="complete"))
    pvalues=c(pvalues,rcorr(p_raw[[k]]$yearly,q_raw[[k]]$yearly,"spearman")[[3]][1,2])
  }
  print(paste("yearly q p:",mean(cors)))
  print(paste("yearly q p:",min(pvalues),length(which(pvalues>0.05))))
  plot(cors,pch= as.numeric(pvalues>0.05)*2+16,xaxt='n',xlab="",main="Yearly q p",col=col)
  axis(side=1,at=c(1:length(names(rivers_data))), labels=riv_abbr,las=2,cex.axis=0.5)
  abline(v=c(1:length(names(rivers_data))),col=8,lty=3)

  cors=c()
  pvalues=c()
  for (k in c(1:length(q_raw))){
    while(length(q_raw[[k]]$DJF)>length(p_raw[[k]]$DJF)){
      p_raw[[k]]$DJF=c(p_raw[[k]]$DJF,NA)
    }
    while(length(p_raw[[k]]$DJF)>length(q_raw[[k]]$DJF)){
      q_raw[[k]]$DJF=c(q_raw[[k]]$DJF,NA)
    }
    cors=c(cors,cor(p_raw[[k]]$DJF,q_raw[[k]]$DJF,method="spearman",use="complete"))
    pvalues=c(pvalues,rcorr(p_raw[[k]]$DJF,q_raw[[k]]$DJF,"spearman")[[3]][1,2])
  }
  print(paste("DJF q p:",mean(cors)))
  print(paste("DJF q p:",min(pvalues),length(which(pvalues>0.05))))
  plot(cors,pch= as.numeric(pvalues>0.05)*2+16,xaxt='n',xlab="",main="DJF q p",col=col)
  axis(side=1,at=c(1:length(names(rivers_data))), labels=riv_abbr,las=2,cex.axis=0.5)
  abline(v=c(1:length(names(rivers_data))),col=8,lty=3)

  cors=c()
  pvalues=c()
  for (k in c(1:length(q_raw))){
    while(length(q_raw[[k]]$MAM)>length(p_raw[[k]]$MAM)){
      p_raw[[k]]$MAM=c(p_raw[[k]]$MAM,NA)
    }
    while(length(p_raw[[k]]$MAM)>length(q_raw[[k]]$MAM)){
      q_raw[[k]]$MAM=c(q_raw[[k]]$MAM,NA)
    }
    cors=c(cors,cor(p_raw[[k]]$MAM,q_raw[[k]]$MAM,method="spearman",use="complete"))
    pvalues=c(pvalues,rcorr(p_raw[[k]]$MAM,q_raw[[k]]$MAM,"spearman")[[3]][1,2])
  }
  print(paste("MAM q p:",mean(cors)))
  print(paste("MAM q p:",min(pvalues),length(which(pvalues>0.05))))
  plot(cors,pch= as.numeric(pvalues>0.05)*2+16,xaxt='n',xlab="",main="MAM q p",col=col)
  axis(side=1,at=c(1:length(names(rivers_data))), labels=riv_abbr,las=2,cex.axis=0.5)
  abline(v=c(1:length(names(rivers_data))),col=8,lty=3)

  cors=c()
  pvalues=c()
  for (k in c(1:length(q_raw))){
    while(length(q_raw[[k]]$JJA)>length(p_raw[[k]]$JJA)){
      p_raw[[k]]$JJA=c(p_raw[[k]]$JJA,NA)
    }
    while(length(p_raw[[k]]$JJA)>length(q_raw[[k]]$JJA)){
      q_raw[[k]]$JJA=c(q_raw[[k]]$JJA,NA)
    }
    cors=c(cors,cor(p_raw[[k]]$JJA,q_raw[[k]]$JJA,method="spearman",use="complete"))
    pvalues=c(pvalues,rcorr(p_raw[[k]]$JJA,q_raw[[k]]$JJA,"spearman")[[3]][1,2])
  }
  print(paste("JJA q p:",mean(cors)))
  print(paste("JJA q p:",min(pvalues),length(which(pvalues>0.05))))
  plot(cors,pch= as.numeric(pvalues>0.05)*2+16,xaxt='n',xlab="",main="JJA q p",col=col)
  axis(side=1,at=c(1:length(names(rivers_data))), labels=riv_abbr,las=2,cex.axis=0.5)
  abline(v=c(1:length(names(rivers_data))),col=8,lty=3)

  cors=c()
  pvalues=c()
  for (k in c(1:length(q_raw))){
    while(length(q_raw[[k]]$SON)>length(p_raw[[k]]$SON)){
      p_raw[[k]]$SON=c(p_raw[[k]]$SON,NA)
    }
    while(length(p_raw[[k]]$SON)>length(q_raw[[k]]$SON)){
      q_raw[[k]]$SON=c(q_raw[[k]]$SON,NA)
    }
    cors=c(cors,cor(p_raw[[k]]$SON,q_raw[[k]]$SON,method="spearman",use="complete"))
    pvalues=c(pvalues,rcorr(p_raw[[k]]$SON,q_raw[[k]]$SON,"spearman")[[3]][1,2])
  }
  plot(cors,pch= as.numeric(pvalues>0.05)*2+16,xaxt='n',xlab="",main="SON q p",col=col)
  axis(side=1,at=c(1:length(names(rivers_data))), labels=riv_abbr,las=2,cex.axis=0.5)
  abline(v=c(1:length(names(rivers_data))),col=8,lty=3)
  print(paste("SON q p:",mean(cors)))
  print(paste("SON q p:",min(pvalues),length(which(pvalues>0.05))))


  par(mfrow=c(1,1))



  dev.off()

  next_seas <- function(s1,s2)
  {
    if(s1==s2){return(TRUE)}
    else if(s1=="MAM" && s2=="DJF"){return(TRUE)}
    else if(s1=="JJA" && (s2=="DJF" || s2=="MAM")){return(TRUE)}
    else if(s1=="SON"){return(TRUE)}
    else{return(FALSE)}
  }

  print("T TO T")
  t_raw
  for (s1 in  c("DJF","MAM","JJA","SON"))
  {
    str=paste(s1,"&")
    for (s2 in c("DJF","MAM","JJA","SON"))
    {
      cors=c()
      pvalues=c()
      for (k in c(1:length(t_raw))){
        a=t_raw[[k]][[s1]]
        b=t_raw[[k]][[s2]]
        if(next_seas(s1,s2)){
          cors=c(cors,cor(a[1:(length(a)-1)],b[2:length(b)],method="spearman",use="complete"))
          pvalues=c(pvalues,rcorr(a[1:(length(a)-1)],b[2:length(b)],"spearman")[[3]][1,2])
        }
        else{
          cors=c(cors,cor(a,b,method="spearman",use="complete"))
          pvalues=c(pvalues,rcorr(a,b,"spearman")[[3]][1,2])
        }
      }
      str=paste(str,(paste0(round(mean(cors),2)," (",length(which(pvalues>0.05)),")")),"&")
    }
    print(str)
  }

  print("P TO Q")

  for (s1 in  c("DJF","MAM","JJA","SON"))
  {
    str=paste(s1,"&")
    for (s2 in c("DJF","MAM","JJA","SON"))
    {
      cors=c()
      pvalues=c()
      for (k in c(1:length(t_raw))){
        a=p_raw[[k]][[s1]]
        b=q_raw[[k]][[s2]]
        a=a[1:length(b)]
        if(next_seas(s1,s2)){
          cors=c(cors,cor(a[1:(length(a)-1)],b[2:length(b)],method="spearman",use="complete"))
          pvalues=c(pvalues,rcorr(a[1:(length(a)-1)],b[2:length(b)],"spearman")[[3]][1,2])
        }
        else{
          cors=c(cors,cor(a,b,method="spearman",use="complete"))
          pvalues=c(pvalues,rcorr(a,b,"spearman")[[3]][1,2])
        }
      }
      str=paste(str,(paste0(round(mean(cors),2)," (",length(which(pvalues>0.05)),")")),"&")
    }
    print(str)
  }

  print("P TO T")

  for (s1 in  c("DJF","MAM","JJA","SON"))
  {
    str=paste(s1,"&")
    for (s2 in c("DJF","MAM","JJA","SON"))
    {
      cors=c()
      pvalues=c()
      for (k in c(1:length(t_raw))){
        a=p_raw[[k]][[s1]]
        b=t_raw[[k]][[s2]]
        a=a[1:length(b)]
        if(next_seas(s1,s2)){
          cors=c(cors,cor(a[1:(length(a)-1)],b[2:length(b)],method="spearman",use="complete"))
          pvalues=c(pvalues,rcorr(a[1:(length(a)-1)],b[2:length(b)],"spearman")[[3]][1,2])
        }
        else{
          cors=c(cors,cor(a,b,method="spearman",use="complete"))
          pvalues=c(pvalues,rcorr(a,b,"spearman")[[3]][1,2])
        }
      }
      str=paste(str,(paste0(round(mean(cors),2)," (",length(which(pvalues>0.05)),")")),"&")
    }
    print(str)
  }
}
