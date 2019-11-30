# This script is related to the paper:
# "Stream temperature evolution in Switzerland over the last 50 years, Adrien
# Michel, Tristan Brauchli, Michael Lehning, Bettina Schaefli, and Hendrik Huwald,
# HESS, 2019"

# This material is distributed under the GPLv3 license
# (https://www.gnu.org/licenses/gpl-3.0.html)

#' Returns plots for the 15C and 25C thresholds analysis
#'
#' This function return plots for the 15C and 25C thresholds analysis shown
#' in Figures 17 and 18 and in Figure S24. PLots are saved under
#' plots/25_degs.pdf and plots/15_degs.pdf
#'
#' @param rivers_data The dataset of rivers data
#' @param rivers_data_1h The dataset of rivers data at 1 hour resolution
#' @export
plot_thresholds <- function(rivers_data,rivers_data_1h)
{

  pdf(paste0("plots/25_degs.pdf"),width=12,height=6)

  # Modified from function provided at: http://www.phaget4.org/R/myImagePlot.R
  myImagePlot <- function(x, ...){
    min <- min(x,na.rm=TRUE)
    max <- max(x,na.rm=TRUE)
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
    layout(matrix(data=c(1,2), nrow=1, ncol=2), widths=c(8,1), heights=c(1,1))
    ColorLevels <- seq(min, max, length=256)
    if(min>=0){
      ColorRamp <- rev(colorRampPalette(brewer.pal(9,"Spectral"))(256))
      ColorRamp[1]=rgb(0.8,0.8,0.8)
    }
    else{
      lim=max(which(ColorLevels<0))
      ColorRamp1 <- rgb( seq(1,0.9,length=lim),  # Red
                         seq(0,0.9,length=lim),  # Green
                         seq(0,0.9,length=lim))
      ColorRamp2 <- rgb( seq(0.9,0,length=256-lim),  # Red
                         seq(0.9,0,length=256-lim),  # Green
                         seq(0.9,1,length=256-lim))
      ColorRamp=c(ColorRamp1,ColorRamp2)
    }
    # Reverse Y axis
    reverse <- nrow(x) : 1
    yLabels <- yLabels[reverse]
    x <- x[reverse,]
    # Data Map
    par(mar = c(3,5,2.5,0),mgp=c(1.7,0.7,0))
    image(1:length(xLabels), 1:length(yLabels), t(x), col=ColorRamp, xlab="",
          ylab="", axes=FALSE, zlim=c(min,max))
    if( !is.null(title) ){
      title(main=title)
    }
    axis(BELOW<-1, at=1:length(xLabels), labels=xLabels, cex.axis=1)
    axis(LEFT <-2, at=1:length(yLabels), labels=yLabels, las= HORIZONTAL<-1,
         cex.axis=1)
    # Color Scale
    par(mar = c(3,3.5,2.5,2),mgp=c(1.7,0.5,0))
    image(1, ColorLevels,
          matrix(data=ColorLevels, ncol=length(ColorLevels),nrow=1),
          col=ColorRamp,
          xlab="",ylab=ylab,
          xaxt="n",cex.lab=1.1)
    layout(1)
  }

  myImagePlot2 <- function(x, ...){
    min <- min(x,na.rm=TRUE)
    max <- max(x,na.rm=TRUE)
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
    layout(matrix(data=c(1,2,3,4), nrow=2, ncol=2,byrow=TRUE), widths=c(8,1), heights=c(4,2))
    ColorLevels <- seq(min, max, length=256)
    if(min>=0){
      ColorRamp <- rev(colorRampPalette(brewer.pal(9,"Spectral"))(256))
      ColorRamp[1]=rgb(0.8,0.8,0.8)
    }
    else{
      lim=max(which(ColorLevels<0))
      ColorRamp1 <- rgb( seq(1,0,length=lim),  # Red
                         seq(0,0,length=lim),  # Green
                         seq(0,0,length=lim))
      ColorRamp2 <- rgb( seq(0,0,length=256-lim),  # Red
                         seq(0,0,length=256-lim),  # Green
                         seq(0,1,length=256-lim))
      ColorRamp=c(ColorRamp1,ColorRamp2)
    }
    # Reverse Y axis
    reverse <- nrow(x) : 1
    yLabels <- yLabels[reverse]
    x <- x[reverse,]
    # Data Map
    par(mar = c(3,5,2.5,0),mgp=c(1.7,0.7,0))
    image(1:length(xLabels), 1:length(yLabels), t(x), col=ColorRamp, xlab="",
          ylab="", axes=FALSE, zlim=c(min,max))
    if( !is.null(title) ){
      title(main=title)
    }
    axis(BELOW<-1, at=1:length(xLabels), labels=xLabels, cex.axis=1.2)
    axis(LEFT <-2, at=1:length(yLabels), labels=yLabels, las= HORIZONTAL<-1,
         cex.axis=1.2)
    # Color Scale
    par(mar = c(3,3.5,2.5,2),mgp=c(1.7,0.5,0))
    image(1, ColorLevels,
          matrix(data=ColorLevels, ncol=length(ColorLevels),nrow=1),
          col=ColorRamp,
          xlab="",ylab=ylab,
          xaxt="n",cex.lab=1.2)
  }
  ####

  rivers_25=list()
  for (river_station in names(rivers_data_1h))
  {
    print(paste("Computing 25 deg days for:",river_station))
    #Get all years in the timeseries
    years=format(rivers_data_1h[[river_station]]$data$timestamp,"%Y")
    #extract only the choosen year
    yrs=unique(years)
    tresh_25=c()
    for (y in unique(years))
    {
      sel=which(years==y)
      temp=rivers_data_1h[[river_station]]$data[sel,]
      all25sel=which(temp$T>25)
      dates=temp$timestamp[all25sel]
      days= strftime(dates, format = "%j")
      tresh_25=c(tresh_25,length(unique(days)))
    }
    rivers_25[[rivers_data[[river_station]]$header$abbr]]=
      data.frame("yrs"=as.character(yrs),"tresh_25"=tresh_25)
  }

  be_set=names(rivers_data)[which(grepl("A0",names(rivers_data)))]
  for (river_station in be_set)
  {
    print(paste("Computing 25 deg days for:",river_station))
    #Get all years in the timeseries
    years=format(rivers_data[[river_station]]$data$timestamp,"%Y")
    #extract only the choosen year
    yrs=unique(years)
    tresh_25=c()
    for (y in unique(years))
    {
      sel=which(years==y)
      temp=rivers_data[[river_station]]$data[sel,]
      all25sel=which(temp$T>25)
      dates=temp$timestamp[all25sel]
      days= strftime(dates, format = "%j")
      tresh_25=c(tresh_25,length(unique(days)))
    }
    rivers_25[[rivers_data[[river_station]]$header$abbr]]=
      data.frame("yrs"=as.character(yrs),"tresh_25"=tresh_25)
  }

  zh_set=names(rivers_data)[which(grepl("ZH",names(rivers_data)))]
  for (river_station in zh_set)
  {
    print(paste("Computing 25 deg days for:",river_station))
    #Get all years in the timeseries
    years=format(rivers_data[[river_station]]$data$timestamp,"%Y")
    #extract only the choosen year
    yrs=unique(years)
    tresh_25=c()
    for (y in unique(years))
    {
      sel=which(years==y)
      temp=rivers_data[[river_station]]$data[sel,]
      all25sel=which(temp$T>25)
      dates=temp$timestamp[all25sel]
      days= strftime(dates, format = "%j")
      tresh_25=c(tresh_25,length(unique(days)))
    }
    rivers_25[[rivers_data[[river_station]]$header$abbr]]=
      data.frame("yrs"=as.character(yrs),"tresh_25"=tresh_25)
  }


  rivers_25_red=list()
  for (river_station in names (rivers_25))
  {
    if(sum(rivers_25[[river_station]]$tresh_25)>0)
    {
      rivers_25_red[[river_station]]=rivers_25[[river_station]]
    }
  }


  rivers_25_mat=matrix(NA, nrow = length(rivers_25_red), ncol = 50)
  colnames(rivers_25_mat)=c(1969:2018)
  rownames(rivers_25_mat)=names(rivers_25_red)

  for (river_station in names (rivers_25_red))
  {
    for (i in c(1:length(rivers_25_red[[river_station]]$yrs)))
    {
      y=as.character(rivers_25_red[[river_station]]$yrs[i])
      s=rivers_25_red[[river_station]]$tresh_25[i]
      rivers_25_mat[river_station,y]=s
    }
  }


  rivers_25_mat_Q=matrix(NA, nrow = length(rivers_25_red), ncol = 50)
  colnames(rivers_25_mat_Q)=c(1969:2018)
  rownames(rivers_25_mat_Q)=names(rivers_25_red)

  abbr_to_name=list()
  for(river_station in names(rivers_data))
  {
    abbr=rivers_data[[river_station]]$header$abbr
    abbr_to_name[[abbr]]=river_station
  }


  for (river_station in  names(rivers_25_red))
  {
    for (i in c(1:length(rivers_25_red[[river_station]]$yrs)))
    {
      y1=as.character(rivers_25_red[[river_station]]$yrs[i])
      y1=as.numeric(y1)

      if(abbr_to_name[[river_station]] %in% names(rivers_data) & y1 %in%
         rivers_data[[abbr_to_name[[river_station]]]]$JJA$Q$timestamp)
      {
        y=as.character(rivers_25_red[[river_station]]$yrs[i])
        ind=which(rivers_data[[abbr_to_name[[river_station]]]]$JJA$Q$timestamp==y1)
        start=which(rivers_data[[abbr_to_name[[river_station]]]]$JJA$Q$timestamp==1999)
        end=length(rivers_data[[abbr_to_name[[river_station]]]]$JJA$Q$timestamp)

        s=(rivers_data[[abbr_to_name[[river_station]]]]$JJA$Q$values[ind]-
          mean(rivers_data[[abbr_to_name[[river_station]]]]$JJA$Q$value[c(start:end)],
          na.rm=TRUE))/mean(rivers_data[[abbr_to_name[[river_station]]]]$JJA$Q$value[c(start:end)],
          na.rm=TRUE)*100
        rivers_25_mat_Q[river_station,y]=s
      }
    }
  }


  myImagePlot(rivers_25_mat,ylab=expression(paste("# of days where 25",degree,"C theshold is reached")))

  myImagePlot(rivers_25_mat_Q,ylab="Summer runoff anomaly (%)")

  dev.off()


  t_thresh=15
  thresh=28
  ma_window=1
  river_station
  rivers_15=list()
  for (river_station in names(rivers_data_1h))
  {
    print(paste("Computing 15 deg days for:",river_station))
    tmp=rivers_data_1h[[river_station]]$data
    dates=tmp$timestamp
    tmp$T=movingFun(tmp$T, n= ma_window, fun = mean, circular = TRUE)
    years=format(tmp$timestamp,"%Y")

    rivers_15[[river_station]]=list()
    for (y in unique(years))
    {
      rivers_15[[river_station]][[y]]=c(0)
      sel=which(years==y)
      days=as.numeric(strftime(dates[sel], format = "%j",tz="GMT"))
      for(i in c(1:365))
      {
        sel_day=seq((i-1)*24+1,(i*24))
        #tmp_day=rivers_data_1h[[river_station]]$data[sel,][sel_day,]$T>t_thresh
        tmp_day=tmp[sel,][sel_day,]$T>t_thresh
        all15sel=sum(tmp_day,na.rm=TRUE)
        if(all15sel==24)
        {
          rivers_15[[river_station]][[y]]=c(rivers_15[[river_station]][[y]],i)
        }
      }
    }
  }

  be_set=names(rivers_data)[which(grepl("A0",names(rivers_data)))]
  for (river_station in be_set)
  {
    print(paste("Computing 15 deg days for:",river_station))
    tmp=rivers_data[[river_station]]$data
    dates=tmp$timestamp
    tmp$T=movingFun(tmp$T, n= ma_window, fun = mean, circular = TRUE)
    years=format(tmp$timestamp,"%Y")
    rivers_15[[river_station]]=list()
    for (y in unique(years))
    {
      rivers_15[[river_station]][[y]]=c(0)
      sel=which(years==y)
      days=as.numeric(strftime(dates[sel], format = "%j",tz="GMT"))
      for(i in c(1:365))
      {
        sel_day=seq((i-1)*24+1,(i*24))
        #tmp_day=rivers_data[[river_station]]$data[sel,][sel_day,]$T>t_thresh
        tmp_day=tmp[sel,][sel_day,]$T>t_thresh
        all15sel=sum(tmp_day,na.rm=TRUE)
        if(all15sel==24)
        {
          rivers_15[[river_station]][[y]]=c(rivers_15[[river_station]][[y]],i)
        }
      }
    }
  }
  zh_set=names(rivers_data)[which(grepl("ZH",names(rivers_data)))]
  for (river_station in zh_set)
  {
    print(paste("Computing 15 deg days for:",river_station))
    tmp=rivers_data[[river_station]]$data
    dates=tmp$timestamp
    tmp$T=movingFun(tmp$T, n= ma_window, fun = mean, circular = TRUE)
    years=format(tmp$timestamp,"%Y")
    rivers_15[[river_station]]=list()
    for (y in unique(years))
    {
      rivers_15[[river_station]][[y]]=c(0)
      sel=which(years==y)
      days=as.numeric(strftime(dates[sel], format = "%j",tz="GMT"))
      for(i in c(1:365))
      {
        sel_day=seq((i-1)*24+1,(i*24))
        #tmp_day=rivers_data[[river_station]]$data[sel,][sel_day,]$T>t_thresh
        tmp_day=tmp[sel,][sel_day,]$T>t_thresh
        all15sel=sum(tmp_day,na.rm=TRUE)
        if(all15sel==24)
        {
          rivers_15[[river_station]][[y]]=c(rivers_15[[river_station]][[y]],i)
        }
      }
    }
  }
  sum_15=list()
  for (river_station in names(rivers_15))
  {
    yrs=as.character(names(rivers_15[[river_station]]))
    all_total=c()
    for (y in yrs){
      pos=1
      dat=rivers_15[[river_station]][[y]]
      serie=1
      total=0
      while(pos<length(dat)-1){
        if(dat[pos+1]==dat[pos]+1){
          serie=serie+1
        }
        else
        {
          if(serie > thresh){
            total=total+serie-thresh
            }
          serie=1
        }
        pos=pos+1
      }
      if(serie > thresh){
        total=total+serie-thresh
      }
      all_total=c(all_total,total)

    }
    sum_15[[river_station]]=data.frame("yrs"=as.character(yrs),"tresh_15"=all_total)
  }

  sum_15_red=list()
  for (river_station in names (sum_15))
  {
    if(sum(sum_15[[river_station]]$tresh_15)>0)
    {
      sum_15_red[[river_station]]=sum_15[[river_station]]
    }
  }

  sum_15_red
  rivers_15_mat=matrix(NA, nrow =  length(sum_15_red), ncol = 60)
  colnames(rivers_15_mat)=c(1960:2019)
  rownames(rivers_15_mat)=names(sum_15_red)
  for (river_station in names (sum_15_red))
  {
    for (i in c(1:length(sum_15_red[[river_station]]$yrs)))
    {
      y=as.character(sum_15_red[[river_station]]$yrs[i])
      s=sum_15_red[[river_station]]$tresh_15[i]
      rivers_15_mat[river_station,y]=s
    }
  }

  rivers_15_mat
  dec_vals=c()
  dec_dates=c()
  dec=c(1960,1970,1980,1990,2000,2010)
  for (river_station in rownames(rivers_15_mat))
  {
    m=mean(rivers_15_mat[river_station,21:59],na.rm=TRUE)
    for (i in c(3:6))
    {
      start=(i-1)*10+1
      end=(i*10)
      if(!is.na(rivers_15_mat[river_station,start])){
        dec_dates=c(dec_dates,dec[i])
        dec_vals=c(dec_vals,mean(rivers_15_mat[river_station,start:end],na.rm=TRUE)-m)
      }
      else{break}
    }
  }

  abbr=c()
  for (r in rownames(rivers_15_mat))
  {
    abbr=c(abbr,rivers_data[[r]]$header$abbr)
  }
  rownames(rivers_15_mat)=abbr

  pdf(paste0("plots/15_degs_1.pdf"),width=12,height=8)



  myImagePlot2(rivers_15_mat[,-1:-10],ylab=expression(paste(
    "# of days where the 15", degree,"C theshold is reached for at least 28 days")))
  dec_vals
  dec_dates
  mean(dec_vals[which(dec_dates=="1980")])
  mean(dec_vals[which(dec_dates=="1990")])
  mean(dec_vals[which(dec_dates=="2000")])
  mean(dec_vals[which(dec_dates=="2010")])
  par(mar = c(3,5,0.5,0.5),cex.lab=1.3,cex.axis=1.2,mgp=c(1.7,0.5,0))

  boxplot(dec_vals ~ dec_dates,
          xlab="Decade (starting year)",ylab="Anomaly (days per year)",xlim=c(-0.4,4.25))

  grid()
  layout(1)


  par(mfrow=c(3,3))
  for(n in names(sum_15_red))
  {
    plot(1,1,yaxt="n",xlab="DOY",ylab="year", main=n,
         xlim=c(0,365),ylim=c(1,1+0.05*(length(names(rivers_15[[n]]))-1)),type="n")
    abline(h=seq(1,1+0.05*length(names(rivers_15[[n]])),by=0.05),col="lightgray",lty=3)
    axis(2, at=seq(1,1+0.05*(length(names(rivers_15[[n]]))-1),by=0.05),
         labels=names(rivers_15[[n]]))
    abline(v=c(152,243),col=2)
    i=0
    for(y in names(rivers_15[[n]]))
    {
      if(length(rivers_15[[n]][[y]])>1)
      {
        points(rivers_15[[n]][[y]][2:length(rivers_15[[n]][[y]])],cex=0.5,pch=15,
           rep(1+i,times=length(rivers_15[[n]][[y]])-1),xlim=c(0,365))
    }
      i=i+0.05
    }
  }

  dev.off()
}
