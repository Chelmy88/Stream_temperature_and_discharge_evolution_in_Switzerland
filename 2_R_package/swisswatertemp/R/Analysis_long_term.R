# This script is related to the paper:
# "Stream temperature evolution in Switzerland over the last 50 years, Adrien
# Michel, Tristan Brauchli, Michael Lehning, Bettina Schaefli, and Hendrik Huwald,
# HESS, 2019"

# This material is distributed under the GPLv3 license
# (https://www.gnu.org/licenses/gpl-3.0.html)

#' Produce long term anomaly plots
#'
#' This function produces the decadal animalies plots (Figure 4 and Figure S9
#' in suplpementary), seasonnal decadal anomalies pots (Figure 8 and 9 and
#' Figures S18 and S19 in suplementary) and hysteresys plots (Figure 15). These
#' Figures are saved in plots/long_term_plots.pdf.
#' This function also print to the console the partially overlapping samples
#' two-sided t-test (see section 4.1) and the figure showing discharge and
#' precipitation decadal anomalies along with the NAO and AMO (Figure S10 in
#' suplpementary).
#'
#' Note that plots of meteorological data use meteo stations related to water
#' station except the long term precipitation decadal anomalies plot (Figure 4
#' and Figure S10 in suplpementary), which uses all available homegenous
#' MeteoSwiss data not necessarly linked to catchments (as stated in the paper).
#'
#' @param rivers_data The dataset of rivers data
#'
#' @param meteo_data The dataset of homegenous MeteoSwiss data
#'
#' @export

plot_long_term<-function(rivers_data, meteo_data)
{

  # Only the first block of code to retrieve
  # the mean water T anomalies is commented,
  # all other bloks beelow follow the same
  # logic
  print("Parsing river data")
  # Prepare lis tto store annual and seasonnal anomalies
  all_mean_T=list()
  all_mean_T$yearly=list()
  for (season in c("DJF","MAM","JJA","SON"))
  {
    all_mean_T[[season]]=list()
  }
  for (river_station in names(rivers_data))
  {
    # Gather anual data for one river, skip if starting after 1970
    T=rivers_data[[river_station]]$STL$T
    if(T$timestamp[1]>1970){next}
    # Prepare variable to store intemediate data and define decades
    meant=c()
    yrs=c()
    periods=c(1970,1980,1990,2000,2010)
    dec_means=list()
    # Compute the mean of teh data over the full period, used to comput the
    # anomaly
    mean_period=mean(T$raw[which(T$timestamp>1970)],na.rm=TRUE)

    for (p in periods)
    {
      # Gather data for one decade
      daily_means_temp_T=matrix(NA, nrow=365,ncol=10)
      for(j in c(1:10))
      {
        if(T$timestamp[length(T$timestamp)]<p+j-1){next}
        year_mean=T$raw[which(floor(T$timestamp)==p+j-1)]
        # Compute yearly anomaly
        daily_means_temp_T[,j]=year_mean-mean_period
      }
      # Computedecadal mean of yearly anomalies
      dec_means=rowMeans(daily_means_temp_T,na.rm = TRUE)
      # Add result for the current decade to the results vector
      meant=c(meant,mean(dec_means))
    }
    # Store decadale anomalie and correspondinf decades
    all_mean_T$yearly$T=c(all_mean_T$yearly$T,meant)
    all_mean_T$yearly$decade=c(all_mean_T$yearly$decade,periods)

    # Same as above, but repeated for each seasons
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

  ### ADD METEO #######
  print("Parsing meteo data")

  all_mean_TA=list()
  all_mean_TA$yearly=list()
  for (season in c("DJF","MAM","JJA","SON"))
  {
    all_mean_TA[[season]]=list()
  }
  for (river_station in names(rivers_data))
  {
    TA=rivers_data[[river_station]]$STL$meteo[[1]]$TA
    T=rivers_data[[river_station]]$STL$T

    if(T$timestamp[1]>1970){next}
    meant=c()
    yrs=c()
    periods=c(1970,1980,1990,2000,2010)
    dec_means=list()
    mean_period=mean(TA$raw[which(TA$timestamp>1970)],na.rm=TRUE)

    for (p in periods)
    {
      daily_means_temp_TA=matrix(NA, nrow=365,ncol=10)
      for(j in c(1:10))
      {
        if(TA$timestamp[length(TA$timestamp)]<p+j-1){next}
        if(TA$timestamp[1]>p+j-1){next}

        year_mean=TA$raw[which(floor(TA$timestamp)==p+j-1)]
        daily_means_temp_TA[,j]=year_mean-mean_period
      }

      dec_means=rowMeans(daily_means_temp_TA,na.rm = TRUE)
      meant=c(meant,mean(dec_means))
    }
    all_mean_TA$yearly$TA=c(all_mean_TA$yearly$TA,meant)
    all_mean_TA$yearly$decade=c(all_mean_TA$yearly$decade,periods)

    for (season in c("DJF","MAM","JJA","SON"))
    {
      TA=rivers_data[[river_station]]$meteo[[1]][[season]]$TA
      T=rivers_data[[river_station]][[season]]$T

      if(T$timestamp[1]>1970){next}
      meant=c()
      yrs=c()
      periods=c(1970,1980,1990,2000,2010)
      dec_means=list()
      mean_period=mean(TA$values[which(TA$timestamp>1970)],na.rm=TRUE)
      for (p in periods)
      {
        daily_means_T=matrix(NA, nrow=365,ncol=10)
        for(j in c(1:10))
        {
          if(TA$timestamp[length(TA$timestamp)]<p+j-1){next}
          if(TA$timestamp[1]>p+j-1){next}
          year_mean=TA$values[which(floor(TA$timestamp)==p+j-1)]
          daily_means_T[,j]=year_mean
        }
        dec_means=rowMeans(daily_means_T,na.rm = TRUE)
        meant=c(meant,mean(dec_means-mean_period))
      }
      all_mean_TA[[season]]$TA=c(all_mean_TA[[season]]$TA,meant)
      all_mean_TA[[season]]$decade=c(all_mean_TA[[season]]$decade,periods)
    }
  }

  all_max_Q=list()
  all_max_Q$Q=c()
  all_max_Q$decade=c()
  for (river_station in names(rivers_data))
  {
    Q=rivers_data[[river_station]]$STL$Q
    if(Q$timestamp[1]>1960 | rivers_data[[river_station]]$header$regime2=="Strong influence of Hydropeaking"
       | rivers_data[[river_station]]$header$regime2=="After lakes"){next}
    maxq=c()
    yrs=c()
    periods=c(1960,1970,1980,1990,2000,2010)
    dec_means=list()
    for (p in periods)
    {
      daily_means_temp_Q=matrix(NA, nrow=365,ncol=10)
      for(j in c(1:10))
      {
        if(Q$timestamp[length(Q$timestamp)]<p+j-1){next}
        year_mean=Q$raw[which(floor(Q$timestamp)==p+j-1)]
        daily_means_temp_Q[,j]=year_mean
      }

      dec_means=rowMeans(daily_means_temp_Q,na.rm = TRUE)
      maxq=c(maxq,which(dec_means==max(dec_means[0:240],na.rm=TRUE))[1])
    }
    all_max_Q$Q=c(all_max_Q$Q,maxq-mean(maxq))
    all_max_Q$decade=c(all_max_Q$decade,periods)
  }


  all_mean_Q=list()
  all_mean_Q$Q=c()
  all_mean_Q$decade=c()
  all_mean_Q$yearly=list()
  for (season in c("DJF","MAM","JJA","SON"))
  {
    all_mean_Q[[season]]=list()
  }
  for (river_station in names(rivers_data))
  {
    Q=rivers_data[[river_station]]$STL$Q
    if(Q$timestamp[1]>1960){next}
    meanq=c()
    yrs=c()
    periods=c(1960,1970,1980,1990,2000,2010)
    dec_means=list()
    mean_period=mean(Q$raw[which(Q$timestamp>1960)],na.rm=TRUE)

    for (p in periods)
    {
      daily_means_Q=matrix(NA, nrow=365,ncol=10)
      for(j in c(1:10))
      {
        if(Q$timestamp[length(Q$timestamp)]<p+j-1){next}
        if(Q$timestamp[1]>p+j-1){next}

        year_mean=Q$raw[which(floor(Q$timestamp)==p+j-1)]
        daily_means_Q[,j]=year_mean
      }
      dec_means=rowMeans(daily_means_Q,na.rm = TRUE)
      meanq=c(meanq,mean(dec_means-mean_period)/mean_period*100)
    }
    all_mean_Q$yearly$Q=c(all_mean_Q$yearly$Q,meanq)
    all_mean_Q$yearly$decade=c(all_mean_Q$yearly$decade,periods)

    for (season in c("DJF","MAM","JJA","SON"))
    {
      Q=rivers_data[[river_station]][[season]]$Q
      if(Q$timestamp[1]>1960){next}
      meanq=c()
      yrs=c()
      periods=c(1960,1970,1980,1990,2000,2010)
      dec_means=list()
      mean_period=mean(Q$values[which(Q$timestamp>1960)],na.rm=TRUE)
      for (p in periods)
      {
        daily_means_Q=matrix(NA, nrow=365,ncol=10)
        for(j in c(1:10))
        {
          if(Q$timestamp[length(Q$timestamp)]<p+j-1){next}
          if(Q$timestamp[1]>p+j-1){next}
          year_mean=Q$values[which(floor(Q$timestamp)==p+j-1)]
          daily_means_Q[,j]=year_mean
        }
        dec_means=rowMeans(daily_means_Q,na.rm = TRUE)
        meanq=c(meanq,mean(dec_means-mean_period)/mean_period*100)
      }
      all_mean_Q[[season]]$Q=c(all_mean_Q[[season]]$Q,meanq)
      all_mean_Q[[season]]$decade=c(all_mean_Q[[season]]$decade,periods)
    }
  }

  all_mean_P=list()
  all_mean_P$P=c()
  all_mean_P$decade=c()
  all_mean_P$yearly=list()
  for (season in c("DJF","MAM","JJA","SON"))
  {
    all_mean_P[[season]]=list()
  }
  for (river_station in names(rivers_data))
  {
    P=rivers_data[[river_station]]$STL$meteo[[1]]$P
    Q=rivers_data[[river_station]]$STL$Q
    if(Q$timestamp[1]>1960){next}
    meanq=c()
    yrs=c()
    periods=c(1960,1970,1980,1990,2000,2010)
    dec_means=list()
    mean_period=mean(P$raw[which(P$timestamp>1960)],na.rm=TRUE)

    for (p in periods)
    {
      daily_means_P=matrix(NA, nrow=365,ncol=10)
      for(j in c(1:10))
      {
        if(P$timestamp[length(P$timestamp)]<p+j-1){next}
        if(P$timestamp[1]>p+j-1){next}

        year_mean=P$raw[which(floor(P$timestamp)==p+j-1)]
        daily_means_P[,j]=year_mean
      }
      dec_means=rowMeans(daily_means_P,na.rm = TRUE)
      meanq=c(meanq,mean(dec_means-mean_period)/mean_period*100)
    }
    all_mean_P$yearly$P=c(all_mean_P$yearly$P,meanq)
    all_mean_P$yearly$decade=c(all_mean_P$yearly$decade,periods)


    for (season in c("DJF","MAM","JJA","SON"))
    {
      P=rivers_data[[river_station]]$meteo[[1]][[season]]$P
      Q=rivers_data[[river_station]][[season]]$Q
      if(Q$timestamp[1]>1960){next}
      meanq=c()
      yrs=c()
      periods=c(1960,1970,1980,1990,2000,2010)
      dec_means=list()
      mean_period=mean(P$values[which(P$timestamp>1960)],na.rm=TRUE)
      for (p in periods)
      {
        daily_means_P=matrix(NA, nrow=365,ncol=10)
        for(j in c(1:10))
        {
          if(P$timestamp[length(P$timestamp)]<p+j-1){next}
          if(P$timestamp[1]>p+j-1){next}
          year_mean=P$values[which(floor(P$timestamp)==p+j-1)]
          daily_means_P[,j]=year_mean
        }
        dec_means=rowMeans(daily_means_P,na.rm = TRUE)
        meanq=c(meanq,mean(dec_means-mean_period)/mean_period*100)
      }
      all_mean_P[[season]]$P=c(all_mean_P[[season]]$P,meanq)
      all_mean_P[[season]]$decade=c(all_mean_P[[season]]$decade,periods)

    }
  }

  print("Parsing long term data")

  all_mean_Q_long=list()
  all_mean_Q_long$Q=c()
  all_mean_Q_long$decade=c()
  all_mean_Q_long$yearly=list()
  for (river_station in names(rivers_data))
  {
    Q=rivers_data[[river_station]]$daily$Q
    if(year(Q$timestamp)[1]>1920){next}
    meanq=c()
    yrs=c()
    periods=c(1920,1930,1940,1950,1960,1970,1980,1990,2000,2010)
    dec_means=list()
    mean_period=mean(Q$values[which(year(Q$timestamp)>1920)],na.rm=TRUE)
    for (p in periods)
    {
      daily_means_Q=matrix(NA, nrow=365,ncol=10)
      for(j in c(1:10))
      {
        if(year(Q$timestamp[length(Q$timestamp)])<p+j-1){next}
        if(year(Q$timestamp[1])>p+j-1){next}
        which(floor(year(Q$timestamp))==p+j-1)

        year_mean=Q$values[which(floor(year(Q$timestamp))==p+j-1)]
        daily_means_Q[,j]=year_mean
      }
      dec_means=rowMeans(daily_means_Q,na.rm = TRUE)
      meanq=c(meanq,mean(dec_means-mean_period)/mean_period*100)
    }
    all_mean_Q_long$yearly$Q=c(all_mean_Q_long$yearly$Q,meanq)
    all_mean_Q_long$yearly$decade=c(all_mean_Q_long$yearly$decade,periods)
  }

  all_mean_P_long=list()
  all_mean_P_long$P=c()
  all_mean_P_long$decade=c()
  all_mean_P_long$yearly=list()
  for (meteo_station in names(meteo_data))
  {
    P=meteo_data[[meteo_station]]$daily$P_HOM
    if(year(P$timestamp)[1]>1920){next}
    meanp=c()
    yrs=c()
    periods=c(1920,1930,1940,1950,1960,1970,1980,1990,2000,2010)
    dec_means=list()
    mean_period=mean(P$values[which(year(P$timestamp)>1920)],na.rm=TRUE)

    for (per in periods)
    {
      daily_means_P=matrix(NA, nrow=365,ncol=10)
      for(j in c(1:10))
      {
        if(year(P$timestamp[length(P$timestamp)])<per+j-1){next}
        if(year(P$timestamp[1])>p+j-1){next}
        which(floor(year(P$timestamp))==per+j-1)

        year_mean=P$values[which(floor(year(P$timestamp))==per+j-1)]
        daily_means_P[,j]=year_mean
      }
      dec_means=rowMeans(daily_means_P,na.rm = TRUE)
      meanp=c(meanp,mean(dec_means-mean_period)/mean_period*100)
    }
    all_mean_P_long$yearly$P=c(all_mean_P_long$yearly$P,meanp)
    all_mean_P_long$yearly$decade=c(all_mean_P_long$yearly$decade,periods)
  }

  all_mean_Q_short=list()
  all_mean_Q_short$Q=c()
  all_mean_Q_short$decade=c()
  all_mean_Q_short$yearly=list()
  all_mean_Q_short$regime=c()
  all_mean_Q_short$altitude=c()
  for (season in c("DJF","MAM","JJA","SON"))
  {
    all_mean_Q_short[[season]]=list()
  }
  for (river_station in names(rivers_data))
  {
    Q=rivers_data[[river_station]]$STL$Q
    if(Q$timestamp[1]>1980){next}
    meanq=c()
    yrs=c()
    periods=c(1980,1990,2000,2010)
    dec_means=list()
    mean_period=mean(Q$raw[which(Q$timestamp>1980)],na.rm=TRUE)

    for (p in periods)
    {
      daily_means_Q=matrix(NA, nrow=365,ncol=10)
      for(j in c(1:10))
      {
        if(Q$timestamp[length(Q$timestamp)]<p+j-1){next}
        if(Q$timestamp[1]>p+j-1){next}

        year_mean=Q$raw[which(floor(Q$timestamp)==p+j-1)]
        daily_means_Q[,j]=year_mean
      }
      dec_means=rowMeans(daily_means_Q,na.rm = TRUE)
      meanq=c(meanq,mean(dec_means-mean_period)/mean_period*100)
    }
    all_mean_Q_short$yearly$Q=c(all_mean_Q_short $yearly$Q,meanq)
    all_mean_Q_short$yearly$decade=c(all_mean_Q_short $yearly$decade,periods)
    all_mean_Q_short$yearly$altitude=c(all_mean_Q_short$yearly$altitude,
                                      rep(rivers_data[[river_station]]$header$altitude,length(periods)))
    all_mean_Q_short$yearly$regime=c(all_mean_Q_short$yearly$regime,
                                    rep(rivers_data[[river_station]]$header$regime2,length(periods)))

    for (season in c("DJF","MAM","JJA","SON"))
    {
      Q=rivers_data[[river_station]][[season]]$Q
      if(Q$timestamp[1]>1980){next}
      meanq=c()
      yrs=c()
      dec_means=list()
      mean_period=mean(Q$values[which(Q$timestamp>1980)],na.rm=TRUE)
      for (p in periods)
      {
        daily_means_Q=matrix(NA, nrow=365,ncol=10)
        for(j in c(1:10))
        {
          if(Q$timestamp[length(Q$timestamp)]<p+j-1){next}
          if(Q$timestamp[1]>p+j-1){next}
          year_mean=Q$values[which(floor(Q$timestamp)==p+j-1)]
          daily_means_Q[,j]=year_mean
        }
        dec_means=rowMeans(daily_means_Q,na.rm = TRUE)
        meanq=c(meanq,mean(dec_means-mean_period)/mean_period*100)
      }
      all_mean_Q_short[[season]]$Q=c(all_mean_Q_short[[season]]$Q,meanq)
      all_mean_Q_short[[season]]$decade=c(all_mean_Q_short[[season]]$decade,periods)
    }
  }


  pdf(paste0("plots/long_term_plots.pdf"),width=12,height=6)
  par(mfrow=c(1,1))
  par(mar=c(4,4,2,1),mgp=c(2.3,0.8,0),oma=c(0,0,0,0))
  par(fg = 1)
  boxplot(all_mean_T$yearly$T ~ all_mean_T$yearly$decade,main=
          paste("Annual mean water temperature decadal anomaly, for",
                sum(all_mean_T$yearly$decade==1980),"catchments"),
          ylab=expression(paste("Decadal water temperature anomaly (",degree,"C)")),
          xlab="Decade (starting year)")

  means <- aggregate(all_mean_T$yearly$T ,
                     by = list(all_mean_T$yearly$decade),
                     FUN = mean,  na.rm=TRUE)

  points(1:5, means$x, col = "red",pch=16)

  x=all_mean_T$yearly$T[which(all_mean_T$yearly$decade==1970)]
  y=all_mean_T$yearly$T[which(all_mean_T$yearly$decade==1980)]
  print(t.test(x, y, paired=TRUE))

  x=all_mean_T$yearly$T[which(all_mean_T$yearly$decade==1980)]
  y=all_mean_T$yearly$T[which(all_mean_T$yearly$decade==1990)]
  print(t.test(x, y, paired=TRUE))

  x=all_mean_T$yearly$T[which(all_mean_T$yearly$decade==1990)]
  y=all_mean_T$yearly$T[which(all_mean_T$yearly$decade==2000)]
  print(t.test(x, y, paired=TRUE))

  x=all_mean_T$yearly$T[which(all_mean_T$yearly$decade==2000)]
  y=all_mean_T$yearly$T[which(all_mean_T$yearly$decade==2010)]
  print(t.test(x, y, paired=TRUE))

  print(t.test((all_mean_T$yearly$T[which(all_mean_T$yearly$decade==1970)]-
                all_mean_T$yearly$T[which(all_mean_T$yearly$decade==1990)]),
         (all_mean_T$yearly$T[which(all_mean_T$yearly$decade==1990)]-
          all_mean_T$yearly$T[which(all_mean_T$yearly$decade==2010)]),paired=TRUE))

  print(
    Partover.test(
      all_mean_T$yearly$T[which(all_mean_T$yearly$decade==1970)]-
      all_mean_T$yearly$T[which(all_mean_T$yearly$decade==1990)],
      all_mean_T$yearly$T[which(all_mean_T$yearly$decade==1990)]-
      all_mean_T$yearly$T[which(all_mean_T$yearly$decade==2010)],var.equal = FALSE,
      mu = 0, alternative = "two.sided",
      conf.level = NULL, stacked = TRUE)
  )


  mean(all_mean_T$yearly$T[which(all_mean_T$yearly$decade==1970)]-
       all_mean_T$yearly$T[which(all_mean_T$yearly$decade==1990)])
  mean(all_mean_T$yearly$T[which(all_mean_T$yearly$decade==1990)]-
       all_mean_T$yearly$T[which(all_mean_T$yearly$decade==2010)])

  par(mfrow=c(1,1))
  par(mar=c(2,3,2,1),mgp=c(1.7,0.5,0),oma=c(0,0,0,0))

  par(fg = 1)
  boxplot(all_max_Q$Q ~ all_max_Q$decade,
          main=paste("D.O.Y of maximum discharge per decade, for",
          sum(all_max_Q$decade==1980),"catchments"),
          ylab="D.O.Y. of maximum discharge (Day)",xlab="Decade (starting year)")

  x=all_max_Q$Q[which(all_max_Q$decade==1960)]
  y=all_max_Q$Q[which(all_max_Q$decade==1970)]
  t.test(x, y, paired=TRUE)

  x=all_max_Q$Q[which(all_max_Q$decade==1970)]
  y=all_max_Q$Q[which(all_max_Q$decade==1980)]
  t.test(x, y, paired=TRUE)

  x=all_max_Q$Q[which(all_max_Q$decade==1980)]
  y=all_max_Q$Q[which(all_max_Q$decade==1990)]
  t.test(x, y, paired=TRUE)

  x=all_max_Q$Q[which(all_max_Q$decade==1990)]
  y=all_max_Q$Q[which(all_max_Q$decade==2000)]
  t.test(x, y, paired=TRUE)

  x=all_max_Q$Q[which(all_max_Q$decade==2000)]
  y=all_max_Q$Q[which(all_max_Q$decade==2010)]
  t.test(x, y, paired=TRUE)


  plot_hysteresis(rivers_data[["Lutschine-Gsteig"]],output_type = "NONE")
  plot_hysteresis(rivers_data[["Arve-Geneve_Bout_du_Monde"]],output_type = "NONE")
  plot_hysteresis(rivers_data[["Lonza-Blatten"]],output_type = "NONE")



  par(mfrow=c(1,1))
  par(mar=c(3,3,1,1),mgp=c(1.7,0.5,0),oma=c(0,0,0,0),cex.axis=1.1,cex.lab=1.1)
  all_mean_Q
  boxplot(all_mean_Q$yearly$Q ~ all_mean_Q$yearly$decade,
          ylab="Discharge anomaly (%)",xlab="Decade (starting year)")

  par(mfrow=c(2,1))
  par(mar=c(3,3,1,1),mgp=c(1.7,0.5,0),oma=c(0,0,0,0),cex.axis=1.1,cex.lab=1.1)
  boxplot(all_mean_Q_long$yearly$Q ~ all_mean_Q_long$yearly$decade,
          ylab="Discharge anomaly (%)",xlab="Decade (starting year)")

  boxplot(all_mean_P_long$yearly$P ~ all_mean_P_long$yearly$decade,
          ylab="Precipitations anomaly (%)",xlab="Decade (starting year)")




  par(mfrow=c(2,2))
  par(mar=c(3,3,1,1),mgp=c(1.7,0.5,0),oma=c(0,0,0,0),cex.axis=1.2,cex.lab=1.25)
  boxplot(all_mean_T$DJF$T ~ all_mean_T$DJF$decade,
          ylab=expression(paste("Decadal water temp. anomaly (",degree,"C)")),
          xlab="Decade (starting year)",ylim=c(-1.5,1.5))
  legend("topleft",title="DJF",legend="",bty="n",cex=1.3)

  boxplot(all_mean_T$JJA$T ~ all_mean_T$JJA$decade,
          ylab=expression(paste("Decadal  water temp. anomaly (",degree,"C)")),
          xlab="Decade (starting year)",ylim=c(-1.5,1.5))
  legend("topleft",title="JJA",legend="",bty="n",cex=1.3)

  boxplot(all_mean_T$MAM$T ~ all_mean_T$MAM$decade,
          ylab=expression(paste("Decadal water temp. anomaly (",degree,"C)")),
          xlab="Decade (starting year)",ylim=c(-1.5,1.5))
  legend("topleft",title="MAM",legend="",bty="n",cex=1.3)

  boxplot(all_mean_T$SON$T ~ all_mean_T$SON$decade,
          ylab=expression(paste("Decadal water temp. anomaly (",degree,"C)")),
          xlab="Decade (starting year)",ylim=c(-1.5,1.5))
  legend("topleft",title="SON",legend="",bty="n",cex=1.3)


  par(mfrow=c(2,2))
  par(mar=c(3,3,1,1),mgp=c(1.7,0.5,0),oma=c(0,0,0,0),cex.axis=1.2,cex.lab=1.25)
  boxplot(all_mean_Q$DJF$Q ~ all_mean_Q$DJF$decade,
          ylab="Decadal discharge anomaly (%)",xlab="Decade (starting year)",ylim=c(-25,35))
  legend("topleft",title="DJF",legend="",bty="n",cex=1.3)

  boxplot(all_mean_Q$JJA$Q ~ all_mean_Q$JJA$decade,
          ylab="Decadal discharge anomaly (%)",xlab="Decade (starting year)",ylim=c(-25,35))
  legend("topleft",title="JJA",legend="",bty="n",cex=1.3)

  boxplot(all_mean_Q$MAM$Q ~ all_mean_Q$MAM$decade,
          ylab="Decadal discharge anomaly (%)",xlab="Decade (starting year)",ylim=c(-25,35))
  legend("topleft",title="MAM",legend="",bty="n",cex=1.3)

  boxplot(all_mean_Q$SON$Q ~ all_mean_Q$SON$decade,
          ylab="Decadal discharge anomaly (%)",xlab="Decade (starting year)",ylim=c(-25,35))
  legend("topleft",title="SON",legend="",bty="n",cex=1.3)



  par(mfrow=c(2,2))
  par(mar=c(3,3,1,1),mgp=c(1.7,0.5,0),oma=c(0,0,0,0),cex.axis=1.2,cex.lab=1.25)
  boxplot(all_mean_TA$DJF$TA ~ all_mean_TA$DJF$decade,
          ylab=expression(paste("Decadal air temp. anomaly (",degree,"C)")),
          xlab="Decade (starting year)",ylim=c(-1.5,1.5))
  legend("topleft",title="DJF",legend="",bty="n",cex=1.3)
  boxplot(all_mean_TA$JJA$TA ~ all_mean_TA$JJA$decade,
          ylab=expression(paste("Decadal air temp. anomaly (",degree,"C)")),
          xlab="Decade (starting year)",ylim=c(-1.5,1.5))
  legend("topleft",title="JJA",legend="",bty="n",cex=1.3)

  boxplot(all_mean_TA$MAM$TA ~ all_mean_TA$MAM$decade,
          ylab=expression(paste("Decadal air temp. anomaly (",degree,"C)")),
          xlab="Decade (starting year)",ylim=c(-1.5,1.5))
  legend("topleft",title="MAM",legend="",bty="n",cex=1.3)

  boxplot(all_mean_TA$SON$TA ~ all_mean_TA$SON$decade,
          ylab=expression(paste("Decadal air temp. anomaly (",degree,"C)")),
          xlab="Decade (starting year)",ylim=c(-1.5,1.5))
  legend("topleft",title="SON",legend="",bty="n",cex=1.3)


  par(mfrow=c(2,2))
  par(mar=c(3,3,1,1),mgp=c(1.7,0.5,0),oma=c(0,0,0,0),cex.axis=1.2,cex.lab=1.25)
  boxplot(all_mean_P$DJF$P ~ all_mean_P$DJF$decade,
          ylab="Decadal precipitation anomaly (%)",xlab="Decade (starting year)",ylim=c(-25,35))
  legend("topleft",title="DJF",legend="",bty="n",cex=1.3)

  boxplot(all_mean_P$JJA$P ~ all_mean_P$JJA$decade,
          ylab="Decadal precipitation anomaly (%)",xlab="Decade (starting year)",ylim=c(-25,35))
  legend("topleft",title="JJA",legend="",bty="n",cex=1.3)

  boxplot(all_mean_P$MAM$P ~ all_mean_P$MAM$decade,
          ylab="Decadal precipitation anomaly (%)",xlab="Decade (starting year)",ylim=c(-25,35))
  legend("topleft",title="MAM",legend="",bty="n",cex=1.3)

  boxplot(all_mean_P$SON$P ~ all_mean_P$SON$decade,
          ylab="Decadal precipitation anomaly (%)",xlab="Decade (starting year)",ylim=c(-25,35))
  legend("topleft",title="SON",legend="",bty="n",cex=1.3)


  sel=which(all_mean_Q_short$yearly$regime=="Alpine regime")
  boxplot(all_mean_Q_short$MAM$Q[sel] ~ all_mean_Q_short$MAM$decade[sel],
          main=paste("Alpine regimes spring runoff anomaly per decade, for",
          sum(all_mean_Q_short$MAM$decade[sel]==1980),"catchments"),
          ylab="Spring runoff decadal anomaly (%)",xlab="Decade (starting year)",
          ylim=c(-25,35),cex=1.1,cex.lab=1.15)


  sel=which(all_mean_Q_short$yearly$regime=="After lakes")
  boxplot(all_mean_Q_short$MAM$Q[sel] ~ all_mean_Q_short$MAM$decade[sel],
          main=paste("After lakes spring runoff anomaly per decade, for",
          sum(all_mean_Q_short$MAM$decade[sel]==1980),"catchments"),
          ylab="Spring runoff decadal anomaly (%)",xlab="Decade (starting year)",
          ylim=c(-25,35),cex=1.1,cex.lab=1.15)

  sel=which(all_mean_Q_short$yearly$regime=="Strong influence of Hydropeaking")
  boxplot(all_mean_Q_short$MAM$Q[sel] ~ all_mean_Q_short$MAM$decade[sel],
          main=paste("Hydropeaking regimes spring runoff anomaly per decade, for",
          sum(all_mean_Q_short$MAM$decade[sel]==1980),"catchments"),
          ylab="Spring runoff decadal anomaly (%)",xlab="Decade (starting year)",
          ylim=c(-25,35),cex=1.1,cex.lab=1.15)


  sel=which(all_mean_Q_short$yearly$regime=="Regime from Plateau and Jura")
  boxplot(all_mean_Q_short$MAM$Q[sel] ~ all_mean_Q_short$MAM$decade[sel],
          main=paste("Plateau/Jura lakes spring runoff anomaly per decade, for",
          sum(all_mean_Q_short$MAM$decade[sel]==1980),"catchments"),
          ylab="Spring runoff decadal anomaly (%)",xlab="Decade (starting year)",
          ylim=c(-25,35),cex=1.1,cex.lab=1.15)

  par(mfrow=c(1,1))
  par(mar=c(4,4,2,1),mgp=c(2.3,0.8,0),oma=c(0,0,0,0))
  par(fg = 1)
  boxplot(all_mean_TA$yearly$TA ~ all_mean_TA$yearly$decade,
          main=paste("Annual mean air temperature decadal anomaly, for",
          sum(all_mean_TA$yearly$decade==1980),"catchments"),
          ylab=expression(paste("Decadal air temperature anomaly (",degree,"C)")),
          xlab="Decade (starting year)",cex.axis=1.3,cex.lab=1.4,cex.main=1.2)



  dev.off()


  pdf(paste0("plots/indices.pdf"),width=12,height=10)

  par(mfrow=c(4,1))
  par(mar=c(3,3,2,1),mgp=c(1.7,0.6,0),oma=c(0,0,0,0),cex.axis=1.4,cex.main=1.6,cex.lab=1.5)
  boxplot(all_mean_Q_long$yearly$Q ~ all_mean_Q_long$yearly$decade,
          main=paste("Discharge decadal anomaly, for",
          sum(all_mean_Q_long$yearly$decade==1920),"catchments"),
          ylab="Discharge anomaly (%)",xlab="Decade (starting year)")

  means <- aggregate(all_mean_Q_long$yearly,
                      by = list(all_mean_Q_long$yearly$decade),
                      FUN = mean)
  lines(means$Q,col=2,lwd=2)

  boxplot(all_mean_P_long$yearly$P ~ all_mean_P_long$yearly$decade,
        main=paste("Precipitation decadal anomaly, for",
        sum(all_mean_P_long$yearly$decade==1920),"stations"),
        ylab="Precipitations anomaly (%)",xlab="Decade (starting year)")

  means <- aggregate(all_mean_P_long$yearly,
                     by = list(all_mean_P_long$yearly$decade),
                     FUN = mean)
  lines(means$P,col=2,lwd=2)


  ma <- function(x, n = 4){filter(x, rep(1 / n, n), sides = 2)}

  nao=as.data.frame(fread("data/indicators/NAO.txt"))
  nao[nao==-99.99]=NaN

  amon=as.data.frame(fread("data/indicators/AMON.txt"))
  amon[amon==-99.99]=NaN


  nao_means <- aggregate(nao[,2],
                         by = list(floor(nao[,1])),
                         FUN = mean)

  amon_means <- aggregate(amon[,2],
                          by = list(floor(amon[,1])),
                          FUN = mean)


  plot(nao_means[,1],ma(nao_means[,2],n=1),type="l",col=2,xlim=c(1915,2020),lwd=2,
       ylim=c(-2,2),xlab="Year",ylab="NAO (-)",main="NAO")
  abline(h=0)

  plot(amon_means[,1],amon_means[,2],col=3,lwd=2,type="l",ylab="AMO",xlab="Year",
       main="AMO",xlim=c(1915,2020))
  abline(h=0)

  dev.off()

}

