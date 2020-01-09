# This script is related to the paper:
# "Stream temperature evolution in Switzerland over the last 50 years, Adrien
# Michel, Tristan Brauchli, Michael Lehning, Bettina Schaefli, and Hendrik Huwald,
# HESS, 2019"

# This material is distributed under the GPLv3 license
# (https://www.gnu.org/licenses/gpl-3.0.html)

#' Produce snow cover and glacier mass balance plots
#'
#' This function produces the monthly snow cover plots (Figures S31 and S32 in
#' suplpementary) and the glaciers mass balance plot (Figure S33 in supplementary).
#' The plots are saved in plots/snow_plots.pdf
#'
#' @param rivers_data The dataset of rivers data
#'
#' @export
plot_snow<-function(rivers_data)
{
  pdf(paste0("plots/snow_plots.pdf"),width=12,height=4)

  par(mfrow=c(1,1))
  par(mar=c(2,3,2,1),mgp=c(1.7,0.5,0),oma=c(0,0,0,0),cex.lab=1.2,cex.axis=1.1)

  march=as.data.frame(fread("data/snow_cover/march.txt"))
  plot(march,type="l",ylim=c(0,300),ylab="SWE at begining of month (mm)",xlab="")
  april=as.data.frame(fread("data/snow_cover/april.txt"))
  lines(april,type="l",col=2)
  may=as.data.frame(fread("data/snow_cover/may.txt"))
  lines(may,type="l",col=3)
  june=as.data.frame(fread("data/snow_cover/june.txt"))
  lines(june,type="l",col=4)
  july=as.data.frame(fread("data/snow_cover/july.txt"))
  lines(july,type="l",col=5)
  grid()
  legend("top",c("March","April","May","June","July"),col=c(1:5),lty=1, ncol=5, bty='n')

  plot(march[,1],march[,2]-june[,2],type="l",ylab="Spring snow melt (mm WE)",xlab="")
  grid()
  mtext(side=3, outer=TRUE,"All seasons",font=2,line=0.5)


  glaciers=list()
  for (f in list.files("data/glaciers/","*.txt"))
  {
    glaciers[[ gsub(".txt","",f)]]=as.data.frame(fread(paste0("data/glaciers/",f)))
  }

  par(mfrow=c(1,1),mar=c(3,3.5,1,1),mgp=c(1.7,0.5,0),oma=c(0,0,0,0),cex.lab=1.2,cex.axis=1.05)
  plot(as.numeric(substr(glaciers[[1]]$start_obs,1,4))+1,
       glaciers[[1]]$summer_mb*glaciers[[1]]$area*1000000/(1.e10),
       xlim=c(1950,2018),type="l",ylim=c(0,-30000*1000000/(1.e10)),
       xlab="Year",ylab=expression(paste("Summer glacier mass balance (10"^"10"," kg)")))

  names(glaciers)

  i=1
  c=1
  removed=c()
  for (g in glaciers){
    if((as.numeric(substr(g$start_obs,1,4))+1)[1]>1980){
      removed=c(removed,c)
      c=c+1
      next
    }
    lines(as.numeric(substr(g$start_obs,1,4))+1,g$summer_mb*g$area*1000000/(1.e10),col=i)
    i=i+1
    c=c+1
  }
  print(removed)
  legend("top",legend=names(glaciers)[-removed],col=c(1:10),lty=1,ncol=3,bty='n')

  dev.off()
}
