# This script is related to the paper:
# "Stream temperature evolution in Switzerland over the last 50 years, Adrien
# Michel, Tristan Brauchli, Michael Lehning, Bettina Schaefli, and Hendrik Huwald,
# HESS, 2019"

# This material is distributed under the GPLv3 license
# (https://www.gnu.org/licenses/gpl-3.0.html)

#' Produces plots for lakes
#'
#' This function produces the plots for the trends before and after lakes, shown
#' in Figure 7 and in Figures S22 to S25 in supplementary. Figures are saved
#' under plots/lakes_plots.pdf
#'
#' @param rivers_data The dataset of rivers data
#' @export
plot_lakes <- function(rivers_data)
{
  pdf(paste0("plots/lakes_plots.pdf"),width=12,height=6)

  par(mfrow=c(2,1),cex.axis=1.1,cex.main=1,cex.lab=1.2)
  par(mar=c(2,3,0.5,1),mgp=c(1.7,0.5,0),oma=c(0,0,0,0))

  ### LAKES THUN AND BRIENZ
  leg=c()
  l=rivers_data[["Aare-Brienzwiler"]]$yearly$T
  l2=rivers_data[["Aare-Brienzwiler"]]$STL$T$lm$"1979-2018"
  plot(l$timestamp,l$values-mean(l2$values,na.rm=TRUE),type="l",ylim=c(-2,1.5),xlim=c(1980,2018),
       xlab="",ylab=expression(paste("Water Temp. (",degree,"C)")),col=3,lty=3, lwd=2)
  lines(l2$timestamp,l2$trend*l2$timestamp+l2$intercept-mean(l2$values,na.rm=TRUE),col=3,type="l")
  leg=c(leg,paste("Aar-Bri (inflow)"))

  l=rivers_data[["Aare-Ringgenberg_Goldswil"]]$yearly$T
  l2=rivers_data[["Aare-Ringgenberg_Goldswil"]]$STL$T$lm$"1979-2018"
  lines(l$timestamp,l$values-mean(l2$values,na.rm=TRUE),type="l",col=2,lty=3, lwd=2)
  lines(l2$timestamp,l2$trend*l2$timestamp+l2$intercept-mean(l2$values,na.rm=TRUE),type="l",col=2)
  leg=c(leg,paste("Aar-Rin (between)"))

  l=rivers_data[["Aare-Thun"]]$yearly$T
  l2=rivers_data[["Aare-Thun"]]$STL$T$lm$"1979-2018"
  lines(l$timestamp,l$values-mean(l2$values,na.rm=TRUE),type="l",col=1,lty=3, lwd=2)
  lines(l2$timestamp,l2$trend*l2$timestamp+l2$intercept-mean(l2$values,na.rm=TRUE),type="l")
  leg=c(leg,paste("Aar-Thu (outflow)"))

  legend("bottom",legend=leg,col=c(3,2,1),lty=1,ncol=3,bty='n',cex=1.1)

  ## METEO
  leg=c()
  l=rivers_data[["Aare-Brienzwiler"]][["meteo"]]$MER$yearly$T
  l2=rivers_data[["Aare-Brienzwiler"]]$STL$meteo$MER$TA$lm$"1979-2018"
  plot(l$timestamp,l$values-mean(l2$values,na.rm=TRUE),type="l",ylim=c(-2,1.5),xlim=c(1980,2018),
       ylab=expression(paste("Air Temp. (",degree,"C)")),lty=3, lwd=2,col=4,xlab="")
  lines(l2$timestamp,l2$trend*l2$timestamp+l2$intercept-mean(l2$values,na.rm=TRUE),type="l")
  leg=c(leg,paste("MER"))

  l=rivers_data[["Aare-Brienzwiler"]][["meteo"]]$GRH$yearly$T
  l2=rivers_data[["Aare-Brienzwiler"]]$STL$meteo$GRH$TA$lm$"1979-2018"
  lines(l$timestamp,l$values-mean(l2$values,na.rm=TRUE),type="l",col=5,lty=3, lwd=2)
  lines(l2$timestamp,l2$trend*l2$timestamp+l2$intercept-mean(l2$values,na.rm=TRUE),type="l",col=5)
  leg=c(leg,paste("GRH"))

  l=rivers_data[["Aare-Thun"]][["meteo"]]$INT$yearly$T
  l2=rivers_data[["Aare-Thun"]]$STL$meteo$INT$TA$lm$"1979-2018"
  lines(l$timestamp,l$values-mean(l2$values,na.rm=TRUE),type="l",col=6,lty=3, lwd=2)
  lines(l2$timestamp,l2$trend*l2$timestamp+l2$intercept-mean(l2$values,na.rm=TRUE),type="l",col=6)
  leg=c(leg,paste("INT"))

  l=rivers_data[["Aare-Bern_Schonau"]][["meteo"]]$BER$yearly$T
  l2=rivers_data[["Aare-Bern_Schonau"]]$STL$meteo$BER$TA$lm$"1979-2018"
  lines(l$timestamp,l$values-mean(l2$values,na.rm=TRUE),type="l",col=8,lty=3, lwd=2)
  lines(l2$timestamp,l2$trend*l2$timestamp+l2$intercept-mean(l2$values,na.rm=TRUE),type="l",col=8)
  leg=c(leg,paste("BER"))

  legend("bottom",legend=leg,col=c(4,5,6,8),lty=1,ncol=4,bty='n',cex=1.1)


  ### LAKES LUZERN
  leg=c()
  l=rivers_data[["Reuss-Seedorf"]]$yearly$T
  l2=rivers_data[["Reuss-Seedorf"]]$STL$T$lm$"1979-2018"
  plot(l$timestamp,l$values-mean(l2$values,na.rm=TRUE),type="l",ylim=c(-1.5,1.5),xlim=c(1980,2018),
       xlab="",ylab=expression(paste("Water Temp. (",degree,"C)")),col=3,lty=3, lwd=2)
  lines(l2$timestamp,l2$trend*l2$timestamp+l2$intercept-mean(l2$values,na.rm=TRUE),type="l",lwd=1,col=3)
  leg=c(leg,paste("Reu-See (inflow)"))

  l=rivers_data[["Muota-Ingenbohl"]]$yearly$T
  l2=rivers_data[["Muota-Ingenbohl"]]$STL$T$lm$"1979-2018"
  lines(l$timestamp,l$values-mean(l2$values,na.rm=TRUE),type="l",col=2,lty=3, lwd=2)
  lines(l2$timestamp,l2$trend*l2$timestamp+l2$intercept-mean(l2$values,na.rm=TRUE),type="l",col=2,lwd=1)
  leg=c(leg,paste("Muo-Ing (inflow)"))

  l=rivers_data[["Engelberger_Aa-Buochs_Flugplatz"]]$yearly$T
  l2=rivers_data[["Engelberger_Aa-Buochs_Flugplatz"]]$STL$T$lm$"1999-2018"
  lines(l$timestamp,l$values-mean(l2$values,na.rm=TRUE),type="l",col=4,lty=3, lwd=2)
  lines(l2$timestamp,l2$trend*l2$timestamp+l2$intercept-mean(l2$values,na.rm=TRUE),type="l",col=4,lwd=1)
  leg=c(leg,paste("Eaa-Buo (inflow)"))

  l=rivers_data[["Reuss-Luzern_Geissmattbrucke"]]$yearly$T
  l2=rivers_data[["Reuss-Luzern_Geissmattbrucke"]]$STL$T$lm$"1979-2018"
  lines(l$timestamp,l$values-mean(l2$values,na.rm=TRUE),type="l",col=1,lty=3, lwd=2)
  lines(l2$timestamp,l2$trend*l2$timestamp+l2$intercept-mean(l2$values,na.rm=TRUE),type="l",col=1,lwd=1)
  leg=c(leg,paste("Reu-Luz (outflow)"))

  l=rivers_data[["Kleine_Emme-Emmen"]]$yearly$T
  l2=rivers_data[["Kleine_Emme-Emmen"]]$STL$T$lm$"1979-2018"
  lines(l$timestamp,l$values-mean(l2$values,na.rm=TRUE),type="l",col=6,lty=3, lwd=2)
  lines(l2$timestamp,l2$trend*l2$timestamp+l2$intercept-mean(l2$values,na.rm=TRUE),type="l",col=6,lwd=1)
  leg=c(leg,paste("Kem-Emm (other)"))

  legend("bottom",legend=leg,col=c(3,2,4,1,6),lty=1,ncol=5,bty='n',cex=1.1)

  ## METEO
  leg=c()
  l=rivers_data[["Reuss-Seedorf"]][["meteo"]]$ALT$yearly$T
  l2=rivers_data[["Reuss-Seedorf"]]$STL$meteo$ALT$TA$lm$"1979-2018"
  plot(l$timestamp,l$values-mean(l2$values,na.rm=TRUE),type="l",ylim=c(-2,1.5),xlim=c(1980,2018),
       xlab="",ylab=expression(paste("Air Temp. (",degree,"C)")),lty=3, lwd=2,col=5)
  lines(l2$timestamp,l2$trend*l2$timestamp+l2$intercept-mean(l2$values,na.rm=TRUE),type="l",col=5)
  leg=c(leg,paste("ALT"))

  l=rivers_data[["Engelberger_Aa-Buochs_Flugplatz"]][["meteo"]]$ENG$yearly$T
  l2=rivers_data[["Engelberger_Aa-Buochs_Flugplatz"]]$STL$meteo$ENG$TA$lm$"1979-2018"
  lines(l$timestamp,l$values-mean(l2$values,na.rm=TRUE),type="l",col=8,lty=3, lwd=2)
  lines(l2$timestamp,l2$trend*l2$timestamp+l2$intercept-mean(l2$values,na.rm=TRUE),type="l",col=8)
  leg=c(leg,paste("ENG"))


  l=rivers_data[["Reuss-Luzern_Geissmattbrucke"]][["meteo"]]$LUZ$yearly$T
  l2=rivers_data[["Reuss-Luzern_Geissmattbrucke"]]$STL$meteo$LUZ$TA$lm$"1979-2018"
  lines(l$timestamp,l$values-mean(l2$values,na.rm=TRUE),type="l",col=9,lty=3, lwd=2)
  lines(l2$timestamp,l2$trend*l2$timestamp+l2$intercept-mean(l2$values,na.rm=TRUE),type="l",col=9)
  leg=c(leg,paste("LUZ"))

  legend("bottom",legend=leg,col=c(5,8,9),lty=1,ncol=3,bty='n',cex=1.1)


  ### LAKES BIEL

  leg=c()
  l=rivers_data[["Aare-Hagneck"]]$yearly$T
  l2=rivers_data[["Aare-Hagneck"]]$STL$T$lm$"1979-2018"
  plot(l$timestamp,l$values-mean(l2$values,na.rm=TRUE),type="l",ylim=c(-1.5,1.5),xlim=c(1980,2018),
       xlab="",ylab=expression(paste("Water Temp. (",degree,"C)")),col=2,lty=3, lwd=2)
  lines(l2$timestamp,l2$trend*l2$timestamp+l2$intercept-mean(l2$values,na.rm=TRUE),type="l",col=2)
  leg=c(leg,paste("Aar-Hag (inflow)"))

  l=rivers_data[["Aare-Brugg_Aegerten"]]$yearly$T
  l2=rivers_data[["Aare-Brugg_Aegerten"]]$STL$T$lm$"1979-2018"
  lines(l$timestamp,l$values-mean(l2$values,na.rm=TRUE),type="l",col=1,lty=3, lwd=2)
  lines(l2$timestamp,l2$trend*l2$timestamp+l2$intercept-mean(l2$values,na.rm=TRUE),type="l",col=1)
  leg=c(leg,paste("Aar-Bru (outflow)"))
  legend("bottom",legend=leg,col=c(2,1),lty=1,ncol=2,bty='n',cex=1.1)

  ## METEO
  leg=c()
  l=rivers_data[["Aare-Hagneck"]][["meteo"]]$BER$yearly$T
  l2=rivers_data[["Aare-Hagneck"]]$STL$meteo$BER$TA$lm$"1979-2018"
  plot(l$timestamp,l$values-mean(l2$values,na.rm=TRUE),type="l",ylim=c(-2,1.5),xlim=c(1980,2018),
       xlab="",ylab=expression(paste("Air Temp. (",degree,"C)")),lty=3, lwd=2,col=4)
  lines(l2$timestamp,l2$trend*l2$timestamp+l2$intercept-mean(l2$values,na.rm=TRUE),type="l",col=4)
  leg=c(leg,paste("BER"))

  l=rivers_data[["A022-Suze_Villeret"]][["meteo"]]$CDF$yearly$T
  l2=rivers_data[["A022-Suze_Villeret"]]$STL$meteo$CDF$TA$lm$"1979-2018"
  lines(l$timestamp,l$values-mean(l2$values,na.rm=TRUE),type="l",col=5,lty=3, lwd=2)
  lines(l2$timestamp,l2$trend*l2$timestamp+l2$intercept-mean(l2$values,na.rm=TRUE),type="l",col=5)
  leg=c(leg,paste("CDF"))

  l=rivers_data[["A025-Langete_Roggwil"]][["meteo"]]$WYN$yearly$T
  l2=rivers_data[["A025-Langete_Roggwil"]]$STL$meteo$WYN$TA$lm$"1979-2018"
  lines(l$timestamp,l$values-mean(l2$values,na.rm=TRUE),type="l",col=6,lty=3, lwd=2)
  lines(l2$timestamp,l2$trend*l2$timestamp+l2$intercept-mean(l2$values,na.rm=TRUE),type="l",col=6)
  leg=c(leg,paste("WYN"))

  legend("bottom",legend=leg,col=c(4:6),lty=1,ncol=3,bty='n',cex=1.1)


  ### LAKES WALEN
  leg=c()
  l=rivers_data[["Linth-Mollis_Linthbrucke"]]$yearly$T
  l2=rivers_data[["Linth-Mollis_Linthbrucke"]]$STL$T$lm$"1979-2018"
  plot(l$timestamp,l$values-mean(l2$values,na.rm=TRUE),type="l",ylim=c(-1.5,1.5),xlim=c(1980,2018),
       xlab="",ylab=expression(paste("Water Temp. (",degree,"C)")),col=2,lty=3, lwd=2)
  lines(l2$timestamp,l2$trend*l2$timestamp+l2$intercept-mean(l2$values,na.rm=TRUE),type="l",col=2)
  leg=c(leg,paste("Lin-Mol (inflow)"))

  l=rivers_data[["Linth-Weesen_Biaesche"]]$yearly$T
  l2=rivers_data[["Linth-Weesen_Biaesche"]]$STL$T$lm$"1979-2018"
  lines(l$timestamp,l$values-mean(l2$values,na.rm=TRUE),type="l",col=1,lty=3, lwd=2)
  lines(l2$timestamp,l2$trend*l2$timestamp+l2$intercept-mean(l2$values,na.rm=TRUE),type="l",col=1)
  leg=c(leg,paste("Lin-Wee (outflow)"))

  legend("bottom",legend=leg,col=c(2,1),lty=1,ncol=2,bty='n',cex=1.1)

  ## METEO
  leg=c()
  l=rivers_data[["Linth-Mollis_Linthbrucke"]][["meteo"]]$GLA$yearly$T
  l2=rivers_data[["Linth-Mollis_Linthbrucke"]]$STL$meteo$GLA$TA$lm$"1979-2018"
  plot(l$timestamp,l$values-mean(l2$values,na.rm=TRUE),type="l",ylim=c(-2,1.5),xlim=c(1980,2018),
       xlab="",ylab=expression(paste("Air Temp. (",degree,"C)")),lty=3, lwd=2,col=4)
  lines(l2$timestamp,l2$trend*l2$timestamp+l2$intercept-mean(l2$values,na.rm=TRUE),type="l",col=4)
  leg=c(leg,paste("GLA"))

  l=rivers_data[["Linth-Mollis_Linthbrucke"]][["meteo"]]$ELM$yearly$T
  l2=rivers_data[["Linth-Mollis_Linthbrucke"]]$STL$meteo$ELM$TA$lm$"1979-2018"
  lines(l$timestamp,l$values-mean(l2$values,na.rm=TRUE),type="l",col=5,lty=3, lwd=2)
  lines(l2$timestamp,l2$trend*l2$timestamp+l2$intercept-mean(l2$values,na.rm=TRUE),type="l",col=5)
  leg=c(leg,paste("ELM"))

  l=rivers_data[["Glatt-Rheinsfelden"]][["meteo"]]$SMA$yearly$T
  l2=rivers_data[["Glatt-Rheinsfelden"]]$STL$meteo$SMA$TA$lm$"1979-2018"
  lines(l$timestamp,l$values-mean(l2$values,na.rm=TRUE),type="l",col=6,lty=3, lwd=2)
  lines(l2$timestamp,l2$trend*l2$timestamp+l2$intercept-mean(l2$values,na.rm=TRUE),type="l",col=6)
  leg=c(leg,paste("SMA"))

  legend("bottom",legend=leg,col=c(3:6),lty=1,ncol=3,bty='n',cex=1.1)


  ### LAKES GENEVA
  leg=c()
  l=rivers_data[["Arve-Geneve_Bout_du_Monde"]]$yearly$T
  l2=rivers_data[["Arve-Geneve_Bout_du_Monde"]]$STL$T$lm$"1979-2018"
  plot(l$timestamp,l$values-mean(l2$values,na.rm=TRUE),type="l",ylim=c(-1.5,1.5),xlim=c(1980,2018),
       xlab="",ylab=expression(paste("Water Temp. (",degree,"C)")),col=2,lty=3, lwd=2)
  lines(l2$timestamp,l2$trend*l2$timestamp+l2$intercept-mean(l2$values,na.rm=TRUE),type="l",col=2)
  leg=c(leg,paste("Arv-Gva (other)"))

  l=rivers_data[["Rhone-Porte_du_Scex"]]$yearly$T
  l2=rivers_data[["Rhone-Porte_du_Scex"]]$STL$T$lm$"1979-2018"
  lines(l$timestamp,l$values-mean(l2$values,na.rm=TRUE),type="l",ylim=c(-1.5,1.5),col=3,lty=3, lwd=2)
  lines(l2$timestamp,l2$trend*l2$timestamp+l2$intercept-mean(l2$values,na.rm=TRUE),type="l",col=3)
  leg=c(leg,paste("Rho-Pds (inflow)"))

  l=rivers_data[["Rhone-Chancy_Aux_Ripes"]]$yearly$T
  l2=rivers_data[["Rhone-Chancy_Aux_Ripes"]]$STL$T$lm$"1979-2018"
  lines(l$timestamp,l$values-mean(l2$values,na.rm=TRUE),type="l",col=1,lty=3, lwd=2)
  lines(l2$timestamp,l2$trend*l2$timestamp+l2$intercept-mean(l2$values,na.rm=TRUE),type="l",col=1)
  leg=c(leg,paste("Rho-Cha (outflow)"))

  legend("bottom",legend=leg,col=c(2,3,1),lty=1,ncol=3,bty='n',cex=1.1)


  ## METEO
  leg=c()
  l=rivers_data[["Rhone-Chancy_Aux_Ripes"]][["meteo"]]$GVE$yearly$T
  l2=rivers_data[["Rhone-Chancy_Aux_Ripes"]]$STL$meteo$GVE$TA$lm$"1979-2018"
  plot(l$timestamp,l$values-mean(l2$values,na.rm=TRUE),type="l",ylim=c(-2,1.5),xlim=c(1980,2018),
       xlab="",ylab=expression(paste("Air Temp. (",degree,"C)")),lty=3, lwd=2,col=4)
  lines(l2$timestamp,l2$trend*l2$timestamp+l2$intercept-mean(l2$values,na.rm=TRUE),type="l",col=4)
  leg=c(leg,paste("GVE"))

  l=rivers_data[["Rhone-Porte_du_Scex"]][["meteo"]]$GSB$yearly$T
  l2=rivers_data[["Rhone-Porte_du_Scex"]]$STL$meteo$GSB$TA$lm$"1979-2018"
  lines(l$timestamp,l$values-mean(l2$values,na.rm=TRUE),type="l",col=5,lty=3, lwd=2)
  lines(l2$timestamp,l2$trend*l2$timestamp+l2$intercept-mean(l2$values,na.rm=TRUE),type="l",col=5)
  leg=c(leg,paste("GSB"))

  l=rivers_data[["Rhone-Porte_du_Scex"]][["meteo"]]$SIO$yearly$T
  l2=rivers_data[["Rhone-Porte_du_Scex"]]$STL$meteo$SIO$TA$lm$"1979-2018"
  lines(l$timestamp,l$values-mean(l2$values,na.rm=TRUE),type="l",col=6,lty=3, lwd=2)
  lines(l2$timestamp,l2$trend*l2$timestamp+l2$intercept-mean(l2$values,na.rm=TRUE),type="l",col=6)
  leg=c(leg,paste("SIO"))

  legend("bottom",legend=leg,col=c(4,5,6),lty=1,ncol=3,bty='n',cex=1.1)

  dev.off()
}
