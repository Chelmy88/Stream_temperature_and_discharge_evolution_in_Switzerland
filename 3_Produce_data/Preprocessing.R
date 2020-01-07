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

# This scripts generate the data sets to be used in 4_Run_analysis/Analysis.R
# The Raw data required to trun this script are not provided, please look at
# the README file in 1_Obtain_and_pocess_raw_data for infromation about getting
# these data. The XLS file in data directory must be kept.
#
# User must have installed the package in 2_R_package, the package gdata and
# the package lubridate (see README in 2_R_package and
# https://www.r-bloggers.com/installing-r-packages/)
#
# These script will owerite existing data sets provided in 4_Run_analysis/rds_data
# directory


# This will automatically set the working directory in RStudio
# If not using RStudio you should put this value to the directory
# containing this file
wd=dirname(rstudioapi::getActiveDocumentContext()$path)

setwd(wd)
rm(list=ls())

library(gdata)
library(lubridate)

library(swisswatertemp)

########################################
########################################
###### LOADING AND PRE-PROCESSING ######
########################################
########################################

#Water stations list to be loaded
rivers_stations=list(
  #BAFU
  list("name"="Aare-Bern_Schonau.smet",
       "start"=1972,"end"=2018,"meteo"=c("BER","INT")),
  list("name"="Aare-Brienzwiler.smet",
       "start"=1971,"end"=2018,"meteo"=c("GRH","MER")),
  list("name"="Aare-Brugg.smet",
       "start"=1963,"end"=2017,"meteo"=c("SMA","WYN")),
  list("name"="Aare-Brugg_Aegerten.smet",
       "start"=1989,"end"=2018,"meteo"=c("BER","MUB","PAY","NEU")),
  list("name"="Aare-Hagneck.smet",
       "start"=1984,"end"=2018,"meteo"=c("BER","MUB")),
  list("name"="Aare-Ringgenberg_Goldswil.smet",
       "start"=1964,"end"=2018,"meteo"=c("MER","INT")),
  list("name"="Aare-Thun.smet",
       "start"=1971,"end"=2018,"meteo"=c("MER","INT")),
  list("name"="Arve-Geneve_Bout_du_Monde.smet",
       "start"=1969,"end"=2018,"meteo"=c("GVE")),
  list("name"="Birs-Munchenstein_Hofmatt.smet",
       "start"=1972,"end"=2018,"meteo"=c("BAS","DEM")),
  list("name"="Broye-Payerne_Caserne_d_aviation.smet",
       "start"=1976,"end"=2018,"meteo"=c("PAY")),
  list("name"="Emme-Emmenmatt_nur_Hauptstation.smet",
       "start"=1976,"end"=2018,"meteo"=c("LAG","NAP")),
  list("name"="Engelberger_Aa-Buochs_Flugplatz.smet",
       "start"=1983,"end"=2018,"meteo"=c("ENG")),
  list("name"="Glatt-Rheinsfelden.smet",
       "start"=1977,"end"=2018,"meteo"=c("SMA","KLO")),
  list("name"="Inn-S-Chanf.smet",
       "start"=1999,"end"=2018,"meteo"=c("SAM","SIA","BEH")),
  list("name"="Kleine_Emme-Emmen.smet",
       "start"=1973,"end"=2018,"meteo"=c("LUZ","NAP")),
  list("name"="Limmat-Baden_Limmatpromenade.smet",
       "start"=1969,"end"=2018,"meteo"=c("SMA","WAE")),
  list("name"="Linth-Mollis_Linthbrucke.smet",
       "start"=1964,"end"=2018,"meteo"=c("GLA","ELM")),
  list("name"="Linth-Weesen_Biaesche.smet",
       "start"=1964,"end"=2018,"meteo"=c("GLA","ELM","RAG")),
  list("name"="Lonza-Blatten.smet",
       "start"=1967,"end"=2018,"meteo"=c("ABO","GRH")),
  list("name"="Lutschine-Gsteig.smet",
       "start"=1964,"end"=2018,"meteo"=c("INT","GRH")),
  list("name"="Muota-Ingenbohl.smet",
       "start"=1974,"end"=2018,"meteo"=c("ALT")),
  list("name"="Reuss-Luzern_Geissmattbrucke.smet",
       "start"=1973,"end"=2018,"meteo"=c("LUZ")),
  list("name"="Reuss-Mellingen.smet",
       "start"=1969,"end"=2018,"meteo"=c("SMA","LUZ")),
  list("name"="Reuss-Seedorf.smet",
       "start"=1971,"end"=2018,"meteo"=c("ALT")),
  list("name"="Rhein-Diepoldsau_Rietbrucke.smet",
       "start"=1970,"end"=2018,"meteo"=c("RAG","VAD","CHU")),
  list("name"="Rhein-Rekingen.smet",
       "start"=1969,"end"=2018,"meteo"=c("HLL","KLO")),
  list("name"="Rhein-Rheinfelden_Messstation.smet",
       "start"=1971,"end"=2018,"meteo"=c("BAS","KLO")),
  list("name"="Rhone-Chancy_Aux_Ripes.smet",
       "start"=1971,"end"=2017,"meteo"=c("GVE")),
  list("name"="Rhone-Porte_du_Scex.smet",
       "start"=1968,"end"=2018,"meteo"=c("SIO","GSB")),
  list("name"="Rhone-Sion.smet",
       "start"=1974,"end"=2018,"meteo"=c("SIO","GRC","GRH")),
  list("name"="Thur-Andelfingen.smet",
       "start"=1963,"end"=2018,"meteo"=c("KLO","SAE","STG")),
  list("name"="Ticino-Riazzino.smet",
       "start"=1997,"end"=2017,"meteo"=c("SBE","OTL")),
  list("name"="Worble-Ittigen.smet",
       "start"=1989,"end"=2018,"meteo"=c("BER")),
  #AWA
  list("name"="A017-Kander_Frutigen.smet",
       "start"=1995,"end"=2018,"meteo"=c("ABO")),
  list("name"="A019-Alte_Aare_Lyss.smet",
       "start"=1997,"end"=2018,"meteo"=c("MUB","BER")),
  list("name"="A022-Suze_Villeret.smet",
       "start"=1995,"end"=2018,"meteo"=c("CDF","CHA")),
  list("name"="A025-Langete_Roggwil.smet",
       "start"=1996,"end"=2018,"meteo"=c("KOP","WYN")),
  list("name"="A029-Onz_Heimenhausen.smet",
       "start"=1995,"end"=2018,"meteo"=c("KOP","WYN")),
  list("name"="A031-Osch_Koppigen.smet",
       "start"=1997,"end"=2018,"meteo"=c("KOP")),
  list("name"="A047-Sagibach_Worben.smet",
       "start"=1996,"end"=2018,"meteo"=c("MUB","BER")),
  list("name"="A049-Raus_Moutier.smet",
       "start"=1997,"end"=2018,"meteo"=c("DEM")),
  list("name"="A062-Chrouchtalb_Kraucht.smet",
       "start"=1999,"end"=2018,"meteo"=c("BER")),
  list("name"="A070-Luterbach_Oberburg.smet",
       "start"=1999,"end"=2018,"meteo"=c("BER")),
  #AWEL
  list("name"="ZH517_Kempt-Illnau.smet",
       "start"=1999,"end"=2018,"meteo"=c("SMA","TAE")),
  list("name"="ZH520_Toss-Ramismuhle.smet",
       "start"=1999,"end"=2018,"meteo"=c("SMA","TAE")),
  list("name"="ZH522_Eulach-Winterthur.smet",
       "start"=1999,"end"=2018,"meteo"=c("SMA","TAE")),
  list("name"="ZH527_Aabach-Monchaltorf.smet",
       "start"=1999,"end"=2018,"meteo"=c("SMA")),
  list("name"="ZH531_Glatt-Wuhrbrucke.smet",
       "start"=1999,"end"=2018,"meteo"=c("SMA")),
  list("name"="ZH534_Glatt-Rumlang.smet",
       "start"=1999,"end"=2018,"meteo"=c("SMA")),
  list("name"="ZH547_Sihl-Blattwag.smet",
       "start"=1999,"end"=2018,"meteo"=c("WAE","EIN")),
  list("name"="ZH570_Toss-Freienstein.smet",
       "start"=1999,"end"=2018,"meteo"=c("SMA","TAE")),
  list("name"="ZH572_Reppisch-Dietikon.smet",
       "start"=1999,"end"=2018,"meteo"=c("SMA"))

)

# Add meteo station to list, only station required coording to meteo
# specified in bafu station
used_meteosuisse_stations=c()
for (station in rivers_stations){
  used_meteosuisse_stations=c(used_meteosuisse_stations,(station$meteo))
}
used_meteosuisse_stations=unique(used_meteosuisse_stations)
length(used_meteosuisse_stations)
meteo_stations=list()
i=1
for (station in used_meteosuisse_stations)
{
  meteo_stations[[i]]<-list("name"=paste0(station,".smet"))
  i=i+1
}

# Load raw data, set lar argument to TRUE to laod from saved RData.
rivers_data=get_data(rivers_stations,"data/","WATER",FALSE)
meteo_data=get_data(meteo_stations,"meteo/data/","METEO",FALSE)

# Save data as RData file for fast load at next usage
# The target directory must exist
# saveRDS(rivers_data,file="rds_data/rivers.RDS")
# saveRDS(meteo_data,file="rds_data/meteo.RDS")


#############################################
#############################################
############ PREPROCESSING DATA #############
#############################################
#############################################

### Hysteresis data
# Define period of averaging (in yr) and smooting time (in days)
period=10
smoothing=30
rivers_data=get_hysteresis_data(rivers_data,period,smoothing)

### STL data
# Produce plots to decide on STL value to use
# Use le full table to test all values used in the paper, long computation time
#s_windows=c(7,9,13,17,21,25,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57,59,65,75,95)
s_windows=c(37)

STL_output_t_daily <- get_STL_analysis(rivers_data,meteo_data,"T",s_windows,frequency="daily")

STL_output_q_daily <- get_STL_analysis(rivers_data,meteo_data,"Q",s_windows,frequency="daily")

STL_output_t_daily <- get_remainder_analysis(STL_output_t_daily,s_windows,"post_seasons_loess")

STL_output_q_daily <- get_remainder_analysis(STL_output_q_daily,s_windows,"post_seasons_loess")

# Uncomment to plot the remainder analysis if multiple s_windows values are used
# Plot will be saved in 3_Produce_data/plots/remainder_analysis
# This creates the plots shown in Figures S7 and S8 in supplementary.
#dir.create("plots", showWarnings = FALSE)
#plot_remainder_analysis(STL_output_t_daily,s_windows,"post_seasons_loess","T")
#plot_remainder_analysis(STL_output_q_daily,s_windows,"post_seasons_loess","Q")

############################
############################
### BUILD FINAL DATA SET ###
############################
############################

############################
#### ADD STL DO DATA SET ###
############################

# Choosen s_window value
st="37"
sq="37"

for (station in names(rivers_data)){

  rivers_data[[station]][["STL"]]=list()
  rivers_data[[station]][["STL"]][["T"]]=list()
  rivers_data[[station]][["STL"]][["T"]]$timestamp=
    as.vector(time(STL_output_t_daily[[station]]$post_seasons_loess[[st]]$time.series))
  rivers_data[[station]][["STL"]][["T"]]$seasonal=
    as.vector(STL_output_t_daily[[station]]$post_seasons_loess[[st]]$time.series[,1])
  rivers_data[[station]][["STL"]][["T"]]$trend=
    as.vector(STL_output_t_daily[[station]]$post_seasons_loess[[st]]$time.series[,2])
  rivers_data[[station]][["STL"]][["T"]]$remainder=
    as.vector(STL_output_t_daily[[station]]$post_seasons_loess[[st]]$time.series[,3])
  rivers_data[[station]][["STL"]][["T"]]$raw=
    as.vector(STL_output_t_daily[[station]]$post_seasons_loess[[st]]$time.series[,4])
  rivers_data[[station]][["STL"]][["T"]]$acf=
    as.vector(STL_output_t_daily[[station]][["remainder_analysis"]]$post_seasons_loess[[st]]$acf)
  rivers_data[[station]][["STL"]][["T"]]$pacf=
    as.vector(STL_output_t_daily[[station]][["remainder_analysis"]]$post_seasons_loess[[st]]$pacf)

  rivers_data[[station]][["STL"]][["Q"]]=list()
  rivers_data[[station]][["STL"]][["Q"]]$timestamp=
    as.vector(time(STL_output_q_daily[[station]]$post_seasons_loess[[sq]]$time.series))
  rivers_data[[station]][["STL"]][["Q"]]$seasonal=
    as.vector(STL_output_q_daily[[station]]$post_seasons_loess[[sq]]$time.series[,1])
  rivers_data[[station]][["STL"]][["Q"]]$trend=
    as.vector(STL_output_q_daily[[station]]$post_seasons_loess[[sq]]$time.series[,2])
  rivers_data[[station]][["STL"]][["Q"]]$remainder=
    as.vector(STL_output_q_daily[[station]]$post_seasons_loess[[sq]]$time.series[,3])
  rivers_data[[station]][["STL"]][["Q"]]$raw=
    as.vector(STL_output_q_daily[[station]]$post_seasons_loess[[sq]]$time.series[,4])
  rivers_data[[station]][["STL"]][["Q"]]$acf=
    as.vector(STL_output_q_daily[[station]][["remainder_analysis"]]$post_seasons_loess[[sq]]$acf)
  rivers_data[[station]][["STL"]][["Q"]]$pacf=
    as.vector(STL_output_q_daily[[station]][["remainder_analysis"]]$post_seasons_loess[[sq]]$pacf)

  rivers_data[[station]][["STL"]][["meteo"]]=list()

  for (meteo_station in rivers_data[[station]][["meteo"]])
  {
    rivers_data[[station]][["STL"]][["meteo"]][[meteo_station]]=list()

    rivers_data[[station]][["STL"]][["meteo"]][[meteo_station]][["TA"]]=list()

    rivers_data[[station]][["STL"]][["meteo"]][[meteo_station]][["TA"]]$timestamp=
      as.vector(time(STL_output_t_daily[[station]]$post_seasons_loess$meteo[[meteo_station]][[st]]$time.series))

    rivers_data[[station]][["STL"]][["meteo"]][[meteo_station]][["TA"]]$seasonal=
      as.vector(STL_output_t_daily[[station]]$post_seasons_loess$meteo[[meteo_station]][[st]]$time.series[,1])
    rivers_data[[station]][["STL"]][["meteo"]][[meteo_station]][["TA"]]$trend=
      as.vector(STL_output_t_daily[[station]]$post_seasons_loess$meteo[[meteo_station]][[st]]$time.series[,2])
    rivers_data[[station]][["STL"]][["meteo"]][[meteo_station]][["TA"]]$remainder=
      as.vector(STL_output_t_daily[[station]]$post_seasons_loess$meteo[[meteo_station]][[st]]$time.series[,3])
    rivers_data[[station]][["STL"]][["meteo"]][[meteo_station]][["TA"]]$raw=
      as.vector(STL_output_t_daily[[station]]$post_seasons_loess$meteo[[meteo_station]][[st]]$time.series[,4])
    rivers_data[[station]][["STL"]][["meteo"]][[meteo_station]][["TA"]]$acf=
      as.vector(STL_output_t_daily[[station]][["remainder_analysis"]]$post_seasons_loess[[st]]$meteo[[meteo_station]]$acf)
    rivers_data[[station]][["STL"]][["meteo"]][[meteo_station]][["TA"]]$pacf=
      as.vector(STL_output_t_daily[[station]][["remainder_analysis"]]$post_seasons_loess[[st]]$meteo[[meteo_station]]$pacf)
    rivers_data[[station]][["STL"]][["meteo"]][[meteo_station]][["TA"]]$ccf=
      as.vector(STL_output_t_daily[[station]][["remainder_analysis"]]$post_seasons_loess[[st]]$meteo[[meteo_station]]$ccf)

    rivers_data[[station]][["STL"]][["meteo"]][[meteo_station]][["P"]]=list()

    rivers_data[[station]][["STL"]][["meteo"]][[meteo_station]][["P"]]$timestamp=
      as.vector(time(STL_output_q_daily[[station]]$post_seasons_loess$meteo[[meteo_station]][[sq]]$time.series))
    rivers_data[[station]][["STL"]][["meteo"]][[meteo_station]][["P"]]$seasonal=
      as.vector(STL_output_q_daily[[station]]$post_seasons_loess$meteo[[meteo_station]][[sq]]$time.series[,1])
    rivers_data[[station]][["STL"]][["meteo"]][[meteo_station]][["P"]]$trend=
      as.vector(STL_output_q_daily[[station]]$post_seasons_loess$meteo[[meteo_station]][[sq]]$time.series[,2])
    rivers_data[[station]][["STL"]][["meteo"]][[meteo_station]][["P"]]$remainder=
      as.vector(STL_output_q_daily[[station]]$post_seasons_loess$meteo[[meteo_station]][[sq]]$time.series[,3])
    rivers_data[[station]][["STL"]][["meteo"]][[meteo_station]][["P"]]$raw=
      as.vector(STL_output_q_daily[[station]]$post_seasons_loess$meteo[[meteo_station]][[sq]]$time.series[,4])
    rivers_data[[station]][["STL"]][["meteo"]][[meteo_station]][["P"]]$acf=
      as.vector(STL_output_q_daily[[station]][["remainder_analysis"]]$post_seasons_loess[[sq]]$meteo[[meteo_station]]$acf)
    rivers_data[[station]][["STL"]][["meteo"]][[meteo_station]][["P"]]$pacf=
      as.vector(STL_output_q_daily[[station]][["remainder_analysis"]]$post_seasons_loess[[sq]]$meteo[[meteo_station]]$pacf)
    rivers_data[[station]][["STL"]][["meteo"]][[meteo_station]][["P"]]$ccf=
      as.vector(STL_output_q_daily[[station]][["remainder_analysis"]]$post_seasons_loess[[sq]]$meteo[[meteo_station]]$ccf)
  }
}

#########################################
#### ADD LINEAR REGRESSION TO DATASET ###
#########################################

#### STL ####

get_lm_STL <- function(station)
{
  lm=list()
  periods=list(c(1999,2018),c(1979,1998),c(1979,2018),c(1970,2018))
  for(p in periods){
    period=paste0(p[1],"-",p[2])
    sel=which(station$timestamp>=p[1] & station$timestamp<(p[2]+1))
    len=(p[2]-p[1]+1)
    if(length(sel)==len*365|| length(sel)==(len-1)*365 || length(sel)==(len-2)*365 ){
      lin_mod=lm(station$trend[sel] +station$remainder[sel] ~ station$timestamp[sel])
      lm[[period]]=get_lm_summary(lin_mod)

      # Estimate uncertainty by removing a year at the gegining and at then end
      # of the period
      sel_1=which(station$timestamp>=p[1]+1 & station$timestamp<(p[2]+1))
      sel_2=which(station$timestamp>=p[1] & station$timestamp<(p[2]))
      lin_mod_1=lm(station$trend[sel_1] +station$remainder[sel_1] ~ station$timestamp[sel_1])
      lin_mod_2=lm(station$trend[sel_2] +station$remainder[sel_2] ~ station$timestamp[sel_2])
      sum_1=get_lm_summary(lin_mod_1)
      sum_2=get_lm_summary(lin_mod_2)
      err=max(abs(lm[[period]]$trend-sum_1$trend),abs(lm[[period]]$trend-sum_2$trend))
      lm[[period]][["trend_std"]]=err

      lm[[period]][["timestamp"]]=station$timestamp[sel]
      lm[[period]][["values"]]=station$trend[sel] +station$remainder[sel]
      lm[[period]][["printable"]]=get_lm_summary_printable(lin_mod)
    }
  }
  return(lm)
}

for (station in names(rivers_data))
{
  rivers_data[[station]][["STL"]][["T"]][["lm"]]=get_lm_STL(rivers_data[[station]][["STL"]][["T"]])
  rivers_data[[station]][["STL"]][["Q"]][["lm"]]=get_lm_STL(rivers_data[[station]][["STL"]][["Q"]])
  for (meteo_station in rivers_data[[station]][["meteo"]])
  {
    rivers_data[[station]][["STL"]][["meteo"]][[meteo_station]][["TA"]][["lm"]]=
      get_lm_STL(rivers_data[[station]][["STL"]][["meteo"]][[meteo_station]][["TA"]])
    rivers_data[[station]][["STL"]][["meteo"]][[meteo_station]][["P"]][["lm"]]=
      get_lm_STL(rivers_data[[station]][["STL"]][["meteo"]][[meteo_station]][["P"]])
  }
}

#### ADD METEO DATA ###

for (station in names(rivers_data))
{
  meteo_list=rivers_data[[station]]$meteo
  rivers_data[[station]]$meteo=list()
  for (meteo_station in meteo_list)
  {
    rivers_data[[station]]$meteo[[meteo_station]]=meteo_data[[meteo_station]]
  }
}

########################################
#### CUT AND ADD LM TO AVERAGED DATA ###
########################################

get_lm <- function(station)
{
  lm=list()
  periods=list(c(1999,2018),c(1979,1998),c(1979,2018),c(1970,2018))
  for(p in periods)
  {
    period=paste0(p[1],"-",p[2])
    len=(p[2]-p[1]+1)
    sel=which(floor(station$timestamp)>=p[1] & floor(station$timestamp)<=p[2])
    if(length(sel)==len || length(sel)==len*12 || length(sel)==(len-1) ||
       length(sel)==(len-1)*12 || length(sel)==(len-2) || length(sel)==(len-2)*12){
      lin_mod=lm(station$values[sel] ~ station$timestamp[sel])
      lm[[period]]=get_lm_summary(lin_mod)

      # Estimate uncertainty by removing a year at the gegining and at then end
      # of the period
      sel_1=which(station$timestamp>=p[1]+1 & station$timestamp<(p[2]+1))
      sel_2=which(station$timestamp>=p[1] & station$timestamp<(p[2]))
      lin_mod_1=lm(station$values[sel_1] ~ station$timestamp[sel_1])
      lin_mod_2=lm(station$values[sel_2] ~ station$timestamp[sel_2])
      sum_1=get_lm_summary(lin_mod_1)
      sum_2=get_lm_summary(lin_mod_2)
      err=max(abs(lm[[period]]$trend-sum_1$trend),abs(lm[[period]]$trend-sum_2$trend))
      lm[[period]][["trend_std"]]=err

      lm[[period]][["timestamp"]]=station$timestamp[sel]
      lm[[period]][["values"]]=station$values[sel]
      lm[[period]][["printable"]]=get_lm_summary_printable(lin_mod)
    }
  }
  return(lm)
}

for (station in names(rivers_data))
{
  for (period in c("monthly","yearly","DJF","MAM","JJA","SON")){
    tmp=list()
    for (var in c("T","Q")){
      tmp[[var]]=list()
      rivers_data[[station]][[period]][[var]]$timestamp=
        decimal_date(rivers_data[[station]][[period]][[var]]$timestamp)
      if(period!="monthly"){
        rivers_data[[station]][[period]][[var]]$timestamp=floor(rivers_data[[station]][[period]][[var]]$timestamp)
      }
      tmp[[var]][["timestamp"]]=rivers_data[[station]][[period]][[var]]$timestamp
      tmp[[var]][["values"]]=rivers_data[[station]][[period]][[var]]$values
      tmp[[var]][["lm"]]=get_lm(rivers_data[[station]][[period]][[var]])
    }
    rivers_data[[station]][[period]]=tmp
  }
  for (meteo_station in names(rivers_data[[station]]$meteo))
  {
    keep=list()
    for (period in c("monthly","yearly","DJF","MAM","JJA","SON")){
      tmp=list()
      for (var in c("TA","P")){
        tmp_var=var
        if (var=="TA" && "TA_HOM" %in% names(rivers_data[[station]]$meteo[[meteo_station]][[period]]))
        {
          tmp_var="TA_HOM"
        }
        if (var=="P" && "P_HOM" %in% names(rivers_data[[station]]$meteo[[meteo_station]][[period]]))
        {
          tmp_var="P_HOM"
        }
        tmp[[var]]=list()
        rivers_data[[station]]$meteo[[meteo_station]][[period]][[tmp_var]]$timestamp=
          decimal_date(rivers_data[[station]]$meteo[[meteo_station]][[period]][[tmp_var]]$timestamp)
        if(period!="monthly"){
          rivers_data[[station]]$meteo[[meteo_station]][[period]][[tmp_var]]$timestamp=
            floor(rivers_data[[station]]$meteo[[meteo_station]][[period]][[tmp_var]]$timestamp)
        }
        tmp[[var]][["timestamp"]]= rivers_data[[station]]$meteo[[meteo_station]][[period]][[tmp_var]]$timestamp
        tmp[[var]][["values"]]=rivers_data[[station]]$meteo[[meteo_station]][[period]][[tmp_var]]$values
        tmp[[var]][["lm"]]=get_lm(rivers_data[[station]]$meteo[[meteo_station]][[period]][[tmp_var]])
      }
      rivers_data[[station]]$meteo[[meteo_station]][[period]]=tmp
    }
  }
}


########################################
#### SET HEADER VALUES FROM XLS FILE ###
########################################

stations_info = as.data.frame(read.xls("data/discharge_gauging_station.xlsx", sheet = 1, header = TRUE))
for (i in c(1:length(stations_info))){
  stations_info[,i]=as.character(stations_info[,i])
}
col_names=as.character(stations_info[1,])
stations_info=stations_info[c(3:length(stations_info[,1])),]
colnames(stations_info)=col_names
for (station in names(rivers_data))
{
  line=which(stations_info$Filename==station)
  rivers_data[[station]]$header[["abbr"]]=stations_info[line,"Abbreviation"]
  rivers_data[[station]]$header[["easting"]]=as.numeric(stations_info[line,"X"])
  rivers_data[[station]]$header[["northing"]]=as.numeric(stations_info[line,"Y"])
  rivers_data[[station]]$header[["altitude"]]=as.numeric(stations_info[line,"Z"])
  rivers_data[[station]]$header[["operator"]]=stations_info[line,"Operator"]
  rivers_data[[station]]$header[["river"]]=stations_info[line,"River"]
  rivers_data[[station]]$header[["area"]]=stations_info[line,"Area [km2]"]
  rivers_data[[station]]$header[["mean_elevation"]]=stations_info[line,"Mean basin \nelevation [m]"]
  rivers_data[[station]]$header[["glacier_percent"]]=stations_info[line,"Glacier \npercent"]
  rivers_data[[station]]$header[["regime1"]]=stations_info[line,"Hydrological regime\n(name)"]
  rivers_data[[station]]$header[["regime2"]]=stations_info[line,"Regime 2"]
  rivers_data[[station]]$header[["regime3"]]=
    stations_info[line,"Hydrological regime\n (following Aschwanden 1985, different from HADES 5.2)"]
}


####################################
####################################
### LOAD 1H RESOLUTION BAFU DATA ###
####################################
####################################

rivers_stations_1H=c("Engelberger_Aa-Buochs_Flugplatz",
                     "Aare-Bern_Schonau",
                     "Aare-Brienzwiler",
                     "Aare-Brugg_Aegerten",
                     "Aare-Brugg",
                     "Aare-Hagneck",
                     "Aare-Ringgenberg_Goldswil",
                     "Aare-Thun",
                     "Arve-Geneve_Bout_du_Monde",
                     "Birs-Munchenstein_Hofmatt",
                     "Broye-Payerne_Caserne_d_aviation",
                     "Emme-Emmenmatt_nur_Hauptstation",
                     "Glatt-Rheinsfelden",
                     "Inn-S-Chanf",
                     "Limmat-Baden_Limmatpromenade",
                     "Linth-Mollis_Linthbrucke",
                     "Linth-Weesen_Biaesche",
                     "Lonza-Blatten",
                     "Lutschine-Gsteig",
                     "Muota-Ingenbohl",
                     "Reuss-Luzern_Geissmattbrucke",
                     "Reuss-Mellingen",
                     "Reuss-Seedorf",
                     "Rhein-Diepoldsau_Rietbrucke",
                     "Rhein-Rekingen",
                     "Rhone-Chancy_Aux_Ripes",
                     "Rhone-Porte_du_Scex",
                     "Thur-Andelfingen",
                     "Ticino-Riazzino",
                     "Worble-Ittigen")
rivers_data_1h=list()
for (station in rivers_stations_1H)
{
  rivers_data_1h[[station]]<-get_file_data_only(paste0("data_1h/",station,".smet"),1950,2019)
  print(station)
  line=which(stations_info$Filename==station)
  rivers_data_1h[[station]]$header[["abbr"]]=stations_info[line,"Abbreviation"]
  rivers_data_1h[[station]]$header[["easting"]]=as.numeric(stations_info[line,"X"])
  rivers_data_1h[[station]]$header[["northing"]]=as.numeric(stations_info[line,"Y"])
  rivers_data_1h[[station]]$header[["altitude"]]=as.numeric(stations_info[line,"Z"])
  rivers_data_1h[[station]]$header[["operator"]]=stations_info[line,"Operator"]
  rivers_data_1h[[station]]$header[["river"]]=stations_info[line,"River"]
  rivers_data_1h[[station]]$header[["area"]]=stations_info[line,"Area [km2]"]
  rivers_data_1h[[station]]$header[["mean_elevation"]]=stations_info[line,"Mean basin \nelevation [m]"]
  rivers_data_1h[[station]]$header[["glacier_percent"]]=stations_info[line,"Glacier \npercent"]
  rivers_data_1h[[station]]$header[["regime1"]]=stations_info[line,"Hydrological regime\n(name)"]
  rivers_data_1h[[station]]$header[["regime2"]]=stations_info[line,"Regime 2"]
  rivers_data_1h[[station]]$header[["regime3"]]=
    stations_info[line,"Hydrological regime\n (following Aschwanden 1985, different from HADES 5.2)"]
}


#######################################
#######################################
### LOAD HOMEGENOUS METEOSWISS DATA ###
#######################################
#######################################

meteo_stat=c("ALT","BAS","BER","CDF","ELM","ENG","GVE","LUZ","MER","NEU","OTL",
             "SAE","SAM","SIA","SIO","SMA","STG")
i=1
meteo_stations=list()
for (station in meteo_stat)
{
  meteo_stations[[i]]<-list("name"=paste0(station,".smet"))
  i=i+1
}
meteo_data_hom=get_data(meteo_stations,"meteo/data_hom/","METEO",FALSE)


#################
#################
### SAVE DATA ###
#################
#################

saveRDS(rivers_data,file="../4_Run_analysis/data/rds_data/dataset.RDS")
saveRDS(rivers_data_1h,file="../4_Run_analysis/data/rds_data/dataset_1h.RDS")
saveRDS(meteo_data_hom,file="../4_Run_analysis/data/rds_data/dataset_meteo_hom.RDS")
