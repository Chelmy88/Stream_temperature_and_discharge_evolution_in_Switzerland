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

#################################
#################################
#### STRUCTURE OF THE DATASET ###
#################################
#################################

# ["station name"]
# |--header
#   |--station_id = station number
#   |--station_name = station name
#   |--latitude: WG94 latitude
#   |--longitude: WG94 longitude
#   |--easting: CH1903 easting
#   |--northing: CH1903 nothing
#   |--altitude: altitude of the station
#   |--operator: source of the data
#   |--river: name of the river
#   |--area: area of the catchment of the station
#   |--mean_elevation: mean elevation of the catchment
#   |--glacier_percent: percentage of the catchment glacier covered
#   |--regime1: hydrological regime (classical)
#   |--regime2: hydrological regime with regards to location
#   |--regime3:Hydrological regime (following Aschwanden 1985, different from HADES 5.2)
#   |--nodata: no data value used
#   |--tz: timezone
#   |--fields: variables in the [data] table
# |--data: raw data
#   |--timestamp: timestamp of the measurement as R date
#   |--T: measured temperature (°C)
#   |--Q: measured discharge (m3/s)
# |--[monthly, yearly, DJF, MAM, JJA or SON]: data averaged over the given period
#   |--[T Q]
#     |--timestamp: timestamp in decimal year
#     |--values: raw data averaged over the indicated period
#     |--lm: output from linear model applied to trend + remainder
#       |--["1999-2018" "1979-1998" "1979-2018" "1970-2018"] Periods over which trend is calculated, not necessarily all available
#         |--timestamp: timestamp over the used period
#         |--values: raw data over the given period
#         |--trend: slope from linear model
#         |--trend_std: std error the trend value
#         |--trend_p: p_value the trend value
#         |--intercept: intercept value from linear model
#         |--intercept_std: std error the intercept value
#         |--intercept_p: p_value the intercept value
#         |--r_squared: r^2
#         |--adj_r_squared: adjusted r^2
#         |--printable:
#           |--[trend, trend_std, trend_p, intercept, intercept_std, intercept_p, r_squared, adj_r_squared]: Same value as above but as string in "e" notation for display
# |--hysteresis:
#   |--[daily_mean or daily_mean_smoothed]: daily decadal mean, with or without smoothing (smoothed data is used in QT plots)
#     |--["from_to" in years, e.g. "2009_1018"]
#       |--T: temperature values(°C), 365 values
#       |--Q: discharge values(m3/s), 365 values
# |--meteo: attached meteo data
#   |--[[station name]]
#     |--header:
#       |--station_id = station ID
#       |--station_name = station name, same as ID
#       |--latitude: WG94 latitude
#       |--longitude: WG94 longitude
#       |--easting: CH1903 easting
#       |--northing: CH1903 nothing
#       |--altitude: altitude of the station
#       |--nodata: no data value used
#       |--source: source of the meteodata
#       |--tz: timezone
#       |--fields: variables in the [data] table
#     |--data: raw meteo data
#       |--timestamp: timestamp of the measurement as R date
#       |--[TA, P, TA_HOM, P_HOM, HS6, HS18, HSAUTO6, HSAUTO18]: available meteo variables
#     |--[monthly, yearly, DJF, MAM, JJA or SON]: data averaged over the given period
#       |--[TA,P]
#         |--timestamp: timestamp in decimal year
#         |--values: raw data averaged over the indicated period
#         |--lm: output from linear model applied to trend + remainder
#           |--["1999-2018" "1979-1998" "1979-2018" "1970-2018"] Periods over which trend is calculated, not necessarily all available
#             |--timestamp: timestamps over the used period
#             |--values: raw data over the given period
#             |--trend: slope from linear model
#             |--trend_std: std error the trend value
#             |--trend_p: p_value the trend value
#             |--intercept: intercept value from linear model
#             |--intercept_std: std error the intercept value
#             |--intercept_p: p_value the intercept value
#             |--r_squared: r^2
#             |--adj_r_squared: adjusted r^2
#             |--printable:
#               |--[trend, trend_std, trend_p, intercept, intercept_std, intercept_p, r_squared, adj_r_squared]: Same value as above but as string in "e" notation for display
# |--STL
#   |--[T or Q]
#     |--timestamp: date, in decimal years
#     |--seasonal: seasonal component from STL
#     |--trend: trend from STL
#     |--remainder: remainders from STL
#     |--raw: raw data used for STL
#     |--acf: acf analysis as R acf object
#     |--pacf: pacf analysis as R pacf object
#     |--lm: output from linear model applied to trend + remainder
#       |--["1999-2018" "1979-1998" "1979-2018" "1970-2018"] Periods over which trend is calculated, not necessarily all available
#         |--timestamp: timestamp over the used period
#         |--values: raw data over the given period
#         |--trend: slope from linear model
#         |--trend_std: std error the trend value
#         |--trend_p: p_value the trend value
#         |--intercept: intercept value from linear model
#         |--intercept_std: std error the intercept value
#         |--intercept_p: p_value the intercept value
#         |--r_squared: r^2
#         |--adj_r_squared: adjusted r^2
#         |--printable:
#           |--[trend, trend_std, trend_p, intercept, intercept_std, intercept_p, r_squared, adj_r_squared]: Same value as above but as string in "e" notation for display
#   |--meteo
#     |--[station name]
#       |--[TA or P]
#         |--timestamp: date, in decimal years
#         |--seasonal: seasonal component from STL
#         |--trend: trend from STL
#         |--remainder: remainders from STL
#         |--raw: raw data used for STL
#         |--acf: acf analysis as R acf object
#         |--pacf: pacf analysis as R pacf object
#         |--ccf: ccf analysis (between meteo and river data T-TA and Q-P) as R ccf object
#         |--lm: output from linear model applied to trend + remainder
#           |--["1999-2018" "1979-1998" "1979-2018" "1970-2018"] Periods over which trend is calculated, not necessarily all available
#             |--timestamp: timestamp over the used period
#             |--values: raw data over the given period
#             |--trend: slope from linear model
#             |--trend_std: std error the trend value
#             |--trend_p: p_value the trend value
#             |--intercept: intercept value from linear model
#             |--intercept_std: std error the intercept value
#             |--intercept_p: p_value the intercept value
#             |--r_squared: r^2
#             |--adj_r_squared: adjusted r^2
#             |--printable:
#               |--[trend, trend_std, trend_p, intercept, intercept_std, intercept_p, r_squared, adj_r_squared]: Same value as above but as string in "e" notation for display

###############################
###############################
#### HOW TO USE THE DATASET ###
###############################
###############################

# Entries can be accessed following the structure describes above
# and with double brackets [["entry name here"]] (the name shoulb be
# between quote marks), or with the "$" signe (in this case no quote
# mark is needed except in the names contains special character).
#
# If a variable containing the name of the entry to be accesses is
# used, double brakets shoudl be used [[var]], note that $var will
# not work (text after $ is taken as string, i.e. variable will not
# be accessed).
#
# Examples:
# 1) rivers_data[["Aare-Brienzwiler"]][["STL"]][["meteo"]][["GRH"]][["TA"]][["trend"]]
#    or
#    rivers_data$"Aare-Brienzwiler"$STL$meteo$GRH$TA$trend
#    are quivalent and return the trend component of the meteo station GRH linked
#    to the Aare-Brienzwiler water station.
# 2) Note that the function "names" is useful to retrieve the next entries at a
#    given entry level. E.g. names(rivers_data$"Aare-Brienzwiler"$meteo) returns
#    a list of the names of meteo station attached to the Aare-Brienzwiler river
#    station.
# 3) for (river_station in names(rivers_data))
#    will loop over all stations names which are stored in river_station. Data can
#    be thus accessed through: rivers_data[[river_station]]$...
#    For example rivers_data[[river_station]]$header$mean_elevation, if in the above
#    loop, will return the mean elevation for each catchment.


wd=dirname(rstudioapi::getActiveDocumentContext()$path)

setwd(wd)
rm(list=ls())

library(swisswatertemp)

rivers_data=readRDS(file="data/rds_data/dataset.RDS")
rivers_data_1h=readRDS(file="data/rds_data/dataset_1h.RDS")
meteo_data_hom=readRDS(file="data/rds_data/dataset_meteo_hom.RDS")

dir.create("plots/", showWarnings = FALSE)
dir.create("plots/analysis", showWarnings = FALSE)

#######################################
#######################################
### LOAD HOMEGENOUS METEOSWISS DATA ###
#######################################
#######################################

plot_general_situation(rivers_data,output_type="PDF")

##
## PLOTS FOR EACH SEPARATES STATIONS
##

for (river_station in names(rivers_data)){
  print(paste0("[I] ---Station ", river_station," ---"))
  dir.create(paste0("plots/analysis/",rivers_data[[river_station]]$header$station_name), showWarnings = FALSE)
  ### RAW ###
  plot_time_series(rivers_data[[river_station]],output_type = "PNG")
  ### TERNDS ###
  plot_trends(rivers_data[[river_station]],output_type = "PNG")
  ### STL ###
  plot_stl(rivers_data[[river_station]],output_type = "PDF")
  ### ACF ###
  plot_acf(rivers_data[[river_station]],output_type = "PDF")
  ### Hysteresis ###
  plot_hysteresis(rivers_data[[river_station]],output_type = "PNG")
}

##
## GENERAL COMPARISONS OF TRENDS FOR ALL PERIODS
##
periods=list(c("1999-2018"),c("1979-1998"),c("1979-2018"),c("1970-2018"))
for (period in periods){
  print(period)
  pdf(paste0("plots/general_analysis_period",period,".pdf"),width=12,height=8)
  genreal_analysis_plots(period,rivers_data)
  dev.off()
}

plot_general(rivers_data)

plot_correlations(rivers_data)

plot_yearly_anomalies(rivers_data)

plot_long_term(rivers_data,meteo_data_hom)

plot_lakes(rivers_data)

plot_thresholds(rivers_data,rivers_data_1h)

plot_snow(rivers_data)

plot_alpine(rivers_data)

plot_trends_robustness(rivers_data)

print_all_trends(rivers_data,meteo_data_hom)

