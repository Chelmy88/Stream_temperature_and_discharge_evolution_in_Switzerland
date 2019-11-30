#-*- coding: UTF-8 -*-

# This script is related to the paper:
# "Stream temperature evolution in Switzerland over the last 50 years, Adrien
# Michel, Tristan Brauchli, Michael Lehning, Bettina Schaefli, and Hendrik Huwald,
# HESS, 2019"

# This material is distributed under the GPLv3 license
# (https://www.gnu.org/licenses/gpl-3.0.html)

import os
import glob
import datetime
from datetime import timedelta
import sys
import shapefile
import numpy as np


currDir = os.path.dirname(os.path.realpath(__file__))
input = currDir+'/Raw/8ffa1b9f54fbd83c25d3bae615da4728665749d3/'
output = currDir+'/SMET/Q/'

#Taken from https://github.com/ValentinMinder/Swisstopo-WGS84-LV03/blob/master/scripts/py/wgs84_ch1903.py
def CHtoWGSlat(y, x):
    # Axiliary values (% Bern)
    y_aux = (y - 600000) / 1000000
    x_aux = (x - 200000) / 1000000
    lat = (16.9023892 + (3.238272 * x_aux)) + \
        - (0.270978 * pow(y_aux, 2)) + \
        - (0.002528 * pow(x_aux, 2)) + \
        - (0.0447 * pow(y_aux, 2) * x_aux) + \
        - (0.0140 * pow(x_aux, 3))
        # Unit 10000" to 1" and convert seconds to degrees (dec)
    lat = (lat * 100) / 36
    return lat
    # Convert CH y/x to WGS long
def CHtoWGSlng(y, x):
    # Axiliary values (% Bern)
    y_aux = (y - 600000) / 1000000
    x_aux = (x - 200000) / 1000000
    lng = (2.6779094 + (4.728982 * y_aux) + \
           + (0.791484 * y_aux * x_aux) + \
           + (0.1306 * y_aux * pow(x_aux, 2))) + \
        - (0.0436 * pow(y_aux, 3))
           # Unit 10000" to 1" and convert seconds to degrees (dec)
    lng = (lng * 100) / 36
    return lng


def isfloat(value):
    try:
        float(value)
        return True
    except ValueError:
        return False

print "Scanning available files"
files=[]
Q=[]
N=[]

for file in os.listdir(input):
    if (not os.path.isfile(input+file)) or file.startswith(".DS") or "Abfluss" not in file:
        continue
    files.append(input+file)
    code= file[3:6]
    if code not in N:
        N.append(code)


print N
t_stations = dict()
t_stations["ZH517"] ={"Easting": -999, "Northing": -999, "Name": "Kempt-Illnau", "Altitide": 475.7,"Area": -999}
t_stations["ZH520"] ={"Easting": -999, "Northing": -999, "Name": "Toss-Ramismuhle", "Altitide": 443.8,"Area": -999}
t_stations["ZH522"] ={"Easting": -999, "Northing": -999, "Name": "Eulach-Winterthur", "Altitide": 414.5,"Area": -999}

t_stations["ZH523"] ={"Easting": -999, "Northing": -999, "Name": "Eulach-Wulflingen", "Altitide": 475.7,"Area": -999}
t_stations["ZH527"] ={"Easting": -999, "Northing": -999, "Name": "Aabach-Monchaltorf", "Altitide": 443.8,"Area": -999}
t_stations["ZH531"] ={"Easting": -999, "Northing": -999, "Name": "Glatt-Wuhrbrucke", "Altitide": 414.5,"Area": -999}

t_stations["ZH534"] ={"Easting": -999, "Northing": -999, "Name": "Glatt-Rumlang", "Altitide": 475.7,"Area": -999}
t_stations["ZH547"] ={"Easting": -999, "Northing": -999, "Name": "Sihl-Blattwag", "Altitide": 443.8,"Area": -999}
t_stations["ZH554"] ={"Easting": -999, "Northing": -999, "Name": "Aabach-Niederuster", "Altitide": 414.5,"Area": -999}

t_stations["ZH570"] ={"Easting": -999, "Northing": -999, "Name": "Toss-Freienstein", "Altitide": 475.7,"Area": -999}
t_stations["ZH572"] ={"Easting": -999, "Northing": -999, "Name": "Reppisch-Dietikon", "Altitide": 443.8,"Area": -999}
t_stations["ZH581"] ={"Easting": -999, "Northing": -999, "Name": "Kempt-Winterthur", "Altitide": 414.5,"Area": -999}



for file in files:
    print file
    n="ZH"+str(os.path.basename(file)[3:6])
    print "Processing for station: " + t_stations[n]["Name"]
    fields = "timestamp Q"
    dates=[]
    data=[]
    print "\tPreaparing file"
    header=[]
    header.append("SMET 1.1 ASCII")
    header.append("[HEADER]")
    header.append("station_id = "+n)
    header.append("station_name = "+ t_stations[n]["Name"])
    header.append("latitude = "+ str(CHtoWGSlat(float(t_stations[n]["Easting"]),float(t_stations[n]["Northing"]))))
    header.append("longitude = "+ str(CHtoWGSlng(float(t_stations[n]["Easting"]),float(t_stations[n]["Northing"]))))
    header.append("easting = "+ str(int(t_stations[n]["Easting"])))
    header.append("northing = "+ str(int(t_stations[n]["Northing"])))
    header.append("altitude = "+str(int(t_stations[n]["Altitide"])))
    header.append("area = "+str(int(t_stations[n]["Area"])))
    header.append("nodata = -999")
    header.append("tz = 1")
    header.append("fields = "+fields)
    header.append("[DATA]")

    with open(file,'rU') as read_file:
        print "\tReading Q from: ",file
        line = 0
        lines = read_file.readlines()
        cur_line=lines[line].rstrip().lstrip()
        while(line < len(lines)):
            cur_line=lines[line].rstrip().lstrip()
            if(cur_line.startswith(";")):
                line = line+1
                continue
            cur_line = cur_line.split()
            if(cur_line[1]=="24:00"):
                cur_line[1]="23:59"
            dates.append(datetime.datetime.strptime(cur_line[0]+"T"+cur_line[1], '%d.%m.%YT%H:%M'))
            data.append(float(cur_line[2].strip(' \t\n\r')))
            line = line+1

    print "\tComputing hourly mean"
    line=0
    date=dates[line].replace(microsecond=0,second=0,minute=0)
    date_next=date+timedelta(hours=1)
    sum=[]
    dates_mean=[]
    data_mean=[]
    while line<len(dates):
        d=dates[line]
        if d<date_next:
            sum.append(data[line])
        else:
            dates_mean.append(date)
            data_mean.append(np.mean(sum))
            sum=[]
            sum.append(data[line])
            date=dates[line].replace(microsecond=0,second=0,minute=0)
            date_next=date+timedelta(hours=1)
        line=line+1
    
    line=0
    while line<len(data_mean)-1:
        if(dates_mean[line+1]-dates_mean[line]!=timedelta(hours=1)):
            print "discontinuity at",
            print dates_mean[line]
        line=line+1
    print "\tWriting output in",output+"/"+t_stations[n]["Name"]+".smet"
    with open(output+"/"+n+"_"+t_stations[n]["Name"]+".smet",'w') as write_file:
        for write_lines in header:
            write_file.write(write_lines+"\n")
        for i in range(len(data_mean)):
             write_file.write(dates_mean[i].strftime('%Y-%m-%dT%H:%M')+"\t"+str(round(data_mean[i],2))+"\n")

