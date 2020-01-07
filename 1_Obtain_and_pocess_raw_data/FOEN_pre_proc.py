#-*- coding: UTF-8 -*-

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

import os
import glob
import datetime
from datetime import timedelta
import sys
import shapefile

currDir = os.path.dirname(os.path.realpath(__file__))
input = currDir+'/RAW_input_water_temp_addition/'
output = currDir+'/SMET_water_temp/'

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
def find_start_end(n,type):
    with open(input+"michel_"+type+"_"+n+"_Tagesmittel.asc") as read_file:
        line = 0
        lines = read_file.readlines()
        cur_line=lines[line].rstrip().lstrip()
        while (not cur_line.startswith(n)):
            line=line+1
            cur_line=lines[line].rstrip().lstrip()
        start = datetime.datetime.strptime(cur_line.split(';')[1].split('-')[0], '%Y.%m.%d')
        line = len(lines)-1
        cur_line=lines[line].rstrip().lstrip()
        while (not cur_line.startswith(n)):
            line=line-1
            cur_line=lines[line].rstrip().lstrip()
        end = datetime.datetime.strptime(cur_line.split(';')[1].split('-')[0], '%Y.%m.%d')
    return start,end
def isfloat(value):
    try:
        float(value)
        return True
    except ValueError:
        return False

print "Scanning available files"
files=[]
P=[]
Q=[]
T=[]
N=[]
for file in os.listdir(input):
    if (not os.path.isfile(input+file)) or file.startswith(".DS"):
        continue
    files.append(input+file)
    code= file[file.index("_",file.index("_")+1)+1 : file.index("_",file.index("_",file.index("_")+1)+1)]
    type = file[file.index("_")+1 : file.index("_",file.index("_")+1)]
    N.append(code)
    if (type=="T"):
        T.append(code)
    elif (type=="Q"):
        Q.append(code)
    elif (type=="P"):
        P.append(code)
print(N)
N=list(set(N))
print(N)
print "Building station names list from shapefile"
sf = shapefile.Reader(input+"data_Hydrologische Messstationen/lhg_UBST", encoding='utf-8')
shapes =sf.shapes()
rec = sf.shapeRecords()
t_stations = dict()
for i in range(0, len(rec)):
    name = rec[i].record[1]
    name = name.replace("\xe4","ae")
    name = name.replace("\xfc","u")
    name = name.replace("\xc4","A")
    name = name.replace("\xe2","a")
    name = name.replace("\xf4","o")
    name = name.replace("\xf2","o")
    name = name.replace("\xf6","o")
    name = name.replace("\xee","i")
    name = name.replace("\xe8","e")
    name = name.replace("\xe9","e")
    name = name.replace("\xe7","c")
    name = name.replace(",","")
    name = name.replace(" ","_")
    name = name.replace("'","_")
    name = name.replace("/","u")
    t_stations[rec[i].record[0]] ={"Easting":shapes[i].points[0][0], "Northing":shapes[i].points[0][1], "Name": name}
del sf, shapes, rec
print t_stations

for n in N:
    print n

    print "Processing for station: " + t_stations[int(n)]["Name"]
    T_def = False
    Q_def = False
    P_def = False
    fields = "timestamp"
    start_arr = [datetime.time(0, 0, 0),datetime.time(0, 0, 0)]
    end_arr = [datetime.time(0, 0, 0),datetime.time(0, 0, 0)]
    if n in T:
        fields+=" T"
        start_arr[0], end_arr[0] = find_start_end(n,"T")
        print "\tFound T"
    if n in Q:
        fields+=" Q"
        start_arr[1], end_arr[1] = find_start_end(n,"Q")
        print "\tFound Q"
    start_arr = filter(lambda s: s != datetime.time(0, 0, 0), start_arr)
    end_arr = filter(lambda s: s != datetime.time(0, 0, 0), end_arr)
    start = min(start_arr)
    end = max(end_arr)
    data=[]
    time = start
    print "\tPreaparing file"
    header=[]
    header.append("SMET 1.1 ASCII")
    header.append("[HEADER]")
    header.append("station_id = "+n)
    header.append("station_name = "+ t_stations[int(n)]["Name"])
    header.append("latitude = "+ str(CHtoWGSlat(t_stations[int(n)]["Easting"],t_stations[int(n)]["Northing"])))
    header.append("longitude = "+ str(CHtoWGSlng(t_stations[int(n)]["Easting"],t_stations[int(n)]["Northing"])))
    header.append("easting = "+ str(int(t_stations[int(n)]["Easting"])))
    header.append("northing = "+ str(int(t_stations[int(n)]["Northing"])))
    header.append("altitude = -999");
    header.append("nodata = -999")
    header.append("tz = 1")
    header.append("fields = "+fields)
    header.append("[DATA]")
    while (time <= end):
        data.append(time.strftime('%Y-%m-%dT%H:%M'))
        time = time + timedelta(days=1)
    if n in T:
        time = start
        pos= 0
        with open(input+"michel_T_"+n+"_Tagesmittel.asc") as read_file:
            print "\tReading T from",start,"to",end
            line = 0
            lines = read_file.readlines()
            cur_line=lines[line].rstrip().lstrip()
            while (not cur_line.startswith(n)):
                line=line+1
                cur_line=lines[line].rstrip().lstrip()
            while(time <= end and line < len(lines)):
                cur_line=lines[line].rstrip().lstrip()
                cur_line = cur_line.split(';')
                date = datetime.datetime.strptime(cur_line[1].split('-')[0], '%Y.%m.%d')
                if (date == time):
                    if not isfloat(cur_line[2].strip(' \t\n\r')):
                        cur_line[2]="-999"
                    data[pos] = data[pos]+"\t"+cur_line[2].strip(' \t\n\r')
                    line = line+1
                    pos = pos+1
                    time = time + timedelta(days=1)
                elif(date > time):
                    data[pos] = data[pos]+"\t-999"
                    pos = pos+1
                    time = time + timedelta(days=1)
                if line >= len(lines) and time < end:
                    while(time <= end):
                        data[pos] = data[pos]+"\t-999"
                        pos = pos+1
                        time = time + timedelta(days=1)
    if n in Q:
        time = start
        pos= 0
        with open(input+"michel_Q_"+n+"_Tagesmittel.asc") as read_file:
            print "\tReading Q from",start,"to",end
            line = 0
            lines = read_file.readlines()
            cur_line=lines[line].rstrip().lstrip()
            while (not cur_line.startswith(n)):
                line=line+1
                cur_line=lines[line].rstrip().lstrip()
            cor=1.
            if (lines[line-2].startswith("Abfluss m")):
                print "In m3/s"
            elif (lines[line-2].startswith("Abfluss l")):
                print "In l/s"
                cor=1000.
            while(time <= end and line < len(lines)):
                cur_line=lines[line].rstrip().lstrip()
                cur_line = cur_line.split(';')
                date = datetime.datetime.strptime(cur_line[1].split('-')[0], '%Y.%m.%d')
                if (date == time):
                    if not isfloat(cur_line[2].strip(' \t\n\r')):
                        cur_line[2]="-999"
                    data[pos] = data[pos]+"\t"+str(float(cur_line[2].strip(' \t\n\r'))/cor)
                    line = line+1
                    pos = pos+1
                    time = time + timedelta(days=1)
                elif(date > time):
                    data[pos] = data[pos]+"\t-999"
                    pos = pos+1
                    time = time + timedelta(days=1)
                if line >= len(lines) and time < end:
                    while(time <= end):
                        data[pos] = data[pos]+"\t-999"
                        pos = pos+1
                        time = time + timedelta(days=1)

    print "\tWriting output in",output+"/"+t_stations[int(n)]["Name"]+".smet"
    with open(output+"/"+t_stations[int(n)]["Name"]+".smet",'w') as write_file:
        for write_lines in header:
            write_file.write(write_lines+"\n")
        for write_lines in data:
             write_file.write(write_lines+"\n")

