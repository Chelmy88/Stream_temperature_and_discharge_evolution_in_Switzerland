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
import re

currDir = os.path.dirname(os.path.realpath(__file__))
input = currDir+'/RAW_input_hourly/data/'
output = currDir+'/SMET_water_temp_hourly/'

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
    if (not os.path.isfile(input+"michel_"+type+"_"+n+"_Stundenmittel.asc")):
        files= glob.glob(input+"michel_"+type+"_"+n+"*")
        regex = re.compile(".*(2018).*")
        file=[f for f in files for m in [regex.search(f)] if m]
        file=file[0]
    else:
        file=input+"michel_"+type+"_"+n+"_Stundenmittel.asc"
    with open(file) as read_file:
        line = 0
        lines = read_file.readlines()
        cur_line=lines[line].rstrip().lstrip()
        while (not cur_line.startswith(n)):
            line=line+1
            cur_line=lines[line].rstrip().lstrip()
        start = datetime.datetime.strptime(cur_line.split(';')[1].split('-')[0], '%Y.%m.%d %H:%M')
        line = len(lines)-1
        cur_line=lines[line].rstrip().lstrip()
        while (not cur_line.startswith(n)):
            line=line-1
            cur_line=lines[line].rstrip().lstrip()
        end = datetime.datetime.strptime(cur_line.split(';')[1].split('-')[0], '%Y.%m.%d %H:%M')
    return start,end

def isfloat(value):
    try:
        float(value)
        return True
    except ValueError:
        return False



def get_completion(n,file,st,en,t_stations):
    print "\t Completing for station: " + t_stations[int(n)]["Name"]
    fields = "timestamp"
    fields+=" T"
    data=[]
    start = datetime.datetime.strptime(st, '%Y.%m.%d %H:%M')
    end = datetime.datetime.strptime(en, '%Y-%m-%dT%H:%M')
    time = start
    while (time <= end):
        data.append(time.strftime('%Y-%m-%dT%H:%M'))
        time = time + timedelta(hours=1)
    time=start
    pos=0
    
    with open(file) as read_file:
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
            date = datetime.datetime.strptime(cur_line[1].split('-')[0], '%Y.%m.%d %H:%M')
            
            if(date < time):
                print "I have a problem"
                break
            if (date == time):
                if not isfloat(cur_line[2].strip(' \t\n\r')):
                    cur_line[2]="-999"
                data[pos] = data[pos]+"\t"+cur_line[2].strip(' \t\n\r')
                line = line+1
                pos = pos+1
                time = time + timedelta(hours=1)
            elif(date > time):
                data[pos] = data[pos]+"\t-999"
                pos = pos+1
                time = time + timedelta(hours=1)
        while(time <= end):
            data[pos] = data[pos]+"\t-999"
            pos = pos+1
            time = time + timedelta(hours=1)
    return data

###############
###############

print "Scanning available files"
files=[]

T=[]
N=[]
for file in os.listdir(input):
    if (not os.path.isfile(input+file)) or file.startswith(".DS"):
        continue
    code= file[file.index("_",file.index("_")+1)+1 : file.index("_",file.index("_",file.index("_")+1)+1)]
    type = file[file.index("_")+1 : file.index("_",file.index("_")+1)]
    if (type=="T"):
        T.append(code)
        N.append(code)
        files.append(input+file)

print "Building station names list from shapefile"
sf = shapefile.Reader(input+"/../data_Hydrologische Messstationen/lhg_UBST", encoding='utf-8')
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

for n in N:
    print "Processing for station: " + t_stations[int(n)]["Name"]
    T_def = False
    fields = "timestamp"
    start_arr = [datetime.time(0, 0, 0)]
    end_arr = [datetime.time(0, 0, 0)]
    if n in T:
        fields+=" T"
        start_arr[0], end_arr[0] = find_start_end(n,"T")
        print "\tFound T"

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
        time = time + timedelta(hours=1)
    if n in T:
        time = start
        pos= 0
        if (not os.path.isfile(input+"michel_T"+"_"+n+"_Stundenmittel.asc")):
            files= glob.glob(input+"michel_T"+"_"+n+"*")
            regex = re.compile(".*(2018).*")
            file=[f for f in files for m in [regex.search(f)] if m]
            file=file[0]
        else:
            file=input+"michel_T_"+n+"_Stundenmittel.asc"
        with open(file) as read_file:
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
                date = datetime.datetime.strptime(cur_line[1].split('-')[0], '%Y.%m.%d %H:%M')

                if(date < time):
                    break
                if (date == time):
                    if not isfloat(cur_line[2].strip(' \t\n\r')):
                        cur_line[2]="-999"
                    data[pos] = data[pos]+"\t"+cur_line[2].strip(' \t\n\r')
                    line = line+1
                    pos = pos+1
                    time = time + timedelta(hours=1)
                elif(date > time):
                    data[pos] = data[pos]+"\t-999"
                    pos = pos+1
                    time = time + timedelta(hours=1)
                if line >= len(lines) and time < end:
                    while(time <= end):
                        data[pos] = data[pos]+"\t-999"
                        pos = pos+1
                        time = time + timedelta(hours=1)

    completion=None
    if n=="2174":
        print "YYYY"
        completion=get_completion(n,"RAW_input_hourly/data/michel_T_2174_Stundenmittel_von1972bis1999.asc","1972.01.01 00:00","2000-01-31T23:00",t_stations)

    print "\tWriting output in",output+"/"+t_stations[int(n)]["Name"]+".smet"
    with open(output+"/"+t_stations[int(n)]["Name"]+".smet",'w') as write_file:
        for write_lines in header:
            write_file.write(write_lines+"\n")
        if completion is not None:
            print "XXXXXXXX"
            for write_lines in completion:
                print write_lines
                write_file.write(write_lines+"\n")
        else:
            print "Completion not found"
        for write_lines in data:
             write_file.write(write_lines+"\n")
