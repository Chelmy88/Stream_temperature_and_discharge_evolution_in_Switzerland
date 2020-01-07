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
import unicodedata
import codecs

currDir = os.path.dirname(os.path.realpath(__file__))
input = currDir+'/RAW/Data_utf8/'
output = currDir+'/SMET/'

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

def find_start_end(file):
    with codecs.open(input+file) as read_file:
        line = 0
        lines = read_file.readlines()
        cur_line=lines[line].rstrip().lstrip()
        while (cur_line.startswith(";")):
            line = line+1
            cur_line=lines[line].rstrip().lstrip()

        split_line=cur_line.split(',')
        if(len(split_line)<2):
            split_line=cur_line.split('\t')
        start = datetime.datetime.strptime(split_line[0],'%d.%m.%Y')
        line = len(lines)-1
        cur_line=lines[line].rstrip().lstrip()
        while (len(cur_line)<10):
            line=line-1
            cur_line=lines[line].rstrip().lstrip()
        split_line=cur_line.split(',')
        if(len(split_line)<2):
          split_line=cur_line.split('\t')
        end = datetime.datetime.strptime(split_line[0],'%d.%m.%Y') \
              + timedelta(hours=int(split_line[1].split(':')[0]), minutes=int(split_line[1].split(':')[1]))
    return start,end

def isfloat(value):
    try:
        float(value)
        return True
    except ValueError:
        return False

meta=dict()
print "Reading station meta informations"
with open("RAW/station_info.csv") as read_file:
    lines = read_file.readlines()
    for line in lines:
        if(len(line)<2):
            break
        cur_line=line.rstrip().lstrip().split(";")
        print cur_line


print "Scanning available files"
files=[]
Q=dict()
T=dict()

t_stations = dict()
for file in os.listdir(input):
    if (not os.path.isfile(input+file)) or file.startswith(".DS"):
        continue
    files.append(file)
    if ("-Abfl" in file):
        Q[file[0:4]]=file
    elif ("-Wtfl" in file):
        T[file[0:4]]=file
    name = file.replace("-Abfl","").replace("-Wtfl","").replace(".txt","")
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
    name = name.replace("é","e")
    name = name.replace("ö","o")
    name = name.replace("ö","o")
    name = name.replace("Ö","o")
    name = name.replace(".","")
    name = name.replace(".","")
    name = name.replace(",","")
    name = name.replace(" ","_")
    name = name.replace("'","_")
    name = name.replace("/","u")
    name = unicodedata.normalize('NFD', unicode(name, 'utf-8'))\
    .encode('ascii', 'ignore')\
    .decode("utf-8")
    t_stations[str(name[0:4])]={ "Name":name}


for code, station in t_stations.iteritems():
    print "Processing for station: " + station["Name"]
    T_def = False
    Q_def = False

    fields = "timestamp"
    start_arr = [datetime.time(0, 0, 0),datetime.time(0, 0, 0)]
    end_arr = [datetime.time(0, 0, 0),datetime.time(0, 0, 0)]
    if code in T:
        fields+=" T"
        start_arr[0], end_arr[0] = find_start_end(T[code])
        print "\tFound T"
    if code in Q:
        fields+=" Q"
        start_arr[1], end_arr[1] = find_start_end(Q[code])
        print "\tFound Q"

    start_arr = filter(lambda s: s != datetime.time(0, 0, 0), start_arr)
    end_arr = filter(lambda s: s != datetime.time(0, 0, 0), end_arr)
    start = min(start_arr)
    end = min(end_arr)
    data=[]
    time = start
    print "\tPreaparing file"
    header=[]
    header.append("SMET 1.1 ASCII")
    header.append("[HEADER]")
    header.append("station_id = "+code)
    header.append("station_name = "+ station["Name"])
    header.append("latitude = -999")
    header.append("longitude = -999")
    header.append("easting = -999")
    header.append("northing = -999")
    header.append("altitude = -999")
    header.append("nodata = -999")
    header.append("tz = 1")
    header.append("fields = "+fields)
    header.append("[DATA]")
    while (time <= end):
        data.append(time.strftime('%Y-%m-%dT%H:%M'))
        time = time + timedelta(hours=1)
    if code in T:
        time = start
        pos= 0
        with open(input+T[code]) as read_file:
            print "\tReading T from",start,"to",end
            line = 0
            lines = read_file.readlines()
            cur_line=lines[line].rstrip().lstrip()
            while (cur_line.startswith(";")):
                line=line+1
                cur_line=lines[line].rstrip().lstrip()
            while(time <= end and line < len(lines)):
                cur_line=lines[line].rstrip().lstrip()
                cur_line=cur_line.split(',')
                if(len(cur_line)<2):
                    cur_line=cur_line[0].split('\t')
                date = datetime.datetime.strptime(cur_line[0],'%d.%m.%Y') \
                        + timedelta(hours=int(cur_line[1].split(':')[0]), minutes=int(cur_line[1].split(':')[1]))
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
                elif(date < time):
                   line=line+1
                if line >= len(lines) and time < end:
                    while(time <= end):
                        data[pos] = data[pos]+"\t-999"
                        pos = pos+1
                        time = time + timedelta(hours=1)
    if code in Q:
        time = start
        pos= 0
        with open(input+Q[code]) as read_file:
            print "\tReading Q from",start,"to",end
            line = 0
            lines = read_file.readlines()
            cur_line=lines[line].rstrip().lstrip()
            while (cur_line.startswith(";")):
                line=line+1
                cur_line=lines[line].rstrip().lstrip()
            while(time <= end and line < len(lines)):
                cur_line=lines[line].rstrip().lstrip()
                cur_line = cur_line.split(',')
                if(len(cur_line)<2):
                    cur_line=cur_line[0].split('\t')
                date = datetime.datetime.strptime(cur_line[0],'%d.%m.%Y') \
                    + timedelta(hours=int(cur_line[1].split(':')[0]), minutes=int(cur_line[1].split(':')[1]))
                if (date == time):
                    if not isfloat(cur_line[2].strip(' \t\n\r')):
                        cur_line[2]="-999"
                    data[pos] = data[pos]+"\t"+str(float(cur_line[2].strip(' \t\n\r')))
                    line = line+1
                    pos = pos+1
                    time = time + timedelta(hours=1)
                elif(date > time):
                    data[pos] = data[pos]+"\t-999"
                    pos = pos+1
                    time = time + timedelta(hours=1)
                elif(date < time):
                    line=line+1
                if line >= len(lines) and time < end:
                    while(time <= end):
                        data[pos] = data[pos]+"\t-999"
                        pos = pos+1
                        time = time + timedelta(hours=1)

    print "\tWriting output in",output+"/"+station["Name"]+".smet"
    with open(output+"/"+station["Name"]+".smet",'w') as write_file:
        for write_lines in header:
            write_file.write(write_lines+"\n")
        for write_lines in data:
             write_file.write(write_lines+"\n")


