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
import shutil

currDir = os.path.dirname(os.path.realpath(__file__))
input = currDir+'/Smet/'
output = currDir+'/Smet/'

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

ZH_T=[]
ZH_Q=[]

for file in os.listdir(input+"T"):
    if (not os.path.isfile(input+"T/"+file)) or file.startswith(".DS") :
        continue
    ZH_T.append(file)
for file in os.listdir(input+"Q"):
    if (not os.path.isfile(input+"Q/"+file)) or file.startswith(".DS") :
        continue
    ZH_Q.append(file)

def find_start_end(file_T,file_Q):
    with open(file_T,'rU') as read_file_T, open(file_Q,'rU') as read_file_Q:
        lines = read_file_T.readlines()
        line=0
        cur_line=lines[line].rstrip().lstrip()
        while(not cur_line.startswith("[DATA]")):
            line=line+1
            cur_line=lines[line].rstrip().lstrip()
        line=line+1
        cur_line=lines[line].rstrip().lstrip()
        start_T = datetime.datetime.strptime(cur_line.split()[0],'%Y-%m-%dT%H:%M')
        line=len(lines)-1
        cur_line=lines[line].rstrip().lstrip()
        end_T = datetime.datetime.strptime(cur_line.split()[0],'%Y-%m-%dT%H:%M')
    
        lines = read_file_Q.readlines()
        line=0
        cur_line=lines[line].rstrip().lstrip()
        while(not cur_line.startswith("[DATA]")):
            line=line+1
            cur_line=lines[line].rstrip().lstrip()
        line=line+1
        cur_line=lines[line].rstrip().lstrip()
        start_Q = datetime.datetime.strptime(cur_line.split()[0],'%Y-%m-%dT%H:%M')
        line=len(lines)-1
        cur_line=lines[line].rstrip().lstrip()
        end_Q = datetime.datetime.strptime(cur_line.split()[0],'%Y-%m-%dT%H:%M')
    return min(start_T,start_Q),max(end_T,end_Q)


for file in ZH_T:
    if file in ZH_Q:
        print file
        file_Q=input+"Q/"+file
        file_T=input+"T/"+file
        start,end= find_start_end(file_T,file_Q)
        data=[]
        header=[]
        d=start
        while(d<=end):
            data.append(d.strftime('%Y-%m-%dT%H:%M'))
            d=d+timedelta(hours=1)
        with open(file_T,'rU') as read_file_T, open(file_Q,'rU') as read_file_Q:
            time = start
            pos= 0
            print "\tReading T from",start,"to",end
            line = 0
            lines = read_file_T.readlines()
            cur_line=lines[line].rstrip().lstrip()
            while (not cur_line.startswith("[DATA]")):
                line=line+1
                cur_line=lines[line].rstrip().lstrip()
            line=line+1
            while(time <= end and line < len(lines)):
                cur_line=lines[line].rstrip().lstrip()
                cur_line = cur_line.split()
                date = datetime.datetime.strptime(cur_line[0], '%Y-%m-%dT%H:%M')
                if (date == time):
                    data[pos] = data[pos]+"\t"+cur_line[1].strip(' \t\n\r')
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
            time = start
            pos= 0
            print "\tReading Q from",start,"to",end
            line = 0
            lines = read_file_Q.readlines()
            cur_line=lines[line].rstrip().lstrip()
            while (not cur_line.startswith("[DATA]")):
                header.append(cur_line)
                line=line+1
                cur_line=lines[line].rstrip().lstrip()
            header[-1]="fields = timestamp\tT\tQ"
            header.append(cur_line)
            line=line+1
            while(time <= end and line < len(lines)):
                cur_line=lines[line].rstrip().lstrip()
                cur_line = cur_line.split()
                date = datetime.datetime.strptime(cur_line[0], '%Y-%m-%dT%H:%M')
                if (date == time):
                    data[pos] = data[pos]+"\t"+cur_line[1].strip(' \t\n\r')
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
            print header
            with open(output+"/"+file,'w') as write_file:
                for write_lines in header:
                    write_file.write(write_lines+"\n")
                for i in range(len(data)):
                    write_file.write(data[i]+"\n")

