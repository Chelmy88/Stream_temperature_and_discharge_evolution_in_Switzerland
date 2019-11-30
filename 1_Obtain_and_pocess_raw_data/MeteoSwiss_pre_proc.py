# This script is related to the paper:
# "Stream temperature evolution in Switzerland over the last 50 years, Adrien
# Michel, Tristan Brauchli, Michael Lehning, Bettina Schaefli, and Hendrik Huwald,
# HESS, 2019"

# This material is distributed under the GPLv3 license
# (https://www.gnu.org/licenses/gpl-3.0.html)

import os
import glob
import datetime
import sys
import math
import multiprocessing as mp

import datetime
import re
import warnings

def strftime(dt, fmt):
    if dt.year < 1900:
        # create a copy of this datetime, just in case, then set the year to
        # something acceptable, then replace that year in the resulting string
        tmp_dt = datetime.datetime(1988, dt.month, dt.day,
                                dt.hour, dt.minute,
                                dt.second, dt.microsecond,
                                dt.tzinfo)
        if re.search('(?<!%)((?:%%)*)(%y)', fmt):
           warnings.warn("Using %y time format with year prior to 1900 "
                         "could produce unusual results!")
       
        tmp_fmt = fmt
        tmp_fmt = re.sub('(?<!%)((?:%%)*)(%y)', '\\1\x11\x11', tmp_fmt, re.U)
        tmp_fmt = re.sub('(?<!%)((?:%%)*)(%Y)', '\\1\x12\x12\x12\x12', tmp_fmt, re.U)
        tmp_fmt = tmp_fmt.replace(str(datetime.MAXYEAR), '\x13\x13\x13\x13')
        tmp_fmt = tmp_fmt.replace(str(datetime.MAXYEAR)[-2:], '\x14\x14')

        result = tmp_dt.strftime(tmp_fmt)
        
        if '%c' in fmt:
            # local datetime format - uses full year but hard for us to guess
            # where.
            result = result.replace(str(1988), str(dt.year))
    
        result = result.replace('\x11\x11', str(dt.year)[-2:])
        result = result.replace('\x12\x12\x12\x12', str(dt.year))
        result = result.replace('\x13\x13\x13\x13', str(datetime.MAXYEAR))
        result = result.replace('\x14\x14', str(datetime.MAXYEAR)[-2:])
        
        return result
    
    else:
        return dt.strftime(fmt)


########## UTILS FUNCTIONS ###################

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

########## INPUT READING FUNCTIONS #####################

#Read station information from legend files
def getStationsInfo(legend_files):
    stations_temp={} #Stor in dictionnary, only one entry per label
    for file in legend_files:
        with open(file,'r') as legend:
            lines = legend.readlines()
            line=0
            #Check where the station section starts
            for l in lines :
                line=line+1
                if(l.startswith("Stations")):
                    break
            #Read the content of the station section (skip 2 lines after "sections", see raw file) and stop at empty line
            for i in range(line+2,len(lines)):
                if not lines[i].strip():
                    break
                data=lines[i].strip().split()
                #Store station short name, position and elevation
                stations_temp[data[0]]=list( data[i] for i in [0,-2,-1] )

    #Complete stations list
    stations={}
    for name, st in stations_temp.iteritems():
        coord=st[1].split("/")
        stations[name]={"name":st[0],"easting":coord[0],"northing":coord[1],\
            "longitude":CHtoWGSlng(float(coord[0]),float(coord[1])),"latitude":CHtoWGSlat(float(coord[0]),float(coord[1])),\
            "altitude":st[2]}

    return stations

def loadData(stations,data_files):
    global loaded_data
    for name in stations:
        loaded_data[name]=[]
    for file in data_files:
        with open(file,'r') as data:
            print "Reading file:",file
            lines = data.readlines()
            line=0
            #Loop over the file
            while(line<len(lines)):
                #Check where the next station section starts
                for l in range(line,len(lines)) :
                    line=l
                    if(lines[l].startswith("stn")):
                        break
                #Stop if we reach the end of file
                if(line==len(lines)-1):
                    break
                
                #Read info about incomming data
                head=lines[line].strip().split(";")
                stn=lines[line+1].strip().split(";")[0]
                
                #Select column to keep, adapt header and prepare data container
                to_keep=[]
                data_tmp=[]
                for i in range(0,len(head)):
                    if(head[i] != "stn" and not head[i].startswith("q") and not head[i].startswith("m")):
                        to_keep.append(i)
                        data_tmp.append([])
                head=[head[i] for i in to_keep]
                
                #Store the chunk of data found in the temporary table
                line=line+1 #Jumps to first line of data
                strp=lines[line].strip()
                while (strp):
                    dat=strp.split(";")
                    for i in range(0,len(to_keep)):
                        d=dat[to_keep[i]]
                        if not isfloat(d):
                            d=-999.
                        data_tmp[i].append(float(d))
                    #Incremet and strip
                    line=line+1
                    strp=lines[line].strip()
                
                #Add a dictionnary in the station array to store read data
                loaded_data[stn].append({})
                #To store the tmp_data at the right position in loaded_data
                position=len(loaded_data[stn])-1
                for i in range(0,len(head)):
                    loaded_data[stn][position][head[i]]=data_tmp[i]
                head = to_keep = stn = data_tmp = dat = None



########## TIME CONVERSION FUNCTIONS ###################

def convertTime(times,name,j):
    if(int(math.log10(times[0]))+1==8):
        for i in range(0,len(times)):
            times[i]=datetime.datetime.strptime(str(int(times[i])), '%Y%m%d')
    elif(int(math.log10(times[0]))+1==10):
        for i in range(0,len(times)):
            times[i]=datetime.datetime.strptime(str(int(times[i])), '%Y%m%d%H')
    else:
        print "NOPE"
    return (name,j,times)

def setTime(tuple):
    global loaded_data
    loaded_data[tuple[0]][tuple[1]]["time"]=tuple[2]

def convertTimes():
    global loaded_data
    pool = mp.Pool(processes=6)
    results = []
    for name, data in loaded_data.iteritems():
        for i in range(0,len(data)):
            r=pool.apply_async(convertTime,args = (data[i]["time"],name,i), callback=setTime)
            results.append(r)
    pool.close()
    pool.join()



########## OUTPUT WRITING FUNCTIONS ###################

def getTimeStep(table):
    return ((table[-1]-table[0])/(len(table)-1)).total_seconds()

#Get start time and end time for time series indicated in info
def getStartEnd(info,station_data):
    #Store start and end from the first table listed in info
    start=station_data[info[0]["table"]]["time"][0]
    end=station_data[info[0]["table"]]["time"][-1]
    #Check with other tables, store min start and max end
    for i in range(1,len(info)):
        start=start if start < station_data[info[i]["table"]]["time"][0] \
                    else station_data[info[i]["table"]]["time"][0]
        end=end if end > station_data[info[i]["table"]]["time"][-1] \
                else station_data[info[i]["table"]]["time"][-1]
    return(start,end)


def getTimeInfo(station_data):
    hourly=[]
    daily=[]
    for i in range(0,len(station_data)):
        time_step=getTimeStep(station_data[i]["time"])
        variables=[v for v in station_data[i].keys() if not v=="time"]
        if(time_step==3600.):
            hourly.append({"table":i,"variables":variables})
        elif(time_step==86400.0):
            daily.append({"table":i,"variables":variables})
        else:
            print ("ERROR")
            continue
    return(hourly,daily)

def getWritingData(tables_info,station_data,timedelta):
    #Get start and end date
    start,end=getStartEnd(tables_info,station_data)
    #Prepare table with timestamps
    time=start
    times_table=[]
    while(time<=end):
        times_table.append(time)
        time+=timedelta
    time=None
    #Prepare table to store data to write
    writer_data=["" for x in range(len(times_table))]
    variables=["time"]
    #Loop over all the tables with this time step
    for table_info in tables_info:
        writer_pos=0
        data_pos=0
        #Extract one single table
        data=station_data[table_info["table"]]
        #Add variables to variable list
        for var in table_info["variables"]:
            variables.append(var)
        while(writer_pos<len(times_table)):
            time=times_table[writer_pos]
            #If no data available in this table for this time, fill with no data until data are found
            if(data["time"][data_pos]>time):
                for var in table_info["variables"]:
                    writer_data[writer_pos]+="\t-999"
                writer_pos+=1
                continue
            #If date of the data corresponds to the current writer date, extract variables
            if(data["time"][data_pos]==time):
                for var in table_info["variables"]:
                    writer_data[writer_pos]+="\t"+str(data[var][data_pos] if not data[var][data_pos]==-999 else -999 )
                writer_pos+=1
                data_pos+=1
            else:
                print("Error, should never be here")
    return(times_table,writer_data,variables)

def transalteVariables(var):
    global variables_dictionnary
    variables=[]
    for v in var:
        if v in variables_dictionnary.keys():
            variables.append(variables_dictionnary[v])
        else:
            print "WARNING",v,"not in variable dictionnary, it won't be translated!"
            variables.append(v)
    return variables


def writeFile(times_table,writer_data,station_header,variables,suffix):
    variables=transalteVariables(variables)
    global output
    header=[]
    header.append("SMET 1.1 ASCII")
    header.append("[HEADER]")
    header.append("station_id = " + station_header["name"])
    header.append("station_name = " + station_header["name"])
    header.append("latitude = " + str(station_header["latitude"]))
    header.append("longitude = " + str(station_header["longitude"]))
    header.append("easting = " + str(station_header["easting"]))
    header.append("northing = " + str(station_header["northing"]))
    header.append("altitude = " + str(station_header["altitude"]))
    header.append("nodata = -999")
    header.append("source = meteoSwiss, generated with Conc_raw_water_temperature.py by Adrien Michel")
    header.append("tz = 1")
    header.append("fields = "+"\t".join(variables))
    header.append("[DATA]")
    with open(output+station_header["name"]+".smet",'w') as write_file:
        for line in header:
            write_file.write(line+"\n");
        for line in range(len(writer_data)):
            write_file.write(strftime(times_table[line],'%Y-%m-%dT%H:%M')+writer_data[line]+"\n")

def printStation(name,station_header,station_data):
    hourly_tables,daily_tables = getTimeInfo(station_data)
    #write hourly data
    if(hourly_tables):
        times_table,writer_data,variables = getWritingData(hourly_tables,station_data,datetime.timedelta(hours=1))
        writeFile(times_table,writer_data,station_header,variables,"hourly")
    if(daily_tables):
        times_table,writer_data,variables = getWritingData(daily_tables,station_data,datetime.timedelta(days=1))
        writeFile(times_table,writer_data,station_header,variables,"daily")

def printStations(stations):
    global loaded_data
    for name in loaded_data.keys():
        printStation(name,stations[name],loaded_data[name])


##########################


#### DEFINE GLOBAL VARIABLES ###
currDir = os.path.dirname(os.path.realpath(__file__))
input = currDir+'/Raw_input_water_temperature_final/'
output = currDir+'/SMET_water_temperature_final/'
loaded_data={}
variables_dictionnary={"time":"timestamp","tre200b0":"TA_C","ure200b0":"RH_C","rre150h0":"PSUM_H","tso050s0":"TSOIL_50_I","rre150b0":"PSUM_C", \
                       "dkl010h0":"DW_H","tso005s0":"TSOIL_5_I","tso010s0":"TSOIL_10_I","tso020s0":"TSOIL_20_I","gre000h0":"ISWR", \
                       "gre000b0":"ISWR_C","tre200h0":"TA","ure200h0":"RH","prestah0":"P","prestab0":"P_C","tso005hs":"TSOIL_5_I_P10", \
                       "tso010hs":"TSOIL_10_I_P10","tso020hs":"TSOIL_20_I_P10","tso005b0":"TSOIL_5_C","fu3010h0":"VW_H","fu3010h1":"MAXW", \
                       "dkl010b0":"DW_C","fu3010b0":"VW_C","fu3010b1":"MAXW_C","tso100s0":"TSOIL_100_I","tso100hs":"TSOIL_100_I_P10", \
                       "oli000b0":"ILWR_C","oli000h0":"ILWR","osr000b0":"OSWR_C","osr000h0":"OSWR","hto000sw":"HS","hto000hs":"HS_I", \
                       "htoauths":"HS_A","olo000h0":"OLWR_H","olo000b0":"OLWR_C","ods000h0":"DRAD","ods000b0":"DRAD_C", \
                       "ets150d0":"ET_DAY","eva000hs":"ET","tre005h0":"TA_5","tre200d0":"TA","ths200d0":"TA_HOM","rhs150d0":"P_HOM","rre150d0":"P", \
                       "hto000j0":"HS18","hto000d0":"HS6","htoautd0":"HS6AUTO","htoautj0":"HS18AUTO"}

#### MAIN ####

def main():
    global input
    global loaded_data
    print("Checking input")
    #Check for input data directories
    dirs=[]
    for file in os.listdir(input):
        if (not os.path.isfile(input+file)) and not file.startswith(".DS"):
            dirs.append(input+file)
    #Gather available data files and legend files
    legend_files=[]
    data_files=[]
    for dir in dirs:
        for file in os.listdir(dir):
            if (os.path.isfile(dir+"/"+file) and file.endswith("_legend.txt")  and file.startswith("order")) :
                legend_files.append(dir+"/"+file)
            if (os.path.isfile(dir+"/"+file) and file.endswith("_data.txt")  and file.startswith("order")) :
                data_files.append(dir+"/"+file)
    #Gather information from legend files
    stations=getStationsInfo(legend_files)
    #Remove useless data
    legend_files=None

    #Load data
    print("Loading Data")
    loadData(stations,data_files)

    #Process data, set them in the writting form
    print("Converting Time")
    convertTimes()

    print("Printing output")
    printStations(stations)

if __name__ == "__main__":
    main()
