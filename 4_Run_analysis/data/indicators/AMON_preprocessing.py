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

def main():
    currDir = os.path.dirname(os.path.realpath(__file__))
    input = currDir+'/amon.sm.long.data'
    output = currDir+'/AMON.txt'
    dates=[]
    data=[]
    with open(input,'r') as file:
        lines = file.readlines()
        line=0
        while(line<len(lines)):
            cur_line=lines[line].strip()
            if cur_line.startswith("-99.9") or  cur_line.startswith("AMO") \
                or cur_line.startswith("Calculated") or  cur_line.startswith("http"):
                line=line+1
                continue
            else:
                dat=cur_line.split()
                for i in range(1,13):
                    dates.append(float(dat[0])+(i-1.)/12.)
                    data.append(dat[i])
                line=line+1

    with open(output,'w') as write_file:
        header = "# Data obtained and re-formatted from:\n"
        header = header + "# AMO smoothed from the Kaplan SST V2\n"
        header = header + "# Calculated at NOAA/ESRL/PSD1\n"
        header = header + "# http://www.esrl.noaa.gov/psd/data/timeseries/AMO/\n"
        header = header + "#\n"
        header = header + "# The purpose of the data provided here is the reproduction\n"
        header = header + "# of the results of the paper, people who want to use this\n"
        header = header + "# data should obtain them for from the above mentioned website.\n"
        header = header + "# Python scripts are provided to translate the raw data to the\n"
        header = header + "# format provided here.\n"
        header = header + "# Data are provided here with the authorisation of the provider.\n\n"
        write_file.write(header)
        for line in range(len(dates)):
            write_file.write(str(dates[line])+"\t"+data[line]+"\n");



if __name__ == "__main__":
    main()
