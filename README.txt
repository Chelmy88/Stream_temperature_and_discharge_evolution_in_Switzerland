README
======

This content is related to the paper:
"Stream temperature evolution in Switzerland over the last 50 years, Adrien
Michel, Tristan Brauchli, Michael Lehning, Bettina Schaefli, and Hendrik Huwald,
HESS, 2019"

Any use of the material (code or data) presented here should clearly reference
to this paper and to the providers of the data mentioned in the documentation.

This material is distributed under the GPLv3 license
(https://www.gnu.org/licenses/gpl-3.0.html)

Author: Adrien Michel, adrien.michel@epfl.ch, 08.2019

INTRODUCTION
------------

This folder contains all necessary steps to reproduce the analysis done in the
above mentioned paper. The data provided are intended to be used to
reproduce the results of the paper. Any user who wants to use the data for
new/other scientific applications is requested to ask permission from the data
providers. For more details see the README files in each subdirectory.


USAGE 
-----

Each directory contains a README file that users should read for details. 
All the material provided comes with various licenses; see individual files for
details.  The directory “1_Obtain_and_pocess_raw_data” and “3_Produce_data”
contains the necessary information and scripts on how to get the dataset
reproduced from raw data. The raw  meteorological and hydrological data are
not provided and should be obtained from  the provider. These steps are not
required for running the analysis in “4_Run_analysis”, all necessary data for
this are already provided.  The directory “2_R_package” contains the R
package that must be installed to run  scripts provided in “3_Produce_data”
and “4_Run_analysis”. See the corresponding README file in that directory to
install the package and to obtain information about the structure of the
dataset.

All source code of the package is also provided.  The main datasets are
provided in "4_Run_analysis" as an R object, along with some other  input
data. The script provided in "4_Run_analysis" allows to do the analysis and
produce the plots of the paper.  

DATA PROVIDED AND AUTHORISATION
-------------------------------

  1. 1_Obtain_and_pocess_raw_data/data_Hydrologische_Messstationen: This 
directory contains shpaefiles with the positions of the water measurements
stations operated by the Swiss Federal Office of Environment. These data 
are provided by Swiss Federal Office of Environment and are available at:
https://data.geo.admin.ch/ch.bafu.hydrologie-hydromessstationen/.  Data are
provided here with the authorisation of the provider

  2. 4_Run_analysis/data/glaciers: This directory contains mass balance
measurements for some selected glaciers. They are provided by [GLAMOS2018] and
the raw data are available at: https://www.glamos.ch/.  The purpose of the data
provided here is the reproduction of the results of the paper, interested users
should obtain the data from the above mentioned website. Data are provided
here with the authorisation of the provider

  3. 4_Run_analysis/data/indicators: This directory contains AMON and NAO values.
They are provided by [JONES1997] and [ENFIELD2001] respectively.  The raw data
are available at: https://www.esrl.noaa.gov/psd/gcos_wgsp/Timeseries/.  The
purpose of the data provided here is the reproduction of the results of the
paper, interested users should obtain the data from the above mentioned website.
Python scripts are provided to translate the raw data to the format provided
here.  PROVIDER HAVE NOT YET AGREED TO PUBLISH THE DATA ON OPEN SERVER

  4. 4_Run_analysis/data/maps: This directory contains raster and shape files
data of Switzerland. These were obtained from the open dataset provided at
https://data.geo.admin.ch/ch.swisstopo.swiss-map-vector1000.metadata/ The files
provided have been slightly modified to remove some information outside
Switzerland.  The purpose of the data provided here is the reproduction of the
results of the paper, interested users should obtain data from the above
mentioned website. Data are provided here with the authorisation of the
provider.

  5. 4_Run_analysis/data/rds_data: This directory contains the main dataset
produced for this study. The details on the structure of the dataset are given
in the documentation 2_R_package/swisswatertemp_1.0.0.pdf. Users are invited to
read the section 'swisswatertemp' starting on page 25, which describes in detail
the datasets provided in rds_data. Usage of this data set is open, it is
published under GPLv3 license. However for usage of the raw measurements
provided in the data sets, people are invited to obtain them from the providers
described in 1_Obtain_and_pocess_raw_data/README.txt. PROVIDER HAVE NOT YET 
AGREED TO PUBLISH THE DATA ON OPEN SERVER

  6. 4_Run_analysis/data/snow_cover 5. Snow_cover This directory contains mean
snow water equivalent computed from snow maps provided by the WSL Institute for
Snow and Avalanche Research (SLF) [MAGNUSSON2014].  The purpose of the data
provided here is the reproduction of the results of the paper, interested users
should obtain data from the SLF. Data are provided here with the authorisation 
of the provider.


REFERENCES
----------

[ENFIELD2001] Enfield, D.B., A.M. Mestas-Nunez, and P.J. Trimble, 2001: The
Atlantic Multidecadal Oscillation and its relationship to rainfall and river
flows in the continental U.S., Geophys. Res. Lett., 28: 2077-2080.

[GLAMOS2018] GLAMOS (2018). Swiss Glacier Mass Balance, release 2018, Glacier
onitoring Switzerland, doi:10.18750/massbalance.2018.r2018.

[JONES1997] Jones, P.D., Jónsson, T. and Wheeler, D., 1997: Extension to the
North Atlantic Oscillation using early instrumental pressure observations from
Gibraltar and South-West Iceland. Int. J. Climatol. 17, 1433-1450

[MAGNUSSON2014] Magnusson, J., Gustafsson, D., Hüsler, F., & Jonas, T. (2014).
Assimilation of point SWE data into a distributed snow cover model comparing two
contrasting methods. Water resources research, 50(10), 7816-7835.
