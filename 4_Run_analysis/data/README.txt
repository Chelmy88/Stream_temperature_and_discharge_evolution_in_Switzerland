TITLE
=====

This content is related to the paper: "Stream temperature evolution in
Switzerland over the last 50 years, Adrien Michel, Tristan Brauchli, Michael
Lehning, Bettina Schaefli, and Hendrik Huwald, HESS, 2019"

Any use of the material (code or data) presented here should clearly reference
to this paper and to the providers of the data mentioned in the documentation.

This material is distributed under the GPLv3 license
(https://www.gnu.org/licenses/gpl-3.0.html)

Author: Adrien Michel, adrien.michel@epfl.ch, 08.2019

INTRODUCTION
------------

This directory contains the various datasets used in the study. See below for
the various authorizations.

DETAILS
-------

  1. Glaciers: This directory contains mass balance measurements for some
selected glaciers. They are provided by [GLAMOS2018] and the raw data are
available at: https://www.glamos.ch/.  The purpose of the data provided here is
the reproduction of the results of the paper, interested users should obtain the
data from the above mentioned website.  Data are provided here with the
authorisation of the provider


  2. Indicators: This directory contains AMON and NAO values. They are provided
by [JONES1997] and [ENFIELD2001] respectively.  The raw data are available at:
https://www.esrl.noaa.gov/psd/gcos_wgsp/Timeseries/.  The purpose of the data
provided here is the reproduction of the results of the paper, interested users
should obtain the data from the above mentioned website.  Python scripts are
provided to translate the raw data to the format provided here.  PROVIDER HAVE
NOT YET AGREED TO PUBLISH THE DATA ON OPEN SERVER


  3. Maps: This directory contains raster and shape files data of Switzerland.
These were obtained from the open dataset provided at
https://data.geo.admin.ch/ch.swisstopo.swiss-map-vector1000.metadata/ The files
provided have been slightly modified to remove some information outside
Switzerland.  The purpose of the data provided here is the reproduction of the
results of the paper, interested users should obtain data from the above
mentioned website.  Data are provided here with the authorisation of the
provider.

  4. Rds_data: This directory contains the main dataset produced for this study.
The details on the structure of the dataset are given in the documentation
2_R_package/swisswatertemp_1.0.0.pdf. Users are invited to read the section
'swisswatertemp' starting on page 25, which describes in detail the datasets
provided in rds_data. Usage of this data set is open, it is published under
GPLv3 license. However for usage of the raw measurments provided in the data
sets, people are invited to obtain them from the providers described in
1_Obtain_and_pocess_raw_data/README.txt. PROVIDER HAVE NOT YET AGREED TO 
PUBLISH THE DATA ON OPEN SERVER


  5. Snow_cover: This directory contains mean snow water equivalent computed from
snow maps provided by the WSL Institute for Snow and Avalanche Research (SLF)
[MAGNUSSON2014].  The purpose of the data provided here is the reproduction of
the results of the paper, interested users should obtain data from the SLF.
Data are provided here with the authorisation of the provider.

REFERENCES
----------

[ENFIELD2001] Enfield, D.B., A.M. Mestas-Nunez, and P.J. Trimble, 2001: The
Atlantic Multidecadal Oscillation and its relationship to rainfall and river
flows in the continental U.S., Geophys. Res. Lett., 28: 2077-2080.

[GLAMOS2018] GLAMOS (2018). Swiss Glacier Mass Balance, release 2018, Glacier
Monitoring Switzerland, doi:10.18750/massbalance.2018.r2018.

[JONES1997] Jones, P.D., Jónsson, T. and Wheeler, D., 1997: Extension to the
North Atlantic Oscillation using early instrumental pressure observations from
Gibraltar and South-West Iceland. Int. J. Climatol. 17, 1433-1450

[MAGNUSSON2014] Magnusson, J., Gustafsson, D., Hüsler, F., & Jonas, T. (2014).
Assimilation of point SWE data into a distributed snow cover model comparing two
contrasting methods. Water resources research, 50(10), 7816-7835.

