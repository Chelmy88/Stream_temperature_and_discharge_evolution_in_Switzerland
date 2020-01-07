3_Produce_data
==============

This content is related to the paper:

Stream temperature and discharge evolution in Switzerland over the last 50 years: 
annual and seasonal behaviour

Adrien Michel, Tristan Brauchli, Michael Lehning, Bettina Schaefli, 
and Hendrik Huwald

Hydrol. Earth Syst. Sci., 2020


Any use of the material (code or data) presented here should clearly reference
to this paper and to the providers of the data mentioned in the documentation.

This material is distributed under the GPLv3 license
(https://www.gnu.org/licenses/gpl-3.0.html)

Author: Adrien Michel, adrien.michel@epfl.ch, 01.2020

INTRODUCTION
------------

This folder contains the scripts to produce the datasets used in
"4_Run_analysis".  
The production of the datasets consists of loading the hydrological
and meteorological data, link them together, compute some temporal means,
compute deseasonalized time series with STL, and compute the liner regression
over the variables. For more details about the dataset see the README and the
documentation in "2_R_package". Prior to running this script, the steps
described in 1_Obtain_and_pocess_raw_data and 2_R_package should be performed.

USAGE
-----

Simply run the "Preprocessing.R" script, see comments in the script for details.

Note that in addition to the provided package, the packages 'lubridate' and
'gdata' must be installed.
