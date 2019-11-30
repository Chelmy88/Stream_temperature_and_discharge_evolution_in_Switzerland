1_Obtain_and_pocess_raw_data
============================

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

This folder contains the scripts to produce SMET files from raw data from
various providers.  SMET files are required as input for the analysis.  The SMET
format is described here:
https://models.slf.ch/docserver/meteoio/SMET_specifications.pdf

The providers and requested data are :

Amt für Wasser und Abfall (AWA) Canton Bern, Switzerland
https://www.bve.be.ch/bve/de/index/direktion/organisation/awa.html Hourly
discharge and water temperature

Amt für Abfall, Wasser, Energie und Luft (AWEL), Canton Zürich, Switzerland
https://awel.zh.ch/internet/baudirektion/awel/de/home.html Hourly discharge and
water temperature

Federal Office for the Environment (FOEN), Bern, Switzerland
https://www.hydrodaten.admin.ch/en/ Daily and hourly (when available) discharge
and water temperature

Federal Office of Meteorology and Climatology (MeteoSwiss), Zurich, Switzerland
https://gate.meteoswiss.ch/idaweb/ Daily 2m air temperature and cumulative
precipitation, as homogenous time series when available.  Data should be ordered
as CSV files

The data in the data_Hydrologische_Messstationen are provided by the Federal
Office of Environment (FOEN) and are available at:
https://data.geo.admin.ch/ch.bafu.hydrologie-hydromessstationen/. Data are
reproduced in agreement with the provider.

A list of the various stations used can be found in
“3_Produce_data/Preprocessing.R”, in
“3_Produce_data/data/discharge_gauging_station.xlsx”, or in the tables given in
the paper and in the supplementary.  The dates of the time series used also can
be found in the tables given in the paper and in the supplementary.

These data are not provided here, users must contact the providers to obtain
data before running the scripts.

Once the data are produced they should be moved to the corresponding directories
in “3_Produce_data”.  AWA, AWEL and FOEN data should go to
“3_Produce_data/data”, FOEN 1H data should go to “3_Produce_data/data_1h”,
MeteoSwiss data go to “3_Produce_data/meteo/data/” and MeteoSwiss homogenous
data to “3_Produce_data/meteo/data_hom”.

FOEN hourly data are used for some analysis but the main dataset is built on
daily data to reduce the size of the dataset. MeteoSwiss homogenous time series
are already used when possible in the MeteoSwiss file. A dataset of homogenous
time series is produced separately and used for some analysis. This dataset is
built on using the longest available time series, while the MeteoSwiss data used
along with the hydrological data The main dataset is tailored to the length of
the hydrological data. It is also to reduce size that it is produced separately.
Indeed, while the data set produced with hydrological data at daily resolution
(FOEN) and hourly resolution (AWA and AWEL) contains many other derived data
(e.g. temporal means, STL analysis), the dataset for FOEN hourly data and the
homogenous MeteoSwiss data contain only the desired and minimum data.

USAGE
-----

  1. AWA: To generate SMET files, the 'input' and 'output' variables at the
beginning of the 'AWA_pre_proc.py' script should be set to point to the
directory containing the input files provided by AWA and to an existing
directory where output SMET files should be written. The path should be relative
to the path from where the script is run.

  2. AWEL: To generate SMET files, the 'input' and 'output' variables at the
beginning of the 'AWEL_pre_proc_T.py' and 'AWEL_pre_proc_Q.py' scripts should be
set to point to the directory containing the input files provided by AWEL and to
an existing directory where output SMET files should be written. The path should
be relative to the path from where the script is run.  'AWEL_pre_proc_T.py' and
'AWEL_pre_proc_Q.py' should be run first. Then 'input' and 'output' variables at
the beginning of the 'AWEL_pre_proc_merge.py' file should also be adjusted and
the script should be run to merge discharge and water temperature data into a
single file.

  3. FOEN: To generate SMET files, the 'input' and 'output' variables at the
beginning of the 'FOEN_pre_proc.py' script should be set to point to the
directory containing the input files of daily data provided by FOEN and to an
existing directory where output SMET files should be written. The path should be
relative to the path from where the script is run.


  4. FOEN hourly: To generate SMET files, the 'input' and 'output' variables at
the beginning of the 'FOEN_pre_proc_hhourly.py' script should be set to point to
the directory containing the input files of hourly data provided by FOEN and to
an existing directory where output SMET files should be written. The path should
be relative to the path from where the script is run.

  5. MeteoSwiss: To generate SMET files, the 'input' and 'output' variables at
lines 333-334 of the 'MeteoSwiss_pre_proc.py' script should be set to point to
the directory containing the directories obtained from MeteoSwiss (directories
named "order######") and to an existing directory where output SMET files should
be written. The path should be relative to the path from where the script is
run.


  6. MeteoSwiss homogenous: To generate SMET files, the 'input' and 'output'
variables at lines 333-334 of the 'MeteoSwiss_pre_proc_HOM.py' script should be
set to point to the directory containing the directories obtained from
MeteoSwiss (directories named "order######") and to an existing directory where
output SMET files should be written. The path should be relative to the path
from where the script is run.

