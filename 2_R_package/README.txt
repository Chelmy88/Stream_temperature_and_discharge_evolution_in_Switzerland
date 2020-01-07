2_R_package
===========

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

This folder contains the R package required to run scripts in "3_Produce_data"
and in "4_Run_analysis". In addition, it provides the documentation and the
source code of the package.

In the documentation file (the pdf file), user are invited to read the section
'swisswatertemp' starting on page 25, which describes in detail the datasets
provided in "4_Run_analysis/data/rds_data"


USAGE
----- 

First, the following packages should be install in R:
 - data.table
 - rgdal 
 - sp
 - raster 
 - zoo 
 - gridExtra
 - GISTools 
 - RColorBrewer 
 - graphics 
 - Hmisc 
 - lubridate 
 - Partiallyoverlapping
 - MASS

This can be done by installing each packages individually using:

   install.package("package_nam")

Or by running the provided 'install_dependencies.R' script.

The installation of the R package can be done by running:

  install.packages(path_to_file, repos = NULL, type="source")

This should be run within an R session, and 'path_to_file' should point to the
file 'swisswatertemp_1.0.0.tar.gz'. This path should be relative to the current
working directory of the R session which can be obtained by running getwd().

For more information see e.g.
https://stackoverflow.com/questions/1474081/how-do-i-install-an-r-package-from-source
