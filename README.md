# aladin2camx
preprocessor to converts grib files provided by ALADIN NWP model into input files for CAMx CTM model

I will add some better documentation soon ;).

Quick usage:

change to dir where you want to run aladin2camx
from aladin2camx/scripts copy WRITE_INFO_RUN.sh & FILE_FROM_DATE.sh & job.sample.sh
from aladin2camx/src copy INFO_CAMx_GRID.nml.sample & INFO_ALADIN_GRIBS.nml.sample

change dir to aladin2camx/src
   run git checkout CAMx_v5.41 (version for CAMx 6.x is not ready yet)
   copy config.make.sample to config.make ad and adjust it 
   run make
   copy aladin2camx_MAIN.exe & aladin2camx_control.nml.sample to dir where you want to run aladin2camx
   
 Good luck!

