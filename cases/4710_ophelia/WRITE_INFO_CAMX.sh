#!/bin/bash


#!/bin/bash

#Script to write  INFO_CAMX_GRID.nml
# Settings are hard-coded, but use of loops makes it 
# shorter and easier to modify

NML="./INFO_CAMx_GRID.nml"
rm -f ${NML}

# message 

echo "! This namelist was generated using: ${0}" > "$NML"
echo "" >> "${NML}"

cat >> "${NML}" <<EOF
&camx_grid_info

ngridnumber =   2

! relative to grid in NWP GRIBS
! 1,1 is lower left (SW) grid point

CAMx_grid_xbeg(1)  =   1
CAMx_grid_ybeg(1)  =   1
CAMx_grid_xend(1)  = 513
CAMx_grid_yend(1)  = 405
CAMx_grid_step(1)  =   3

CAMx_grid_xbeg(2)  = 203
CAMx_grid_ybeg(2)  = 194
CAMx_grid_xend(2)  = 324
CAMx_grid_yend(2)  = 273
CAMx_grid_step(2)  =   1


!------------------------------------
! == CAMx vertical grid structure == |
! .. the same for all grids          |
!------------------------------------
! In order to be able to define layer interfaces, we need one more CAMx level => CAMx_nLev+1
! note: in gfortran 4.6 we can use array slicing (e.g. "CAMx_levDef(2,:) =  2, 2" ... )
!       in gfortran 4.3 we can't, so specify it for each element separately 
!                                 (e.g. "CAMx_levDef(2,1) =  2" 
!                                       "CAMx_levDef(2,2) =  2" ...)

Alad_maxLev     = 68 ! number of ALADIN levels (counted from bottom) to be used for calculating CAMx levels
CAMx_nLev       = 67 ! number of CAMx levels 

EOF

for l in $(seq 1 68) ; do
  echo "CAMx_levDef(${l},1) =  ${l}" >> $NML
  echo "CAMx_levDef(${l},2) =  ${l}" >> $NML
done
echo "/"  >> $NML  


