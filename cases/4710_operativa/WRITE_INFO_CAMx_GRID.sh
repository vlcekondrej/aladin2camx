#!/bin/bash

if [ $# -ne 1 ] ; then
cat << EOF

Script to write  INFO_CAMx_grid.nml

To run this script, call it from this directory
  ./WRITE_INFO_CAMx_GRID.sh NML

 NML         namelist file name to be written.

EOF
exit 1
fi

#------------------------------------------
# Command line inputs
CAMx_GRID_NML=${1:-./INFO_CAMx_GRID.nml} # namelist name

#------------------------------------------
# Write the namelist
cat > "${CAMx_GRID_NML}" <<EOF
&camx_grid_info

ngridnumber =   2

! relative to grid in NWP GRIBS
!   1,1 is lower left (SW) grid point
!   nested grids include buffer cells

CAMx_grid_xbeg(1)  =   1
CAMx_grid_ybeg(1)  =   1
CAMx_grid_xend(1)  = 513
CAMx_grid_yend(1)  = 405
CAMx_grid_step(1)  =   3

CAMx_grid_xbeg(2)  = 204
CAMx_grid_ybeg(2)  = 195
CAMx_grid_xend(2)  = 325
CAMx_grid_yend(2)  = 274
CAMx_grid_step(2)  =   1


!------------------------------------
! == CAMx vertical grid structure == |
! .. the same for all grids          |
!------------------------------------
Alad_maxLev     = 68 ! number of ALADIN levels (counted from bottom) to be used for calculating CAMx levels
CAMx_nLev       = 32 ! number of CAMx levels 

! In order to be able to define layer interfaces, we need one more CAMx level => CAMx_nLev+1
EOF

mez[0]=0
mez[1]=0  # az po tuto hladinu se budou vypisovat po jedne. Pokud chci agregovat uz od prvni, musim zde zadat 0 a nasledny cylkus mezet od 2 namisto 1 
mez[2]=24 # az po tuto hladinu se budou agregovat po 2
mez[3]=51 # az po tuto hladinu se budou agregovat po 3
mez[4]=67 # az po tuto hladinu se budou agregovat po 4
nlev=1

for i in $(seq 2 4); do
  for lev in $( seq $((${mez[$i-1]}+1)) $i ${mez[$i]}); do

    echo "CAMx_levDef(" $nlev",1) = " $lev >> ${CAMx_GRID_NML}
    echo "CAMx_levDef(" $nlev",2) = " $(($lev+$i-1)) >> ${CAMx_GRID_NML}
    nlev=$(($nlev+1))
  done
  echo "" >> ${CAMx_GRID_NML}
done
echo "/" >> ${CAMx_GRID_NML}
