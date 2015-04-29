#!/bin/bash
for year in `seq 1979 2012`
do
        for month in 01 02 03 04 05 06 07 08 09 10 11 12
        do

ncatted -O -a min,,d,, QTEC_buffer30_Forcing_${year}${month}.nc
ncatted -O -a max,,d,, QTEC_buffer30_Forcing_${year}${month}.nc
ncatted -O -a history,global,d,c,"" QTEC_buffer30_Forcing_${year}${month}.nc

	done
done
