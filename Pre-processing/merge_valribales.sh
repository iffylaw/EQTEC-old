#/bin/bash

for year in `seq 1979 2012`
do
	for month in 01 02 03 04 05 06 07 08 09 10 11 12
	do
 
cdo merge prec_buffer30km/prec_ITPCAS-CMFD_V0106_B-01_${year}${month}-masking.nc pres_buffer30km/pres_ITPCAS-CMFD_V0106_B-01_${year}${month}-masking.nc lrad_buffer30km/lrad_ITPCAS-CMFD_V0106_B-01_${year}${month}-masking.nc shum_buffer30km/shum_ITPCAS-CMFD_V0106_B-01_${year}${month}-masking.nc srad_buffer30km/srad_ITPCAS-CMFD_V0106_B-01_${year}${month}-masking.nc temp_buffer30km/temp_ITPCAS-CMFD_V0106_B-01_${year}${month}-masking.nc wind_buffer30km/wind_ITPCAS-CMFD_V0106_B-01_03hr_010deg_${year}${month}-masking.nc QTEC_buffer30_Forcing_${year}${month}.nc

	done
done

