#!/bin/bash

module load netcdf-c
module load netcdf-fortran

year=$1

# nothing to do except flip upside down and time unlimited
reformat_ERA5 ./original_lowestlevel/ERA5_u_component_of_wind_${year}_1000hPa.nc u ./processed_lowestlevel/ERA5_u_component_of_wind_${year}_1000hPa.nc
reformat_ERA5 ./original_lowestlevel/ERA5_v_component_of_wind_${year}_1000hPa.nc v ./processed_lowestlevel/ERA5_v_component_of_wind_${year}_1000hPa.nc
reformat_ERA5 ./original_lowestlevel/ERA5_temperature_${year}_1000hPa.nc t ./processed_lowestlevel/ERA5_temperature_${year}_1000hPa.nc
reformat_ERA5 ./original_lowestlevel/ERA5_specific_humidity_${year}_1000hPa.nc q ./processed_lowestlevel/ERA5_specific_humidity_${year}_1000hPa.nc

