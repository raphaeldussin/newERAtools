#!/bin/bash

module load netcdf-c
module load netcdf-fortran

year=1979
yearm1=$(( $year - 1 ))

echo ${yearm1}

create_bogus_firstyear ./processed_lowestlevel/ERA5_u_component_of_wind_${year}_1000hPa.nc u ./processed_lowestlevel/ERA5_u_component_of_wind_${yearm1}_1000hPa.nc

create_bogus_firstyear ./processed_lowestlevel/ERA5_v_component_of_wind_${year}_1000hPa.nc v ./processed_lowestlevel/ERA5_v_component_of_wind_${yearm1}_1000hPa.nc

create_bogus_firstyear ./processed_lowestlevel/ERA5_specific_humidity_${year}_1000hPa.nc q ./processed_lowestlevel/ERA5_specific_humidity_${yearm1}_1000hPa.nc

create_bogus_firstyear ./processed_lowestlevel/ERA5_temperature_${year}_1000hPa.nc t ./processed_lowestlevel/ERA5_temperature_${yearm1}_1000hPa.nc

