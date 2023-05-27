#!/bin/bash

module load netcdf-fortran/4.5.3
module load netcdf-c/4.8.1

year=1980
yearm1=$(( $year - 1 ))

create_bogus_firstyear ./processed/ERA5_10m_u_component_of_wind_${year}.nc u10 ./processed/ERA5_10m_u_component_of_wind_${yearm1}.nc

create_bogus_firstyear ./processed/ERA5_10m_v_component_of_wind_${year}.nc v10 ./processed/ERA5_10m_v_component_of_wind_${yearm1}.nc

create_bogus_firstyear ./processed/ERA5_2m_specific_humidity_${year}.nc q2m ./processed/ERA5_2m_specific_humidity_${yearm1}.nc

create_bogus_firstyear ./processed/ERA5_2m_temperature_${year}.nc t2m ./processed/ERA5_2m_temperature_${yearm1}.nc

create_bogus_firstyear ./processed/ERA5_mean_sea_level_pressure_${year}.nc msl ./processed/ERA5_mean_sea_level_pressure_${yearm1}.nc

create_bogus_firstyear ./processed/ERA5_surface_pressure_${year}.nc sp ./processed/ERA5_surface_pressure_${yearm1}.nc

create_bogus_firstyear ./processed/ERA5_surface_solar_radiation_downwards_${year}.nc ssrd ./processed/ERA5_surface_solar_radiation_downwards_${yearm1}.nc

create_bogus_firstyear ./processed/ERA5_surface_thermal_radiation_downwards_${year}.nc strd ./processed/ERA5_surface_thermal_radiation_downwards_${yearm1}.nc

create_bogus_firstyear ./processed/ERA5_total_rainfall_rate_${year}.nc rain ./processed/ERA5_total_rainfall_rate_${yearm1}.nc

create_bogus_firstyear ./processed/ERA5_total_snowfall_rate_${year}.nc snow ./processed/ERA5_total_snowfall_rate_${yearm1}.nc


