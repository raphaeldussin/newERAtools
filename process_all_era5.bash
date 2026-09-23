#!/bin/bash

era5dir=./download

#---------------------------------------------------------
mkdir original processed

#for year in $( seq 2020 2025 ) ; do
for year in 20260101 ; do

  date

  #--- revert to netcdf short
  ./revert_ERA5 ./download/ERA5_mean_sea_level_pressure_${year}.nocomp.nc msl ./original/ERA5_mean_sea_level_pressure_${year}.nc
  ./revert_ERA5 ./download/ERA5_10m_u_component_of_wind_${year}.nocomp.nc u10 ./original/ERA5_10m_u_component_of_wind_${year}.nc
  ./revert_ERA5 ./download/ERA5_10m_v_component_of_wind_${year}.nocomp.nc v10 ./original/ERA5_10m_v_component_of_wind_${year}.nc
  ./revert_ERA5 ./download/ERA5_2m_dewpoint_temperature_${year}.nocomp.nc d2m ./original/ERA5_2m_dewpoint_temperature_${year}.nc
  ./revert_ERA5 ./download/ERA5_2m_temperature_${year}.nocomp.nc t2m ./original/ERA5_2m_temperature_${year}.nc
  ./revert_ERA5 ./download/ERA5_surface_pressure_${year}.nocomp.nc sp ./original/ERA5_surface_pressure_${year}.nc
  # ! this is non the right thing to do as it sets a small value when ssrd = 0. and makes it spval
  ###./revert_ERA5 ./download/ERA5_surface_solar_radiation_downwards_${year}.nocomp.nc ssrd ./original/ERA5_surface_solar_radiation_downwards_${year}.nc
  # ! this fixes the problem and does the reversing of latitude with it so we don't have to do it in the next step:
  ./revert_ERA5_radiative ./download/ERA5_surface_solar_radiation_downwards_${year}.nocomp.nc ssrd ./original/ERA5_surface_solar_radiation_downwards_${year}.nc
  ./revert_ERA5 ./download/ERA5_surface_thermal_radiation_downwards_${year}.nocomp.nc strd ./original/ERA5_surface_thermal_radiation_downwards_${year}.nc
  ./revert_ERA5 ./download/ERA5_convective_rain_rate_${year}.nocomp.nc crr ./original/ERA5_convective_rain_rate_${year}.nc
  ./revert_ERA5 ./download/ERA5_convective_snowfall_rate_water_equivalent_${year}.nocomp.nc csfr ./original/ERA5_convective_snowfall_rate_water_equivalent_${year}.nc
  ./revert_ERA5 ./download/ERA5_large_scale_rain_rate_${year}.nocomp.nc lsrr ./original/ERA5_large_scale_rain_rate_${year}.nc
  ./revert_ERA5 ./download/ERA5_large_scale_snowfall_rate_water_equivalent_${year}.nocomp.nc lssfr ./original/ERA5_large_scale_snowfall_rate_water_equivalent_${year}.nc

  date

  #--- process the files
  echo "processing files for $year"
  ./process_era5_year.bash $year
  date

done

#--- do the bogus first year
#echo "finishing up"
#./process_era5_firstyear.bash
#date
