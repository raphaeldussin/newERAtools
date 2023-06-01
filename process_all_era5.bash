#!/bin/bash

era5dir=/archive/Raphael.Dussin/ERA5

#---------------------------------------------------------
mkdir original processed

for year in $( seq 1979 2018 ) ; do

  #--- unarchive files
  date
  echo "unarchive files for $year"
  dmget ${era5dir}/ERA5_mean_sea_level_pressure_${year}.nc ${era5dir}/ERA5_10m_u_component_of_wind_${year}.nc ${era5dir}/ERA5_10m_v_component_of_wind_${year}.nc ${era5dir}/ERA5_2m_dewpoint_temperature_${year}.nc ${era5dir}/ERA5_2m_temperature_${year}.nc ${era5dir}/ERA5_surface_pressure_${year}.nc ${era5dir}/ERA5_surface_solar_radiation_downwards_${year}.nc ${era5dir}/ERA5_surface_thermal_radiation_downwards_${year}.nc ${era5dir}/new/ERA5_convective_rain_rate_${year}.nc ${era5dir}/new/ERA5_convective_snowfall_rate_water_equivalent_${year}.nc ${era5dir}/new/ERA5_large_scale_rain_rate_${year}.nc ${era5dir}/new/ERA5_large_scale_snowfall_rate_water_equivalentrunoff_${year}.nc
  date

  #--- link into local directory
  ln -s /archive/Raphael.Dussin/ERA5/ERA5_mean_sea_level_pressure_${year}.nc ./original/.
  ln -s /archive/Raphael.Dussin/ERA5/ERA5_10m_u_component_of_wind_${year}.nc ./original/.
  ln -s /archive/Raphael.Dussin/ERA5/ERA5_10m_v_component_of_wind_${year}.nc ./original/.
  ln -s /archive/Raphael.Dussin/ERA5/ERA5_2m_dewpoint_temperature_${year}.nc ./original/.
  ln -s /archive/Raphael.Dussin/ERA5/ERA5_2m_temperature_${year}.nc ./original/.
  ln -s /archive/Raphael.Dussin/ERA5/ERA5_surface_pressure_${year}.nc ./original/.
  ln -s /archive/Raphael.Dussin/ERA5/ERA5_surface_solar_radiation_downwards_${year}.nc ./original/.
  ln -s /archive/Raphael.Dussin/ERA5/ERA5_surface_thermal_radiation_downwards_${year}.nc ./original/.
  ln -s /archive/Raphael.Dussin/ERA5/new/ERA5_convective_rain_rate_${year}.nc ./original/.
  ln -s /archive/Raphael.Dussin/ERA5/new/ERA5_convective_snowfall_rate_water_equivalent_${year}.nc ./original/.
  ln -s /archive/Raphael.Dussin/ERA5/new/ERA5_large_scale_rain_rate_${year}.nc ./original/.
  ln -s /archive/Raphael.Dussin/ERA5/new/ERA5_large_scale_snowfall_rate_water_equivalentrunoff_${year}.nc ./original/.

  #--- process the files
  echo "processing files for $year"
  ./process_era5_year.bash $year
  date

done

#--- do the bogus first year
echo "finishing up"
./process_era5_firstyear.bash
date
