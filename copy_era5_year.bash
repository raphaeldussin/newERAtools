#!/bin/bash

mkdir original processed

year=1980

cp /archive/Raphael.Dussin/ERA5/ERA5_mean_sea_level_pressure_${year}.nc ./original/.
cp /archive/Raphael.Dussin/ERA5/ERA5_10m_u_component_of_wind_${year}.nc ./original/.
cp /archive/Raphael.Dussin/ERA5/ERA5_10m_v_component_of_wind_${year}.nc ./original/.
cp /archive/Raphael.Dussin/ERA5/ERA5_2m_dewpoint_temperature_${year}.nc ./original/.
cp /archive/Raphael.Dussin/ERA5/ERA5_2m_temperature_${year}.nc ./original/.
cp /archive/Raphael.Dussin/ERA5/ERA5_surface_pressure_${year}.nc ./original/.
cp /archive/Raphael.Dussin/ERA5/ERA5_surface_solar_radiation_downwards_${year}.nc ./original/.
cp /archive/Raphael.Dussin/ERA5/ERA5_surface_thermal_radiation_downwards_${year}.nc ./original/.
cp /archive/Raphael.Dussin/ERA5/new/ERA5_convective_rain_rate_${year}.nc ./original/.
cp /archive/Raphael.Dussin/ERA5/new/ERA5_convective_snowfall_rate_water_equivalent_${year}.nc ./original/.
cp /archive/Raphael.Dussin/ERA5/new/ERA5_large_scale_rain_rate_${year}.nc ./original/.
cp /archive/Raphael.Dussin/ERA5/new/ERA5_large_scale_snowfall_rate_water_equivalentrunoff_${year}.nc ./original/.
