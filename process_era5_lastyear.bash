#!/bin/bash

module load netcdf-c/4.8.1
module load netcdf-fortran/4.5.3

# we keep this separate from the rest of the files because it is not permanent

year=2023
tmpdir=/work/Raphael.Dussin/work_ERA5/2023_jan1

mkdir -p ${tmpdir}/processed

# nothing to do except flip upside down and time unlimited
reformat_ERA5 ${tmpdir}/ERA5_10m_u_component_of_wind_${year}.nc u10 ${tmpdir}/processed/ERA5_10m_u_component_of_wind_${year}.nc
reformat_ERA5 ${tmpdir}/ERA5_10m_v_component_of_wind_${year}.nc v10 ${tmpdir}/processed/ERA5_10m_v_component_of_wind_${year}.nc
reformat_ERA5 ${tmpdir}/ERA5_2m_temperature_${year}.nc t2m ${tmpdir}/processed/ERA5_2m_temperature_${year}.nc
# msl pressure is more adequate since it will take care of orographic gradients effect on surface pressure
reformat_ERA5 ${tmpdir}/ERA5_mean_sea_level_pressure_${year}.nc msl ${tmpdir}/processed/ERA5_mean_sea_level_pressure_${year}.nc
#reformat_ERA5 ${tmpdir}/ERA5_surface_pressure_${year}.nc sp ${tmpdir}/processed/ERA5_surface_pressure_${year}.nc

# compute specific humidity
compute_q2_ERA5 ${tmpdir}/ERA5_2m_dewpoint_temperature_${year}.nc ${tmpdir}/ERA5_surface_pressure_${year}.nc d2m sp ${tmpdir}/processed/ERA5_2m_specific_humidity_${year}.nc
# this is not technically correct
#compute_q2_ERA5 ${tmpdir}/ERA5_2m_dewpoint_temperature_${year}.nc ${tmpdir}/ERA5_mean_sea_level_pressure_${year}.nc d2m msl ${tmpdir}/processed/ERA5_2m_specific_humidity_${year}.nc

# radiative fluxes to W/m2
fix_radiative_ERA5 ${tmpdir}/ERA5_surface_solar_radiation_downwards_${year}.nc ssrd ${tmpdir}/processed/ERA5_surface_solar_radiation_downwards_${year}.nc
fix_radiative_ERA5 ${tmpdir}/ERA5_surface_thermal_radiation_downwards_${year}.nc strd ${tmpdir}/processed/ERA5_surface_thermal_radiation_downwards_${year}.nc

# combine precips
merge_precips_ERA5 ${tmpdir}/ERA5_convective_rain_rate_${year}.nc ${tmpdir}/ERA5_large_scale_rain_rate_${year}.nc crr lsrr ${tmpdir}/processed/ERA5_total_rainfall_rate_${year}.nc rain

merge_precips_ERA5 ${tmpdir}/ERA5_convective_snowfall_rate_water_equivalent_${year}.nc ${tmpdir}/ERA5_large_scale_snowfall_rate_water_equivalent_${year}.nc csfr lssfr ${tmpdir}/processed/ERA5_total_snowfall_rate_${year}.nc snow
