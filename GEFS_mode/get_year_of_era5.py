#!/usr/bin/env python

import cdsapi
import argparse

MONTHS = [f"{m:02d}" for m in range(1, 13)]
DAYS = [f"{d:02d}" for d in range(1, 32)]
TIMES = [f"{h:02d}:00" for h in range(24)]

def format_year(value):
    """Parses the input to an integer and formats it as a 4-digit string."""
    try:
        return f"{int(value):04d}"
    except ValueError:
        raise argparse.ArgumentTypeError(f"Invalid year: '{value}'. Must be an integer.")

### -------------------- download helpers -----------------------------------------------

def download_single_level_variable(client, var, year):
    """Download 1 year of data for one single-level variable in one cdsapi request."""
    dataset = "reanalysis-era5-single-levels"
    request = {
        "product_type": ["reanalysis"],
        "variable": [var],
        "year": [str(year)],
        "month": MONTHS,
        "day": DAYS,
        "time": TIMES,
        "data_format": "netcdf",
        "download_format": "unarchived"
    }

    target = f"ERA5_{var}_{year}.nc"
    print(f"Retrieving {var} for year {year} -> {target}")
    client.retrieve(dataset, request, target)

def download_lowest_model_level_variable(client, var, param, year):
    """Download 1 year of data for one lowest model level variable in one cdsapi request."""
    request = {
        'class': 'ea',
        'date': f"{year}-01-01/to/{year}-12-31",
        'expver': '1',
        'levelist': '137',
        'levtype': 'ml',
        'param': param,
        'stream': 'oper',
        'time': '00/to/23/by/1',
        'type': 'an',
        'grid': '0.25/0.25',
        'format': 'netcdf',
    }

    target = f"ERA5_{var}_{year}_lev137.nc"
    print(f"Retrieving lowest model level {var} (param {param}) for year {year} -> {target}")
    client.retrieve('reanalysis-era5-complete', request, target)

### -------------------- lowest level ---------------------------------------------------

def download_lowest_model_level(args, client=None):
    """Download the lowest model level through MARS server"""
    if client is None:
        client = cdsapi.Client()

    variables = {
        "temperature": "130",
        "u_component_of_wind": "131",
        "v_component_of_wind": "132",
        "specific_humidity": "133",
    }

    for var, param in variables.items():
        download_lowest_model_level_variable(client, var, param, args.year)

    return None

### -------------------- standard level ---------------------------------------------------

def download_single_level_inst(args, client=None):
    """instantaneous field near surface"""
    if client is None:
        client = cdsapi.Client()

    variables = [
        "10m_u_component_of_wind",
        "10m_v_component_of_wind",
        "2m_dewpoint_temperature",   # needed for specific humidity
        "2m_temperature",
        "mean_sea_level_pressure",
        "sea_surface_temperature",
        "surface_pressure",          # needed for specific humidity
        "geopotential",
        "sea_ice_cover",
    ]

    for var in variables:
        download_single_level_variable(client, var, args.year)

    return None

def download_single_level_wave(args, client=None):
    """instantaneous field near surface from wave model"""
    if client is None:
        client = cdsapi.Client()

    variables = [
        "air_density_over_the_oceans",
    ]

    for var in variables:
        download_single_level_variable(client, var, args.year)

    return None

def download_single_level_accum(args, client=None):
    """accumulated surface heat fluxes (diagnostics)"""
    if client is None:
        client = cdsapi.Client()

    variables = [
        "surface_latent_heat_flux",
        "surface_net_solar_radiation",
        "surface_net_thermal_radiation",
        "surface_sensible_heat_flux",
        "surface_solar_radiation_downwards",
        "surface_thermal_radiation_downwards",
    ]

    for var in variables:
        download_single_level_variable(client, var, args.year)

    return None

def download_single_level_avg(args, client=None):
    """mean rates freshwater/radiative fluxes"""
    if client is None:
        client = cdsapi.Client()

    variables = [
        "mean_snowfall_rate",
        "mean_surface_downward_long_wave_radiation_flux",
        "mean_surface_downward_short_wave_radiation_flux",
        "mean_total_precipitation_rate",
    ]

    for var in variables:
        download_single_level_variable(client, var, args.year)

    return None

### -------------------- main script  ---------------------------------------------------

def main():
    parser = argparse.ArgumentParser(description="Download ERA5 data for a whole year.")

    # Define the arguments using the custom formatting functions
    parser.add_argument('-y', '--year', type=format_year, required=True, help="Year (e.g., 2026 or 99)")
    parser.add_argument('--with_diag_fluxes', action=argparse.BooleanOptionalAction, help="Download diagnostic heat fluxes")

    args = parser.parse_args()

    # The arguments are now pre-formatted strings
    print(f"Formatted Year:  {args.year}")

    client = cdsapi.Client()

    # lowest model level
    download_lowest_model_level(args, client=client)

    # near-surface
    download_single_level_inst(args, client=client)

    # air density surface from wave model (may or may not be used)
    #download_single_level_wave(args, client=client)

    # heat fluxes (diagnostics)
    if args.with_diag_fluxes:
        download_single_level_accum(args, client=client)

    # freshwater/radiative fluxes
    download_single_level_avg(args, client=client)

if __name__ == "__main__":
    main()
