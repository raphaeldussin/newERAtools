#!/usr/bin/env python

import xarray as xr
import numpy as np
import argparse
import yaml


def format_year(value):
    """Parses the input to an integer and formats it as a 4-digit string."""
    try:
        return f"{int(value):04d}"
    except ValueError:
        raise argparse.ArgumentTypeError(f"Invalid year: '{value}'. Must be an integer.")

def format_two_digits(value):
    """Parses the input to an integer and formats it as a 2-digit string."""
    try:
        return f"{int(value):02d}"
    except ValueError:
        raise argparse.ArgumentTypeError(f"Invalid input: '{value}'. Must be an integer.")

def flip_ud(da, flip=True):
    """ flip array upside down (increasing latitude) """
    if flip:
        return da.squeeze().transpose(*("valid_time", "latitude", "longitude")).values[:,::-1,:]
    else:
        return da.squeeze().transpose(*("valid_time", "latitude", "longitude")).values[:,:,:]

def flip_ud_1d(da, flip=True):
    """ flip array upside down (increasing latitude) """
    if flip:
        return da.values[::-1]
    else:
        return da.values

### -------------------- main script  ---------------------------------------------------

def main():
    parser = argparse.ArgumentParser(description="Format date components into 4-digit and 2-digit strings.")

    # Define the arguments using the custom formatting functions
    parser.add_argument('-y', '--year', type=format_year, required=True, help="Year (e.g., 2026 or 99)")
    parser.add_argument('-m', '--month', type=format_two_digits, required=True, help="Month (e.g., 9)")
    parser.add_argument('-d', '--days', type=format_two_digits, required=True, help="Days (e.g., 5)")
    parser.add_argument('--flip', action=argparse.BooleanOptionalAction, help="flip latitudes")

    args = parser.parse_args()

    # The arguments are now pre-formatted strings
    print(f"Formatted Year:  {args.year}")
    print(f"Formatted Month: {args.month}")
    print(f"Formatted Days:  {args.days}")
    print(f"Combined Date:   {args.year}-{args.month}-{args.days}")
    print(f"Latitude flip: {args.flip}")

    year = args.year ; month = args.month ; days = args.days
    flip = args.flip

    with open('constants.yaml', 'r') as file:
        param = yaml.safe_load(file)

    # Syntax: dictionary.get('key', default_value)
    gas_constant = param.get('constants', {}).get('Rd', 287.0597)
    gravity = param.get('constants', {}).get('g', 9.80665)

    print(f"Constants - Rd = {gas_constant}, g = {gravity}")

    vis_dir = param.get('solar', {}).get('visible_direct', 0.285)
    vis_dif = param.get('solar', {}).get('visible_diffuse', 0.285)
    nir_dir = param.get('solar', {}).get('near_IR_direct', 0.215)
    nir_dif = param.get('solar', {}).get('near_IR_diffuse', 0.215)

    print(f"Visible Diffuse: {vis_dif}, Direct: {vis_dir}")
    print(f"Near InfraRed Diffuse: {nir_dif}, Direct: {nir_dir}")

    # ----------------------------------------------------------------------------------------
    # create dataset
    out = xr.Dataset()

    # write coords (all files should be consistent)
    fluxes = xr.open_dataset(f"ERA5_y{year}m{month}d{days}_mean_fluxes.nc", decode_times=False)
    instant = xr.open_dataset(f"ERA5_y{year}m{month}d{days}_inst.nc", decode_times=False)
    lowest = xr.open_dataset(f"ERA5_y{year}m{month}d{days}_lev137.nc", decode_times=False)

    assert np.allclose(fluxes["valid_time"].values, instant["valid_time"].values)
    assert np.allclose(fluxes["valid_time"].values, lowest["valid_time"].values)

    assert np.allclose(fluxes["longitude"].values, instant["longitude"].values)
    assert np.allclose(fluxes["longitude"].values, lowest["longitude"].values)

    assert np.allclose(fluxes["latitude"].values, instant["latitude"].values)
    assert np.allclose(fluxes["latitude"].values, lowest["latitude"].values)

    out["time"] = xr.DataArray(data=fluxes["valid_time"].values.astype(np.float64), dims=("time"),
                               attrs={"units": "seconds since 1970-01-01 00:00:00.0",
		                      "calendar": "gregorian", "axis": "T"})

    out["lat"] = xr.DataArray(data=flip_ud_1d(fluxes["latitude"], flip=flip).astype(np.float32), dims=("lat"),
                              attrs={"units": "degrees_north", "long_name": "Latitude"})

    out["lon"] = xr.DataArray(data=fluxes["longitude"].values.astype(np.float32), dims=("lon"),
                              attrs={"units": "degrees_east", "long_name": "Longitude",
                                     "modulo": " "})


    # ----------------------------------------------------------------------------------------
    # radiative fluxes

    # total downwelling longwave (thermal) radiation
    out["DLWRF"] = xr.DataArray(data=flip_ud(fluxes["avg_sdlwrf"], flip=flip), dims=("time", "lat", "lon"),
                                attrs={"units": "W/m**2", "long_name": "surface downward longwave flux"})

    # total downwelling shortwave (solar) radiation
    out["DSWRF"] = xr.DataArray(data=flip_ud(fluxes["avg_sdswrf"], flip=flip), dims=("time", "lat", "lon"),
                                attrs={"units": "W/m**2", "long_name": "averaged surface downward shortwave flux"})

    # Split shortwave into 4 bands
    # vbdsf_ave = Visible Beam Downward Solar Flux = sw_flux_vis_dir_dn = 0.285 * solar
    # vddsf_ave = Visible Diffuse Downward Solar Flux = sw_flux_vis_dif_dn = 0.285 * solar
    # nbdsf_ave = Near IR Beam Downward Solar Flux = sw_flux_nir_dir_dn = 0.215 * solar
    # nddsf_ave = Near IR Diffuse Downward Solar Flux = sw_flux_nir_dif_dn = 0.215 * solar

    out["vbdsf_ave"] = vis_dir * out["DSWRF"]
    out["vbdsf_ave"].attrs.update({"units": "W/m**2", "long_name": "Visible Beam Downward Solar Flux"})

    out["vddsf_ave"] = vis_dif * out["DSWRF"]
    out["vddsf_ave"].attrs.update({"units": "W/m**2", "long_name": "Visible Diffuse Downward Solar Flux"})

    out["nbdsf_ave"] = nir_dir * out["DSWRF"]
    out["nbdsf_ave"].attrs.update({"units": "W/m**2", "long_name": "Near IR Beam Downward Solar Flux"})

    out["nddsf_ave"] = nir_dif * out["DSWRF"]
    out["nddsf_ave"].attrs.update({"units": "W/m**2", "long_name": "Near IR Diffuse Downward Solar Flux"})

    # ----------------------------------------------------------------------------------------
    # freshwater fluxes

    # combined precip
    # np.allclose(gefs["precp"].values + gefs["fprecp"].values, gefs["totprcp_ave"].values, atol=1e-16) = True

    out["totprcp_ave"] = xr.DataArray(data=flip_ud(fluxes["avg_tprate"], flip=flip), dims=("time", "lat", "lon"),
                                      attrs={"units": "kg/m**2/s", "long_name": "surface precipitation rate"})

    # snowfall rate
    out["fprecp"] = xr.DataArray(data=flip_ud(fluxes["avg_tsrwe"], flip=flip), dims=("time", "lat", "lon"),
                                 attrs={"units": "kg/m**2/s", "long_name": "surface snow precipitation rate"})

    # liquid precip rate = total - snow
    out["precp"] = (out["totprcp_ave"] - out["fprecp"]).clip(min=0)
    out["precp"].attrs.update({"units": "kg/m**2/s", "long_name": "surface rain precipitation rate"})


    # ----------------------------------------------------------------------------------------
    # turbulent variables at reference level (2m/10m)

    # compute specific humidity, for reference: Grant Petty, A First Course in Atmospheric Thermodynamics
    d2 = instant["d2m"]  # dewpoint temperature
    pres = instant["sp"] # surface pressure

    # saturation water pressure using Bolton approximation (Petty 7.3)
    d2C = d2 - 273.15 # converted to Celsius
    e2 = 611.2 * np.exp((17.67 * d2C)/(d2C + 243.5))

    # specific humidity from pressure and saturation water pressure (Petty 3.41)
    q2 = (0.622 * e2) / (pres - (1.0 - 0.622) * e2)

    out["q2m"] = xr.DataArray(data=flip_ud(q2, flip=flip), dims=("time", "lat", "lon"),
                              attrs={"units": "kg/kg", "long_name": "2m specific humidity"})

    out["t2m"] = xr.DataArray(data=flip_ud(instant["t2m"], flip=flip), dims=("time", "lat", "lon"),
                              attrs={"units": "K", "long_name": "2m temperature"})

    out["u10m"] = xr.DataArray(data=flip_ud(instant["u10"], flip=flip), dims=("time", "lat", "lon"),
                               attrs={"units": "m/s", "long_name": "10 meter u wind"})

    out["v10m"] = xr.DataArray(data=flip_ud(instant["v10"], flip=flip), dims=("time", "lat", "lon"),
                               attrs={"units": "m/s", "long_name": "10 meter v wind"})

    out["psurf"] = xr.DataArray(data=flip_ud(instant["sp"], flip=flip), dims=("time", "lat", "lon"),
                               attrs={"units": "Pa", "long_name": "surface pressure"})

    # ----------------------------------------------------------------------------------------
    # turbulent variables at lowest model level (137)

    out["spfh_hyblev1"] = xr.DataArray(data=flip_ud(lowest["q"], flip=flip), dims=("time", "lat", "lon"),
                                       attrs={"units": "kg/kg", "long_name": "layer 1 specific humidity"})

    out["tmp_hyblev1"] = xr.DataArray(data=flip_ud(lowest["t"], flip=flip), dims=("time", "lat", "lon"),
                                      attrs={"units": "K", "long_name": "layer 1 temperature"})

    out["ugrd_hyblev1"] = xr.DataArray(data=flip_ud(lowest["u"], flip=flip), dims=("time", "lat", "lon"),
                                       attrs={"units": "m/s", "long_name": "layer 1 zonal wind"})

    out["vgrd_hyblev1"] = xr.DataArray(data=flip_ud(lowest["v"], flip=flip), dims=("time", "lat", "lon"),
                                       attrs={"units": "m/s", "long_name": "layer 1 meridional wind"})

    # Compute pressure and Height AGL (Above ground level) for model level 137
    # Constants
    Rd = gas_constant  # Gas constant for dry air (J/kg·K)
    g = gravity        # Gravity (m/s^2)

    # t = temperature at level 137 (Kelvin)
    # q = specific humidity at level 137 (kg/kg)
    # sp = surface pressure (Pascals)
    t = out["tmp_hyblev1"]
    q = out["spfh_hyblev1"]
    sp = out["psurf"]

    # 1. Calculate Virtual Temperature (Tv)
    # Moist air is less dense than dry air, so we adjust the temperature
    Tv = t * (1.0 + 0.609133 * q)

    # 2. Calculate Pressure at the boundaries of Level 137
    # https://confluence.ecmwf.int/spaces/UDOC/pages/108117123/L137+model+level+definitions
    # Level 137 is bounded by half-level 137.5 (the exact surface) and half-level 136.5 (the top)
    # ECMWF B-coefficient for level 137.5 = 1 and 136.5 = 0.997630 so the pressure of lev 137 is
    # P_137 = B_137 * Psurf with B_137 = 0.5 * (1 + 0.997630) = 0.998815
    # alternatively we can find the same factor using pf (pressure full level) / ph (pressure half level)
    # for level 137: pf / ph = 1012.0494 / 1013.2500 = 0.998815

    P_surf = sp
    B_137 = 0.998815
    P_137 = B_137 * sp

    # 3. Calculate Height AGL (in meters)
    height_agl = (Rd * Tv / g) * np.log(P_surf / P_137)

    out["pres_hyblev1"] = xr.DataArray(P_137.values, dims=("time", "lat", "lon"),
                                       attrs={"units": "Pa", "long_name": "layer 1 pressure"})
    out["hgt_hyblev1"] = xr.DataArray(data=height_agl.values, dims=("time", "lat", "lon"),
                                      attrs={"units": "m", "long_name": "layer 1 height"})

    # ----------------------------------------------------------------------------------------
    # land/sea/ice mask from sea_ice_cover (not needed)

    # siconc has values ranging from 0 (open ocean) to 1, land is fill value
    # sea-ice has label 2 regardless of concentration, open ocean is 0, land is one
    # ice extent is typically defined by concentration > 15%, this is the threshold used here
    loim = xr.where(instant["siconc"] > 0.15, 2, 0).where(~np.isnan(instant["siconc"])).fillna(1.)
    out["slmsksfc"] = xr.DataArray(data=flip_ud(loim, flip=flip), dims=("time", "lat", "lon"),
                                   attrs={"units": "numerical", "long_name": "sea-land-ice mask (0-sea, 1-land, 2-ice)"})

    # ----------------------------------------------------------------------------------------
    # write the dataset

    encoding = {}
    for v in list(out.variables):
        encoding.update({v: {"_FillValue": None}})

    fout = f"era5_forcing_y{year}m{month}d{days}.nc"
    out.to_netcdf(fout, unlimited_dims=("time"), format="NETCDF4", encoding=encoding)

if __name__ == "__main__":
    main()
