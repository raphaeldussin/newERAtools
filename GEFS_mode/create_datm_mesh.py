#!/usr/bin/env python

import argparse
import numpy as np
import xarray as xr
import cf_xarray
import subprocess
import matplotlib.pyplot as plt


def generate_era5_mesh(era5_path, out_mesh_path):
    # 1. Read coordinates and FORCE 64-bit precision to prevent drift
    ds = xr.open_dataset(era5_path)
    lat = ds['lat'].values.astype(np.float64)
    lon = ds['lon'].values.astype(np.float64)
    nlat, nlon = len(lat), len(lon)
    grid_size = nlat * nlon

    # 2. Build edges
    lat_edges = np.zeros(nlat + 1, dtype=np.float64)
    lat_edges[1:-1] = (lat[:-1] + lat[1:]) / 2.0
    lat_edges[0] = lat[0] - (lat[1] - lat[0]) / 2.0
    lat_edges[-1] = lat[-1] + (lat[-1] - lat[-2]) / 2.0
    lat_edges = np.clip(lat_edges, -90.0, 90.0)

    lon_edges = np.zeros(nlon + 1, dtype=np.float64)
    lon_edges[1:-1] = (lon[:-1] + lon[1:]) / 2.0
    lon_edges[0] = lon[0] - (lon[1] - lon[0]) / 2.0

    # EXACT SEAM CLOSURE: If the grid is global, force the easternmost edge
    # to be mathematically identical to the westernmost edge + 360.0.
    # This guarantees ESMF perfectly merges the 769 nodes on the prime meridian.
    if np.isclose(lon[-1] + (lon[-1] - lon[-2]), lon[0] + 360.0, atol=1.0):
        lon_edges[-1] = lon_edges[0] + 360.0
    else:
        lon_edges[-1] = lon[-1] + (lon[-1] - lon[-2]) / 2.0

    is_lat_descending = lat[0] > lat[-1]

    if is_lat_descending:
        lat_bl_edge = lat_edges[1:]
        lat_tr_edge = lat_edges[:-1]
    else:
        lat_bl_edge = lat_edges[:-1]
        lat_tr_edge = lat_edges[1:]

    lon_bl_edge = lon_edges[:-1]
    lon_tr_edge = lon_edges[1:]

    # 3. Build the 4 Corners
    lat_corners = np.zeros((nlat, nlon, 4), dtype=np.float64)
    lon_corners = np.zeros((nlat, nlon, 4), dtype=np.float64)

    # UFS SCRIP Vertex Order Convention:
    # 0: Bottom-Left (BL)
    # 1: Bottom-Right (BR)
    # 2: Top-Right (TR)
    # 3: Top-Left (TL)

    lat_corners = np.zeros((nlat, nlon, 4), dtype=np.float64)
    lon_corners = np.zeros((nlat, nlon, 4), dtype=np.float64)

    # rewritten for UFS_utils
    lon_corners[:, :, 0], lat_corners[:, :, 0] = np.meshgrid(lon_bl_edge, lat_bl_edge) # Corner 0: BL
    lon_corners[:, :, 1], lat_corners[:, :, 1] = np.meshgrid(lon_tr_edge, lat_bl_edge) # Corner 1: BR
    lon_corners[:, :, 2], lat_corners[:, :, 2] = np.meshgrid(lon_tr_edge, lat_tr_edge) # Corner 2: TR
    lon_corners[:, :, 3], lat_corners[:, :, 3] = np.meshgrid(lon_bl_edge, lat_tr_edge) # Corner 3: TL

    # use periodicity to remove negative lon values
    lon_corners = np.mod(lon_corners+360.,360.)

    # 4. Calculate exact Element Area
    lat_edges_rad = np.deg2rad(lat_edges)
    lon_edges_rad = np.deg2rad(lon_edges)

    if is_lat_descending:
        sin_lat_bl = np.sin(lat_edges_rad[1:])
        sin_lat_tr = np.sin(lat_edges_rad[:-1])
    else:
        sin_lat_bl = np.sin(lat_edges_rad[:-1])
        sin_lat_tr = np.sin(lat_edges_rad[1:])

    lon_diff_rad = lon_edges_rad[1:] - lon_edges_rad[:-1]
    sin_diff = sin_lat_tr - sin_lat_bl

    area_2d = np.outer(sin_diff, lon_diff_rad)
    lon2d, lat2d = np.meshgrid(lon, lat)

    # 5. Construct SCRIP Dataset
    ds_scrip = xr.Dataset(
        data_vars={
            "grid_dims": (["grid_rank"], np.array([nlon, nlat], dtype=np.int32)),
            "grid_center_lat": (["grid_size"], lat2d.flatten(), {"units": "degrees"}),
            "grid_center_lon": (["grid_size"], lon2d.flatten(), {"units": "degrees"}),
            "grid_corner_lat": (["grid_size", "grid_corners"], lat_corners.reshape(grid_size, 4), {"units": "degrees"}),
            "grid_corner_lon": (["grid_size", "grid_corners"], lon_corners.reshape(grid_size, 4), {"units": "degrees"}),
            "grid_imask": (["grid_size"], np.ones(grid_size, dtype=np.int32), {"units": "none"}),
            "grid_area": (["grid_size"], area_2d.flatten(), {"units": "steradian",
                                                             "long_name": "Solid Angle Subtended on Source Grid"}),
        },
        coords={
            "grid_size": np.arange(grid_size),
            "grid_corners": np.arange(4),
            "grid_rank": np.arange(2),
        }
    )

    # Strip xarray's default _FillValue encoding to perfectly match NCL outputs
    for var in ds_scrip.variables:
        ds_scrip[var].encoding['_FillValue'] = None

    out_scrip_path = "tmp.SCRIP.nc"
    ds_scrip.to_netcdf(out_scrip_path)

    # 6. Execute ESMF conversion
    command = ["ESMF_Scrip2Unstruct", out_scrip_path, out_mesh_path, "0"]
    subprocess.run(command, check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    print(f"Successfully generated mesh file: {out_mesh_path}")


def generate_mesh_nco():
    """ An alternative way using NetCDF Operators:
        The lon/lat reproduce exactly, the area differs at roundoff.
        When used in UFS both mesh files give bitwise identical results.
        If the grid has small negative lon values, atm fields are identical but ice/ocean differ.
    """

    # ncremap -g tmp_scrip.nc -G ttl='ERA5'#latlon=721,1440#lat_typ=cap#lon_typ=grn_ctr
    # ncap2 -O -s 'where(grid_center_lon < 0.0) grid_center_lon=grid_center_lon+360.0; \
    # where(grid_corner_lon < 0.0) grid_corner_lon=grid_corner_lon+360.0' tmp_scrip.nc final_era5_scrip.nc

    # create the SCRIP file
    command = ["ncremap", "-g", "tmp_scrip.nc", "-G", "ttl='ERA5'#latlon=721,1440#lat_typ=cap#lon_typ=grn_ctr"]
    subprocess.run(command, check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)

    # make all longitudes positive
    command = ["ncap2", "-O", "-s",
               "where(grid_center_lon < 0.0) grid_center_lon=grid_center_lon+360.0; where(grid_corner_lon < 0.0) grid_corner_lon=grid_corner_lon+360.0",
               "tmp_scrip.nc", "final_era5_scrip.nc"]
    subprocess.run(command, check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)

    # Execute ESMF conversion
    command = ["ESMF_Scrip2Unstruct", "final_era5_scrip.nc", "esmf_mesh_era5.nc", "0"]
    subprocess.run(command, check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    print(f"Successfully generated mesh file: esmf_mesh_era5.nc")


def plot_test_point_and_corners(ds, testpoint=500000, title=""):
    # read data
    lon_center = ds["grid_center_lon"].isel(grid_size=testpoint).values
    lat_center = ds["grid_center_lat"].isel(grid_size=testpoint).values
    lon_corner = ds["grid_corner_lon"].isel(grid_size=testpoint).values
    lat_corner = ds["grid_corner_lat"].isel(grid_size=testpoint).values
    # plot data
    fig = plt.figure()
    ax = plt.axes()
    ax.plot(lon_center,lat_center,"ks", label="center")
    ax.plot(lon_corner[0],lat_corner[0],"ro", label="grid_corner=0")
    ax.plot(lon_corner[1],lat_corner[1],"go", label="grid_corner=1")
    ax.plot(lon_corner[2],lat_corner[2],"bo", label="grid_corner=2")
    ax.plot(lon_corner[3],lat_corner[3],"co", label="grid_corner=3")
    ax.legend()
    plt.title(title)
    return fig


def main():
    parser = argparse.ArgumentParser(description="create datm mesh file")

    # Define the arguments using the custom formatting functions
    parser.add_argument('-i', '--infile', type=str, required=True, help="ERA5 processed input file")
    parser.add_argument('-o', '--outfile', type=str, required=False, default="mesh.datm.era5.nc",
                        help="name of output mesh grid")
    parser.add_argument('-c', '--check', type=bool, required=False, default=False, help="run nco version and compare")
    parser.add_argument('-p', '--point', type=int, required=False, default=0, help="optional point for verif point")

    args = parser.parse_args()

    generate_era5_mesh(args.infile, args.outfile)

    if (args.check):
        generate_mesh_nco()

        mymesh = xr.open_dataset("tmp.SCRIP.nc")
        ncomesh = xr.open_dataset("final_era5_scrip.nc")

        plot_test_point_and_corners(ncomesh, testpoint=args.point, title="NCO")
        plot_test_point_and_corners(mymesh, testpoint=args.point, title="this script")
        plt.show()


if __name__ == "__main__":
    main()
