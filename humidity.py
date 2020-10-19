#!/usr/bin/env python

import numpy as np
import xarray as xr
from numba import vectorize
import argparse


def q2_from_d2_and_msl(d2, msl):
    """Compute specific humidity from dewpoint and sea level pressure

    Args:
        d2 (xarray.DataArray): dewpoint at 2m
        msl (xarray.DataAray): sea level pressure

    """

    q2 = xr.apply_ufunc(q2_from_d2_and_msl_raw, d2, msl, dask='parallelized',
                        output_dtypes=[d2.dtype])

    return q2


@vectorize
def q2_from_d2_and_msl_raw(d2, msl):
    """
    compute specific humidity from dewpoint and sea level pressure (ufunc)

    Parameters
    ----------

    d2 : float/np.ndarray
        dew point at 2m
    msl : float/np.ndarray
        mean sea level pressure

    Returns
    -------

    q2 : float/np.ndarray
        specific humidity at 2m
    """

    reps = 0.62197

    psat = 100 * (
        10
        ** (
            10.79574 * (1 - 273.16 / d2)
            - 5.028 * np.log10(d2 / 273.16)
            + 1.50475 * 10 ** (-4) * (1 - 10 ** (-8.2969 * (d2 / 273.16 - 1)))
            + 0.42873 * 10 ** (-3) * (10 ** (4.76955 * (1 - 273.16 / d2)) - 1)
            + 0.78614
        )
    )

    q2 = reps * psat / (msl - (1.0 - reps) * psat)

    return q2


if __name__ == "__main__":

    parser = argparse.ArgumentParser(
        description="compute specific humidity file from d2 and msl"
    )
    parser.add_argument(
        "--d2",
        type=str,
        required=True,
        help="name of dewpoint file",
    )
    parser.add_argument(
        "--msl",
        type=str,
        required=True,
        help="name of pressure file",
    )
    parser.add_argument(
        "--d2var",
        type=str,
        required=False,
        default='d2m',
        help="name of dewpoint variable",
    )
    parser.add_argument(
        "--mslvar",
        type=str,
        required=False,
        default='msl',
        help="name of pressure variable",
    )
    parser.add_argument(
        "-o",
        "--fileout",
        type=str,
        required=True,
        help="name of produced specific humidity file",
    )
    parser.add_argument(
        "-c",
        "--chunks",
        type=int,
        required=False,
        default=10,
        help="size of chunk in time",
    )

    args = vars(parser.parse_args())

    d2 = xr.open_dataset(args["d2"], chunks={'time': args["chunks"]})[args["d2var"]]
    msl = xr.open_dataset(args["msl"], chunks={'time': args["chunks"]})[args["mslvar"]]

    q2 = q2_from_d2_and_msl(d2, msl)

    q2.to_dataset(name='q2').to_netcdf(args["fileout"])
