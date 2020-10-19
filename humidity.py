import numpy as np
import xarray as xr
from numba import vectorize


def q2_from_d2_and_msl(d2, msl):
    """Compute specific humidity from dewpoint and sea level pressure

    Args:
        d2 (xarray.DataArray): dewpoint at 2m
        msl (xarray.DataAray): sea level pressure

    """

    q2 = xr.apply_ufunc(q2_from_d2_and_msl_raw, d2, msl)

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