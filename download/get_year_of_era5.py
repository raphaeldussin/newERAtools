#!/usr/bin/env python

import cdsapi
import sys

year=sys.argv[-1]
cyear=str(year)

varlist=[
            '10m_u_component_of_wind','10m_v_component_of_wind','2m_dewpoint_temperature',
            '2m_temperature','mean_sea_level_pressure',
            'surface_pressure','surface_solar_radiation_downwards','surface_thermal_radiation_downwards',
            'convective_rain_rate', 'large_scale_rain_rate',
            'convective_snowfall_rate_water_equivalent',
            'large_scale_snowfall_rate_water_equivalent',
            'runoff'
        ]

c = cdsapi.Client()

for var in varlist:

    c.retrieve(
        'reanalysis-era5-single-levels',
        {
            'product_type':'reanalysis',
            'format':'netcdf',
            'variable':[ var ],
            'year':cyear,
            'month':[
                '01','02','03',
                '04','05','06',
                '07','08','09',
                '10','11','12'
            ],
            'day':[
                '01','02','03',
                '04','05','06',
                '07','08','09',
                '10','11','12',
                '13','14','15',
                '16','17','18',
                '19','20','21',
                '22','23','24',
                '25','26','27',
                '28','29','30',
                '31'
            ],
            'time':[
                '00:00','01:00','02:00',
                '03:00','04:00','05:00',
                '06:00','07:00','08:00',
                '09:00','10:00','11:00',
                '12:00','13:00','14:00',
                '15:00','16:00','17:00',
                '18:00','19:00','20:00',
                '21:00','22:00','23:00'
            ]
        },
        'ERA5_' + var + '_' + cyear + '.nc')
