#!/bin/bash

fyear=1979
lyear=2022

# concurrent download (for large bandwidth)
#seq -f %02g $fyear $lyear | parallel --jobs 2 --lb --tagstring log.{} ./get_year_of_era5.py {}

# sequential
for year in $( seq -f %02g $fyear $lyear ) ; do

./get_year_of_era5.py $year

done
