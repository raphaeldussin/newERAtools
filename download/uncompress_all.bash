#!/bin/bash

module load netcdf-c

for file in $( ls *.nc ) ; do
  fileout=$( echo $file | sed -e "s;\.nc;\.nocomp\.nc;;" )
  echo running nccopy -d 0 -c valid_time/1,latitude/721,longitude/1440 $file -o $fileout
  nccopy -d 0 -c valid_time/1,latitude/721,longitude/1440 $file $fileout
done
