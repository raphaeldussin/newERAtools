PROGRAM revert_ERA5
  !!-----------------------------------------------------------------------------
  !!         *** Program revert_ERA5 ***
  !!
  !!   Purpose : Convert a new ERA5 NetCDF file (cfgrib) back to old format.
  !!             Optimized using array syntax and elemental functions.
  !!----------------------------------------------------------------------------

  USE netcdf
  IMPLICIT NONE

  INTEGER :: iargc, narg
  INTEGER :: npi, npj, npt

  ! Input arrays
  REAL(KIND=8), DIMENSION(:), ALLOCATABLE :: rlon_in, rlat_in
  INTEGER(KIND=8), DIMENSION(:), ALLOCATABLE :: time_in
  REAL(KIND=4), DIMENSION(:,:), ALLOCATABLE :: var_slice_in

  ! Output arrays
  REAL(KIND=4), DIMENSION(:), ALLOCATABLE :: rlon_out, rlat_out
  INTEGER(KIND=4), DIMENSION(:), ALLOCATABLE :: time_out
  INTEGER(KIND=2), DIMENSION(:,:), ALLOCATABLE :: var_slice_out

  CHARACTER(LEN=512) :: cfilin, cfilout, cvar

  ! NetCDF IDs
  INTEGER :: ncid, id, idv_in
  INTEGER :: ncout, idx, idy, idt, idvx, idvy, idvt, idv_out
  INTEGER :: kt

  ! Packing variables
  REAL(KIND=4) :: f_min, f_max, scale_factor, add_offset
  INTEGER(KIND=2) :: fill_val_short = -32767_2

  narg = iargc()
  IF ( narg /= 3 ) THEN
     PRINT *,' USAGE: revert_ERA5 file_in variable file_out '
     STOP
  ENDIF

  CALL getarg(1,cfilin)
  CALL getarg(2,cvar)
  CALL getarg(3,cfilout)

  PRINT *, "Converting ", TRIM(cvar), " from ", TRIM(cfilin)
  PRINT *, "Outputting legacy format to: ", TRIM(cfilout)

  !--------------------- 1. Open Input & Get Dimensions ---------------------
  CALL check_nc( NF90_OPEN(TRIM(cfilin), NF90_NOWRITE, ncid) )

  CALL check_nc( NF90_INQ_DIMID(ncid, 'longitude', id) )
  CALL check_nc( NF90_INQUIRE_DIMENSION(ncid, id, len=npi) )

  CALL check_nc( NF90_INQ_DIMID(ncid, 'latitude', id) )
  CALL check_nc( NF90_INQUIRE_DIMENSION(ncid, id, len=npj) )

  CALL check_nc( NF90_INQ_DIMID(ncid, 'valid_time', id) )
  CALL check_nc( NF90_INQUIRE_DIMENSION(ncid, id, len=npt) )

  ALLOCATE(rlon_in(npi), rlon_out(npi))
  ALLOCATE(rlat_in(npj), rlat_out(npj))
  ALLOCATE(time_in(npt), time_out(npt))
  ALLOCATE(var_slice_in(npi, npj), var_slice_out(npi, npj))

  !--------------------- 2. Read and Cast Coordinates ---------------------
  CALL check_nc( NF90_INQ_VARID(ncid, 'longitude', id) )
  CALL check_nc( NF90_GET_VAR(ncid, id, rlon_in) )
  rlon_out = REAL(rlon_in, KIND=4)

  CALL check_nc( NF90_INQ_VARID(ncid, 'latitude', id) )
  CALL check_nc( NF90_GET_VAR(ncid, id, rlat_in) )
  rlat_out = REAL(rlat_in, KIND=4)

  CALL check_nc( NF90_INQ_VARID(ncid, 'valid_time', id) )
  CALL check_nc( NF90_GET_VAR(ncid, id, time_in) )

  DO kt = 1, npt
     time_out(kt) = INT( (time_in(kt) / 3600_8) + 613608_8, KIND=4 )
  END DO

  !--------------------- 3. Pass 1: Find Min/Max (Vectorized) -------------
  PRINT *, "Pass 1: Calculating scale_factor and add_offset..."
  CALL check_nc( NF90_INQ_VARID(ncid, cvar, idv_in) )

  f_min = HUGE(1.0)
  f_max = -HUGE(1.0)

  DO kt = 1, npt
     CALL check_nc( NF90_GET_VAR(ncid, idv_in, var_slice_in, start=(/1,1,kt/), count=(/npi,npj,1/) ) )
     ! Vectorized search across the entire 2D array slice
     f_min = MIN(f_min, MINVAL(var_slice_in))
     f_max = MAX(f_max, MAXVAL(var_slice_in))
  END DO

  scale_factor = (f_max - f_min) / 65534.0
  add_offset = (f_max + f_min) / 2.0

  !--------------------- 4. Create Legacy Output File ---------------------
  PRINT *, "Creating legacy file..."
  CALL check_nc( NF90_CREATE(cfilout, NF90_CLOBBER, ncout) )

  CALL check_nc( NF90_DEF_DIM(ncout, 'longitude', npi, idx) )
  CALL check_nc( NF90_DEF_DIM(ncout, 'latitude', npj, idy) )
  CALL check_nc( NF90_DEF_DIM(ncout, 'time', npt, idt) )

  CALL check_nc( NF90_DEF_VAR(ncout, 'longitude', NF90_FLOAT, (/idx/), idvx) )
  CALL check_nc( NF90_DEF_VAR(ncout, 'latitude', NF90_FLOAT, (/idy/), idvy) )
  CALL check_nc( NF90_DEF_VAR(ncout, 'time', NF90_INT, (/idt/), idvt) )
  CALL check_nc( NF90_DEF_VAR(ncout, cvar, NF90_SHORT, (/idx,idy,idt/), idv_out) )

  CALL check_nc( NF90_PUT_ATT(ncout, idvx, 'units', 'degrees_east') )
  CALL check_nc( NF90_PUT_ATT(ncout, idvx, 'long_name', 'longitude') )

  CALL check_nc( NF90_PUT_ATT(ncout, idvy, 'units', 'degrees_north') )
  CALL check_nc( NF90_PUT_ATT(ncout, idvy, 'long_name', 'latitude') )

  CALL check_nc( NF90_PUT_ATT(ncout, idvt, 'units', 'hours since 1900-01-01 00:00:00.0') )
  CALL check_nc( NF90_PUT_ATT(ncout, idvt, 'long_name', 'time') )
  CALL check_nc( NF90_PUT_ATT(ncout, idvt, 'calendar', 'gregorian') )

  CALL check_nc( NF90_PUT_ATT(ncout, idv_out, 'scale_factor', scale_factor) )
  CALL check_nc( NF90_PUT_ATT(ncout, idv_out, 'add_offset', add_offset) )
  CALL check_nc( NF90_PUT_ATT(ncout, idv_out, '_FillValue', fill_val_short) )
  CALL check_nc( NF90_PUT_ATT(ncout, idv_out, 'missing_value', fill_val_short) )

  CALL check_nc( NF90_ENDDEF(ncout) )

  !--------------------- 5. Write Coordinates -----------------------------
  CALL check_nc( NF90_PUT_VAR(ncout, idvx, rlon_out) )
  CALL check_nc( NF90_PUT_VAR(ncout, idvy, rlat_out) )
  CALL check_nc( NF90_PUT_VAR(ncout, idvt, time_out) )

  !--------------------- 6. Pass 2: Pack and Write Data (Vectorized) ------
  PRINT *, "Pass 2: Packing and writing data to 16-bit short..."
  DO kt = 1, npt
     CALL check_nc( NF90_GET_VAR(ncid, idv_in, var_slice_in, start=(/1,1,kt/), count=(/npi,npj,1/) ) )

     ! Fully vectorized whole-array calculation
     var_slice_out = NINT((var_slice_in - add_offset) / scale_factor, KIND=2)

     CALL check_nc( NF90_PUT_VAR(ncout, idv_out, var_slice_out, start=(/1,1,kt/), count=(/npi,npj,1/) ) )
  END DO

  CALL check_nc( NF90_CLOSE(ncid) )
  CALL check_nc( NF90_CLOSE(ncout) )
  PRINT *, "Conversion complete."

CONTAINS

  SUBROUTINE check_nc(status)
    INTEGER, INTENT(IN) :: status
    IF (status /= NF90_NOERR) THEN
      PRINT *, "NetCDF Error: ", TRIM(NF90_STRERROR(status))
      STOP "Execution halted."
    END IF
  END SUBROUTINE check_nc

END PROGRAM revert_ERA5
