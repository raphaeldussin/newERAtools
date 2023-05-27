PROGRAM create_bogus_firstyear
  !!-----------------------------------------------------------------------------
  !!                 *** Program create_bogus_firstyear  ***
  !!
  !!   Purpose : produce a netcdf files with 2 records copied from the first
  !!             record of the input file.
  !!
  !!   Method : read the processed file, write first record twice.
  !!
  !!  history:  Adapted from J.M. Molines (Septembre 2009)
  !!            R. Dussin (May 2023)
  !!----------------------------------------------------------------------------
  !!----------------------------------------------------------------------------

  USE netcdf
  INTEGER :: iargc, narg, ipos
  INTEGER :: npi, npj, npt
  REAL(KIND=4), DIMENSION(:), ALLOCATABLE :: rlon, rlat
  REAL(KIND=4), DIMENSION(:), ALLOCATABLE :: time, timeout

  REAL(KIND=4), DIMENSION(:,:) , ALLOCATABLE :: var, ivar
  CHARACTER(LEN=80) :: cfilin, cfilout, cvar
  CHARACTER(LEN=512) :: catt


  ! netcdf stuff
  INTEGER :: istatus, ncid, id, idx1, idy1, idt1, idvx1, idvy1, idvt1, idv1
  INTEGER :: ncout, idx, idy, idt, idvx, idvy, idvt, idv
  INTEGER :: k, kt, natt

  narg=iargc()
  IF ( narg /= 3 ) THEN
     PRINT *,' USAGE: create_bogus_firstyear file variable fileout '
     PRINT *,'      produces fileout with 2 records copied of first record of the input file '
     PRINT *,'      with same variables and attributes '
     STOP
  ENDIF

  CALL getarg(1,cfilin)
  CALL getarg(2,cvar)
  CALL getarg(3,cfilout)

  PRINT *, "read ", TRIM(cvar), "from ", TRIM(cfilin)
  PRINT *, "output in: ", TRIM(cfilout)

  !--------------------- open input file
  istatus=NF90_OPEN(cfilin,NF90_NOWRITE,ncid)

  istatus=NF90_INQ_DIMID(ncid,'lon',id)  ; istatus=NF90_INQUIRE_DIMENSION(ncid,id,len=npi)
  istatus=NF90_INQ_DIMID(ncid,'lat',id)  ; istatus=NF90_INQUIRE_DIMENSION(ncid,id,len=npj)
  istatus=NF90_INQ_DIMID(ncid,'time',id) ; istatus=NF90_INQUIRE_DIMENSION(ncid,id,len=npt)

  ALLOCATE ( rlon(npi) )
  ALLOCATE ( rlat(npj) )
  ALLOCATE ( var(npi,npj) )
  ALLOCATE ( time(1), timeout(2) )

  !--------------------- Read in lon/lat and flip lat
  istatus=NF90_INQ_VARID(ncid,'lon',id)
  istatus=NF90_GET_VAR(ncid,id,rlon)

  istatus=NF90_INQ_VARID(ncid,'lat',id)
  istatus=NF90_GET_VAR(ncid,id,rlat)

  istatus=NF90_INQ_VARID(ncid,cvar,idv1)

  !--------------------- create new file
  istatus=NF90_CREATE(cfilout, NF90_CLOBBER, ncout)
  ! define dimensions
  istatus=NF90_DEF_DIM(ncout, 'lon', npi, idx)
  istatus=NF90_DEF_DIM(ncout, 'lat', npj, idy)
  istatus=NF90_DEF_DIM(ncout, 'time', NF90_UNLIMITED, idt)
  ! define coordinates
  istatus=NF90_DEF_VAR(ncout, 'lon', NF90_FLOAT, (/idx/), idvx)
  istatus=NF90_DEF_VAR(ncout, 'lat', NF90_FLOAT, (/idy/), idvy)
  istatus=NF90_DEF_VAR(ncout, 'time', NF90_FLOAT, (/idt/), idvt)
  ! variable
  istatus=NF90_DEF_VAR(ncout,cvar,NF90_SHORT,(/idx,idy,idt/),idv)

  ! copy attributes
  ! lon :
  istatus=NF90_INQ_VARID(ncid,'lon',idvx1)
  istatus=NF90_INQUIRE_VARIABLE(ncid,idvx1,natts=natt)
  DO k=1,natt
     istatus=NF90_INQ_ATTNAME(ncid,idvx1,k,catt)
     istatus=NF90_COPY_ATT(ncid,idvx1,catt,ncout,idvx)
  ENDDO
  ! lat :
  istatus=NF90_INQ_VARID(ncid,'lat',idvy1)
  istatus=NF90_INQUIRE_VARIABLE(ncid,idvy1,natts=natt)
  DO k=1,natt
     istatus=NF90_INQ_ATTNAME(ncid,idvy1,k,catt)
     istatus=NF90_COPY_ATT(ncid,idvy1,catt,ncout,idvy)
  ENDDO
  ! time :
  istatus=NF90_INQ_VARID(ncid,'time',idvt1)
  istatus=NF90_INQUIRE_VARIABLE(ncid,idvt1,natts=natt)
  DO k=1,natt
     istatus=NF90_INQ_ATTNAME(ncid,idvt1,k,catt)
     istatus=NF90_COPY_ATT(ncid,idvt1,catt,ncout,idvt)
  ENDDO
  ! data:
  istatus=NF90_INQ_VARID(ncid, cvar,idv1)
  istatus=NF90_INQUIRE_VARIABLE(ncid,idv1,natts=natt)
  DO k=1,natt
     istatus=NF90_INQ_ATTNAME(ncid,idv1,k,catt)
     istatus=NF90_COPY_ATT(ncid,idv1,catt,ncout,idv)
  ENDDO

  ! finish header
  istatus=NF90_ENDDEF(ncout)

  ! add values for coords
  istatus=NF90_PUT_VAR(ncout, idvx, rlon)
  istatus=NF90_PUT_VAR(ncout, idvy, rlat)

  ! get first record of data
  istatus=NF90_GET_VAR(ncid, idv1, var, start=(/1,1,1/), count=(/npi,npj,1/) )

  ! get first time/date
  istatus=NF90_INQ_VARID(ncid,'time',id)
  istatus=NF90_GET_VAR(ncid, id, time, start=(/1/), count=(/1/) )

  ! time is in hours since origin, freq is one hour
  ! so we remove one hour for each new record
  DO kt=1,2
    timeout(kt) = time(1) -3 + kt
  ENDDO

  istatus=NF90_PUT_VAR(ncout, idvt, timeout,  start=(/1/), count=(/2/) )

  DO kt=1,2
    istatus=NF90_PUT_VAR(ncout, idv, var, start=(/1,1,kt/), count=(/npi,npj,1/) )
  ENDDO

  istatus=NF90_CLOSE(ncid)
  istatus=NF90_CLOSE(ncout)

END PROGRAM create_bogus_firstyear
