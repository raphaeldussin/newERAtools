PROGRAM fix_radiative_ERA5
  !!-----------------------------------------------------------------------------
  !!                 *** Program fix_radiative_ERA5  ***
  !!
  !!   Purpose : produce a netcdf files which is just the symetric of the
  !!            input file. + correct units to W/m2
  !!
  !!   Method : read the original file, flip data, fix scale/offset and write.
  !!
  !!  history:  Adapted from J.M. Molines (Septembre 2009)
  !!            R. Dussin (May 2023)
  !!----------------------------------------------------------------------------
  !!----------------------------------------------------------------------------

  USE netcdf
  INTEGER :: iargc, narg, ipos
  INTEGER :: npi, npj, npt
  REAL(KIND=4), DIMENSION(:), ALLOCATABLE :: rlon, rlat, zrlat
  REAL(KIND=4), DIMENSION(:), ALLOCATABLE :: time

  REAL(KIND=4), DIMENSION(:,:) , ALLOCATABLE :: var, ivar

  REAL, PARAMETER :: dt=3600.
  REAL :: ao, sf
  CHARACTER(LEN=512) :: cfilin, cfilout, cvar
  CHARACTER(LEN=512) :: catt


  ! netcdf stuff
  INTEGER :: istatus, ncid, id, idx1, idy1, idt1, idvx1, idvy1, idvt1, idv1
  INTEGER :: ncout, idx, idy, idt, idvx, idvy, idvt, idv
  INTEGER :: k, kt, natt

  narg=iargc()
  IF ( narg /= 3 ) THEN
     PRINT *,' USAGE: fix_radiative_ERA5 file variable fileout '
     PRINT *,'      produces fileout symetric to the input file '
     PRINT *,'      with variable in W/m2 '
     STOP
  ENDIF

  CALL getarg(1,cfilin)
  CALL getarg(2,cvar)
  CALL getarg(3,cfilout)

  PRINT *, "read ", TRIM(cvar), "from ", TRIM(cfilin)
  PRINT *, "output in: ", TRIM(cfilout)

  !--------------------- open input file
  istatus=NF90_OPEN(cfilin,NF90_NOWRITE,ncid)

  istatus=NF90_INQ_DIMID(ncid,'longitude',id)  ; istatus=NF90_INQUIRE_DIMENSION(ncid,id,len=npi)
  istatus=NF90_INQ_DIMID(ncid,'latitude',id)   ; istatus=NF90_INQUIRE_DIMENSION(ncid,id,len=npj)
  istatus=NF90_INQ_DIMID(ncid,'time',id)       ; istatus=NF90_INQUIRE_DIMENSION(ncid,id,len=npt)

  ALLOCATE ( rlon(npi) )
  ALLOCATE ( rlat(npj), zrlat(npj) )
  ALLOCATE ( var(npi,npj), ivar(npi,npj) )
  ALLOCATE ( time(npt) )

  !--------------------- Read in lon/lat and flip lat
  istatus=NF90_INQ_VARID(ncid,'longitude',id)
  istatus=NF90_GET_VAR(ncid,id,rlon)

  istatus=NF90_INQ_VARID(ncid,'latitude',id)
  istatus=NF90_GET_VAR(ncid,id,rlat)
  zrlat(:)=rlat(npj:1:-1)

  istatus=NF90_INQ_VARID(ncid,'time',id)
  istatus=NF90_GET_VAR(ncid,id,time)

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
  istatus=NF90_INQ_VARID(ncid,'longitude',idvx1)
  istatus=NF90_INQUIRE_VARIABLE(ncid,idvx1,natts=natt)
  DO k=1,natt
     istatus=NF90_INQ_ATTNAME(ncid,idvx1,k,catt)
     istatus=NF90_COPY_ATT(ncid,idvx1,catt,ncout,idvx)
  ENDDO
  ! lat :
  istatus=NF90_INQ_VARID(ncid,'latitude',idvy1)
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
  ! override units
  istatus=NF90_PUT_ATT(ncout,idv,'units', "W/m2")


  !! here's the tricky part: since numbers are represented
  !! as: X = scale_factor * x + add_offset
  !! with X = true value and x value on file
  !! To divide X by dt, we only need to change its representation as
  !! X/dt = (scale_factor/dt) * x + (add_offset/dt)

  istatus=NF90_GET_ATT(ncid,idv1,'scale_factor', sf)
  istatus=NF90_GET_ATT(ncid,idv1,'add_offset', ao)
  istatus=NF90_PUT_ATT(ncout,idv,'scale_factor', sf/dt)
  istatus=NF90_PUT_ATT(ncout,idv,'add_offset', ao/dt)

  ! finish header
  istatus=NF90_ENDDEF(ncout)

  ! add values for coords
  istatus=NF90_PUT_VAR(ncout, idvx, rlon)
  istatus=NF90_PUT_VAR(ncout, idvy, zrlat)
  istatus=NF90_PUT_VAR(ncout, idvt, time)

  ! main loop
  DO kt=1,npt

    istatus=NF90_GET_VAR(ncid, idv1, var, start=(/1,1,kt/), count=(/npi,npj,1/) )
    ! flip only, division happened in the definition of scale/offset
    ivar(:,:)=var(:,npj:1:-1)
    istatus=NF90_PUT_VAR(ncout, idv, ivar, start=(/1,1,kt/), count=(/npi,npj,1/) )

  ENDDO

  istatus=NF90_CLOSE(ncid)
  istatus=NF90_CLOSE(ncout)

END PROGRAM fix_radiative_ERA5
