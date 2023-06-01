PROGRAM merge_precips_ERA5
  !!-----------------------------------------------------------------------------
  !!                 *** Program merge_precips_ERA5  ***
  !!
  !!   Purpose : produce a netcdf files which is just the symetric of the
  !!            input file. + add precips for 2 sources
  !!
  !!   Method : read the original file, add 2 precips, flip data,
  !!            adjust scale/offset and write.
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

  REAL(KIND=4), DIMENSION(:,:) , ALLOCATABLE :: var1, var2, ivar

  REAL, PARAMETER :: dt=3600.
  REAL :: ao, sf, ao1, sf1, ao2, sf2
  CHARACTER(LEN=512) :: cfilin1, cfilin2, cfilout, cvar, cvar1, cvar2
  CHARACTER(LEN=512) :: catt, clongname1, clongname2


  ! netcdf stuff
  INTEGER :: istatus
  INTEGER :: ncid1, id1, idx1, idy1, idt1, idvx1, idvy1, idvt1, idv1
  INTEGER :: ncid2, id2, idx2, idy2, idt2, idvx2, idvy2, idvt2, idv2
  INTEGER :: ncout, idx, idy, idt, idvx, idvy, idvt, idv
  INTEGER :: k, kt, natt

  narg=iargc()
  IF ( narg /= 6 ) THEN
     PRINT *,' USAGE: merge_precips_ERA5 file1 file2 variable1 variable2 fileout varout '
     PRINT *,'      produces fileout symetric to the input files '
     PRINT *,'      with sum of 2 precip variables'
     STOP
  ENDIF

  CALL getarg(1,cfilin1)
  CALL getarg(2,cfilin2)
  CALL getarg(3,cvar1)
  CALL getarg(4,cvar2)
  CALL getarg(5,cfilout)
  CALL getarg(6,cvar)

  PRINT *, "read ", TRIM(cvar1), "from ", TRIM(cfilin1)
  PRINT *, "read ", TRIM(cvar2), "from ", TRIM(cfilin2)
  PRINT *, "output in: ", TRIM(cfilout)

  !--------------------- open input files
  istatus=NF90_OPEN(cfilin1,NF90_NOWRITE,ncid1)

  istatus=NF90_INQ_DIMID(ncid1,'longitude',id1)  ; istatus=NF90_INQUIRE_DIMENSION(ncid1,id1,len=npi)
  istatus=NF90_INQ_DIMID(ncid1,'latitude',id1)   ; istatus=NF90_INQUIRE_DIMENSION(ncid1,id1,len=npj)
  istatus=NF90_INQ_DIMID(ncid1,'time',id1)       ; istatus=NF90_INQUIRE_DIMENSION(ncid1,id1,len=npt)

  ALLOCATE ( rlon(npi) )
  ALLOCATE ( rlat(npj), zrlat(npj) )
  ALLOCATE ( var1(npi,npj), var2(npi,npj), ivar(npi,npj) )
  ALLOCATE ( time(npt) )

  istatus=NF90_OPEN(cfilin2,NF90_NOWRITE,ncid2)

  !--------------------- Read in lon/lat and flip lat
  istatus=NF90_INQ_VARID(ncid1,'longitude',id1)
  istatus=NF90_GET_VAR(ncid1,id1,rlon)

  istatus=NF90_INQ_VARID(ncid1,'latitude',id1)
  istatus=NF90_GET_VAR(ncid1,id1,rlat)
  zrlat(:)=rlat(npj:1:-1)

  istatus=NF90_INQ_VARID(ncid1,'time',id1)
  istatus=NF90_GET_VAR(ncid1,id1,time)

  istatus=NF90_INQ_VARID(ncid1,cvar1,idv1)
  istatus=NF90_INQ_VARID(ncid2,cvar2,idv2)

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
  istatus=NF90_INQ_VARID(ncid1,'longitude',idvx1)
  istatus=NF90_INQUIRE_VARIABLE(ncid1,idvx1,natts=natt)
  DO k=1,natt
     istatus=NF90_INQ_ATTNAME(ncid1,idvx1,k,catt)
     istatus=NF90_COPY_ATT(ncid1,idvx1,catt,ncout,idvx)
  ENDDO
  ! lat :
  istatus=NF90_INQ_VARID(ncid1,'latitude',idvy1)
  istatus=NF90_INQUIRE_VARIABLE(ncid1,idvy1,natts=natt)
  DO k=1,natt
     istatus=NF90_INQ_ATTNAME(ncid1,idvy1,k,catt)
     istatus=NF90_COPY_ATT(ncid1,idvy1,catt,ncout,idvy)
  ENDDO
  ! time :
  istatus=NF90_INQ_VARID(ncid1,'time',idvt1)
  istatus=NF90_INQUIRE_VARIABLE(ncid1,idvt1,natts=natt)
  DO k=1,natt
     istatus=NF90_INQ_ATTNAME(ncid1,idvt1,k,catt)
     istatus=NF90_COPY_ATT(ncid1,idvt1,catt,ncout,idvt)
  ENDDO
  ! data:
  istatus=NF90_INQ_VARID(ncid1, cvar,idv1)
  istatus=NF90_INQUIRE_VARIABLE(ncid1,idv1,natts=natt)
  DO k=1,natt
     istatus=NF90_INQ_ATTNAME(ncid1,idv1,k,catt)
     istatus=NF90_COPY_ATT(ncid1,idv1,catt,ncout,idv)
  ENDDO
  ! override longname
  istatus=NF90_GET_ATT(ncid1,idv1,'long_name', clongname1)
  istatus=NF90_GET_ATT(ncid2,idv2,'long_name', clongname2)
  istatus=NF90_PUT_ATT(ncout,idv,'long_name', trim(clongname1)//" + "//trim(clongname2))


  !! here's the tricky part: since numbers are represented
  !! as: X = scale_factor * x + add_offset
  !! with X = true value and x value on file
  !! to properly capture the range of values for the sum
  !! the combined offset/scale should be the sum of both
  !! SF = sf1 + sf2 ; AO = ao1 + ao2

  istatus=NF90_GET_ATT(ncid1,idv1,'scale_factor', sf1)
  istatus=NF90_GET_ATT(ncid1,idv1,'add_offset', ao1)
  istatus=NF90_GET_ATT(ncid2,idv2,'scale_factor', sf2)
  istatus=NF90_GET_ATT(ncid2,idv2,'add_offset', ao2)
  istatus=NF90_PUT_ATT(ncout,idv,'scale_factor', sf1 + sf2)
  istatus=NF90_PUT_ATT(ncout,idv,'add_offset', ao1 + ao2)

  ! finish header
  istatus=NF90_ENDDEF(ncout)

  ! add values for coords
  istatus=NF90_PUT_VAR(ncout, idvx, rlon)
  istatus=NF90_PUT_VAR(ncout, idvy, zrlat)
  istatus=NF90_PUT_VAR(ncout, idvt, time)

  ! main loop
  DO kt=1,npt

    istatus=NF90_GET_VAR(ncid1, idv1, var1, start=(/1,1,kt/), count=(/npi,npj,1/) )
    istatus=NF90_GET_VAR(ncid2, idv2, var2, start=(/1,1,kt/), count=(/npi,npj,1/) )
    ! flip only, division happened in the definition of scale/offset
    ivar(:,:)=( sf1*var1(:,npj:1:-1) + sf2*var2(:,npj:1:-1) ) / ( sf1 + sf2 )
    istatus=NF90_PUT_VAR(ncout, idv, ivar, start=(/1,1,kt/), count=(/npi,npj,1/) )

  ENDDO

  istatus=NF90_CLOSE(ncid)
  istatus=NF90_CLOSE(ncout)

END PROGRAM merge_precips_ERA5
