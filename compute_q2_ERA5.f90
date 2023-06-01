PROGRAM compute_q2_ERA5
  !!-----------------------------------------------------------------------------
  !!                 *** Program compute_q2_ERA5  ***
  !!
  !!   Purpose : Compute the specific humidity
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

  REAL(KIND=4), DIMENSION(:,:) , ALLOCATABLE :: d2, pres, ivar
  REAL(KIND=4), DIMENSION(:,:) , ALLOCATABLE :: var1, var2
  REAL(KIND=4), DIMENSION(:,:) , ALLOCATABLE :: d2C, e2, q2

  REAL, PARAMETER :: reps=0.622

  REAL :: ao, sf, ao1, sf1, ao2, sf2
  CHARACTER(LEN=512) :: cfilin1, cfilin2, cfilout, cvar, cvar1, cvar2
  CHARACTER(LEN=512) :: catt, clongname1, clongname2


  ! netcdf stuff
  INTEGER :: istatus
  INTEGER :: ncid1, id1, idx1, idy1, idt1, idvx1, idvy1, idvt1, idv1
  INTEGER :: ncid2, id2, idx2, idy2, idt2, idvx2, idvy2, idvt2, idv2
  INTEGER :: ncout, idx, idy, idt, idvx, idvy, idvt, idv
  INTEGER :: jj, ji, k, kt, natt

  narg=iargc()
  IF ( narg /= 5 ) THEN
     PRINT *,' USAGE: compute_q2_ERA5 file_d2 file_pressure var_d2 var_pres fileout '
     PRINT *,'      produces q2 from d2 and msl '
     PRINT *,'      '
     STOP
  ENDIF

  CALL getarg(1,cfilin1)
  CALL getarg(2,cfilin2)
  CALL getarg(3,cvar1)
  CALL getarg(4,cvar2)
  CALL getarg(5,cfilout)

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
  ALLOCATE ( var1(npi,npj), var2(npi,npj) )
  ALLOCATE ( d2(npi,npj), pres(npi,npj), ivar(npi,npj) )
  ALLOCATE ( d2C(npi,npj), e2(npi,npj), q2(npi,npj) )
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
  istatus=NF90_DEF_VAR(ncout,"q2m",NF90_SHORT,(/idx,idy,idt/),idv)

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
  istatus=NF90_COPY_ATT(ncid1,idv1,'_FillValue',ncout,idv)
  istatus=NF90_COPY_ATT(ncid1,idv1,'missing_value',ncout,idv)

  istatus=NF90_PUT_ATT(ncout,idv,'long_name', "Specific humidity")
  istatus=NF90_PUT_ATT(ncout,idv,'units', "kg/kg")
  istatus=NF90_PUT_ATT(ncout,idv,'scale_factor', 0.016/32767)
  istatus=NF90_PUT_ATT(ncout,idv,'add_offset', 0.012)

  ! finish header
  istatus=NF90_ENDDEF(ncout)

  ! add values for coords
  istatus=NF90_PUT_VAR(ncout, idvx, rlon)
  istatus=NF90_PUT_VAR(ncout, idvy, zrlat)
  istatus=NF90_PUT_VAR(ncout, idvt, time)

  istatus=NF90_GET_ATT(ncid1,idv1,'scale_factor', sf1)
  istatus=NF90_GET_ATT(ncid1,idv1,'add_offset', ao1)
  istatus=NF90_GET_ATT(ncid2,idv2,'scale_factor', sf2)
  istatus=NF90_GET_ATT(ncid2,idv2,'add_offset', ao2)

  ! main loop
  DO kt=1,npt

    istatus=NF90_GET_VAR(ncid1, idv1, var1, start=(/1,1,kt/), count=(/npi,npj,1/) )
    istatus=NF90_GET_VAR(ncid2, idv2, var2, start=(/1,1,kt/), count=(/npi,npj,1/) )

    d2 = ao1 + var1*sf1
    pres = ao2 + var2*sf2

    ! saturation water pressure from Bolton (Petty 7.3)
    d2C(:,:) = d2(:,:) - 273.15
    e2(:,:) = 611.2 * exp((17.67 * d2C(:,:))/(d2C(:,:) + 243.5))

    ! specific humidity from pressure and wator pressure (Petty 3.41)
    q2(:,:) = (reps * e2(:,:)) / (pres(:,:) - (1.0 - reps) * e2(:,:))

    ! flip only and compute reduced value from offset/scale
    ivar(:,:)= nint( (q2(:,npj:1:-1) - 0.012) / (0.016/32767) )

    ! write
    istatus=NF90_PUT_VAR(ncout, idv, ivar, start=(/1,1,kt/), count=(/npi,npj,1/) )

  ENDDO

  istatus=NF90_CLOSE(ncid1)
  istatus=NF90_CLOSE(ncid2)
  istatus=NF90_CLOSE(ncout)

END PROGRAM compute_q2_ERA5
