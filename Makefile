#----------------------------------------------------

C=$(shell nf-config --fc)
NCFLAGS=$(shell nc-config --cflags )
NFFLAGS=$(shell nf-config --fflags )
NCLIBS=$(shell nc-config --libs )
NFLIBS=$(shell nf-config --flibs )

FFLAGS=-O $(NCFLAGS) $(NFFLAGS)
LIBS=$(NCLIBS) $(NFLIBS)

#-----------------------------------------------------

EXEC = reformat_ERA5 fix_radiative_ERA5 merge_precips_ERA5 compute_q2_ERA5

#-----------------------------------------------------

all: $(EXEC)

reformat_ERA5: reformat_ERA5.f90
	$(FC) $(FFLAGS) reformat_ERA5.f90 -o reformat_ERA5 $(LIBS)

fix_radiative_ERA5: fix_radiative_ERA5.f90
	$(FC) $(FFLAGS) fix_radiative_ERA5.f90 -o fix_radiative_ERA5 $(LIBS)

merge_precips_ERA5: merge_precips_ERA5.f90
	$(FC) $(FFLAGS) merge_precips_ERA5.f90 -o merge_precips_ERA5 $(LIBS)

compute_q2_ERA5: compute_q2_ERA5.f90
	$(FC) $(FFLAGS) compute_q2_ERA5.f90 -o compute_q2_ERA5 $(LIBS)

#---------------------------------------------------------

clean:
	\rm -f $(EXEC) *~

install: all
	mv $(EXEC) ../bin

#--------------------------------------------------------------
