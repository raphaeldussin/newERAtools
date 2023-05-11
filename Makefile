#----------------------------------------------------

C=$(shell nf-config --fc)
NCFLAGS=$(shell nc-config --cflags )
NFFLAGS=$(shell nf-config --fflags )
NCLIBS=$(shell nc-config --libs )
NFLIBS=$(shell nf-config --flibs )

FFLAGS=-O $(NCFLAGS) $(NFFLAGS)
LIBS=$(NCLIBS) $(NFLIBS)

#-----------------------------------------------------

EXEC = reformat_ERA5

#-----------------------------------------------------

all: $(EXEC)

reformat_ERA5: reformat_ERA5.f90
	$(FC) $(FFLAGS) reformat_ERA5.f90 -o reformat_ERA5 $(LIBS)

#---------------------------------------------------------

clean:
	\rm -f $(EXEC) *~

install: all
	mv $(EXEC) ../bin

#--------------------------------------------------------------
