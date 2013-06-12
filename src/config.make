SHELL           = /bin/sh
MAKE            = make 
RM              = rm -f
GREP            = grep
F90AIB          = f90aib
BASENAME        = basename
TR              = tr

FC              = gfortran
FCFLAGS         =  -O2 -g -fbounds-check -fconvert=big-endian -ffixed-line-length-none -ffree-line-length-none
#FCFLAGS         = -g -Wall -O2 -fconvert=big-endian -I$(GRIB_API_MOD)

FPP             = cpp
FPPFLAGS        = -P -traditional -C -I. -I$(INCL_DIR)

GRIB_API_MOD    = /usr/local/include 
GRIB_API_LIB    = /usr/local/lib/ -lgrib_api_f90 -lgrib_api
#JASPER_LIB      = /usr/lib -ljasper
JASPER_LIB      = /usr/local/lib -ljasper


HOSTNAME        = $(shell uname -n)
ifeq ($(HOSTNAME),ophelia)
 NCDF_MOD        = /home/met/install/netcdf/netcdf-4.1.1_modAtt/include #ophelia
 NCDF_LIB        = /home/met/install/netcdf/netcdf-4.1.1_modAtt/lib/libnetcdf.a #ophelia
 LDOPTIONS       = -L$(GRIB_API_LIB) -L$(JASPER_LIB) $(NCDF_LIB) #ophelia
else
 NCDF_MOD        = /home/vlcek/install/netcdf-4.1.1_modAtt/include #vlcek
 NCDF_LIB        = /home/vlcek/install/netcdf-4.1.1_modAtt/lib -lnetcdf  #vlcek
 LDOPTIONS       = -L$(GRIB_API_LIB) -L$(JASPER_LIB) -L$(NCDF_LIB) #vlcek
endif

.SUFFIXES: .exe .o .mod .F90 .f90 .ifc

# pouzije FPP preprocesor a vytvori soubory *.f90
%.f90 : %.F90
	$(RM) $@
	$(FPP) $(FPPFLAGS) $*.F90 > $@


# pouzije FPP preprocesor a vytvori soubory *.f90; ## zkompiluje *.f90 do *.o ## odstrani *.f90
%.o   : %.F90
	$(RM) $@
	$(FPP) $(FPPFLAGS) $*.F90 > $*.f90
	$(FC) -c $(FCFLAGS) -I$(GRIB_API_MOD) -I$(NCDF_MOD) $*.f90
	${RM} $*.f90

%.mod : %.F90
	$(RM) $@
	$(FPP) $(FPPFLAGS) $*.F90 > $*.f90
	$(FC) -c $(FCFLAGS) -I$(GRIB_API_MOD) $*.f90
	${RM} $*.f90

# v adresari $(INCL_DIR) vytvori interface jednotlivych programu *.F90
%.ifc : %.F90
	$(RM) $(INCL_DIR)/$@
	$(FPP) $(FPPFLAGS) $*.F90 | $(GREP) -v Inter_Faces | $(F90AIB) > $(INCL_DIR)/$@

%.exe : %.o
	$(RM) $@
	$(FC) $(FCFLAGS) -o $@  $*.o  $(OBJECTS) $(LDOPTIONS)
