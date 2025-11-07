#  $Id: Makefile
#  VLIDORT version 2.6
SHELL=/bin/bash


# Optimization or debug?
# make DEBUG=yes for debug
REGEXP             := (^[Yy]|^[Yy][Ee][Ss])
ifeq ($(shell [[ "$(DEBUG)" =~ $(REGEXP) ]] && echo true),true)
  FFLAG      = -c -g -cpp -O3 -zero -convert big_endian -mcmodel=medium -fp-model=precise -traceback -check all -CB
else
  FFLAG      = -c -g -cpp -O3 -zero -convert big_endian -mcmodel=medium -fp-model=precise -xHost -traceback
endif

# This is for DSCOVR
# Compiler flags
# for debug
#FFLAG      = -c -g -cpp -O3 -zero -convert big_endian -mcmodel=medium  -shared_intel -traceback -check all -CB
# for regular
#FFLAG      = -c -g -cpp -O3 -zero -convert big_endian -mcmodel=medium  -shared_intel

# Name the compilers
F90        = ifort $(FFLAG)
F90_SIMPLE = ifort -c -O3 -zero
LINK.f     = ifort -convert big_endian -mcmodel=medium -fp-model=precise

# Used libraries
# NOTE: NetCDFLIB and NetCDFINC are user specified!!!!!!!! 
#NetCDFLIB  = /util/opt/netcdf/4.2/intel/13/lib      #for crane.unl.edu
#NetCDFINC  = /util/opt/netcdf/4.2/intel/13/include  #for crane.unl.edu
NetCDFLIB  = $(ROOT_NETCDF_FORTRAN)/lib
NetCDFINC  = $(ROOT_NETCDF_FORTRAN)/include

# All linked libraries
LIBS       = -L$(NetCDFLIB)  -lnetcdff     


# Complier with libraries
F90_netCDF  = $(F90) -I$(NetCDFINC)
F90_DIAG    = $(F90) -I$(NetCDFINC)

# Name the executable
APPLICATION = 'hygroscopic.exe'

# Object modules
OBJ_MODULES = error_mod.o           \
              charpak_mod.o         \
              namelist_array_mod.o  \
              namelist_mod.o        \
              netcdf_mod.o          \
              spectra_mod.o         \
              sulfate_mod.o         \
              mix_mod.o             \
              time_mod.o            \
              diag_mod.o

OBJ_ROUTINE = main.o                \
              cleanup.o             \
              sulf_std_mie.o        \
              angle_gauss.o         \
              lgnrm1.o              \
              Mie.o                 \
              legndr.o              \
              ceflgn.o              \
              lobato.o              \
              shellcore_routine.o   \
              DMiLay.o
              
# Executables
#all: $(APPLICATION)

$(APPLICATION):   $(OBJ_MODULES) $(OBJ_ROUTINE)
	$(LINK.f) $(OBJ_MODULES) $(OBJ_ROUTINE) $(LIBS) -o $(APPLICATION)

# Common compilations
error_mod.o             : error_mod.f
charpak_mod.o           : charpak_mod.f
namelist_array_mod.o    : namelist_array_mod.f90
namelist_mod.o          : namelist_mod.f90
sulfate_mod.o           : sulfate_mod.f90
main.o                  : main.f90
cleanup.o               : cleanup.f
spectra_mod.o           : spectra_mod.f
mix_mod.o               : mix_mod.f90
sulf_std_mie.o          : sulf_std_mie.f90
angle_gauss.o           : angle_gauss.f90
lgnrm1.o                : lgnrm1.f90
Mie.o                   : Mie.f90
legndr.o                : legndr.f90
ceflgn.o                : ceflgn.f90
lobato.o                : lobato.f90
shellcore_routine.o     : shellcore_routine.f90
DMiLay.f90              : DMiLay.o
diag_mod.f90            : diag_mod.o
time_mod.f90            : time_mod.o

# Special compilations
netcdf_mod.o            : netcdf_mod.f90
	$(F90_netCDF) -free netcdf_mod.f90

# Other Makefile Commands
%.o: %.mod
clean: 
	rm -vf *.o *.mod *.exe

.SUFFIXES: .f .F .f90 .F90
.f.o:			; $(F90) $*.f
.F.o: 			; $(F90) $*.F
.f90.o: 		; $(F90) -free $*.f90
.F90.o: 		; $(F90) -free $*.F90
