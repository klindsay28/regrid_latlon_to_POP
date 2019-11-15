
#  FC, INCLUDE, LIBS obtained with command
#  nc-config --fc --includedir --flibs
FC = ifort
FFLAGS = -c -g -O2 -convert big_endian
INCLUDE = -I/glade/u/apps/dav/opt/netcdf/4.6.1/intel/17.0.1/include
LIBS = -L/glade/u/apps/dav/opt/netcdf/4.6.1/intel/17.0.1/lib -lnetcdff -L/glade/u/apps/dav/opt/netcdf/4.6.1/intel/17.0.1/lib -Wl,-rpath,/glade/u/apps/dav/opt/netcdf/4.6.1/intel/17.0.1/lib -Wl,--disable-new-dtags -lnetcdf -lnetcdf

OBJECTS = \
   array_tools.o \
   char_case.o \
   const_mod.o \
   kinds_mod.o \
   main.o \
   msg_mod.o \
   nf_tools.o \
   nf_wrap.o \
   nf_wrap_stubs.o \
   regrid.o \
   vars_mod.o

regrid: $(OBJECTS)
	$(FC) -o regrid $(OBJECTS) $(LIBS)

clean:
	rm -f *.o *.mod

.SUFFIXES:            # Delete the default suffixes
.SUFFIXES: .F90 .o    # Define our suffix list

.F90.o:
	$(FC) $(FFLAGS) $(INCLUDE) $<

array_tools.o: array_tools.F90 kinds_mod.o msg_mod.o

char_case.o: char_case.F90

const_mod.o: const_mod.F90 kinds_mod.o

kinds_mod.o: kinds_mod.F90

main.o: main.F90 kinds_mod.o vars_mod.o regrid.o

msg_mod.o: msg_mod.F90 kinds_mod.o

nf_tools.o: nf_tools.F90 kinds_mod.o array_tools.o const_mod.o nf_wrap.o

nf_wrap.o: nf_wrap.F90 kinds_mod.o msg_mod.o nf_wrap_stubs.o

nf_wrap_stubs.o: nf_wrap_stubs.F90 kinds_mod.o

regrid.o: regrid.F90 kinds_mod.o vars_mod.o msg_mod.o char_case.o const_mod.o nf_wrap.o nf_tools.o

vars_mod.o: vars_mod.F90 kinds_mod.o
