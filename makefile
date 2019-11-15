
FC := $(shell nc-config --fc)

FFLAGS := -c -g -O2
ifeq ($(FC),gfortran)
   FFLAGS := $(FFLAGS) -fconvert=big-endian
endif
ifeq ($(FC),ifort)
   FFLAGS := $(FFLAGS) -convert big_endian
endif

INCLUDE := $(shell nc-config --fflags)

LIBS := $(shell nc-config --flibs)

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
