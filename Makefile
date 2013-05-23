#some variables
# use intel fortran compiler if available
HAVEIFORT=$(shell which ifort)
ifneq ($(findstring ifort,$(HAVEIFORT)),)
	FC=ifort
	FCFLAGS=-module include -g
else
	FC=gfortran-mp-4.8
	FCFLAGS=-Jinclude -g
endif

# compile everything
all: bin include lib build

# create folders
bin:
	mkdir -p bin

build:
	mkdir -p build

include:
	mkdir -p include

lib:
	mkdir -p lib
	
# compile tests
tests: bin build include bin/test-up-1 bin/test-up-2 bin/test-list bin/test-map

bin/test-up-1: testsrc/test-up-1.f90
	$(FC) $(FCFLAGS) -o $@ $<

bin/test-up-2: testsrc/test-up-2.f90
	$(FC) $(FCFLAGS) -o $@ $<

bin/test-list: build/mod_fillvalue.o build/mod_list.o build/test-list.o
	$(FC) $(FCFLAGS) -o $@ build/mod_fillvalue.o build/mod_list.o build/test-list.o

bin/test-map: build/mod_fstd.o build/mod_map.o testsrc/test-map.f90
	$(FC) $(FCFLAGS) -o $@ build/mod_fstd.o build/mod_map.o testsrc/test-map.f90

# rule to compile fortran files
build/%.o: src/%.f90
	$(FC) $(FCFLAGS) -c -o $@ $<

build/%.o: testsrc/%.f90
	$(FC) $(FCFLAGS) -c -o $@ $<

# clean up everything
clean:
	rm -rf bin build include lib

