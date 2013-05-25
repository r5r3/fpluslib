#some variables
# use intel fortran compiler if available
HAVEIFORT=$(shell which ifort)
ifneq ($(findstring ifort,$(HAVEIFORT)),)
	FC=ifort
	CC=icc
	FCFLAGS=-module include -fpic
	DYLIBFLAGS=-shared -fpic
	LDFLAGS=-Wl,-rpath=lib -Llib -lfstd
else
	FC=gfortran-mp-4.8
	CC=gcc
	FCFLAGS=-Jinclude -fpic
	DYLIBFLAGS=-shared -fpic
	LDFLAGS=-Llib -lfstd
endif

# create a list of all objects
SRCOBJ=$(shell scripts/module_build_order.py --src src --obj build)

# the default extension for a dynamic library
OS=$(shell uname -s)
ifeq ($(OS),Darwin)
	DYLIBEXT=dylib
else
	DYLIBEXT=so
endif

# compile everything
all: bin include lib build lib/libfstd.$(DYLIBEXT)

# create folders
bin:
	mkdir -p bin

build:
	mkdir -p build

include:
	mkdir -p include

lib:
	mkdir -p lib

# build the dynamic library file
lib/libfstd.$(DYLIBEXT): lib $(SRCOBJ)
	$(FC) $(DYLIBFLAGS) -o $@ $(SRCOBJ)

# create the documentation with doxygen
doc/html: src/* testsrc/* doc/Doxyfile
	doxygen doc/Doxyfile

# compile tests
tests: bin build include bin/test-up-1 bin/test-up-2 bin/test-list bin/test-map

bin/test-up-1: testsrc/test-up-1.f90
	$(FC) $(FCFLAGS) -o $@ $<

bin/test-up-2: testsrc/test-up-2.f90
	$(FC) $(FCFLAGS) -o $@ $<

bin/test-list: lib/libfstd.$(DYLIBEXT) build/test-list.o
	$(FC) $(FCFLAGS) -o $@ build/test-list.o $(LDFLAGS)

bin/test-map: lib/libfstd.$(DYLIBEXT) testsrc/test-map.f90
	$(FC) $(FCFLAGS) -o $@ $(LDFLAGS) testsrc/test-map.f90

# rule to compile fortran files
build/%.o: src/%.f90
	$(FC) $(FCFLAGS) -c -o $@ $<

build/%.o: src/%.c
	$(CC) -c -o $@ $<

build/%.o: testsrc/%.f90
	$(FC) $(FCFLAGS) -c -o $@ $<

# clean up everything
clean:
	rm -rf bin build include lib doc/html

