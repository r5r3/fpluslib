#some variables

# the default extension for a dynamic library
OS=Darwin
DYLIBEXT=dylib
ifeq ($(OS),Darwin)
	DYLIBFLAGS=-shared -install_name $(shell pwd)/lib/libfplus.$(DYLIBEXT) -ludunits2 -lcdi  -L/opt/local/lib -L/usr/local/lib /usr/local/lib/mo_cdi.o 
	LDFLAGS=-Llib -lfplus -ludunits2 -lcdi  -L/opt/local/lib -L/usr/local/lib /usr/local/lib/mo_cdi.o 
else
	DYLIBFLAGS=-shared -ludunits2 -lcdi  -L/opt/local/lib -L/usr/local/lib /usr/local/lib/mo_cdi.o 
	LDFLAGS=-Wl,-rpath=$(shell pwd)/lib -Llib -lfplus -ludunits2 -lcdi  -L/opt/local/lib -L/usr/local/lib /usr/local/lib/mo_cdi.o 
endif

# select a compiler
FC=gfortran-mp-4.8
CC=gcc
FCFLAGS= -I/opt/local/include/udunits2 -I/opt/local/include -ffree-line-length-none -fpic -I/usr/local/include -Iinclude -Jinclude
CCFLAGS= -I/opt/local/include/udunits2 -I/opt/local/include -fpic 

# create a list of all objects
SRCOBJ:=$(shell scripts/module_build_order.py --src src --obj build)

# compile everything
all: bin include lib build lib/libfplus.$(DYLIBEXT) lib/libfplus.a

# create folders
bin:
	mkdir -p bin

build:
	scripts/module_build_order.py --src src --obj build --mkobjdir

include:
	mkdir -p include

lib:
	mkdir -p lib

# build the dynamic library file
lib/libfplus.$(DYLIBEXT): lib $(SRCOBJ)
	$(FC) $(DYLIBFLAGS) -o $@ $(SRCOBJ)

# build the static library file
lib/libfplus.a: lib $(SRCOBJ)
	ar rc $@ $(SRCOBJ)

# create the documentation with doxygen
doc/html: src/* src/*/* testsrc/* doc/Doxyfile
	doxygen doc/Doxyfile

# compile tests
tests: bin build include bin/test-up-1 bin/test-up-2 bin/test-list bin/test-map bin/test-datetime bin/test-regex bin/test-cdf bin/test-template bin/test-serialization

bin/test-up-1: testsrc/test-up-1.f90
	$(FC) $(FCFLAGS) -o $@ $<

bin/test-up-2: testsrc/test-up-2.f90
	$(FC) $(FCFLAGS) -o $@ $<


# rule to link binaries
bin/%: build/%.o lib/libfplus.$(DYLIBEXT) 
	$(FC) $(FCFLAGS) -o $@ $(LDFLAGS) $<

	
# rule to compile fortran files
build/%.o: src/%.f90
	$(FC) $(FCFLAGS) -c -o $@ $<

build/%.o: build/%.f90
	$(FC) $(FCFLAGS) -c -o $@ $<

build/%.f90: testsrc/%.F
	scripts/generic.py $< -o $@

build/%.f90: src/%.F
	scripts/generic.py $< -o $@

build/%.f90: src/%.F90
	scripts/fplus < $< > $@

build/%.o: src/%.c
	$(CC) $(CCFLAGS) -c -o $@ $<

build/%.o: testsrc/%.f90
	$(FC) $(FCFLAGS) -c -o $@ $<

# clean up everything
clean:
	rm -rf bin build include lib doc/html

