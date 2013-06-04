#some variables

# the default extension for a dynamic library
OS=$(shell uname -s)
ifeq ($(OS),Darwin)
	DYLIBEXT=dylib
	EXTINC=-I/opt/local/include/udunits2
	EXTLIB=-L/opt/local/lib -ludunits2
else
	DYLIBEXT=so
	EXTLIB=-ludunits2 -lexpat
endif

# select a compiler, first choice if intel, then pgf, then gnu-mp-4.8
FC=$(shell scripts/select_compiler.py --compilers intel pgf gnu-mp-4.8 --fc)
CC=$(shell scripts/select_compiler.py --compilers intel pgf gnu-mp-4.8 --cc)
ifeq ($(FC),ifort)
	FCFLAGS=-module include -fpic -assume realloc_lhs
	CCFLAGS=-fpic $(EXTINC)
	DYLIBFLAGS=-shared $(EXTLIB)
	LDFLAGS=-Wl,-rpath=$(shell pwd)/lib -Llib -lfplus $(EXTLIB)
endif
ifeq ($(FC),pgfortran)
	FCFLAGS=-module include -fPIC
	CCFLAGS=-fPIC $(EXTINC)
	DYLIBFLAGS=-shared $(EXTLIB)
	LDFLAGS=-Wl,-rpath=$(shell pwd)/lib -Llib -lfplus $(EXTLIB)
endif
ifeq ($(FC),gfortran-mp-4.8)
	FCFLAGS=-Jinclude -fpic -ffree-line-length-none
	CCFLAGS=-fpic $(EXTINC)
	DYLIBFLAGS=-shared -install_name $(shell pwd)/lib/libfplus.$(DYLIBEXT) $(EXTLIB)
	LDFLAGS=-Llib -lfplus $(EXTLIB)
endif

# create a list of all objects
SRCOBJ=$(shell scripts/module_build_order.py --src src --obj build)

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
tests: bin build include bin/test-up-1 bin/test-up-2 bin/test-list bin/test-map bin/test-datetime bin/test-regex

bin/test-up-1: testsrc/test-up-1.f90
	$(FC) $(FCFLAGS) -o $@ $<

bin/test-up-2: testsrc/test-up-2.f90
	$(FC) $(FCFLAGS) -o $@ $<

bin/test-list: lib/libfplus.$(DYLIBEXT) build/test-list.o
	$(FC) $(FCFLAGS) -o $@ build/test-list.o $(LDFLAGS)

bin/test-map: lib/libfplus.$(DYLIBEXT) testsrc/test-map.f90
	$(FC) $(FCFLAGS) -o $@ $(LDFLAGS) testsrc/test-map.f90

bin/test-datetime: lib/libfplus.$(DYLIBEXT) testsrc/test-datetime.f90
	$(FC) $(FCFLAGS) -o $@ $(LDFLAGS) testsrc/test-datetime.f90

bin/test-regex: lib/libfplus.$(DYLIBEXT) testsrc/test-regex.f90
	$(FC) $(FCFLAGS) -o $@ $(LDFLAGS) testsrc/test-regex.f90

# rule to compile fortran files
build/%.o: src/%.f90
	$(FC) $(FCFLAGS) -c -o $@ $<

build/%.o: src/%.c
	$(CC) $(CCFLAGS) -c -o $@ $<

build/%.o: testsrc/%.f90
	$(FC) $(FCFLAGS) -c -o $@ $<

# clean up everything
clean:
	rm -rf bin build include lib doc/html

