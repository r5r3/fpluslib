#some variables

# the default extension for a dynamic library
OS=$(shell uname -s)
ifeq ($(OS),Darwin)
	DYLIBEXT=dylib
else
	DYLIBEXT=so
endif

# select a compiler, first choice if intel, then pgf, then gnu-mp-4.8
FC=$(shell scripts/select_compiler.py --compilers intel pgf gnu-mp-4.8 --fc)
CC=$(shell scripts/select_compiler.py --compilers intel pgf gnu-mp-4.8 --cc)
ifeq ($(FC),ifort)
	FCFLAGS=-module include -fpic
	CCFLAGS=-fpic
	DYLIBFLAGS=-shared
	LDFLAGS=-Wl,-rpath=$(shell pwd)/lib -Llib -lfstd
endif
ifeq ($(FC),pgfortran)
	FCFLAGS=-module include -fPIC
	CCFLAGS=-fPIC
	DYLIBFLAGS=-shared
	LDFLAGS=-Wl,-rpath=$(shell pwd)/lib -Llib -lfstd
endif
ifeq ($(FC),gfortran-mp-4.8)
	FCFLAGS=-Jinclude -fpic
	CCFLAGS=-fpic
	DYLIBFLAGS=-shared -install_name $(shell pwd)/lib/libfstd.$(DYLIBEXT)
	LDFLAGS=-Llib -lfstd
endif

# create a list of all objects
SRCOBJ=$(shell scripts/module_build_order.py --src src --obj build)

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
tests: bin build include bin/test-up-1 bin/test-up-2 bin/test-list bin/test-map bin/test-string

bin/test-up-1: testsrc/test-up-1.f90
	$(FC) $(FCFLAGS) -o $@ $<

bin/test-up-2: testsrc/test-up-2.f90
	$(FC) $(FCFLAGS) -o $@ $<

bin/test-list: lib/libfstd.$(DYLIBEXT) build/test-list.o
	$(FC) $(FCFLAGS) -o $@ build/test-list.o $(LDFLAGS)

bin/test-map: lib/libfstd.$(DYLIBEXT) testsrc/test-map.f90
	$(FC) $(FCFLAGS) -o $@ $(LDFLAGS) testsrc/test-map.f90

bin/test-string: lib/libfstd.$(DYLIBEXT) testsrc/test-string.f90
	$(FC) $(FCFLAGS) -o $@ $(LDFLAGS) testsrc/test-string.f90

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

