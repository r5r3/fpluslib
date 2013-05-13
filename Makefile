#some variables
FC=gfortran-mp-4.8
FCFLAGS=-Jbuild

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
tests: bin build bin/test-up-1 bin/test-list

bin/test-up-1: testsrc/test-up-1.f90
	$(FC) $(FCFLAGS) -o $@ $<

bin/test-list: build/mod_list.o build/test-list.o
	$(FC) $(FCFLAGS) -o $@ build/mod_list.o build/test-list.o

# rule to compile fortran files
build/%.o: src/%.f90
	$(FC) $(FCFLAGS) -c -o $@ $<

build/%.o: testsrc/%.f90
	$(FC) $(FCFLAGS) -c -o $@ $<

# clean up everything
clean:
	rm -rf bin build include lib

