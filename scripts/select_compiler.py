#!/usr/bin/env python
'''
This script will automatically select an available compiler. 
The C-Compiler will always correspond to selected fortran compiler.
'''

import argparse
import subprocess

def getstatusoutput(cmd): 
    """Return (status, output) of executing cmd in a shell."""
    """This new implementation should work on all platforms."""
    pipe = subprocess.Popen(cmd, shell=True, universal_newlines=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    output = str.join("", pipe.stdout.readlines()) 
    sts = pipe.wait()
    if sts is None:
        sts = 0
    return sts, output


def find_compiler(suite):
    '''
    @brief Find the compiler belonging to one of the predefined suits intel, pgf or gnu-mp-4.8
    '''
    if suite == "intel":
        fc = "ifort"
        cc = "icc"
    elif suite == "pgf":
        fc = "pgfortran"
        cc = "pgcc"
    elif suite == "gnu-mp-4.8":
        fc = "gfortran-mp-4.8"
        cc = "gcc"
    else:
        print("ERROR: unknown compiler suite: %s", suite)
        exit(-1)
    
    # check the presents of the compilers
    statusfc, whichfc = getstatusoutput("which %s" % fc)
    statuscc, whichcc = getstatusoutput("which %s" % cc)
    if statusfc == 0 and statuscc == 0:
        return (fc, cc)
    else:
        return (None, None)


if __name__ == "__main__":
    # create a parser for command line arguments
    parser = argparse.ArgumentParser(description=__doc__.replace("_at_", "@"), formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument('--compilers', required=True, nargs="+", choices=["intel", "gnu-mp-4.8", "pgf"], help="Which compiler should be searched?")
    parser.add_argument('--fc', required=False, action='store_const', const=True, help = "Find the FORTRAN compiler.")
    parser.add_argument('--cc', required=False, action='store_const', const=True, help = "Find the C compiler.")
    #pares the arguments
    args = parser.parse_args()

    #try each compiler listed in --compilers and return the first one found.
    for comp in args.compilers:
        fc, cc = find_compiler(comp)
	if fc != None and args.fc == True:
            print(fc)
            exit(0)
	if cc != None and args.cc == True:
            print(cc)
            exit(0)

    # no compiler was found
    print("ERROR: no compiler found!")
    exit(-1)
