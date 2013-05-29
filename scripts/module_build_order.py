#!/usr/bin/env python
'''
This scripts computes the order in which fortran modules that depend on each other should be build.
The name of the files has to be identical to the module names.

Questions or comments: schuster_at_meteo.uni-koeln.de
'''

import os
import re
import argparse

def getDependencies(filename):
    '''
    @brief    Compute the dependencies for a given file
    @return   a list of files that have to be build first
    '''
    # open the module file
    infile = open(filename, "r")
    
    # create a regular expression for lines with use statements
    prog = re.compile("[\s]*use[\s]+([\S]+)[\s]*", flags=re.IGNORECASE)
    
    # a list for the dependencies
    deps = []

    # loop over all lines to find use statements
    for line in infile:
        match = prog.match(line)
        if match != None:
            deps.append(match.group(1))
    
    # close the module file
    infile.close()
    
    return deps

if __name__ == '__main__':
    # create a parser for command line arguments
    parser = argparse.ArgumentParser(description=__doc__.replace("_at_", "@"), formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument('--src', required=True, help = "The folder which contains the source files")
    parser.add_argument('--obj', required=True, help = "The folder which will contain the compiled objects")
    parser.add_argument('--mkobjdir', required=False, action='store_const', const=True, help = "Create the folder structure for the object file and do nothing else.")
    #pares the arguments
    args = parser.parse_args()

    # get a list of all files
    fileList = []
    for root, subFolders, files in os.walk(args.src):
        for file in files:
            if not file.endswith("~") and not file.endswith(".inc"):
                fileList.append((root,file))
    
    # create the output folders if requested
    if args.mkobjdir == True:
        for file in fileList:
            dir = file[0].replace(args.src, args.obj)
            if not os.path.exists(dir):
                print("create folder %s" % dir)
                os.makedirs(dir)
        # every thing done, exit
        exit(0)
        
    # loop over all module and check if modules that are used are build before 
    # they are used.
    ready = False
    while ready == False:
        ready = True
        for i in range(len(fileList)):
            deps = getDependencies(os.path.join(fileList[i][0],fileList[i][1]))
            if len(deps) == 0:
                continue
            firstindex = len(fileList)
            # find the index first index with a dependency
            for j in range(len(fileList)):
                for d in deps:
                    if d in fileList[j][1] and j < firstindex and j > i:
                        firstindex = j
            # move the file in front of the first index with a dependency
            if firstindex < len(fileList):
                element = fileList.pop(i)
                fileList.insert(firstindex, element)
                ready = False
                break

    # create the output string which contains all objects to build in the correct order
    output = ""
    for element in fileList:
        base, ext = os.path.splitext(element[1])
        output += element[0].replace(args.src, args.obj) + "/" + base + ".o "
    print(output)
    
        
