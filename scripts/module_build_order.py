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

def is_module_or_program(filename):
    """
    check if a fortran file is a module, a program, or none
    """
    # open the module file
    infile = open(filename, "r")
    
    # create a regular expression for lines with use statements
    re_pro = re.compile("[\s]*program[\s]+([\S]+)[\s]*", flags=re.IGNORECASE)
    re_mod = re.compile("[\s]*module[\s]+([\S]+)[\s]*", flags=re.IGNORECASE)
    
    # loop over all lines to find use statements
    result = None
    for line in infile:
        match = re_mod.match(line)
        if match != None:
            result = "module"
            break
        match = re_pro.match(line)
        if match != None:
            result = "program"
            break

    # close the module file
    infile.close()    
    return result


if __name__ == '__main__':
    # create a parser for command line arguments
    parser = argparse.ArgumentParser(description=__doc__.replace("_at_", "@"), formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument('--src', required=True, help = "The folder which contains the source files")
    parser.add_argument('--obj', required=True, help = "The folder which will contain the compiled objects")
    parser.add_argument('--mkobjdir', required=False, action='store_const', const=True, help="Create the folder structure for the object file and do nothing else.")
    parser.add_argument('--only_modules', required=False, action='store_const', const=True, help="no program files, only modules. Usefull if more than one program is present.")
    parser.add_argument('--only_programs', required=False, action='store_const', const=True, help="no module files, only programs. Usefull if more than one program is present.")
    #pares the arguments
    args = parser.parse_args()

    # get a list of all files
    fileList = []
    for root, subFolders, files in os.walk(args.src):
        for file in files:
            if args.only_modules == True or args.only_programs == True:
                filetype = is_module_or_program(os.path.join(root,file))
                if filetype == "module" and args.only_programs == True:
                    continue
                if filetype == "program" and args.only_modules == True:
                    continue
            if not file.endswith("~") and not file.endswith(".inc") and not file.endswith(".h"):
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
    
        
