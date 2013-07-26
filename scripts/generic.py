#!/usr/bin/env python

"""
generic.py is a very simple preprocessor for FORTRAN which is able to generate 
generic subroutines and functions from templates
"""

import argparse
import os
import re

class template(object):
    
    # regular expression to parse the template
    # group 1: name of the template
    # group 2: declaration section
    # group 3: implementation section
    re_template = re.compile("[ \t]*template[ \t]+(\S[\S ]+)[ \t]*\n([\s|\S]+)contains([\s|\S]+)[ \t]*end[ \t]+template", flags=re.IGNORECASE)   
   
    # regular expression for the declaration of the replacements
    # group 1: string to replace
    # group 2: list of replacements
    re_replace = re.compile("[ \t]*replace[ \t]*::[ \t]*(\S+)[ \t]*=>[ \t]*(.+)", flags=re.IGNORECASE)
    
    # regular expression for the declaration of the replacements with foreach attribute
    # group 1: variable for which this replacement is used
    # group 2: string to replace
    # group 3: list of replacements
    re_replace_foreach = re.compile("[ \t]*replace[ \t]*,[ \t]*foreach[ \t]*\([ \t]*(\S+)[ \t]*\)[ \t]*::[ \t]*(\S+)[ \t]*=>[ \t]*(.+)", flags=re.IGNORECASE)
    
    # a regular expression to get function or subroutine names
    re_proc = re.compile("(function|subroutine)[ \t]+(\S+)[ \t]*\(")

    def __init__(self,text):
        """
        create a new template object with a text 
        """
        self.srctext = text
        
        # parse the text, first step
        match = self.re_template.search(text)
        if match == None:
            Error("wrong template syntax in template.__init__")
        
        # find the declaration of replacements
        self.replacements = list()
        for m in self.re_replace.finditer(match.group(2)):
            replacement_string = m.group(2)
            replacement_string = replacement_string.replace("numeric", "real (kind=4);real (kind=8);integer (kind=4);integer (kind=8);complex (kind=8); complex (kind=16)")
            replacements = replacement_string.split(";")
            for i in range(len(replacements)):
                replacements[i] = replacements[i].strip()
                # nil means nothing, remove it
                if replacements[i].lower() == "nil":
                    replacements[i] = ""
            self.replacements.append((m.group(1), replacements))
        for m in self.re_replace_foreach.finditer(match.group(2)):
            replacement_string = m.group(3)
            replacement_string = replacement_string.replace("numeric", "real (kind=4);real (kind=8);integer (kind=4);integer (kind=8);complex (kind=8); complex (kind=16)")
            replacements_form_line = replacement_string.split(";")
            ref_variables = m.group(1).split(",")
            for i in range(len(ref_variables)):
                ref_variables[i] = ref_variables[i].strip()
            for i in range(len(replacements_form_line)):
                replacements_form_line[i] = replacements_form_line[i].strip()
                # nil means nothing, remove it
                if replacements_form_line[i].lower() == "nil":
                    replacements_form_line[i] = ""
            # serach the replacements belonging to the reference variable
            for ref_variable in ref_variables:
                replacements = replacements_form_line
                found_ref_variable = False
                for i in range(len(self.replacements)):
                    if ref_variable.lower() == self.replacements[i][0].lower():
                        ref_repl_temp = self.replacements[i][1]
                        ref_repl_new = []
                        repl_new = []
                        for ref_repl in ref_repl_temp:
                            for repl in replacements:
                                ref_repl_new.append(ref_repl)
                                repl_new.append(repl)
                        self.replacements[i] = (self.replacements[i][0], ref_repl_new)
                        replacements = repl_new
                        found_ref_variable = True
                        break
                if found_ref_variable == False:
                    Error("variable refered to in foreach attribute not found: %s" % ref_variable)
            self.replacements.append((m.group(2), replacements))

        # at least one replacement is needed
        if len(self.replacements) == 0:
            Error("no replacements defined in template %s" % match.group(1).strip())
        # check if all replacements have the same length
        nvariants = 0
        for r in self.replacements:
            if nvariants == 0:
                nvariants = len(r[1])
            else:
                if nvariants != len(r[1]):
                    Error("all replacements must have the same length!")
        
        # a list for the implementations
        self.impls = []
        # a list of all subroutine or function names
        self.procnames = []
        # loop over all declared replacements and substitute them in the implementation section
        for ri in range(nvariants):
            # create a copy of the implementation
            impl = match.group(3)
        
            # replace the function or procedure name
            ext = ""
            for r in self.replacements:
                temp_ext = self.getNameExtensionForType(r[1][ri])
                if len(temp_ext) > 10:
                    ext += "%s%d" % (r[0], ri+1)
                else:
                    ext += temp_ext
            i = 0
            for m in self.re_proc.finditer(impl):
                parts = devideContent((m.span(2)[0]+i*(1+len(ext)), m.span(2)[1]+i*(1+len(ext))), impl)
                parts[1] = "%s_%s" % (m.group(2), ext)
                impl = recombineSplits(parts)
                # store the new name for the interface list
                self.procnames.append(parts[1])
                i = i + 1
        
            # replace the places where one of the types is used
            for r in self.replacements:
                re_type = re.compile("[\s\(,-]+(%s)[\s\)\(,-]+" % r[0])
                while True:
                    m = re_type.search(impl)
                    if m == None:
                        break
                    parts = devideContent(m.span(1), impl)
                    parts[1] = r[1][ri]
                    # remove a trailing space if the leading letter of the type if a comma
                    if len(r[1][ri]) > 0 and r[1][ri][0] == ",":
                        parts[0] = parts[0][:len(parts[0])-1]
                    impl = recombineSplits(parts)
            
            # add this implementation to the list of all implementations
            self.impls.append(impl)
            
    def getNameExtensionForType(self, t):
        """
        returns a string usable as an extension for function and subroutine names
        """
        name = t.lower()
        name = name.replace("real", "r")
        name = name.replace("type", "t")
        name = name.replace("class", "cl")
        name = name.replace("integer", "i")
        name = name.replace("complex", "c")
        name = name.replace("logical", "l")
        name = name.replace("kind", "k")
        name = name.replace("dimension", "d")
        name = name.replace("allocatable", "a")
        name = name.replace(":", "x")
        name = name.replace(",", "")
        name = name.replace("(", "")
        name = name.replace(")", "")
        name = name.replace("=", "")
        name = name.replace("%", "")
        name = name.replace("/", "")
        name = name.replace("+", "")
        name = name.replace("-", "")
        name = name.replace("_", "")
        name = name.replace("*", "")
        name = name.replace(".", "")
        name = name.replace(" ", "")
        return name
    
    def getImplementations(self):
        """
        returns a list of all implementations
        """
        return self.impls
    
    def getProcedureNames(self):
        """
        returns a list of procedure names for the implementation of the interface
        """
        return self.procnames

def Error(msg):
    print("ERROR: %s" % msg)
    exit(-1)

def recombineSplits(split):
    """
    recombine splits create by divedeContent
    """
    result = ""
    for part in split:
        if type(part) == str:
            result += part
        elif type(part) == list:
            result += recombineSplits(part)
        else:
            Error("unsupported type in recombineSplits: %s" % type(part))
    return result

def devideContent(span, text):
    """
    @param[in]   span    a tuple with two elements: the beginning and the end of the text to separate
    @param[in]   text    the text to work on
    @returns     an tuple with 3 elements: the text before the beginning, the text to separate and the text after the end
    """
    return [text[:span[0]], text[span[0]:span[1]], text[span[1]:]]
        

def workOnFile(infile, outfile):#
    """
    perform the pre-processing on one file
    """

    # open the input file
    ifile = open(infile, "r")
    # read the content
    icontent = ifile.read()
    # close the input file
    ifile.close()
    
    # get a program or module block
    # group 1: program or module
    # group 2: name of program or module
    # group 3: declaration section
    # group 4: implementation section
    re_pm_block = re.compile("[ \t]*(program|module)[ \t]+([\S]+)[ \t]*\n([\s|\S]+)contains([\s|\S]+)[ \t]*end[ \t]+(program|module)", flags=re.IGNORECASE)
    pm_block = re_pm_block.search(icontent)
    if pm_block == None:
        Error("no program or module block found!")
    
    # devide the file content into the part before the block and after the block
    parts = devideContent(pm_block.span(), icontent)

    # a list of all template found in implementation section
    templates = {}

    # find a template in the implementation section
    pindex = 1
    while True:
        part1 = parts[pindex]
        match = re.search("^[ \t]*template[ \t]+(\S[\S ]+)", part1, flags=re.IGNORECASE+re.MULTILINE)
        if match == None:
            break
        # find the end of the block
        endmatch = re.search("[ \t]*end[ \t]+template", part1[match.span()[0]:])
        if endmatch == None:
            Error("template %s has no ending!" % match.group(1).strip())
        span = (match.span()[0], match.span()[0]+endmatch.span()[1])
        parts2 = devideContent(span, part1)
        # create a new template object
        temp = template(parts2[1])
        templates[match.group(1).strip()] = temp
        # override the current list element
        parts[pindex] = parts2[0]
        # insert the end
        parts.insert(pindex+1, parts2[2])
        # insert the implementations
        parts.insert(pindex+1, temp.getImplementations())
        pindex = pindex + 2
        
    # find interfaces in the declaration section
    re_interface = re.compile("^[ \t]*interface[ \t]+template[ \t]+([\S \t]+)[ \t]*\n([ \t]*end[ \t]+interface)", flags=re.IGNORECASE+re.MULTILINE)
    part1 = parts[1]
    while True:
        match = re_interface.search(part1)
        if match == None:
            break
        interface_parts = devideContent(match.span(), part1)
        # get the indention 
        ind = interface_parts[1].lower().index("interface")+4
        # how many templates are listed? 
        template_names = match.group(1).split(",")
        # construct the new interface, the name of the first template is used as generic name
        new_interface = interface_parts[1][:ind+6] + template_names[0].strip()+"\n"
        for template_name in template_names:
            # get the corresponding template
            try:
                temp = templates[template_name.strip()]
            except KeyError:
                Error("the implementation of the interface %s was not found!" % match.group(1))
            for pm in temp.getProcedureNames():
                new_interface += " "*ind
                if pm_block.group(1).lower() == "module":
                    new_interface += "module "
                new_interface += "procedure " + pm + "\n"
        new_interface += match.group(2)
        interface_parts[1] = new_interface
        part1 = recombineSplits(interface_parts)
        #print(part1)
    parts[1] = part1
    
    # find generic type bound procedures
    re_generic = re.compile("^[ \t]*generic[ \t]*,{0,1}[ \t]*(public){0,1}[ \t]*::[ \t]*(\S+)+[ \t]*=>[ \t]*(template[ \t]+(\S+)[ \t]*\n)", flags=re.IGNORECASE+re.MULTILINE)
    while True:
        match = re_generic.search(part1)
        if match == None:
            break
        else:
            # check if we are in a module
            if pm_block.group(1).lower() != "module":
                Error("type-bound procedures are only allowed in modules!")
        # get the indention 
        ind = match.group(0).lower().index("generic")
        # get the corresponding template
        try:
            temp = templates[match.group(4)]
        except KeyError:
            Error("the implementation of the interface %s was not found!" % match.group(1))
        # expand the generic line
        new_proc_list = ", ".join(temp.getProcedureNames())
        # split the string
        generic_parts = devideContent(match.span(3), part1)
        generic_parts[1] = new_proc_list + "\n"
        # add the declaration to the next part
        new_proc_list = ""
        for pn in temp.getProcedureNames():
            new_proc_list += " "*ind + "procedure, private :: " + pn + "\n"
        generic_parts[2] = new_proc_list + generic_parts[2]
        part1 = recombineSplits(generic_parts)
    parts[1] = part1

    # write the result to the output file
    if outfile != None:
        ofile = open(outfile, "w")
        ofile.write(recombineSplits(parts))
        ofile.close()
    else:
        print(recombineSplits(parts))
    #print(parts)

def getOutputnameFromInputname(inname):
    """
    replace the extension .F with .f90
    """
    basename, extension = os.path.splitext(inname)
    outname = "%s.%s" % (basename, "f90")
    return outname

if __name__ == "__main__":
    # create a parser for command line arguments
    parser = argparse.ArgumentParser(description=__doc__.replace("_at_", "@"), formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument('infiles', metavar="INFILE", nargs="+", help = "the name of the input file")
    parser.add_argument('-o', '--out', metavar="OUTFILE", nargs="+", help = "The name of the output file. Default = inputfile.f90")
    # parse the command line
    args = parser.parse_args()
    
    # the number of input files should be the same than the number of output files
    if args.out != None:
        if len(args.out) != len(args.infiles):
            print("ERROR: number of input files must match number of output files!")
            exit(-1)
    else:
        args.out = []
        if len(args.infiles) == 1:
            args.out.append(None)
        else:
            for fn in args.infiles:
                args.out.append(getOutputnameFromInputname(fn))

    # loop over all input files
    for i in range(len(args.infiles)):
        workOnFile(args.infiles[i], args.out[i])
        