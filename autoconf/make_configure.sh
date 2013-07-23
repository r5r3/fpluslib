#!/bin/bash

# create the configure script 

autoconf configure.ac > ../configure
chmod +x ../configure
rm -rf autom4te.cache