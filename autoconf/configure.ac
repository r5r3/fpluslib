AC_INIT(fpluslib, 0.1)

echo "---------------------------------------------------------------"
echo "Testing for OS specific settings"
echo "---------------------------------------------------------------"
AC_SUBST([OS], [$(uname -s)])
echo "Found operating system $OS ..."
AS_VAR_IF([OS], [Darwin], 
	[
		AS_VAR_APPEND([CFLAGS], [" -I/opt/local/include/udunits2"])
		AS_VAR_APPEND([CFLAGS], [" -I/opt/local/include"])
		AS_VAR_APPEND([FCFLAGS], [" -I/opt/local/include/udunits2"])
		AS_VAR_APPEND([FCFLAGS], [" -I/opt/local/include"])
		AS_VAR_APPEND([LDFLAGS], [" -L/opt/local/lib"])
		AC_SUBST([DYLIBEXT], ["dylib"])
	], 
	[
		AC_SUBST([DYLIBEXT], ["so"])
	])


echo "---------------------------------------------------------------"
echo "Testing for a C compiler"
echo "---------------------------------------------------------------"
AC_PROG_CC

echo "---------------------------------------------------------------"
echo "Testing for a FORTRAN compiler"
echo "---------------------------------------------------------------"
# find the compiler
AC_PROG_FC
# find the flag for free form files
AC_FC_SRCEXT([f90])
# find the flag for unlimited line length
AC_FC_LINE_LENGTH(
	[unlimited],
	[],
	[AC_MSG_FAILURE([we need a fortran compiler that supports long lines!])]
	)
# find the flag to specify the module output path
AC_FC_MODULE_OUTPUT_FLAG
# find the flag to specify the module input path
AC_FC_MODULE_FLAG

# add flag for position independent code and reallocation
AS_VAR_IF([FC], [ifort], 
	[
		AS_VAR_APPEND([FCFLAGS], [" -assume realloc_lhs"])
	], [])
AS_VAR_IF([FC], [pgfortran], 
	[
		AS_VAR_APPEND([FCFLAGS], [" -fPIC"])
		AS_VAR_APPEND([CFLAGS], [" -fPIC"])
	], 
	[
		AS_VAR_APPEND([FCFLAGS], [" -fpic"])
		AS_VAR_APPEND([CFLAGS], [" -fpic"])
	])

# testing for additional libraries
echo "---------------------------------------------------------------"
echo "Testing for external libraries"
echo "---------------------------------------------------------------"
AC_LANG_PUSH([Fortran])

# cdi ----------------------------------------------------------------
AC_ARG_WITH([cdi],
            [AS_HELP_STRING([--with-cdi],
              [support for climat data interface @<:@default=no@:>@])],
            [
            	AS_VAR_APPEND([LDFLAGS], [" -L$with_cdi/lib"])
            	AS_VAR_APPEND([FCFLAGS], [" -I$with_cdi/include"])            	
            ],
            [with_cdi=no])

LIBCDI=
        AS_IF([test "x$with_cdi" != xno],
            [
            	echo "checking cdi support ... "
	            AC_COMPILE_IFELSE(AC_LANG_PROGRAM([], 
	            	[
	            		use mo_cdi
	            		vlistID = vlistCreate()
	            	]),
	              	[
	              		echo " ... use cdi in $with_cdi"
	              		AC_SUBST([LIBCDI], [" -lcdi $with_cdi/lib/mo_cdi.o"])
	              		AS_VAR_APPEND([LIBS], ["$LIBCDI"])
	   					AC_DEFINE([HAVE_LIBCDI], [1], [Define if you have libcdi])
	              	],
	              	[
	              		AC_MSG_FAILURE([--with-cdi was given, but test for cdi failed])
	              	])
            ]
          )

AC_LANG_POP([Fortran])

# udunits2
AC_CHECK_LIB([udunits2], [ut_new_system], 
	[], 
	[
		AC_MSG_FAILURE([the udunits2 library is needed for the datetime module])
	], 
	["-lexpat"])

AC_OUTPUT(autoconf/Makefile)
mv autoconf/Makefile Makefile
