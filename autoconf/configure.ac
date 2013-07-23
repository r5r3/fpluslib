AC_INIT(fpluslib, 0.1)

echo "                                                               Testing for a C compiler"
AC_PROG_CC

echo "                                                               Testing for a FORTRAN compiler"
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
AC_LANG_PUSH([Fortran])

AC_ARG_WITH([cdi],
            [AS_HELP_STRING([--with-cdi],
              [support for climat data interface @<:@default=no@:>@])],
            [
            	AS_VAR_APPEND([LDFLAGS], [" -L$with_cdi/lib $with_cdi/lib/mo_cdi.o"])
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
	              		AC_SUBST([LIBCDI], ["-lcdi $with_cdi/lib/mo_cdi.o"])
	   					AC_DEFINE([HAVE_LIBCDI], [1], [Define if you have libcdi])
	              	],
	              	[
	              		AC_MSG_FAILURE([--with-cdi was given, but test for cdi failed])
	              	])
            ]
          )

AC_LANG_POP([Fortran])


AC_OUTPUT(autoconf/Makefile)
mv autoconf/Makefile Makefile
