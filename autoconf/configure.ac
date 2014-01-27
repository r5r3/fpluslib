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
        AS_VAR_APPEND([CFLAGS], [" -I$(nc-config --includedir)"])
        AS_VAR_APPEND([LDFLAGS], [" -L$(nc-config --prefix)/lib -Wl,-rpath=$(nc-config --prefix)/lib"])
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

# cdi ----------------------------------------------------------------
AC_ARG_WITH([cdi],
            [AS_HELP_STRING([--with-cdi],
              [support for climat data interface @<:@default=no@:>@])],
            [
                AS_VAR_IF([with_cdi], [yes], 
                    [],
                    [
                        AS_VAR_APPEND([LDFLAGS], [" -L$with_cdi/lib"])
                        AS_VAR_IF([OS], [Linux], 
                            [
                                AS_VAR_APPEND([LDFLAGS], [" -Wl,-rpath=$with_cdi/lib"])
                            ], 
                            [])             
                        AS_VAR_APPEND([FCFLAGS], [" -I$with_cdi/include"])              
                    ]
                )
            ],
            [with_cdi=no])

AC_ARG_WITH([grib_api],
            [AS_HELP_STRING([--with-grib_api],
              [this parameter is needed when cdi was compiled with grib api support @<:@default=no@:>@])],
            [
                AS_VAR_IF([with_grib_api], [yes], 
                    [],
                    [
                        AS_VAR_APPEND([LDFLAGS], [" -L$with_grib_api/lib"])
                        AS_VAR_IF([OS], [Linux], 
                            [
                                AS_VAR_APPEND([LDFLAGS], [" -Wl,-rpath=$with_grib_api/lib"])
                            ], 
                            [])             
                    ]
                )
            ],
            [with_grib_api=no])

AC_ARG_WITH([jasper],
            [AS_HELP_STRING([--with-jasper],
              [this parameter is needed when grib_api was compiled with jasper support @<:@default=no@:>@])],
            [
                AS_VAR_IF([with_jasper], [yes], 
                    [],
                    [
                        AS_VAR_APPEND([LDFLAGS], [" -L$with_jasper/lib"])
                        AS_VAR_IF([OS], [Linux], 
                            [
                                AS_VAR_APPEND([LDFLAGS], [" -Wl,-rpath=$with_jasper/lib"])
                            ], 
                            [])             
                    ]
                )
            ],
            [with_jasper=no])

AC_ARG_WITH([hdf5],
            [AS_HELP_STRING([--with-hdf5],
              [this parameter is needed when cdi was compiled with hdf5 support @<:@default=no@:>@])],
            [
                AS_VAR_IF([with_hdf5], [yes], 
                    [],
                    [
                        AS_VAR_APPEND([LDFLAGS], [" -L$with_hdf5/lib"])
                        AS_VAR_IF([OS], [Linux], 
                            [
                                AS_VAR_APPEND([LDFLAGS], [" -Wl,-rpath=$with_hdf5/lib"])
                            ], 
                            [])             
                    ]
                )
            ],
            [with_jasper=no])

AC_ARG_WITH([szip],
            [AS_HELP_STRING([--with-szip],
              [this parameter is needed when netcdf was compiled with szip support @<:@default=no@:>@])],
            [
                AS_VAR_IF([with_szip], [yes], 
                    [],
                    [
                        AS_VAR_APPEND([LDFLAGS], [" -L$with_szip/lib"])
                        AS_VAR_IF([OS], [Linux], 
                            [
                                AS_VAR_APPEND([LDFLAGS], [" -Wl,-rpath=$with_szip/lib"])
                            ], 
                            [])             
                    ]
                )
            ],
            [with_jasper=no])


AS_IF([test "x$with_jasper" != xno],
    [
        echo "checking jasper support ... "
        AC_CHECK_LIB([jasper], [jpc_encode], 
            [], 
            [
                AC_MSG_FAILURE([--with-jasper was given, but test for jasper failed])
            ], 
            []
        )
    ]
)

AS_IF([test "x$with_hdf5" != xno],
    [
        echo "checking hdf5 support ... "
        AC_CHECK_LIB([hdf5], [H5D_close], 
            [], 
            [
                AC_MSG_FAILURE([--with-hdf5 was given, but test for hdf5 failed])
            ], 
            []
        )
    ]
)

AS_IF([test "x$with_szip" != xno],
    [
        echo "checking szip support ... "
        AC_CHECK_LIB([sz], [szip_check_params], 
            [], 
            [
                AC_MSG_FAILURE([--with-szip was given, but test for szip failed])
            ], 
            []
        )
    ]
)

AS_IF([test "x$with_grib_api" != xno],
    [
        echo "checking grib_api support ... "
        AC_CHECK_LIB([grib_api], [grib_check], 
            [], 
            [
                AC_MSG_FAILURE([--with-grib_api was given, but test for grib_api failed])
            ], 
            []
        )
    ]
)

AS_IF([test "x$with_cdi" != xno],
    [
        echo "checking cdi support ... "
        AC_CHECK_LIB([cdi], [vlistCreate], 
            [], 
            [
                AC_MSG_FAILURE([--with-cdi was given, but test for cdi failed])
            ], 
            []
        )
    ]
)

#AC_LANG_PUSH([Fortran])
#AC_LANG_POP([Fortran])

# udunits2
AC_CHECK_LIB([udunits2], [ut_new_system], 
	[], 
	[
		AC_MSG_FAILURE([the udunits2 library is needed for the datetime module])
	], 
	["-lexpat"])

AC_OUTPUT(autoconf/Makefile)
mv autoconf/Makefile Makefile
