module mod_fillvalue
    implicit none
    ! default fill values, the same as used by netcdf

    integer           fstd_fill_byte
    integer           fstd_fill_char
    integer           fstd_fill_int
    integer(kind=8)   fstd_fill_intk8
    real              fstd_fill_real
    real(kind=8)      fstd_fill_realk8

    parameter (fstd_fill_byte = -127)
    parameter (fstd_fill_char = 0)
    parameter (fstd_fill_int = -2147483647)
    parameter (fstd_fill_intk8 = -9223372036854775806_8)
    parameter (fstd_fill_real = 9.9692099683868690e+36)
    parameter (fstd_fill_realk8 = 9.9692099683868690d+36)

end module mod_fillvalue
