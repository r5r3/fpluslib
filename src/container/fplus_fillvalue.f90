module fplus_fillvalue
    implicit none
    ! default fill values, the same as used by netcdf

    integer           :: fplus_fill_byte
    integer           :: fplus_fill_char
    integer           :: fplus_fill_int
    integer(kind=8)   :: fplus_fill_intk8
    real              :: fplus_fill_real
    real(kind=8)      :: fplus_fill_realk8

    parameter (fplus_fill_byte = -127)
    parameter (fplus_fill_char = 0)
    parameter (fplus_fill_int = -2147483647)
    parameter (fplus_fill_intk8 = -9223372036854775806_8)
    parameter (fplus_fill_real = 9.9692099683868690e+36)
    parameter (fplus_fill_realk8 = 9.9692099683868690d+36)

end module fplus_fillvalue
