!> @brief       Example for interpolation from one grid to another grid
program test_regrid
    use fplus_regrid

    ! local variables
    real (kind=8), dimension(:), allocatable :: inarray1
    real (kind=8), dimension(:), allocatable :: outarray1
    real (kind=8), dimension(:,:), allocatable :: inarray2
    real (kind=8), dimension(:,:), allocatable :: outarray2
    real (kind=8), dimension(:,:,:), allocatable :: inarray3
    real (kind=8), dimension(:,:,:), allocatable :: outarray3
    real (kind=8), dimension(:,:,:,:), allocatable :: inarray4
    real (kind=8), dimension(:,:,:,:), allocatable :: outarray4
    integer :: err

    ! test 1d
    allocate(inarray1(3))
    allocate(outarray1(4))
    inarray1 = (/1,2,3/)
    print*, "Uniform Linear"
    err = regrid(inarray1, outarray1)
    print*, "Errorcode: ", err
    print*, inarray1
    print*, outarray1

    print*, "Non-Uniform Linear"
    err = regrid(inarray1, outarray1, (/1.0_8,2.0_8,3.0_8/), (/1.5_8,2.0_8,2.9_8,3.0_8/))
    print*, "Errorcode: ", err
    print*, inarray1
    print*, outarray1

    deallocate(inarray1, outarray1)
    allocate(inarray1(4))
    allocate(outarray1(5))
    inarray1 = (/1,2,3,4/)
    print*, "Uniform Cubic"
    err = regrid(inarray1, outarray1, intpol=(/3/))
    print*, "Errorcode: ", err
    print*, inarray1
    print*, outarray1

    print*, "Non-Uniform Cubic"
    err = regrid(inarray1, outarray1, (/1.0_8,2.0_8,3.0_8,4.0_8/), (/1.5_8,2.0_8,2.9_8,3.0_8,3.5_8/), intpol=(/3/))
    print*, "Errorcode: ", err
    print*, inarray1
    print*, outarray1

end program
