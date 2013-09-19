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
    print*, "FROM SMALLER TO LARGER GRID"
    print*, "--------------------------------------------------------------------------------------"
    allocate(inarray1(4))
    allocate(outarray1(5))
    inarray1 = (/1,2,2,1/)
    print*, "Uniform Linear"
    err = regrid(inarray1, outarray1)
    print*, "Errorcode: ", err
    print*, inarray1
    print*, outarray1

    print*, "Non-Uniform Linear"
    err = regrid(inarray1, outarray1, (/1.0_8,2.0_8,3.0_8,4.0_8/), (/1.5_8,2.0_8,2.9_8,3.0_8,4.0_8/))
    print*, "Errorcode: ", err
    print*, inarray1
    print*, outarray1

    deallocate(inarray1, outarray1)
    allocate(inarray1(4))
    allocate(outarray1(5))
    inarray1 = (/1,2,2,1/)
    print*, "Uniform Cubic"
    err = regrid(inarray1, outarray1, intpol=(/3/))
    print*, "Errorcode: ", err
    print*, inarray1
    print*, outarray1

    print*, "Non-Uniform Cubic"
    err = regrid(inarray1, outarray1, (/1.0_8,2.0_8,3.0_8,4.0_8/), (/1.5_8,2.0_8,2.9_8,3.0_8,4.0_8/), intpol=(/3/))
    print*, "Errorcode: ", err
    print*, inarray1
    print*, outarray1

    print*, ""
    print*, "FROM LARGER TO SMALLER GRID"
    print*, "--------------------------------------------------------------------------------------"
    deallocate(inarray1, outarray1)
    allocate(inarray1(5), outarray1(4))
    inarray1 = (/1,2,3,2,1/)
    print*, "Uniform Linear"
    err = regrid(inarray1, outarray1)
    print*, "Errorcode: ", err
    print*, inarray1
    print*, outarray1

    print*, "Uniform Cubic"
    err = regrid(inarray1, outarray1, intpol=(/3/))
    print*, "Errorcode: ", err
    print*, inarray1
    print*, outarray1


    ! test 2d
    print*, ""
    print*, "FROM SMALLER TO LARGER GRID (2-d)"
    print*, "--------------------------------------------------------------------------------------"
    allocate(inarray2(3,3), outarray2(4,4))
    inarray2 = 1
    inarray2(2,2) = 2
    print*, "Uniform Linear"
    err = regrid(inarray2, outarray2)
    print*, "Errorcode: ", err
    print*, inarray2(1,:)
    print*, inarray2(2,:)
    print*, inarray2(3,:)    
    print*, ""
    print*, outarray2(1,:)
    print*, outarray2(2,:)
    print*, outarray2(3,:)
    print*, outarray2(4,:)

    print*, ""
    print*, "FROM LARGER TO SMALLER GRID (2-d)"
    print*, "--------------------------------------------------------------------------------------"
    deallocate(inarray2, outarray2)
    allocate(inarray2(5,5), outarray2(4,4))
    inarray2 = 1
    inarray2(3,3) = 2
    print*, "Uniform Linear"
    err = regrid(inarray2, outarray2)
    print*, "Errorcode: ", err
    print*, inarray2(1,:)
    print*, inarray2(2,:)
    print*, inarray2(3,:)    
    print*, inarray2(4,:)    
    print*, inarray2(5,:)    
    print*, ""
    print*, outarray2(1,:)
    print*, outarray2(2,:)
    print*, outarray2(3,:)
    print*, outarray2(4,:)

    print*, "Uniform Cubic"
    err = regrid(inarray2, outarray2, intpol=(/3,3/))
    print*, "Errorcode: ", err
    print*, inarray2(1,:)
    print*, inarray2(2,:)
    print*, inarray2(3,:)    
    print*, inarray2(4,:)    
    print*, inarray2(5,:)    
    print*, ""
    print*, outarray2(1,:)
    print*, outarray2(2,:)
    print*, outarray2(3,:)
    print*, outarray2(4,:)


end program
