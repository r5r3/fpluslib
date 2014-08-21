!> @brief   A representation of file of directory path names
!> @author  Robert Schuster
module fplus_path
    use fplus_object
    use fplus_hashcode
    use fplus_container
    use fplus_error
    use fplus_strings
    use mo_cdi
    use, intrinsic :: ISO_C_BINDING
    implicit none
    private

    type, extends (object), public :: path
        !> @brief   The name of the representated file or directory
        character (len=1000), public :: name
    contains
        !> @brief   Returns a string representation of this object
        procedure, public :: to_string => path_to_string
        !> @brief   Calculate the hash code for this object
        procedure, public :: hashcode => path_hashcode
        !> @brief   Tests whether the file or directory denoted by this abstract pathname exists.
        procedure, public :: exists => path_exists
        !> @brief   Returns the name as c-compatible string
        procedure, public :: get_cstr_name => path_get_cstr_name
        !> @brief   Returns the extension of the file, if any.
        procedure, public :: get_extension => path_get_extension
        !> @brief   Returns the path without the extension
        procedure, public :: get_basename => path_get_basename
        !> @brief   Returns the content of a text file as character array
        procedure, public :: get_lines => path_get_lines
        !> @brief   Returns a list with the content of a directory
        procedure, public :: list => path_list
        !> @brief   Returns .true. if this path is a directory
        procedure, public :: is_directory => path_is_directory
        !> @brief   Returns the non directory part of the file name
        procedure, public :: get_name_nondirectory => path_get_name_nondirectory
    end type

    ! public procedures
    public :: new_path, to_path

    ! interfaces to c functions
    interface 
        function opendir(dirname) result(res) bind(C,name="opendir")
            import :: C_ptr, C_char
            character (kind=C_char) :: dirname(*)
            type(C_ptr) :: res
        end function
        subroutine closedir(dirp) bind(C,name="closedir")
            import C_ptr
            type(C_ptr), value :: dirp
        end subroutine
        function readdir_wrapper(dirp, filename) result(res) bind(C,name="readdir_wrapper")
            import :: C_ptr, C_char, C_int
            type(C_ptr), value :: dirp
            character (kind=C_char) :: filename(*)
            integer (kind=C_int) :: res
        end function
        function is_directory(filename) result(res) bind(C,name="is_directory")
            import :: C_char, C_int
            character (kind=C_char) :: filename(*)
            integer (kind=C_int) :: res
        end function
        function realpath(file_name, resolved_name) result(res) bind(C,name="realpath")
            import :: C_char, C_ptr
            character (kind=C_char) :: file_name(*)
            character (kind=C_char) :: resolved_name(*)
            type(C_ptr) :: res
        end function
        function c_getcwd(cwd, len_cwd) result(res) bind(C,name="getcwd")
            import :: C_char, C_ptr, C_size_t
            character (kind=C_char) :: cwd(*)
            integer (kind=C_size_t), value :: len_cwd
            type(C_ptr) :: res
        end function
    end interface

contains

    !> @public
    !> @brief       The constructor for a path name representation
    function new_path(pathname) result(res)
        type (path) :: res
        character (len=*) :: pathname
        res%name = pathname
    end function

    !> @public
    !> @brief       Returns a string representation of this object
    function path_to_string(this) result (res)
        class(path) :: this
        character (len=:), allocatable :: res
        res = "Path: " // trim(this%name)
    end function

    !> @public
    !> @brief       Calculate the hash code for this object
    function path_hashcode(this) result (res)
        class (path) :: this
        integer (kind=8) :: res

        ! local variables
        character (len=:), allocatable :: str
        integer (kind=1), dimension(:), allocatable :: chars
        integer :: l

        str = "Path" // trim(this%name)

        l = len(str)
        allocate(chars(l))
        chars = transfer(str, chars)
        call C_sdbm(chars, l, res)
    end function

    !> @brief       Tests whether the file or directory denoted by this abstract pathname exists.
    logical function path_exists(this) result(res)
        class(path) :: this
        inquire(file=this%name, exist=res)
    end function

    !> @public
    !> @brief       Returns the name as c-compatible string
    function path_get_cstr_name(this) result(res)
        class(path) :: this
        character (len=:), allocatable :: res
        res = trim(this%name) // C_NULL_CHAR
    end function

    !> @public
    !> @brief       Returns the extension of the file, if any.
    function path_get_extension(this) result (res)
        class(path) :: this
        character (len=:), allocatable :: res

        !local variables
        integer :: i1, i2

        ! find the last dot
        i1 = index(this%name, ".", back=.true.)
        i2 = index(this%name, "/", back=.true.)
        if (i1==0 .or. i1 <= i2 .or. i1 == len_trim(this%name)) then
            res = ""
        else
            res = trim(this%name(i1+1:))
        end if
    end function

    !> @public
    !> @brief   Returns the path without the extension
    function path_get_basename(this) result (res)
        class(path) :: this
        character (len=:), allocatable :: res

        !local variables
        integer :: i1, i2

        ! find the last dot
        i1 = index(this%name, ".", back=.true.)
        i2 = index(this%name, "/", back=.true.)
        if (i1==0 .or. i1 <= i2 .or. i1 == len_trim(this%name)) then
            res = this%name
        else
            res = trim(this%name(:i1-1))
        end if
    end function

    !> @public
    !> @brief   Returns the filename without a directory
    function path_get_name_nondirectory(this) result (res)
        class(path) :: this
        character (len=:), allocatable :: res

        !local variables
        integer :: i1

        ! find the last slash
        i1 = index(this%name, "/", back=.true.)
        if (i1==0 .or. i1 == len_trim(this%name)) then
            res = this%name
        else
            res = trim(this%name(i1+1:))
        end if
    end function

    !> @public 
    !> @brief       Returns the content of a text file as character array
    !> @param       as_path     set to true if the content of the file is one filename per line. 
    !>                          the returned list will then contain path objects.       
    function path_get_lines(this, as_path) result (res)
        class(path) :: this
        logical, optional :: as_path
        type(list) :: res

        ! local variables
        logical :: as_path_intern
        integer :: unit, iostat, reclen
        character(len=80) :: iomsg
        character(len=1000) :: line
        type(string) :: line_str
        type(path) :: path_str

        as_path_intern = present(as_path)
        if (as_path_intern) as_path_intern = as_path .eqv. .true.

        ! a new list for the result
        res = new_list()

        ! open the input file
        open(newunit=unit, file=this%name, status="OLD", action="READ", iostat=iostat, iomsg=iomsg)
        ! any errors? 
        if (iostat /= 0) call fplus_error_print(iomsg, "path%get_lines")

        ! read as long as no errors occure
        do
            read (unit, "(a)", iostat=iostat,iomsg=iomsg) line
            if (iostat /= 0) exit
            if (as_path_intern) then
                path_str%name = line
                call res%add(path_str, copy=.true.)
            else
                line_str%chars = line
                call res%add(line_str, copy=.true.)
            end if
        end do

        ! close the file again
        close(unit)
    end function

    !> @public
    !> @brief       find all files an directories in below this parent path
    !> @param[in]   this        reference to the path object, automatically set by fortran
    !> @param[in]   max_depth   maximal level of subdirectories to brows. 1=no subdirectiries, default=1
    !> @param[in]   only_files  if .true., only files and no folders are returned. Default=.false.
    !> @param[in]   abspath     return the absolute filename instead of name relative to this path. Default=.false.
    !> @return      list of path objects
    recursive function path_list(this, max_depth, only_files, abspath) result(res)
        class(path) :: this
        integer, optional :: max_depth
        logical, optional :: only_files, abspath
        type(list) :: res

        ! local variables
        integer :: max_depth_intern, ierr, isdir, i
        logical :: only_files_intern
        type(C_ptr) :: dir_ptr, resolved_name
        character(len=1000) :: filename
        type(path) :: direntry, subdir, child
        type(list) :: children
        class(*), pointer :: temp

        max_depth_intern = 1
        if (present(max_depth)) max_depth_intern = max_depth
        only_files_intern = .false.
        if (present(only_files)) only_files_intern = only_files

        ! create a new list for the result
        res = new_list()

        ! is this a directory? if not, return the empty list
        if (.not. this%is_directory()) return 

        ! open the directory 
        dir_ptr = opendir(this%get_cstr_name())

        ! loop over all entries
        do 
            ! read next entry
            ierr = readdir_wrapper(dir_ptr, filename)
            if (ierr /= 0) exit
            call ctrim(filename)
            if (filename == "." .or. filename == "..") cycle
            ! create new path object from it
            direntry = new_path(filename)
            ! a file or a folder?
            isdir = is_directory(trim(this%name) // "/" // trim(direntry%name) // C_NULL_CHAR)
            if (.not. only_files_intern .or. (only_files_intern .and. isdir == 0)) call res%add(direntry, copy=.true.)
            ! traverse the directory
            if (isdir == 1 .and. max_depth_intern > 1) then
                ! get all children
                subdir = new_path(trim(this%name) // "/" // trim(direntry%name))
                children = subdir%list(max_depth_intern-1, only_files_intern, .false.)
                ! loop over all children
                do i = 1, children%length()
                    temp => children%get(i)
                    select type (temp)
                        type is (path)
                            child = new_path(trim(direntry%name) // "/" // trim(temp%name))
                            call res%add(child, copy=.true.)
                    end select
                end do
                call children%clear()
            end if
        end do

        ! close the directory again
        call closedir(dir_ptr)

        ! get the absolute paths of the files, if requested
        if (present(abspath) .and. abspath .eqv. .true.) then
            do i = 1, res%length()
                temp => res%get(i)
                select type(temp)
                    type is (path)
                        if (this%name(1:1) /= '/') then
                            resolved_name = c_getcwd(filename, int(len(filename), C_size_t))
                            call ctrim(filename)
                            filename = trim(filename) // "/" // trim(this%name) // "/" // trim(temp%name) // C_NULL_CHAR
                        else
                            filename = trim(this%name) // "/" // trim(temp%name) // C_NULL_CHAR
                        end if
                        resolved_name = realpath(filename, temp%name)
                        call ctrim(temp%name)
                end select
            end do
        end if
    end function

    !> @public
    !> @brief       Check if this is a directory
    !> @param[in]   this        reference to the path object, automatically set by fortran
    !> @return      .true. for a directory otherwise .false.
    function path_is_directory(this) result(res)
        class(path) :: this
        logical :: res
        if (is_directory(this%get_cstr_name()) == 1) then
            res = .true.
        else
            res = .false.
        end if
    end function

    !> @public
    !> @brief       Cast an class(*) type to path
    !> @param       arg     class(*) type to convert
    function to_path(arg) result (res)
        class(*) :: arg
        type(path) :: res
        select type (arg)
            type is (path)
                res = arg
            class default
                call fplus_error_print("wrong datatype in cast to path", "to_path")
        end select
    end function

end module fplus_path
