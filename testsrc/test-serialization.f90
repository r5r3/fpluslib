program test_serialization
	use fplus_serialization
	implicit none

	! a buffer for 36 bytes
	type(buffer), allocatable :: buf

	! an array to place into the buffer
	integer(kind=4), dimension(2,2) :: test_array
	! a number to place into the buffer
	integer :: test_number
	! an allocatable array for to place into the buffer
	real, dimension(:), allocatable :: test_array_2

	! initialize the buffer with 36 bytes
	buf = new_buffer(36_8)

	! write the test array to the buffer
	test_array = 7
	call buf%put(test_array)
	test_array = 8
	call buf%put(test_array)
	call buf%put(300)

	print*, "Content of the buffer:"
	print*, buf%array
	print*, ""

	! read data from the buffer
	call buf%rewind()
	call buf%get(test_array)

	print*, "Content of test_array:"
	print*, test_array

	call buf%get(test_array)

	print*, "Content of test_array:"
	print*, test_array

	call buf%get(test_number)
	print*, test_number

	! store an allocatable array into the buffer
	call buf%rewind()
	allocate(test_array_2(2))
	test_array_2 = 10
	call buf%put_with_bounds(test_array_2)
	print*, ""
	print*, "Content of the buffer:"
	print*, buf%array

	! restore the array
	call buf%rewind()
	deallocate(test_array_2)
	call buf%get_with_bounds(test_array_2)

	print*, "Content of test_array_2:"
	print*, test_array_2

	! calculate the size of different types
	print*, ""
	print*, "Size of integer:         		   ", byte_size(300)
	print*, "Size of integer (kind=8):		   ", byte_size(300_8)
	print*, "Size of test_array_2:    		   ", byte_size(test_array_2)
	print*, "Size of test_array_2 with bounds: ", byte_size_with_bounds(test_array_2)

end program
