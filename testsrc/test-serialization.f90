program test_serialization
	use fplus_serialization
	implicit none

	! a buffer for 100 bytes
	type(buffer), allocatable :: buf

	! an array to place into the buffer
	integer(kind=4), dimension(2,2) :: test_array

	! initialize the buffer with 100 bytes
	buf = new_buffer(36_8)

	! write the test array to the buffer
	test_array = 7
	call buf%put(test_array)
	test_array = 8
	call buf%put(test_array)
	call buf%put(300)

	print*, buf%array

end program
