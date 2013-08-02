program test_point_in_polygon
	use fplus_geo_tools

	! create a polygone 
	real, dimension(5) :: polygonX = (/0.0,1.0,1.0,0.0,0.0/)
	real, dimension(5) :: polygonY = (/0.0,0.0,1.0,1.0,0.0/)

	! check if some points are inside or outside of the polygon
	print*, point_in_polygon(polygonX, polygonY, 0.5, 0.5)
	print*, point_in_polygon(polygonX, polygonY, 1.5, 0.5)
	print*, point_in_polygon(polygonX, polygonY, 0.9, 0.1)
	print*, point_in_polygon(polygonX, polygonY, -0.5, 0.1)

end program