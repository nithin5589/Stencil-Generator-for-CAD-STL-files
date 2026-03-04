module attachment
   implicit none
contains
   subroutine file_exist(filename, ans)
      character(len=*), intent(in) :: filename
      logical, intent(out) :: ans
      
      inquire(file=filename, exist=ans)
      
   end subroutine file_exist

   logical function find_neighbor(grid, target, n, tolerance)
      real(8), intent(in) :: grid(:,:)
      real(8), intent(in) :: target(3)
      integer, intent(in) :: n
      real(8), intent(in) :: tolerance
      integer :: low, high, mid, k
      
      find_neighbor = .false.
      
      ! Binary search to find starting position based on Y-coordinate
      low = 1
      high = n
      
      do while (low < high)
         mid = (low + high) / 2
         if (grid(mid, 2) + tolerance < target(2)) then
            low = mid + 1
         else
            high = mid
         end if
      end do
      
      ! Linear search from found position (nearby points have similar Y)
      do k = low, n
         ! Exit if Y-coordinate is too far
         if (grid(k, 2) > target(2) + tolerance) exit
         
         ! Check if all coordinates match within tolerance
         if (all(abs(grid(k, :) - target) <= tolerance)) then
            find_neighbor = .true.
            return
         end if
      end do
      
   end function find_neighbor

end module attachment

program IPandBP2D
   use attachment
   implicit none
   real(8) :: dx, dy, dz
   real(8), parameter :: tol = 1.0d-6
   real(8), allocatable :: zsliceplanes(:)
   real(8), allocatable :: grid(:,:)
   character(len=2048) :: arg, foldername, filename, fname, line, charP
   integer :: num_rows, ios
   integer :: i, j, a, b
   real(8) :: d
   integer :: counter
   real(8) :: xgrid, ygrid, zgrid
   real(8) :: neighbor(3)
   real(8) :: temp(3)
   logical :: check
   integer, parameter :: UNIT_INPUT = 10
   integer, parameter :: UNIT_INTERIOR = 150
   integer, parameter :: UNIT_BOUNDARY = 151
   print *, "----------------------------------------------"
   ! Read dx, dy and dz from command line
   if (command_argument_count() >= 3) then
       call get_command_argument(1, arg); read(arg, *) dx
       call get_command_argument(2, arg); read(arg, *) dy
       call get_command_argument(3, arg); read(arg, *) dz
   else
       dx = 1.000; dy = 1.000; dz = 1.000  ! defaults
   end if
   ! ----------------------------------------------------------------------------------
   ! STEP 1: Read z-slicing planes
   ! ----------------------------------------------------------------------------------
   open(unit=UNIT_INPUT, file='slicePlanes/sliceplanes.txt', status='old')
   
   num_rows = 0
   do
      read(UNIT_INPUT, '(A)', iostat=ios) line
      if (ios /= 0) exit
      num_rows = num_rows + 1
   end do
   
   allocate(zsliceplanes(num_rows))
   rewind(UNIT_INPUT)
   
   do i = 1, num_rows
      read(UNIT_INPUT, *) zsliceplanes(i)
   end do
   close(UNIT_INPUT)
   
   print *, "  Number of z-planes: ", num_rows
   print *, "  Grid spacing: dx=", dx, " dy=", dy, " dz=", dz
   
   ! ----------------------------------------------------------------------------------
   ! STEP 2: Open output directory
   ! ----------------------------------------------------------------------------------
   fname = '2D_IP_BP_Data/IP_ZSlicePlane.txt'
   open(unit=UNIT_INTERIOR, file=fname, status='replace')
   write(UNIT_INTERIOR, '(A)') '# Interior Points: x, y, z'
   
   fname = '2D_IP_BP_Data/BP_ZSlicePlane.txt'
   open(unit=UNIT_BOUNDARY, file=fname, status='replace')
   write(UNIT_BOUNDARY, '(A)') '# Boundary Points: x, y, z'
   
   ! ----------------------------------------------------------------------------------
   ! STEP 3: Process each z-plane
   ! ----------------------------------------------------------------------------------
   foldername = '2D_All_Contour_Points/'
   do i = 1, size(zsliceplanes)
      d = zsliceplanes(i)
      write(charP, "(F0.3)") d
      filename = trim(foldername) // 'ZSlicePlane_' // trim(adjustl(charP)) // '.txt'
      
      ! Check if file exists
      call file_exist(filename, check)
      
      if (.not. check) then
         if (i == 1) then
            print *, "  WARNING: First z-plane file not found: ", trim(filename)
         end if
         cycle
      end if
      
      !------------------------------------------------------------------------
      ! Read and sort grid points
      !------------------------------------------------------------------------
      open(unit=UNIT_INPUT, file=filename, status='old')
      
      num_rows = 0
      do
         read(UNIT_INPUT, '(A)', iostat=ios) line
         if (ios /= 0) exit
         if (len_trim(line) == 0) cycle
         if (line(1:1) == '#') cycle
         num_rows = num_rows + 1
      end do
      
      if (num_rows == 0) then
         close(UNIT_INPUT)
         cycle
      end if
      
      allocate(grid(num_rows, 3))
      rewind(UNIT_INPUT)
      
      j = 0
      do
         read(UNIT_INPUT, '(A)', iostat=ios) line
         if (ios /= 0) exit
         if (len_trim(line) == 0) cycle
         if (line(1:1) == '#') cycle
         
         j = j + 1
         read(line, *) grid(j, :)
      end do
      close(UNIT_INPUT)
      
      !------------------------------------------------------------------------
      ! Sort grid by Y-coordinate (for efficient binary search)
      !------------------------------------------------------------------------
      do a = 1, num_rows - 1
         do b = a + 1, num_rows
            if (grid(b, 2) < grid(a, 2)) then
               temp = grid(a, :)
               grid(a, :) = grid(b, :)
               grid(b, :) = temp
            end if
         end do
      end do
      
      !------------------------------------------------------------------------
      ! Classify each point as interior or boundary
      !------------------------------------------------------------------------
      do a = 1, num_rows
         xgrid = grid(a, 1)
         ygrid = grid(a, 2)
         zgrid = grid(a, 3)
         
         counter = 0
         
         ! Check 4 neighbors: left, right, down, up
         ! Neighbor 1: left (x-dx, y, z)
         neighbor = [xgrid - dx, ygrid, zgrid]
         if (find_neighbor(grid, neighbor, num_rows, tol)) counter = counter + 1
         
         ! Neighbor 2: right (x+dx, y, z)
         neighbor = [xgrid + dx, ygrid, zgrid]
         if (find_neighbor(grid, neighbor, num_rows, tol)) counter = counter + 1
         
         ! Neighbor 3: down (x, y-dy, z)
         neighbor = [xgrid, ygrid - dy, zgrid]
         if (find_neighbor(grid, neighbor, num_rows, tol)) counter = counter + 1
         
         ! Neighbor 4: up (x, y+dy, z)
         neighbor = [xgrid, ygrid + dy, zgrid]
         if (find_neighbor(grid, neighbor, num_rows, tol)) counter = counter + 1
         
         ! Classify point
         if (counter == 4) then
            ! Interior point: all 4 neighbors exist
            write(UNIT_INTERIOR, '(3F16.6)') xgrid, ygrid, zgrid
         else
            ! Boundary point: missing at least one neighbor
            write(UNIT_BOUNDARY, '(3F16.6)') xgrid, ygrid, zgrid
         end if
      end do
      
      deallocate(grid)
      
      print *, "  Processed z-plane: ", d
      
   end do
   
   ! ----------------------------------------------------------------------------------
   ! Cleanup
   ! ----------------------------------------------------------------------------------
   close(UNIT_INTERIOR)
   close(UNIT_BOUNDARY)
   deallocate(zsliceplanes)
   print *, "=============================================="
   print *, "  2D Interior and Boundary Points Classified"
   print *, "=============================================="
   print *, "----------------------------------------------"
end program IPandBP2D