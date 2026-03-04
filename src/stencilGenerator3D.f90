module attachment
   implicit none
   
contains
   subroutine file_exist(filename, ans)
      character(len=*), intent(in) :: filename
      logical, intent(out) :: ans
      
      inquire(file=filename, exist=ans)
      
   end subroutine file_exist

   subroutine sort_grid_3D(grid, n)
      real(8), intent(inout) :: grid(:,:)
      integer, intent(in) :: n
      integer :: i, j
      real(8) :: temp(3)
      logical :: swapped
      
      ! Bubble sort
      do i = 1, n - 1
         swapped = .false.
         do j = 1, n - i
            ! Compare: Z first, then Y, then X
            if (grid(j, 3) > grid(j+1, 3) .or. &
               (abs(grid(j, 3) - grid(j+1, 3)) < 1.0d-10 .and. grid(j, 2) > grid(j+1, 2)) .or. &
               (abs(grid(j, 3) - grid(j+1, 3)) < 1.0d-10 .and. &
               abs(grid(j, 2) - grid(j+1, 2)) < 1.0d-10 .and. grid(j, 1) > grid(j+1, 1))) then
               
               ! Swap rows
               temp = grid(j, :)
               grid(j, :) = grid(j+1, :)
               grid(j+1, :) = temp
               swapped = .true.
            end if
         end do
         
         ! Early exit if no swaps made
         if (.not. swapped) exit
         
         ! Progress for large datasets
         if (mod(i, 5000) == 0) then
            print *, "    Sorting progress: ", int(100.0*i/(n-1)), "%"
         end if
      end do
      
   end subroutine sort_grid_3D

   logical function find_neighbor_3D(grid, target, n, tolerance)
      real(8), intent(in) :: grid(:,:)
      real(8), intent(in) :: target(3)
      integer, intent(in) :: n
      real(8), intent(in) :: tolerance
      integer :: low, high, mid, i
      
      find_neighbor_3D = .false.
      
      ! Binary search to find starting position based on Z-coordinate
      low = 1
      high = n
      
      do while (low < high)
         mid = (low + high) / 2
         if (grid(mid, 3) + tolerance < target(3)) then
            low = mid + 1
         else
            high = mid
         end if
      end do
      
      ! Linear search from found position (only search nearby Z values)
      do i = low, n
         ! Exit if Z-coordinate is too far above target
         if (grid(i, 3) > target(3) + tolerance) exit
         
         ! Skip if Z-coordinate is too far below target
         if (grid(i, 3) < target(3) - tolerance) cycle
         
         ! Check X and Y coordinates (Z is already within tolerance)
         if (abs(grid(i, 1) - target(1)) <= tolerance .and. &
            abs(grid(i, 2) - target(2)) <= tolerance) then
            find_neighbor_3D = .true.
            return
         end if
      end do
      
   end function find_neighbor_3D

end module attachment

program stencilGenerator3D
   use attachment
   implicit none
   real(8), parameter :: tol = 1.0e-6
   real(8) :: dx, dy, dz
   real(8), allocatable :: zsliceplanes(:)
   real(8), allocatable :: grid(:,:)
   character(len=2048) :: arg, foldername, filename, fname, line, charP
   integer :: num_rows, ios
   integer :: i, j, a
   real(8) :: d
   integer :: counter, n_interior, n_boundary
   real(8) :: xgrid, ygrid, zgrid
   real(8) :: neighbor(3)
   logical :: check
   integer, parameter :: UNIT_INPUT = 10
   integer, parameter :: UNIT_ALL = 19
   integer, parameter :: UNIT_INTERIOR = 150
   integer, parameter :: UNIT_BOUNDARY = 151
   print *, "----------------------------------------------"
   ! Read dx, dy and dz from command line
   if (command_argument_count() >= 3) then
       call get_command_argument(1, arg); read(arg, *) dx
       call get_command_argument(2, arg); read(arg, *) dy
       call get_command_argument(3, arg); read(arg, *) dz
   else
       dx = 1.000d0; dy = 1.000d0; dz = 0.500d0  ! Default
   end if
   
   
   
   !===========================================================================
   ! STEP 1: Read z-slicing planes
   !===========================================================================
   print *, ""
   print *, "Step 1: Reading z-slice planes..."
   
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
   
   !===========================================================================
   ! STEP 2: Consolidate all points from all z-planes into single file
   !===========================================================================
   foldername = '2D_All_Contour_Points/'
   fname = '3D_All_Points/3D_All_Points.txt'
   open(unit=UNIT_ALL, file=fname, status='replace')
   write(UNIT_ALL, '(A)') '# All 3D points from all z-planes'
   write(UNIT_ALL, '(A)') '# x, y, z'
   num_rows = 0
   
   do i = 1, size(zsliceplanes)
      d = zsliceplanes(i)
      write(charP, "(F0.3)") d
      
      filename = trim(foldername) // 'ZSlicePlane_' // &
                 trim(adjustl(charP)) // '.txt'
      
      ! Check if file exists
      call file_exist(filename, check)
      
      if (.not. check) then
         if (i == 1) then
            print *, "  WARNING: First z-plane file not found"
         end if
         cycle
      end if
      
      ! Read and write points from this z-plane
      open(unit=UNIT_INPUT, file=filename, status='old')
      
      do
         read(UNIT_INPUT, '(A)', iostat=ios) line
         if (ios /= 0) exit
         if (len_trim(line) == 0) cycle
         if (line(1:1) == '#') cycle
         
         write(UNIT_ALL, '(A)') trim(line)
         num_rows = num_rows + 1
      end do
      
      close(UNIT_INPUT)
      
      print *, "  Added z-plane: ", d
   end do
   
   close(UNIT_ALL)
   
   print *, "  Total points consolidated: ", num_rows
   
   !===========================================================================
   ! STEP 3: Load all points into memory
   !===========================================================================
   print *, ""
   print *, "Step 3: Loading all points into memory..."
   
   open(unit=UNIT_ALL, file=fname, status='old')
   
   num_rows = 0
   do
      read(UNIT_ALL, '(A)', iostat=ios) line
      if (ios /= 0) exit
      if (len_trim(line) == 0) cycle
      if (line(1:1) == '#') cycle
      num_rows = num_rows + 1
   end do
   
   allocate(grid(num_rows, 3))
   rewind(UNIT_ALL)
   
   j = 0
   do
      read(UNIT_ALL, '(A)', iostat=ios) line
      if (ios /= 0) exit
      if (len_trim(line) == 0) cycle
      if (line(1:1) == '#') cycle
      
      j = j + 1
      read(line, *) grid(j, :)
   end do
   close(UNIT_ALL)
   
   print *, "  Points loaded: ", num_rows
   print *, "  Sorting points by Z coordinate..."

   ! Sort grid by Z coordinate (primary), then Y (secondary), then X (tertiary)
   call sort_grid_3D(grid, num_rows)

   print *, "  Sorting complete"
   
   !===========================================================================
   ! STEP 4: Classify each point as interior or boundary
   !===========================================================================
   print *, ""
   print *, "Step 4: Classifying points (this may take a while)..."
   
   ! Open output files
   fname = '3D_All_Points/IP_ZSlicePlane.txt'
   open(unit=UNIT_INTERIOR, file=fname, status='replace')
   write(UNIT_INTERIOR, '(A)') '# 3D Interior Points: x, y, z'
   
   fname = '3D_All_Points/BP_ZSlicePlane.txt'
   open(unit=UNIT_BOUNDARY, file=fname, status='replace')
   write(UNIT_BOUNDARY, '(A)') '# 3D Boundary Points: x, y, z'
   
   n_interior = 0
   n_boundary = 0
   
   ! Process each point
   do a = 1, num_rows
      xgrid = grid(a, 1)
      ygrid = grid(a, 2)
      zgrid = grid(a, 3)
      
      counter = 0
      
      ! Check 6 neighbors in 3D space
      
      ! Neighbor 1: left (x-dx, y, z)
      neighbor = [xgrid - dx, ygrid, zgrid]
      if (find_neighbor_3D(grid, neighbor, num_rows, tol)) counter = counter + 1
      
      ! Neighbor 2: right (x+dx, y, z)
      neighbor = [xgrid + dx, ygrid, zgrid]
      if (find_neighbor_3D(grid, neighbor, num_rows, tol)) counter = counter + 1
      
      ! Neighbor 3: front (x, y-dy, z)
      neighbor = [xgrid, ygrid - dy, zgrid]
      if (find_neighbor_3D(grid, neighbor, num_rows, tol)) counter = counter + 1
      
      ! Neighbor 4: back (x, y+dy, z)
      neighbor = [xgrid, ygrid + dy, zgrid]
      if (find_neighbor_3D(grid, neighbor, num_rows, tol)) counter = counter + 1
      
      ! Neighbor 5: down (x, y, z-dz)
      neighbor = [xgrid, ygrid, zgrid - dz]
      if (find_neighbor_3D(grid, neighbor, num_rows, tol)) counter = counter + 1
      
      ! Neighbor 6: up (x, y, z+dz)
      neighbor = [xgrid, ygrid, zgrid + dz]
      if (find_neighbor_3D(grid, neighbor, num_rows, tol)) counter = counter + 1
      
      ! Classify point
      if (counter == 6) then
         ! Interior point: all 6 neighbors exist
         write(UNIT_INTERIOR, '(3F16.6)') xgrid, ygrid, zgrid
         n_interior = n_interior + 1
      else
         ! Boundary point: missing at least one neighbor
         write(UNIT_BOUNDARY, '(3F16.6)') xgrid, ygrid, zgrid
         n_boundary = n_boundary + 1
      end if
      
      ! Progress reporting (every 1000 points)
      if (mod(a, 1000) == 0 .or. a == num_rows) then
         print *, "  Processed: ", a, " / ", num_rows, &
                  " (", int(100.0*a/num_rows), "%)"
      end if
   end do
   
   !===========================================================================
   ! Cleanup
   !===========================================================================
   close(UNIT_INTERIOR)
   close(UNIT_BOUNDARY)
   deallocate(grid, zsliceplanes)
   
   print *, "=============================================="
   print *, "  3D Stencil Generation Complete"
   print *, "=============================================="
   print *, "  Total points: ", num_rows
   print *, "  Interior points: ", n_interior
   print *, "  Boundary points: ", n_boundary
   print *, "  Output files:"
   print *, "    Total: 3D_All_Points/3D_All_Points.txt"
   print *, "    Interior: 3D_All_Points/IP_ZSlicePlane.txt"
   print *, "    Boundary: 3D_All_Points/BP_ZSlicePlane.txt"
   print *, "=============================================="
   print *, "----------------------------------------------"

end program stencilGenerator3D