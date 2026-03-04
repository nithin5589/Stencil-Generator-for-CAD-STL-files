module supplementary
   implicit none
   
contains
   subroutine check_file_exists(filename, file_exists)
      character(len=*), intent(in) :: filename
      logical, intent(out) :: file_exists
      
      inquire(file=filename, exist=file_exists)
      
   end subroutine check_file_exists

   subroutine point_in_polygon(xp, yp, polygon, is_inside)
      !----------------------------------------------------------------------
      ! Ray casting algorithm to determine if point (xp,yp) is inside polygon
      !----------------------------------------------------------------------
      real(8), intent(in) :: xp, yp
      real(8), intent(in) :: polygon(:,:)
      logical, intent(out) :: is_inside
      integer :: i, n_crossings
      real(8) :: x1, y1, x2, y2, x_intersect
      real(8), parameter :: tol = 1.0d-8
      
      n_crossings = 0
      
      do i = 1, size(polygon, 1)
         ! Extract line segment
         x1 = polygon(i, 1)
         y1 = polygon(i, 2)
         x2 = polygon(i, 4)
         y2 = polygon(i, 5)
         
         ! Check y1 <= y2
         if (y2 < y1) then
            ! Swap points
            x1 = polygon(i, 4)
            y1 = polygon(i, 5)
            x2 = polygon(i, 1)
            y2 = polygon(i, 2)
         end if

         if ((y1 - tol <= yp) .and. (yp < y2)) then
            ! Calculate x-coordinate of intersection
            x_intersect = x1 + (x2 - x1) * (yp - y1) / (y2 - y1)
            
            if (xp < x_intersect) then
               n_crossings = n_crossings + 1
            end if
         end if
      end do
      
      ! Point is inside if odd
      is_inside = mod(n_crossings, 2) == 1
      
   end subroutine point_in_polygon
end module supplementary

program gridGenerator
   use supplementary
   implicit none
   real(8), parameter :: tol = 1.0d-3
   integer :: nx, ny, nz
   real(8) :: Xmax, Ymax, Zmax
   real(8) :: dx, dy
   real(8), allocatable :: x(:), y(:), z(:)
   real(8), allocatable :: P(:,:), zsliceplanes(:), Pts(:,:)
   integer :: i, j, k, ii
   integer :: num_rows, ios
   integer :: contour_idx, n_inside_points
   real(8) :: current_z
   character(len=2048) :: arg, line, filename
   character(len=1024) :: contour_folder, output_folder
   character(len=256) :: z_str, contour_str
   logical :: file_exists, is_inside
   integer :: n_inside_contour
   print *, "----------------------------------------------"
   ! Read dx and dy from command line
   if (command_argument_count() >= 2) then
       call get_command_argument(1, arg); read(arg, *) dx
       call get_command_argument(2, arg); read(arg, *) dy
   else
       dx = 1.000d0; dy = 1.000d0  ! defaults
   end if
   ! ----------------------------------------------------------------------------------
   ! Read the Z-slice planes
   ! ----------------------------------------------------------------------------------
   open(unit=10, file='slicePlanes/sliceplanes.txt', status='old')
   
   num_rows = 0
   do
      read(10, '(A)', iostat=ios) line
      if (ios /= 0) exit
      num_rows = num_rows + 1
   end do
   
   allocate(zsliceplanes(num_rows))
   rewind(10)
   
   do i = 1, num_rows
      read(10, *) zsliceplanes(i)
   end do
   close(10)
   
   print *, "  Number of z-planes: ", num_rows
   
   ! ----------------------------------------------------------------------------------
   ! Read the Points Data
   ! ----------------------------------------------------------------------------------
   open(unit=30, file='stlProc/Points.txt', status='old')
   
   num_rows = 0
   do
      read(30, '(A)', iostat=ios) line
      if (ios /= 0) exit
      num_rows = num_rows + 1
   end do
   
   allocate(P(num_rows, 3))
   rewind(30)
   
   do i = 1, num_rows
      read(30, *) P(i, :)
   end do
   close(30)
   P = P + 1.0d0
   ! Determine domain size with buffer
   Xmax = nint(maxval(P(:, 1))) + 2.0d0
   Ymax = nint(maxval(P(:, 2))) + 2.0d0
   Zmax = nint(maxval(P(:, 3))) + 2.0d0
   
   print *, "  Domain bounds:"
   print *, "    Xmax = ", Xmax
   print *, "    Ymax = ", Ymax
   print *, "    Zmax = ", Zmax
   
   deallocate(P)
   
   ! ----------------------------------------------------------------------------------
   ! STEP 3: Generate mesh grid
   ! ----------------------------------------------------------------------------------
   ! Number of grid points
   nx = int(Xmax / dx) + 1
   ny = int(Ymax / dy) + 1
   nz = size(zsliceplanes)
   print *, "  Grid dimensions: ", nx, " x ", ny, " x ", nz
   allocate(x(nx), y(ny), z(nz))
   
   ! Generate x and y uniformly
   do i = 1, nx
      x(i) = (i - 1) * dx
   end do
   do j = 1, ny
      y(j) = (j - 1) * dy
   end do
   z = zsliceplanes
   print *, "  Grid spacing: dx=", dx, " dy=", dy
   
   ! ----------------------------------------------------------------------------------
   ! STEP 4: Process each z-plane and identify interior points
   ! ----------------------------------------------------------------------------------
   contour_folder = 'sorted/'
   output_folder = '2D_All_Points/'
   
   n_inside_points = 0
   
   ! Loop over each z-plane
   do ii = 1, size(zsliceplanes)
      current_z = zsliceplanes(ii)
      
      print *, ""
      print *, "  Processing z-plane: ", current_z
      
      ! Process all contours at this z-plane
      contour_idx = 1
      
      do while (.true.)
         ! Build filename for current contour
         write(z_str, '(F0.3)') current_z
         write(contour_str, '(I0)') contour_idx
         
         filename = trim(contour_folder) // 'ZSlicePlane_' // &
                    trim(adjustl(z_str)) // '_Contour_' // &
                    trim(adjustl(contour_str)) // '.txt'
         
         ! Check if contour file exists
         call check_file_exists(filename, file_exists)
         
         if (.not. file_exists) exit
         
         !-------------------------------------------------------------------
         ! Read contour data
         !-------------------------------------------------------------------
         open(unit=110, file=trim(filename), status='old')
         
         num_rows = 0
         do
            read(110, '(A)', iostat=ios) line
            if (ios /= 0) exit
            if (len_trim(line) > 0 .and. line(1:1) == '#') cycle
            if (len_trim(line) == 0) cycle
            num_rows = num_rows + 1
         end do
         
         if (num_rows == 0) then
            close(110)
            contour_idx = contour_idx + 1
            cycle
         end if
         
         allocate(Pts(num_rows, 6))
         rewind(110)
         
         j = 0
         do
            read(110, '(A)', iostat=ios) line
            if (ios /= 0) exit
            if (len_trim(line) > 0 .and. line(1:1) == '#') cycle
            if (len_trim(line) == 0) cycle
            
            j = j + 1
            read(line, *) Pts(j, :)
         end do
         close(110)
         
         !-------------------------------------------------------------------
         ! Find corresponding z-level in mesh
         !-------------------------------------------------------------------
         k = -1
         do i = 1, nz
            if (abs(z(i) - current_z) <= tol) then
               k = i
               exit
            end if
         end do
         
         if (k > 0) then
            n_inside_contour = 0
            do j = 1, ny
               do i = 1, nx
                  
                  call point_in_polygon(x(i), y(j), Pts, is_inside)
                  
                  if (is_inside) then
                     n_inside_points = n_inside_points + 1
                     n_inside_contour = n_inside_contour + 1
                     ! Write to individual contour file
                     filename = trim(output_folder) // &
                                'inside_points_ZSlicePlane_' // &
                                trim(adjustl(z_str)) // '_Contour_' // &
                                trim(adjustl(contour_str)) // '.txt'
                     
                     open(unit=150, file=filename, status='unknown', &
                          position='append')
                     write(150, '(3F16.6)') x(i), y(j), z(k)
                     close(150)
                  end if
                  
               end do
            end do
            
            print *, "    Contour ", contour_idx, ": ", n_inside_contour, " points"
            
         end if
         
         deallocate(Pts)
         contour_idx = contour_idx + 1
         
      end do
      
   end do   
   deallocate(x, y, z, zsliceplanes)
   print *, "----------------------------------------------"
   
end program gridGenerator