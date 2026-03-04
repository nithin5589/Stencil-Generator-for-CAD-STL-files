module attachment
   implicit none
contains
   subroutine file_exist(filename, ans)
      character(len=*), intent(in) :: filename
      logical, intent(out) :: ans
      inquire(file=filename, exist=ans)
   end subroutine file_exist
end module attachment

program multiContourCheck2D
   use attachment
   implicit none
   real(8), allocatable :: zsliceplanes(:)
   real(8), allocatable :: grid(:,:), Fgrid(:,:)
   character(len=2048) :: foldername, filename, fname, line
   character(len=256) :: charP, charI
   integer :: num_rows, ios
   integer :: i, j, k, kk, kctr
   real(8) :: d
   logical :: check
   print *, "----------------------------------------------"
   ! ----------------------------------------------------------------------------------
   ! STEP 1: Read z-slicing planes
   ! ----------------------------------------------------------------------------------
   open(unit=10, file='src/slicePlanes/sliceplanes.txt', status='old')
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
   ! STEP 2: All contour points at each Z-slice plane
   ! ----------------------------------------------------------------------------------
   foldername = 'src/2D_All_Points/'
   ! Process each z-plane
   do i = 1, size(zsliceplanes)
      d = zsliceplanes(i)
      write(charP, "(F0.3)") d
      print *, ""
      print *, "  Processing z-plane: ", d 
      ! Output for this Z-slice plane
      fname = 'src/2D_All_Contour_Points/ZSlicePlane_' // &
              trim(adjustl(charP)) // '.txt'
      open(unit=24, file=fname, status='replace')
      kctr = 1 ! All contours
      do while (.true.)
         write(charI, "(I0)") kctr
         
         filename = trim(foldername) // 'ZSlicePlane_' // &
                    trim(adjustl(charP)) // '_Contour_' // &
                    trim(adjustl(charI)) // '.txt'
         
         ! Check if contour file exists
         call file_exist(filename, check)
         
         if (.not. check) then
            print *, "    Processed ", kctr - 1, " contour(s)"
            exit
         end if
         
         ! Read contour points
         open(unit=10, file=filename, status='old')
         
         num_rows = 0
         do
            read(10, '(A)', iostat=ios) line
            if (ios /= 0) exit
            if (len_trim(line) == 0) cycle
            if (line(1:1) == '#') cycle
            num_rows = num_rows + 1
         end do
         
         if (num_rows > 0) then
            allocate(grid(num_rows, 3))
            rewind(10)
            
            j = 0
            do
               read(10, '(A)', iostat=ios) line
               if (ios /= 0) exit
               if (len_trim(line) == 0) cycle
               if (line(1:1) == '#') cycle
               
               j = j + 1
               read(line, *) grid(j, :)
            end do
            
            ! Write all points from this contour to consolidated file
            do j = 1, num_rows
               write(24, '(3F16.6)') grid(j, :)
            end do
            
            deallocate(grid)
         end if
         
         close(10)
         kctr = kctr + 1
         
      end do
      
      close(24)
      
   end do
   
   ! ----------------------------------------------------------------------------------
   ! STEP 3: Remove duplicate points from consolidated files
   ! ----------------------------------------------------------------------------------  
   do i = 1, size(zsliceplanes)
      d = zsliceplanes(i)
      write(charP, "(F0.3)") d
      
      fname = 'src/2D_All_Contour_Points/ZSlicePlane_' // &
              trim(adjustl(charP)) // '.txt'
      
      ! Check if consolidated file exists
      call file_exist(fname, check)
      
      if (.not. check) then
         print *, "  Skipping z-plane ", d, " (no data)"
         cycle
      end if
      
      print *, ""
      print *, "  Processing z-plane: ", d
      
      ! Read all points from consolidated file
      open(unit=10, file=fname, status='old', action='read')
      
      num_rows = 0
      do
         read(10, '(A)', iostat=ios) line
         if (ios /= 0) exit
         if (len_trim(line) == 0) cycle
         if (line(1:1) == '#') cycle
         num_rows = num_rows + 1
      end do
      
      if (num_rows == 0) then
         close(10)
         print *, "    No points found"
         cycle
      end if
      
      allocate(Fgrid(num_rows, 3))
      rewind(10)
      
      j = 0
      do
         read(10, '(A)', iostat=ios) line
         if (ios /= 0) exit
         if (len_trim(line) == 0) cycle
         if (line(1:1) == '#') cycle
         
         j = j + 1
         read(line, *) Fgrid(j, :)
      end do
      close(10)
      
      print *, "    Total points before duplicate removal: ", num_rows
      
      ! Mark duplicates as zero
      do k = 1, num_rows - 1
         if (all(abs(Fgrid(k, :)) < 1.0d-10)) cycle  ! Already marked
         
         do kk = k + 1, num_rows
            if (all(abs(Fgrid(kk, :) - Fgrid(k, :)) < 1.0d-10)) then
               Fgrid(kk, :) = 0.0d0
            end if
         end do
      end do
      
      ! Count unique points
      kk = 0
      do k = 1, num_rows
         if (any(abs(Fgrid(k, :)) > 1.0d-10)) then
            kk = kk + 1
         end if
      end do
      
      print *, "    Unique points after duplicate removal: ", kk
      
      ! Write unique points to final file
      filename = 'src/2D_All_Contour_Points/ZSlicePlane_' // &
                 trim(adjustl(charP)) // '.txt'
      
      open(unit=1924, file=filename, status='replace')
      write(1924, '(A,F12.6)') '# Z-plane: ', d
      write(1924, '(A,I0)') '# Number of unique points: ', kk
      write(1924, '(A)') '# x, y, z'
      
      do k = 1, num_rows
         if (any(abs(Fgrid(k, :)) > 1.0d-10)) then
            write(1924, '(3F16.6)') Fgrid(k, :)
         end if
      end do
      close(1924)
      
      deallocate(Fgrid)
      
   end do
   print *, "=============================================="
   print *, " All 2D contour points generated"
   print *, "=============================================="
   print *, "----------------------------------------------"
   
   deallocate(zsliceplanes)

end program multiContourCheck2D
