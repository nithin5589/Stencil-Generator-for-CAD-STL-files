module sort_module
   implicit none
   contains
   subroutine readSideFile(filename, sideData)
      character(len=1024), intent(in) :: filename
      real(8), allocatable, intent(out) :: sideData(:,:)
      integer :: ios, num_rows, i
      character(len=1024) :: line
      open(unit=10, file=filename, status='old')
      num_rows = 0
      do 
         read(10, '(A)', iostat=ios) line
         if (ios /= 0) exit
         num_rows = num_rows + 1
      end do
      allocate(sideData(num_rows, 3))
      rewind(10)
      do i = 1, num_rows
         read(10, *, iostat=ios) sideData(i, :)
      end do
      close(10)
   end subroutine readSideFile

   subroutine compute_area(contour, n_lines, area)
      real(8), intent(in) :: contour(:,:)
      integer, intent(in) :: n_lines
      real(8), intent(out) :: area
      integer :: i
      real(8) :: sum_area
      area = 0.000
      if (n_lines < 3) return
      sum_area = 0.000
      ! Sum of cross products
      do i = 1, n_lines
         ! Each line contributes: x1*y2 - x2*y1
         sum_area = sum_area + (contour(i,1) * contour(i,5) - &
                                contour(i,4) * contour(i,2))
      end do
      area = abs(sum_area) / 2.0d0
   end subroutine compute_area

end module sort_module
program sorting
   use sort_module
   implicit none
   real(8), allocatable :: PE1(:,:), PE2(:,:), PE3(:,:)
   real(8), allocatable :: Pair(:,:), UnqPair(:,:), RemPair(:,:), DumRemPair(:,:)
   real(8), allocatable :: Srt(:,:), CurrPlane(:,:), B(:,:)
   integer :: i, j, l, nCtr, q, count
   real(8) :: tol
   real(8) :: ymin, currZ
   real(8) :: d, area
   integer :: k
   character(len=1024) :: charP, charI
   character(len=1024) :: fname, folder, line_intersect_folder
   integer :: fid
   logical :: has_PE1, has_PE2, has_PE3, PE1eqPE2, PE2eqPE3, PE3eqPE1, foundNext
   tol = 1e-8
   folder = 'sorted/'
   print *, "----------------------------------------------"

   ! ----------------------------------------------------------------------------------
   ! Read the Side1, Side2, Side3
   ! ----------------------------------------------------------------------------------
   line_intersect_folder = 'lineIntersections/'
   call readSideFile(trim(line_intersect_folder)//'Side1.txt', PE1)
   call readSideFile(trim(line_intersect_folder)//'Side2.txt', PE2)
   call readSideFile(trim(line_intersect_folder)//'Side3.txt', PE3)
   ! ----------------------------------------------------------------------------------

   l = size(PE1,1)
   allocate(Pair(l,6))
   Pair = 0.0
   count = 0
   do i = 1, l

      has_PE1 = any(abs(PE1(i,:)) > tol)
      has_PE2 = any(abs(PE2(i,:)) > tol)
      has_PE3 = any(abs(PE3(i,:)) > tol)
      PE1eqPE2 = has_PE1 .and. has_PE2 .and. all(abs(PE1(i,:) - PE2(i,:)) < tol)
      PE2eqPE3 = has_PE2 .and. has_PE3 .and. all(abs(PE2(i,:) - PE3(i,:)) < tol)
      PE3eqPE1 = has_PE3 .and. has_PE1 .and. all(abs(PE3(i,:) - PE1(i,:)) < tol)
      if (has_PE1 .and. has_PE2 .and. .not. PE1eqPE2) then
         if (has_PE3 .and. .not. PE3eqPE1 .and. .not. PE2eqPE3) then
            if (PE1eqPE2) then
               count = count + 1
               Pair(count, 1:3) = PE1(i,:)
               Pair(count, 4:6) = PE3(i,:)
            else if (PE2eqPE3) then
               count = count + 1
               Pair(count, 1:3) = PE1(i,:)
               Pair(count, 4:6) = PE2(i,:)
            else if (PE3eqPE1) then
               count = count + 1
               Pair(count, 1:3) = PE1(i,:)
               Pair(count, 4:6) = PE2(i,:)
            else
               count = count + 1
               Pair(count, 1:3) = PE1(i,:)
               Pair(count, 4:6) = PE2(i,:)
            end if
         else
            count = count + 1
            Pair(count, 1:3) = PE1(i,:)
            Pair(count, 4:6) = PE2(i,:)
         end if
            
      else if (has_PE2 .and. has_PE3 .and. .not. PE2eqPE3) then
            count = count + 1
            Pair(count, 1:3) = PE2(i,:)
            Pair(count, 4:6) = PE3(i,:)
            
      else if (has_PE3 .and. has_PE1 .and. .not. PE3eqPE1) then
            count = count + 1
            Pair(count, 1:3) = PE3(i,:)
            Pair(count, 4:6) = PE1(i,:)
            
      else if (PE1eqPE2 .and. has_PE3 .and. .not. PE3eqPE1) then
            count = count + 1
            Pair(count, 1:3) = PE1(i,:)
            Pair(count, 4:6) = PE3(i,:)
            
      else if (PE2eqPE3 .and. has_PE1 .and. .not. PE1eqPE2) then
            count = count + 1
            Pair(count, 1:3) = PE1(i,:)
            Pair(count, 4:6) = PE2(i,:)
            
      else if (PE3eqPE1 .and. has_PE2 .and. .not. PE2eqPE3) then
            count = count + 1
            Pair(count, 1:3) = PE1(i,:)
            Pair(count, 4:6) = PE2(i,:)
      end if
   end do

   allocate(UnqPair(count,6))
   UnqPair = Pair(1:count, :)
   ! Remove any Duplicates in UnqPair
   do i = 1, count - 1
      if (all(abs(UnqPair(i,:)) < tol)) cycle
      do j = i + 1, count
         if (all(abs(UnqPair(j,:)) < tol)) cycle
         ! Check forward: (P1-P2) == (P1-P2)
         if (all(abs(UnqPair(i,1:3) - UnqPair(j,1:3)) < tol) .and. all(abs(UnqPair(i,4:6) - UnqPair(j,4:6)) < tol)) then
            UnqPair(j, :) = 0.0
         end if
         ! Check reverse: (P1-P2) == (P2-P1)
         if (all(abs(UnqPair(i,1:3) - UnqPair(j,4:6)) < tol) .and. all(abs(UnqPair(i,4:6) - UnqPair(j,1:3)) < tol)) then
            UnqPair(j, :) = 0.0
         end if
      end do
   end do
   j = 0
   do i = 1, count
      if (all(UnqPair(i,:) > tol)) then
         j = j + 1
      endif
   end do
   allocate(RemPair(j,6))
   j = 1
   do i = 1, count
      if (all(UnqPair(i,:) > tol)) then
         RemPair(j,:) = UnqPair(i,:)
         j = j + 1
      endif
   end do
   deallocate(Pair, UnqPair)

   ! -------------------------------------------------------------------------
   ! Sorting the Points (a --> b --> c --> d --> a) order
   ! -------------------------------------------------------------------------
   do while (any(abs(RemPair) > tol))
      currZ = minval(RemPair(:,3), mask=(abs(RemPair(:,3)) > tol)) ! Current Z-plane
      nCtr = 0
      do i = 1, size(RemPair, 1)
         if (abs(RemPair(i,3) - currZ) < tol) then
            nCtr = nCtr + 1
         end if
      end do
      
      allocate(CurrPlane(nCtr, 6))
      j = 0
      do i = 1, size(RemPair, 1)
         if (abs(RemPair(i,3) - currZ) < tol) then
            j = j + 1
            CurrPlane(j, :) = RemPair(i, :)
            RemPair(i, :) = 0.0
         end if
      end do
      
      print *, "Processing Z-plane: ", currZ
      print *, "Line segments at this plane: ", nCtr
      ! Process the contour/contours
      k = 1
      do while (any(abs(CurrPlane) > tol))
         ! Count remaining lines
         nCtr = 0
         do i = 1, size(CurrPlane, 1)
            if (any(abs(CurrPlane(i,:)) > tol)) then
               nCtr = nCtr + 1
            end if
         end do
         if (nCtr == 0) exit
         allocate(Srt(nCtr, 6))
         Srt = 0.0
         ! Find starting line (minimum Y, then minimum X)
         ymin = huge(1.0)
         do i = 1, size(CurrPlane, 1)
            if (all(abs(CurrPlane(i,:)) < tol)) cycle
            if (CurrPlane(i, 2) < ymin) ymin = CurrPlane(i, 2)
         end do
            
         do q = 1, size(CurrPlane, 1)
            if (abs(CurrPlane(q, 2) - ymin) < tol) then
               if (all(abs(Srt(1,:)) < tol)) then
                  Srt(1, :) = CurrPlane(q, :)
               else if (CurrPlane(q, 1) < Srt(1, 1)) then
                  Srt(1, :) = CurrPlane(q, :)
               end if
            end if
         end do
         ! Remove starting line from CurrPlane
         do q = 1, size(CurrPlane, 1)
            if (all(abs(Srt(1,:) - CurrPlane(q,:)) < tol)) then
               CurrPlane(q, :) = 0.0d0
               exit
            end if
         end do
         ! Build ordered contour
         do i = 2, nCtr
            foundNext = .false.
            
            do j = 1, size(CurrPlane, 1)
               if (all(abs(CurrPlane(j,:)) < tol)) cycle
               
               ! Check if start of CurrPlane(j) matches end of Srt(i-1)
               if (all(abs(Srt(i-1, 4:6) - CurrPlane(j, 1:3)) < tol)) then
                  Srt(i, :) = CurrPlane(j, :)
                  CurrPlane(j, :) = 0.0d0
                  foundNext = .true.
                  exit
               end if
               
               ! Check if end of CurrPlane(j) matches end of Srt(i-1) (reverse)
               if (all(abs(Srt(i-1, 4:6) - CurrPlane(j, 4:6)) < tol)) then
                  Srt(i, 1:3) = CurrPlane(j, 4:6)
                  Srt(i, 4:6) = CurrPlane(j, 1:3)
                  CurrPlane(j, :) = 0.0d0
                  foundNext = .true.
                  exit
               end if
            end do
            
            if (.not. foundNext) exit
         end do
         
         ! Write contour to file
         d = Srt(1, 3)
         write(charP, "(F0.3)") d
         write(charI, "(I0)") k
         
         fname = trim(folder) // 'ZSlicePlane_' // trim(adjustl(charP)) // &
               '_Contour_' // trim(adjustl(charI)) // '.txt'
         
         fid = 100
         open(fid, file=fname, status='replace')
         write(fid, '(A,F12.6)') '# Z-plane: ', d
         write(fid, '(A,I0)') '# Contour: ', k
         
         l = 0
         do i = 1, size(Srt, 1)
            if (any(abs(Srt(i,:)) > tol)) then
               write(fid, '(6F20.8)') Srt(i, :)
               l = l + 1
            end if
         end do

         ! Calculate the 2D contour area
         call compute_area(Srt, l , area)
         write(fid, '(A,F20.8)') '# Area: ', area
         close(fid)

         print *, "  Contour ", k, " written (", l, " lines)"
         deallocate(Srt)
         
         ! Reduce CurrPlane by removing zeros
         nCtr = 0
         do i = 1, size(CurrPlane, 1)
            if (any(abs(CurrPlane(i,:)) > tol)) then
               nCtr = nCtr + 1
            end if
         end do
         if (nCtr > 0) then
            allocate(B(nCtr, 6))
            j = 0
            do i = 1, size(CurrPlane, 1)
               if (any(abs(CurrPlane(i,:)) > tol)) then
                  j = j + 1
                  B(j, :) = CurrPlane(i, :)
               end if
            end do
            
            deallocate(CurrPlane)
            allocate(CurrPlane(nCtr, 6))
            CurrPlane = B
            deallocate(B)
            
            k = k + 1
         end if
      end do
      
      deallocate(CurrPlane)
      
      ! Reduce RemPair by removing zeros
      l = 0
      do i = 1, size(RemPair, 1)
         if (any(abs(RemPair(i,:)) > tol)) then
            l = l + 1
         end if
      end do
      
      if (l > 0) then
         allocate(DumRemPair(l, 6))
         j = 0
         do i = 1, size(RemPair, 1)
            if (any(abs(RemPair(i,:)) > tol)) then
               j = j + 1
               DumRemPair(j, :) = RemPair(i, :)
            end if
         end do
         
         deallocate(RemPair)
         allocate(RemPair(l, 6))
         RemPair = DumRemPair
         deallocate(DumRemPair)
         
         print *, "Remaining lines: ", l
         print *, ""
      else
         deallocate(RemPair)
         allocate(RemPair(0, 6))
      end if
   end do
   ! -------------------------------------------------------------------------
   print *, "----------------------------------------------"

   deallocate(PE1, PE2, PE3)
   deallocate(RemPair)
end program sorting