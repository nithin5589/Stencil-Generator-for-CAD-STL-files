module stl_funcs
    implicit none
    contains
    logical function duplicate(Points, count, x, y, z)
        real(8), intent(in) :: Points(:,:)
        integer, intent(in)::count
        real(8), intent(in)::x, y, z
        integer:: i

        do i =1, count-1
            if (Points(i,1)==x .and. Points(i,2)==y .and. Points(i,3)==z) then
                duplicate = .true.
                return
            end if
        end do
        duplicate = .false.
    end function duplicate
end module stl_funcs
! --------------------------------------------------------------------------------
! Main program to read STL file and extract Points, Connectivity List
! --------------------------------------------------------------------------------
program stlRead
    use stl_funcs
    implicit none
    real(8), allocatable :: Points(:,:), ConnectivityList(:,:)
    integer :: ios, num_facets
    integer :: i, j
    character(len=80) :: line
    real(8) :: x, y, z
    integer :: count, ii, jj
    character(len=1000):: arg, stlfolder, stlfile, filename, stlpoints
    count = 0
    print *, "----------------------------------------------"

    !-----------------------------------------------------------------
    !Open STL File
    !-----------------------------------------------------------------
    stlfolder = 'src/STL_Files/'
    ! Read stlfile file
    if (command_argument_count() >= 1) then
        call get_command_argument(1, arg)
        stlfile = trim(arg)
    else
        stlfile = 'Rectangle.STL'  ! Default Case
    end if
    filename = trim(stlfolder) // trim(stlfile)
    open(unit=10, file=filename, status='old', action='read')

    ! Extract # of vertices
    do
        read(10, '(A)', iostat=ios) line
        if (ios /= 0) exit
        if (index(line, 'vertex') /= 0) then
            count = count + 1
        end if
    end do
    rewind(10)

    ! Remove duplicates if any and save the Points list
    allocate(Points(count,3))
    count = 1
    do
        read(10, '(A)', iostat=ios) line
        if (ios /= 0) exit
        if (index(line, 'vertex') /= 0) then
            line = trim(line(17:)) ! Adjust to .stl file format
            read(line, *) x, y, z
            if (.not. duplicate(Points, count, x, y, z)) then
                Points(count,1) = x
                Points(count,2) = y
                Points(count,3) = z
                count = count + 1
            end if
        end if
    end do
    print *, "Total number of Points = ",count-1
    rewind(10)

    ! Save Points.txt file
    stlpoints='stlProc/'
    open(unit=20, file=trim(stlpoints)//'Points.txt', status='replace')
    do i = 1, count - 1
        write(20, '(3(F20.8, F20.8, F20.8))') Points(i, 1), Points(i, 2), Points(i, 3)
    end do
    rewind(20)

    ! Extract # of Facets
    num_facets = 0
    do 
        read(10, '(A)', iostat=ios) line
        if (ios /= 0) exit
        if (index(line, 'outer loop') /= 0) then
            num_facets = num_facets + 1
        end if
    end do
    rewind(10)
    allocate(ConnectivityList(num_facets,3))
    ii = 1
    jj = 1
    do 
        read(10, '(A)', iostat=ios) line
        if (ios /= 0) exit
        if (index(line, 'outer loop') /= 0) then
            jj = 1
            do
                read(10, '(A)', iostat=ios) line
                if (ios /= 0) exit
                if (index(line, 'endloop') /= 0) exit
                if (index(line, 'vertex') /= 0) then
                    line = trim(line(17:)) ! Adjust to .stl file format
                    read(line, *) x, y, z
                    do j = 1, count - 1
                        if (x == Points(j, 1) .and. y == Points(j,2) .and. z ==  Points(j,3)) then
                            ConnectivityList(ii,jj) = j
                            jj = jj + 1
                        endif
                    end do
                end if
            end do
            ii = ii + 1
        end if
    end do
    rewind(10)

    ! Save ConnectivityList.txt file
    stlpoints='src/stlProc/'
    open(unit=30, file=trim(stlpoints)//'ConnectivityList.txt', status='replace')
    do i = 1, size(ConnectivityList,1)
        write(30, '(3(F20.8, F20.8, F20.8))') ConnectivityList(i, 1), ConnectivityList(i, 2), ConnectivityList(i, 3)
    end do
    rewind(20)

    ! Deallocate & Close files
    deallocate(Points)
    deallocate(ConnectivityList)
    close(10)
    close(20)
    close(30)

    print *, "Facets = ", num_facets
    print *, "Processed the STL file"
    print *, "----------------------------------------------"
end program stlRead
