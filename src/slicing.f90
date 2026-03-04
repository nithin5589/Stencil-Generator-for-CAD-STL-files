program slicing
    implicit none
    real(8) :: dz, norm(3), Zmin, Zmax, zinitial
    real(8) :: tol
    real(8), allocatable :: P(:,:), zsliceplanes(:)
    integer :: num_rows, num_columns
    integer :: ios, i
    character(len=1000) :: arg, line, stlpoints, zplanefolder
    tol = 1e-8
    print *, "----------------------------------------------"
    ! ----------------------------------------------------------------------------------
    ! Read the Space Discretization parameter dz
    ! ----------------------------------------------------------------------------------
    ! Read dz
    call get_command_argument(1, arg)
    read(arg, *) dz
    ! ----------------------------------------------------------------------------------
    ! Read the Points Data
    ! ----------------------------------------------------------------------------------
    num_columns = 3
    num_rows = 0
    stlpoints = 'src/stlProc/'
    open(unit=20, file=trim(stlpoints)//'Points.txt', status='old')
    do 
        read(20, '(A)', iostat=ios) line
        if (ios /= 0) exit
        num_rows = num_rows + 1
    end do
    rewind(20)
    allocate(P(num_rows,3))
    do i = 1, num_rows
        read(20, *) P(i, :)
    end do
    do i = 1, num_rows
        P(i, :) = P(i, :) + 1
    end do
    rewind(20)
    close(20)
    ! ----------------------------------------------------------------------------------
    ! Find the Zmax and Zmin for the slicing planes
    Zmin = minval(P(:,3))
    Zmax = maxval(P(:,3))

    norm = [0.0, 0.0, 1.0] ! Normal of z-axis
    zinitial = 1.00
    if (zinitial > Zmin) then
        print *, "Terminated with the issue of Zmin and Initial Slicing Plane"
        stop
    end if

    allocate(zsliceplanes(0))
    zsliceplanes = 0.00
    do while (zinitial <= Zmax + tol)
        zsliceplanes = [zsliceplanes, zinitial]
        zinitial = zinitial + dz
    end do

    print *, "Z - sliceplanes:"
    do i = 1, size(zsliceplanes)
        print *, zsliceplanes(i)
    end do

    ! Write the zsliceplanes to a .txt file
    zplanefolder = 'src/slicePlanes/'
    open(unit=30, file=trim(zplanefolder)//'sliceplanes.txt', status='replace')
    do i = 1, size(zsliceplanes)
        write(30, '(1(F20.8))') zsliceplanes(i)
    end do
    close(30)

    ! Deallocate
    deallocate(zsliceplanes)
    print *, "Z - sliceplanes created successfully"
    print *, "----------------------------------------------"
end program slicing
