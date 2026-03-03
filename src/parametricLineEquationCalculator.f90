program parametricLineEquationCalculator
    implicit none
    real(8), allocatable :: zsliceplanes(:), C(:,:), P(:,:)
    integer :: ios, num_rows, num_columns
    character(len=1000) :: line, stlpoints, zplanefolder, line_intersect_folder
    integer :: i, d
    real(8) :: dd
    integer :: V1, V2, V3
    real(8), dimension(:) :: v(3,3)
    real(8) :: Zmin, Zmax
    real(8) :: x1, y1, z1, x2, y2, z2
    real(8) :: X, Y, Z
    real(8) :: t
    real(8) :: XP, YP, ZP
    real(8) :: norm(3)
    print *, "----------------------------------------------"

    ! ----------------------------------------------------------------------------------
    ! Read the Points Data
    ! ----------------------------------------------------------------------------------
    num_columns = 3
    num_rows = 0
    stlpoints = 'stlProc/'
    open(unit=30, file=trim(stlpoints)//'Points.txt', status='old')
    do 
        read(30, '(A)', iostat=ios) line
        if (ios /= 0) exit
        num_rows = num_rows + 1
    end do
    rewind(30)
    allocate(P(num_rows, num_columns))
    do i = 1, num_rows
        read(30, *) P(i, :)
    end do
    P = P + 1.00
    close(30)
    ! ----------------------------------------------------------------------------------
    
    ! ----------------------------------------------------------------------------------
    ! Read the ConnectivityList
    ! ----------------------------------------------------------------------------------
    open(unit=20, file=trim(stlpoints)//'ConnectivityList.txt', status='old')
    num_columns = 3
    num_rows = 0
    do 
        read(20, '(A)', iostat=ios) line
        if (ios /= 0) exit
        num_rows = num_rows + 1
    end do
    rewind(20)
    allocate(C(num_rows, num_columns))
    do i = 1, num_rows
        read(20, *) C(i, :)
    end do
    close (20)
    ! ----------------------------------------------------------------------------------

    ! ----------------------------------------------------------------------------------
    ! Read the z-slicingplanes
    ! ----------------------------------------------------------------------------------
    zplanefolder = 'slicePlanes/'
    open(unit=10, file=trim(zplanefolder)//'sliceplanes.txt', status='old')
    num_rows = 0
    do 
        read(10, '(A)', iostat=ios) line
        if (ios /= 0) exit
        num_rows = num_rows + 1
    end do
    rewind(10)
    allocate(zsliceplanes(num_rows))
    do i = 1, num_rows
        read(10, *) zsliceplanes(i)
    end do
    ! ----------------------------------------------------------------------------------

    norm = [0.0, 0.0, 1.0] ! Normal to z-plane

    ! ----------------------------------------------------------------------------------
    ! Open Side1, Side2, Side3
    ! ----------------------------------------------------------------------------------
    line_intersect_folder = 'lineIntersections/'
    open(unit=40, file=trim(line_intersect_folder)//'Side1.txt', status='replace')
    open(unit=50, file=trim(line_intersect_folder)//'Side2.txt', status='replace')
    open(unit=60, file=trim(line_intersect_folder)//'Side3.txt', status='replace')

    do d = 1, size(zsliceplanes)
        dd = zsliceplanes(d)
        print *, "Intersecting facets with the Z-sliceplane =", dd
        do i = 1, size(C,1)
            ! Read (C(i, :))
            V1 = int(C(i, 1))
            V2 = int(C(i, 2))
            V3 = int(C(i, 3))
            v(1, :) = P(V1, :)
            v(2, :) = P(V2, :)
            v(3, :) = P(V3, :)

            ! Get Zmax and Zmin
            Zmax = maxval(v(:,3))
            Zmin = minval(v(:,3))

            if (Zmax /= Zmin) then
                if (Zmin <= dd .and. dd <= Zmax) then
                    !---------------------------------------
                    ! For Side1 (v1 --> v2)
                    !---------------------------------------
                    x1=v(1,1)
                    y1=v(1,2)
                    z1=v(1,3)
                    x2=v(2,1)
                    y2=v(2,2)
                    z2=v(2,3)
                    X = x1 + t * (x2 - x1)
                    Y = y1 + t * (y2 - y1)
                    Z = z1 + t * (z2 - z1)
                    t = (dd - norm(1)*x1 - norm(2)*y1 - norm(3)*z1)/(norm(1)*(x2 - x1) + norm(2)*(y2 - y1) + norm(3)*(z2 - z1))
                    if (0 <= t .and. t <= 1) then ! Intercept
                        XP = x1 + t * (x2 - x1)
                        YP = y1 + t * (y2 - y1)
                        ZP = z1 + t * (z2 - z1)
                        ZP = dd
                        write(40, *)  XP, YP, ZP
                    else
                        XP = 0
                        YP = 0
                        ZP = 0
                        write(40, *)  XP, YP, ZP
                    end if
                    !---------------------------------------
                    ! For Side2 (v2 --> v3)
                    !---------------------------------------
                    x1=v(2,1)
                    y1=v(2,2)
                    z1=v(2,3)
                    x2=v(3,1)
                    y2=v(3,2)
                    z2=v(3,3)
                    X = x1 + t * (x2 - x1)
                    Y = y1 + t * (y2 - y1)
                    Z = z1 + t * (z2 - z1)
                    t = (dd - norm(1)*x1 - norm(2)*y1 - norm(3)*z1)/(norm(1)*(x2 - x1) + norm(2)*(y2 - y1) + norm(3)*(z2 - z1))
                    if (0 <= t .and. t <= 1) then ! Intercept
                        XP = x1 + t * (x2 - x1)
                        YP = y1 + t * (y2 - y1)
                        ZP = z1 + t * (z2 - z1)
                        ZP = dd
                        write(50, *)  XP, YP, ZP
                    else
                        XP = 0
                        YP = 0
                        ZP = 0
                        write(50, *)  XP, YP, ZP
                    end if
                    !---------------------------------------
                    ! For Side3 (v3 --> v1)
                    !---------------------------------------
                    x1=v(3,1)
                    y1=v(3,2)
                    z1=v(3,3)
                    x2=v(1,1)
                    y2=v(1,2)
                    z2=v(1,3)
                    X = x1 + t * (x2 - x1)
                    Y = y1 + t * (y2 - y1)
                    Z = z1 + t * (z2 - z1)
                    t = (dd - norm(1)*x1 - norm(2)*y1 - norm(3)*z1)/(norm(1)*(x2 - x1) + norm(2)*(y2 - y1) + norm(3)*(z2 - z1))
                    if (0 <= t .and. t <= 1) then ! Intercept
                        XP = x1 + t * (x2 - x1)
                        YP = y1 + t * (y2 - y1)
                        ZP = z1 + t * (z2 - z1)
                        ZP = dd
                        write(60, *)  XP, YP, ZP
                    else
                        XP = 0
                        YP = 0
                        ZP = 0
                        write(60, *)  XP, YP, ZP
                    end if
                end if
            end if
        end do
    end do

    deallocate(zsliceplanes)
    deallocate(C)
    deallocate(P)

    close(10)
    close(40)
    print *, "----------------------------------------------"
end program parametricLineEquationCalculator