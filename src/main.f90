program MainProgram
    implicit none
    character(len=100) :: command

    ! -----------------------------------------------------------------------------------------------
    ! Delete Folders containing the previous data points and Recreate them
    ! -----------------------------------------------------------------------------------------------
    ! Delete the folder named 'stlProc'
    command = "rm -rf stlProc"
    call system(command)
    ! Create the folder named 'stlProc'
    command = "mkdir stlProc"
    call system(command)

    ! Delete the folder named 'slicePlanes'
    command = "rm -rf slicePlanes"
    call system(command)
    ! Create the folder named 'slicePlanes'
    command = "mkdir slicePlanes"
    call system(command)

    ! Delete the folder named 'sorted'
    command = "rm -rf sorted"
    call system(command)
    ! Create the folder named 'sorted'
    command = "mkdir sorted"
    call system(command)

    ! Delete the folder named 'lineIntersections'
    command = "rm -rf lineIntersections"
    call system(command)
    ! Create the folder named 'lineIntersections'
    command = "mkdir lineIntersections"
    call system(command)

    ! Delete the folder named '2D_All_Points'
    command = "rm -rf 2D_All_Points"
    call system(command)
    ! Create the folder named '2D_All_Points'
    command = "mkdir 2D_All_Points"
    call system(command)

    ! Delete the folder named '2D_All_Contour_Points'
    command = "rm -rf 2D_All_Contour_Points"
    call system(command)
    ! Create the folder named '2D_All_Contour_Points'
    command = "mkdir 2D_All_Contour_Points"
    call system(command)

    ! Delete the folder named '2D_IP_BP_Data'
    command = "rm -rf 2D_IP_BP_Data"
    call system(command)
    ! Create the folder named '2D_IP_BP_Data'
    command = "mkdir 2D_IP_BP_Data"
    call system(command)

    ! Delete the folder named '3D_All_Points'
    command = "rm -rf 3D_All_Points"
    call system(command)
    ! Create the folder named '3D_All_Points'
    command = "mkdir 3D_All_Points"
    call system(command)

    ! -----------------------------------------------------------------------------------------------
    
    ! -----------------------------------------------------------------------------------------------
    ! Commands to run the codes in-order
    ! -----------------------------------------------------------------------------------------------
    command = "gfortran stlRead.f90 -o stlRead"
    call system(command)

    command = "./stlRead Rectangle.STL"
    call system(command)

    command = "gfortran slicing.f90 -o slicing"
    call system(command)

    command = "./slicing 1.000" ! Declare dz
    call system(command)

    command = "gfortran parametricLineEquationCalculator.f90  -o  parametricLineEquationCalculator"
    call system(command)

    command = "./parametricLineEquationCalculator"
    call system(command)

    command = "gfortran sorting.f90 -o sorting"
    call system(command)

    command = "./sorting"
    call system(command)

    command = "gfortran gridGenerator.f90 -o gridGenerator"
    call system(command)

    command = "./gridGenerator 1.000 1.000" ! Declare dx, dy
    call system(command)

    command = "gfortran multiContourCheck2D.f90 -o multiContourCheck2D"
    call system(command)

    command = "./multiContourCheck2D"
    call system(command)

    command = "gfortran IPandBP2D.f90 -o IPandBP2D"
    call system(command)

    command = "./IPandBP2D 1.000 1.000 1.000" ! Declare dx, dy, dz
    call system(command)

    command = "gfortran stencilGenerator3D.f90 -o stencilGenerator3D"
    call system(command)

    command = "./stencilGenerator3D 1.000 1.000 1.000" ! Declare dx, dy, dz
    call system(command) 
    ! -----------------------------------------------------------------------------------------------
    
end program MainProgram