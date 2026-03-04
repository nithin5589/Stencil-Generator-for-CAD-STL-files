program MainProgram
    implicit none
    character(len=100) :: command

    ! -----------------------------------------------------------------------------------------------
    ! Delete Folders containing the previous data points and Recreate them
    ! -----------------------------------------------------------------------------------------------
    ! Delete the folder named 'src/stlProc'
    command = "rm -rf src/stlProc"
    call system(command)
    ! Create the folder named 'src/stlProc'
    command = "mkdir src/stlProc"
    call system(command)

    ! Delete the folder named 'src/slicePlanes'
    command = "rm -rf src/slicePlanes"
    call system(command)
    ! Create the folder named 'src/slicePlanes'
    command = "mkdir src/slicePlanes"
    call system(command)

    ! Delete the folder named 'src/sorted'
    command = "rm -rf src/sorted"
    call system(command)
    ! Create the folder named 'src/sorted'
    command = "mkdir src/sorted"
    call system(command)

    ! Delete the folder named 'src/lineIntersections'
    command = "rm -rf src/lineIntersections"
    call system(command)
    ! Create the folder named 'src/lineIntersections'
    command = "mkdir src/lineIntersections"
    call system(command)

    ! Delete the folder named 'src/2D_All_Points'
    command = "rm -rf src/2D_All_Points"
    call system(command)
    ! Create the folder named 'src/2D_All_Points'
    command = "mkdir src/2D_All_Points"
    call system(command)

    ! Delete the folder named 'src/2D_All_Contour_Points'
    command = "rm -rf src/2D_All_Contour_Points"
    call system(command)
    ! Create the folder named 'src/2D_All_Contour_Points'
    command = "mkdir src/2D_All_Contour_Points"
    call system(command)

    ! Delete the folder named 'src/2D_IP_BP_Data'
    command = "rm -rf src/2D_IP_BP_Data"
    call system(command)
    ! Create the folder named 'src/2D_IP_BP_Data'
    command = "mkdir src/2D_IP_BP_Data"
    call system(command)

    ! Delete the folder named 'src/3D_All_Points'
    command = "rm -rf src/3D_All_Points"
    call system(command)
    ! Create the folder named 'src/3D_All_Points'
    command = "mkdir src/3D_All_Points"
    call system(command)

    ! -----------------------------------------------------------------------------------------------
    
    ! -----------------------------------------------------------------------------------------------
    ! Commands to run the codes in-order
    ! -----------------------------------------------------------------------------------------------
    command = "gfortran src/stlRead.f90 -o src/stlRead"
    call system(command)

    command = "./src/stlRead Rectangle.STL"
    call system(command)

    command = "gfortran src/slicing.f90 -o src/slicing"
    call system(command)

    command = "./src/slicing 1.000" ! Declare dz
    call system(command)

    command = "gfortran src/parametricLineEquationCalculator.f90  -o  src/parametricLineEquationCalculator"
    call system(command)

    command = "./src/parametricLineEquationCalculator"
    call system(command)

    command = "gfortran src/sorting.f90 -o src/sorting"
    call system(command)

    command = "./src/sorting"
    call system(command)

    command = "gfortran src/gridGenerator.f90 -o src/gridGenerator"
    call system(command)

    command = "./src/gridGenerator 1.000 1.000" ! Declare dx, dy
    call system(command)

    command = "gfortran src/multiContourCheck2D.f90 -o src/multiContourCheck2D"
    call system(command)

    command = "./src/multiContourCheck2D"
    call system(command)

    command = "gfortran src/IPandBP2D.f90 -o src/IPandBP2D"
    call system(command)

    command = "./src/IPandBP2D 1.000 1.000 1.000" ! Declare dx, dy, dz
    call system(command)

    command = "gfortran src/stencilGenerator3D.f90 -o src/stencilGenerator3D"
    call system(command)

    command = "./src/stencilGenerator3D 1.000 1.000 1.000" ! Declare dx, dy, dz
    call system(command) 
    ! -----------------------------------------------------------------------------------------------
    
end program MainProgram
