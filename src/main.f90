program MainProgram
    implicit none
    character(len=100) :: command

    ! -----------------------------------------------------------------------------------------------
    ! Delete Folders containing the previous data points and Recreating them
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

    ! Delete the folder named 'lineIntersections'
    command = "rm -rf lineIntersections"
    call system(command)

    ! Create the folder named 'lineIntersections'
    command = "mkdir lineIntersections"
    call system(command)

    ! Delete the folder named 'sorted'
    command = "rm -rf sorted"
    call system(command)

    ! Create the folder named 'sorted'
    command = "mkdir sorted"
    call system(command)

    ! Delete the folder named '3D_Initial_Grid'
    command = "rm -rf 3D_Initial_Grid"
    call system(command)

    ! Create the folder named '3D_Initial_Grid'
    command = "mkdir 3D_Initial_Grid"
    call system(command)

    ! Delete the folder named '2D_All_Inside_Points'
    command = "rm -rf 2D_All_Inside_Points"
    call system(command)

    ! Create the folder named '2D_All_Inside_Points'
    command = "mkdir 2D_All_Inside_Points"
    call system(command)

    ! Delete the folder named '2D_All_Contour_Points'
    command = "rm -rf 2D_All_Contour_Points"
    call system(command)

    ! Create the folder named '2D_All_Contour_Points'
    command = "mkdir 2D_All_Contour_Points"
    call system(command)

    ! Delete the folder named '2D_All_Contour_Points_Final'
    command = "rm -rf 2D_All_Contour_Points_Final"
    call system(command)

    ! Create the folder named '2D_All_Contour_Points_Final'
    command = "mkdir 2D_All_Contour_Points_Final"
    call system(command)

    ! Delete the folder named '2D_Interior_Points_Data'
    command = "rm -rf 2D_Interior_Points_Data"
    call system(command)

    ! Create the folder named '2D_Interior_Points_Data'
    command = "mkdir 2D_Interior_Points_Data"
    call system(command)

    ! Delete the folder named '2D_Boundary_Points_Data'
    command = "rm -rf 2D_Boundary_Points_Data"
    call system(command)

    ! Create the folder named '2D_Boundary_Points_Data'
    command = "mkdir 2D_Boundary_Points_Data"
    call system(command)

    ! Delete the folder named '3D_All_Points'
    command = "rm -rf 3D_All_Points"
    call system(command)

    ! Create the folder named '3D_All_Points'
    command = "mkdir 3D_All_Points"
    call system(command)
    ! -----------------------------------------------------------------------------------------------
    
    ! -----------------------------------------------------------------------------------------------
    ! Commands to run the other Fortran file
    ! -----------------------------------------------------------------------------------------------
    command = "gfortran stlRead.f90 -o stlRead"
    call system(command)

    command = "./stlRead"
    call system(command)

    command = "gfortran slicing.f90 -o slicing"
    call system(command)

    command = "./slicing"
    call system(command)

    command = "gfortran parametricLineEquationCalculator.f90  -o  parametricLineEquationCalculator"
    call system(command)

    command = "./parametricLineEquationCalculator"
    call system(command)

    command = "gfortran sort2.f90 -o sort2"
    call system(command)

    command = "./sort2"
    call system(command)

    ! command = "gfortran fdm_mesh_generator.f90 -o fdm_mesh_generator"
    ! call system(command)

    ! command = "./fdm_mesh_generator"
    ! call system(command)

    ! command = "gfortran Final_Interior_and_Boundary_Points.f90 -o Final_Interior_and_Boundary_Points"
    ! call system(command)

    ! command = "./Final_Interior_and_Boundary_Points"
    ! call system(command)

    ! command = "gfortran Interior_and_Boundary_2D_V24.f90 -o Interior_and_Boundary_2D_V24"
    ! call system(command)

    ! command = "./Interior_and_Boundary_2D_V24"
    ! call system(command)

    ! command = "gfortran Grid_3D_Generator.f90 -o Grid_3D_Generator"
    ! call system(command)

    ! command = "./Grid_3D_Generator"
    ! call system(command)
    ! -----------------------------------------------------------------------------------------------
    
end program MainProgram
