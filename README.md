# Stencil-Generator-for-CAD-STL-files

A Fortran-based pipeline tool for generating 3D stencil grid points from STL (3D CAD model) files. This project processes STL files and gives the datasets for both 2D stencils and 3D stencils with proper classifications for Interior and Boundary points. This is beneficial for numerical implementation of simple PDEs as well as for advanced multi-physics problems for erosion modelling, especially for biodegradable polymeric materials.

## Features

- Convert STL files to 2D stencil patterns
- Adjustable spatial solution parameters (dx, dy, dz)
- Export to various text-based formats for easy human-readable and structured flow
- High-performance Fortran implementation

### Using Make

```bash
# Clone the repository
git clone https://github.com/nithin5589/Stencil-Generator-for-CAD-STL-files.git
cd Stencil-Generator-for-CAD-STL-files

# Compile
make

# Or compile with a specific compiler
make FC=gfortran main.f90 -o main
./main

## Using the pipeline package
1. main.f90 - runs all the Fortran scripts, cleans the previous folders
2. stlRead.f90 - reads the STL file to extract the Points & Connectivity List to the stlProc folder
