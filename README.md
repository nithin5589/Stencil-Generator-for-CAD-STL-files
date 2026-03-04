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

# Execution
make          # Compiles and creates 'main' executable
make clean    # Removes compiled files
make rebuild  # Clean and recompile
./main        # Runs the main program
```

## Work Flow OVerview of the Pipeline
1. **main.f90**  
   - Primary driver script that executes all FORTRAN scripts in a systematic manner.  
   - Declares input STL filename and spatial discretization parameters (`dx`, `dy`, `dz`).  

2. **stlRead.f90**  
   - Reads the STL file and extracts geometry information.  
   - Outputs: Points list and connectivity list, stored in the `stlProc` folder.

3. **slicing.f90**  
   - Generates Z-direction slice planes based on the `dz` parameter.  
   - Ensures coverage of the full height of the CAD geometry.  

4. **parametricLineEquationCalculator.f90**  
   - Computes intersections between STL facets and Z-slice planes.  
   - Handles degenerate facets and special cases robustly.  
   - Stores intersection points for each triangle edge in separate files.

5. **sorting.f90**  
   - Processes intersection points to create contours on each Z-slice plane.  
   - Handles all contour types: distinct, intersecting, void, or nested contours.  

6. **gridGenerator.f90**  
   - Generates a 2D Cartesian stencil grid for each contour.

7. **multiContourCheck2D.f90**  
   - Resolves duplicate points from multiple contours at the same slice plane.  
   - Ensures the uniqueness of stencil points for downstream calculations.  

8. **IPandBP2D.f90**  
   - Classifies 2D stencil points into **interior points (IP)** and **boundary points (BP)**.  
   - Useful for planar simulations or numerical studies.  

9. **stencilGenerator3D.f90**  
   - Generates the final 3D stencil points by stacking 2D slices.  
   - Classifies points as interior or boundary based on 3D criteria.  
   - Outputs ready-to-use datasets for simulations: `Total_3D_points`, `Interior_3D_points`, and `Boundary_3D_points`.
