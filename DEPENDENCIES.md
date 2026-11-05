# Cartino2D – Architecture & Software Dependencies

**Cartino2D** is an automated framework developed by **Cerema** to design, execute, and post-process 2D shallow-water hydrodynamic simulations (fluvial flooding, direct runoff, and marine submersion).

In this architecture, **R acts as the central driver and orchestrator (the "pilot")**. It does not solve the heavy fluid dynamics equations directly; instead, it coordinates a specialized ecosystem of open-source numerical solvers, mesh generators, GIS engines, and high-performance computing (HPC) utilities.

---

## 📊 Summary of Cartino2D Dependencies

| Component / Software | Core Role | Language / Environment | Cartino2D Step | Requirement |
|---|---|---|---|---|
| **R & RStudio** | **Master Driver & Orchestrator** | R (4.4+) | **All Steps** (00 to 09, PreC2D) | **Mandatory** |
| **TELEMAC-2D** | 2D Hydrodynamic numerical solver | Fortran / Python / MPI | Step 05 (Local & Remote runs) | **Mandatory** |
| **GMSH** | 2D Unstructured finite-element mesher | C++ (Windows 64-bit binary) | Step 02 (Mesh generation & Selafin) | **Mandatory** |
| **PPutils** | Selafin `.slf` creation & result raster extraction | Python | Steps 01, 02 (Selafin generation) & Step 07 (Output rasters) | **Mandatory** |
| **Python** | Runtime engine for PPutils scripts | Python 3 (`numpy`, `scipy`, `matplotlib`) | Steps 01, 02, 07 | **Mandatory** |
| **GRASS GIS** | High-performance raster engine & hole-filling | GIS CLI (`grass84.bat`) | Step 01 (DEM) & Step 08 (Post-GRASS) | **Mandatory** |
| **QGIS & OSGeo4W** | Headless GIS processing, GDAL utilities, `.qml` styles & `.gpkg` | GIS engine (`qgis_process`, OSGeo4W) | PreC2D, Step 00, Step 01, Step 02, Steps 07/08/09 | **Mandatory** |
| **TauDEM** | Hydrological terrain analysis (DEM conditioning) | Compiled C++ executables (`.exe`) / MS-MPI | PreC2D (PitRemove, D8Flowdir, AreaD8) | **Mandatory** |
| **PSCP (PuTTY)** | SSH/SCP encrypted bridge to HPC clusters | Windows CLI executable | Step 05 (Remote cluster computing) | Optional (HPC only) |
| **Fortran Routines (`.f`)** | Custom user laws (rain, hydrograph interpolation) | Fortran 90 | Steps 04 / 05 (Compiled by Telemac) | Built-in C2D |
| **Rtools** | C/C++/Fortran Windows compiler toolchain | Windows build tools | R geospatial package compilation | **Mandatory** |

---



## 🔍 Detailed Component Descriptions

### 1. R & RStudio — The Master Orchestrator (The Driver)
**R** is the backbone and brain of Cartino2D. Instead of executing heavy computational fluid dynamics directly in R, it acts as an automation framework:
- Reads user-defined spatial extent, grid resolutions, and boundary condition files.
- Automates domain sectorization and generates all intermediate configuration files (`.cas`, `.cli`, `.geo`, `.bat`) required by downstream software.
- Spawns background system processes and coordinates multicore execution using R packages (`parallel`, `future`, `sf`, `terra`).
- Manages error logging, workspace initialization, and folder structures.

### 2. TELEMAC-2D & MS-MPI — The 2D Hydrodynamic Numerical Solver
Developed by the Open TELEMAC-MASCARET international consortium, **TELEMAC-2D** solves the two-dimensional shallow-water Saint-Venant equations on unstructured triangular meshes.
- Solves water depths ($h$), depth-averaged horizontal velocity fields ($u, v$), bottom friction (Strickler laws), and dynamic infiltration (SCS-CN model).
- **Natively computes Control Section (SC) balances**: During the simulation, TELEMAC-2D internally integrates water fluxes across user-defined control sections at each time step, outputting instantaneous discharges and cumulative volumes directly into the simulation listings (`sections_output.txt` / output listings). R directly parses these text logs in Step 06 to plot interactive hydrographs.
- Simulates river flooding, pluvial surface runoff, and coastal flooding.
- Leverages **Microsoft MPI (MS-MPI)** under Windows to split meshes across multiple CPU cores for high-speed parallel computation.

### 3. GMSH — 2D Unstructured Finite-Element Mesher
**GMSH** is an open-source 3D/2D finite-element mesh generator with a built-in CAD engine.
- Cartino2D feeds boundary contours, elevation breaklines (embankments, roads, ditches, valley bottoms), and variable target mesh sizes into GMSH.
- GMSH generates a high-resolution, topography-adaptive Delaunay triangulation (`.msh`).
- R immediately translates this mesh into the standard TELEMAC binary **Selafin (`.slf`)** format.

### 4. PPutils & Python — Mesh Geometry & Selafin Assembly / Extraction
**PPutils** (*Post-Processing utilities*) is an open-source Python toolchain adapted and enriched by Cerema for hydraulic engineering. It intervenes at multiple key steps of the Cartino2D modeling chain:
- **Pre-simulation (Steps 01 & 02 - Selafin Creation)**: Intervenes after GMSH meshing. Using scripts such as `append_adcirc_cerema.py`, it fuses the triangular mesh grids (`.grd`), interpolated bathymetry/elevation, spatially distributed roughness (Strickler friction), and soil infiltration capacity (Curve Number) into the official binary **Selafin geometry file (`.slf`)** and boundary conditions file (`.cli`) required by TELEMAC-2D.
- **Post-simulation (Step 07 - Mesh-to-Raster Conversion)**: Decodes massive binary TELEMAC 2D mesh simulation results (`.slf` / `.res`) and converts peak hydrodynamic envelopes into standard ESRI ASCII raster grids (`.asc` via `slf2asc.py`) for water depths (`HWH`), velocities (`HV`), unit discharges (`HSF`), and bathymetry (`Bottom_m`).
*(Note: PPutils does not calculate control section discharges; those are computed natively by TELEMAC-2D).*

### 5. GRASS GIS — High-Performance Raster Engine & Culvert Repair
**GRASS GIS** is invoked in headless command-line mode (`grass.bat`). It handles heavy raster operations without the memory overhead of desktop GIS applications:
- **Spatial masking & cleaning**: Sets residual water depths to null (`r.null`) below physical thresholds (e.g., $< 0.005\text{ m}$).
- **Hydraulic culvert anti-hole restoration (Anti-trous)**: During meshing, hydraulic structures (bridges, culverts) are often carved into the 1D/2D mesh. In Step 08, GRASS uses map algebra (`r.mapcalc`) to restore the natural ground elevation (`MNT_BRUT_NAT`) around structures, preventing artificial sinkholes from biasing final inundation maps.

### 6. QGIS & OSGeo4W (`qgis_process`) — Headless Vector/Raster GIS & Cartography
Cartino2D harnesses **QGIS** (in headless batch mode via `qgis_process`) and the **OSGeo4W** GDAL toolchain across multiple phases of the pipeline, far beyond just post-processing:
- **PreC2D & Step 00 (Data Preparation & Sectorization)**: Executes geometric vector operations (polygon buffering, differences, layer merges, deleting holes via `native:deleteholes`, and selecting main connected components) as well as raster clipping and format translations.
- **Step 01 & Step 02 (Topography & Mesh Verification)**: Builds virtual elevation mosaics (`gdalbuildvrt`), generates overviews (`gdaladdo`), and inspects 2D mesh topology via `native:exportmeshfaces` before simulation launch.
- **Steps 07, 08 & 09 (Post-Processing & Delivery)**: Performs mesh rasterization (`native:meshrasterize`), reprojections (`gdal:warpreproject`), crops rasters to validated zones, automatically injects official Cerema symbology templates (`.qml`), and packages multi-scenario outputs into compressed **OGC GeoPackage (`.gpkg`)** databases with built-in pyramids.

### 7. TauDEM — Hydrological Terrain Conditioning (PreC2D)
**TauDEM** (*Terrain Analysis Using Digital Elevation Models*) is a **mandatory** suite of compiled standalone C++ command-line executables (`.exe`) developed by Utah State University. It runs in parallel via **Microsoft MPI (`mpiexec`)** and does **not** rely on Python:
- Removes artificial terrain depressions and sinks from raw LiDAR/DEM data (`PitRemove.exe`).
- Computes flow directions (`D8Flowdir.exe` and `DinfFlowDir.exe`) and contributing drainage areas (`AreaD8.exe`).
- Delineates sub-catchments and stream networks to guide the placement of rainfall and liquid boundary sources.

### 8. PSCP (PuTTY) — Encrypted Supercomputing Bridge (HPC)
For massive regional or national simulations that exceed the capacity of local desktop workstations, **`pscp.exe`** provides an automated, secure SCP transfer pipeline:
- Uploads geometry meshes (`.slf`), steering files (`.cas`), and boundary condition files (`.cli`) to remote high-performance supercomputers (e.g., IFREMER Datarmor, CEA TGCC Irene, DTecRem Margny).
- Synchronizes and downloads result files (`.res`, `.slf`) back to the local workstation once Slurm batch jobs complete.

### 9. Dynamic Fortran Routines (`.f`) — Advanced Hydraulic Laws
Located in Cartino2D subfolders, custom Fortran 90 routines are compiled on the fly by TELEMAC at run launch:
- `discharge_interpol.f`: Performs dynamic temporal linear interpolation of discharge hydrographs injected at inflow boundaries and liquid sources.
- Spatially and temporally distributes radar rainfall matrices and soil infiltration laws across mesh nodes.

### 10. Rtools — Windows Native Build Toolchain
**Rtools** provides the GCC, Gfortran, and C++ compilers required on Windows:
- Allows R to compile native C/C++ dependencies behind core geospatial packages (`sf`, `terra`, `units`, `gdalraster`).
- Ensures binary compatibility and high-performance execution of geospatial algebra in R.
