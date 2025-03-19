# Relaxation Method for Solving the Poisson Equation

Zane Pennella [March 2025]

## Table of Contents
1. [Overview](#overview)
2. [Compilation and Execution](#compilation-and-execution)
3. [Input Parameters](#input-parameters)
4. [Output Files](#output-files)
5. [Pseudocode](#pseudocode)
6. [Implementation Details](#implementation-details)
7. [Convergence Criterion](#convergence-criterion)
8. [Limitations and Improvements](#limitations-and-improvements)

---

## Overview
This program solves the Poisson equation numerically using either the Jacobi or Gauss-Seidel relaxation method. It allows the user to specify grid dimensions, choose a solver method, and control parameters such as over-relaxation and the presence of a central charge. Additionally, the user can now define a charge distribution by specifying a value for `sigma`, which controls the spread of the charge. The program outputs the potential distribution at each iteration and generates visualizations of the results.

---

## Compilation and Execution
### Requirements
To compile and run this code, you will need:
- A Fortran compiler (e.g., `gfortran`).
- GNU Make (for compiling with the provided Makefile).
- Gnuplot (for generating plots and animations).
- FFmpeg (for creating a movie from the animation frames).

### Steps
1. Compile the code:
    - Type make into terminal once in proper directory.
    - This generates the executable named relax.
2. Run the program:
    - Type ./relax into terminal.
    - Follow prompts in terminal to provide input parameters.
3. Clean up:
    - Type make clean to remove all nonessential files

---

## Input Parameters
You will get prompted for the following inputs:
1. Grid Dimensions:
    - Number of columns (must be positive integer).
    - Number of rows (again positive integer).
2. Solver Method:
    - 1 for Jacobi.
    - 2 for Gauss-Seidel.
3. Central Charge:
    - Y or y to include a charge at the center.
    - N or n to exclude the charge.
4. Charge Distribution:
    - If a central charge is included, the user is prompted to input a value for sigma, which controls the spread of the charge distribution
    - Float between 0.5 and 10.0
5. Over-Relaxation (only for Gauss-Seidel):
    - Y or y to enable over-relaxation.
    - N or n to disable over-relaxation.
6. Maximum Iterations:
    - A positive integer specifying the max number of iterations.

---

## Output Files
The program generates the following output files:
1. all_iterations.dat:
    - Contains the potential values at each iteration (can be very large )
2. final.dat:
    - Contains the final potential values after the solver is done.
3. maxIter.txt:
    - Contains the max number of iterations specified by the user.
    - This later gets used by movie.gnu to tell it how many iterations to loop over when making the movie.

Visualization output
This program uses Gnuplot and FFmpeg to generate the visuals:
1. Final plot:
    - The final potential distribution is saved as final.png using final.gnu.
2. Movie:
    - The movie is created using movie.gnu and FFmpeg.
    - Saved as relaxation_movie.mp4.

---

## Pseudocode
Main Program
1. Initialize variables (grid spacing, maximum iterations, etc.).
2. Call choices subroutine to get user input.
3. Allocate memory for potential (f) and charge density (rho) arrays.
4. Call setup subroutine to initialize the grid, boundary conditions, and charge distribution.
5. Call the appropriate solver (jacobi or gauss_seidel).
6. Check if the solver converged.
    - If converged, generate plots and animations.
    - If not converged, print a warning message.
7. Deallocate memory and exit.

Solver Subroutines
1. Initialize variables (tolerance, difference, etc.).
2. Iterate over the grid:
    - Update potential values using the chosen method.
    - Calculate the difference between old and new values.
    - Check for convergence.
3. If converged, exit and return the results.
4. If the maximum number of iterations is reached, exit with a warning.

---

## Implementation Details
Solver Methods
- Jacobi: Updates the potential at each grid point using values from the previous iteration.
- Gauss-Seidel: Updates the potential using the most recent values, leading to faster convergence.

Comparison
- Jacobi: Takes far more iterations, highest I found was close to 500
- Gauss-Seidel: Converged very fast, most sub 50
- More detailed comparison in pdf report

Charge Distribution
- The charge distribution is defined by a Gaussian function centered at the grid's center.
- The user specifies sigma, which controls the spread of the charge:
    - A smaller sigma results in a more localized charge.
    - A larger sigma results in a more spread-out charge.

Over-Relaxation
- Over-relaxation is implemented only for the Gauss-Seidel method. It accelerates convergence by applying a relaxation factor (omega).

## Convergence Criterion
- Convergence is determined by monitoring the average change in the potential values across the grid. At each iteration, the absolute difference between the old and new potential values is calculated for every interior grid point. These differences are summed and normalized by the total number of interior points to compute an average change (\verb|diff|). The solver is considered converged when this average change falls below a predefined tolerance (\verb|tol = 1.0e-3|). If the solver fails to converge within the specified maximum number of iterations (\verb|maxIter|), a warning is issued to indicate that the solution may not have reached the desired accuracy.

---

## Limitations and Improvements
Limitations
- The program assumes a uniform grid and simple boundary conditions.
- Over-relaxation is not implemented for the Jacobi method.
    - I think is ok based on the wording of the project description.
    - Appears to only be relevant for Gauss-Seidel.
- Had boundary issues with Jacobi
    - Fixed boundaries were not staying fixed when update grid.
    - Bunch of extra code at bottom of that subroutine to deal with that.
    - No such issues with Gauss-Seidel.
- The convergence criterion is based on a fixed tolerance, which may not be optimal for all cases.

Improvements
- Implement adaptive tolerance for the convergence criterion.
- Extend the program to handle non-uniform grids and complex boundary conditions.

