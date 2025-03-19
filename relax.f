c     [Implementation of Relaxation method for Poisson Equation].
c
c     Zane Pennella [March 2025]
c      
c     Compile via
c     make, ./relax

c     Clean via
c     make clean

      PROGRAM RELAX
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     DECLARATIONS
      IMPLICIT none

c     [VARIABLES]
      REAL*8       :: omega, h, sigma
      REAL*8, DIMENSION(:,:), ALLOCATABLE  :: f(:,:), rho(:,:)
      LOGICAL      :: charged, overrelaxed, converged
      INTEGER      :: i, j, maxIter, actualIter, method

c     Declare subroutines
      EXTERNAL choices, setup, jacobi, gauss_seidel
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     ASSIGN VALUES
      h = 1.0d0  ! Grid spacing (real number)
      maxIter = 1000 ! Gets changed by user input in choices
      charged = .false.
      overrelaxed = .false.
      converged = .false.   
    
c     end assigning values
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     MAIN BODY
cccccccccccccccccccccccccccc

c     set intial conditions
      call choices(i, j, method, charged, overrelaxed, maxIter, sigma)
      allocate(f(i,j))
      allocate(rho(i,j))
      call setup(f, rho, i, j, charged, overrelaxed, omega, sigma)
      
      if (charged) then
            call write_charge_density(rho, i, j, 'charge_density.dat')
      end if

      if (method == 1) then
      	call jacobi(f, i, j, rho, h, omega, maxIter, converged, actualIter)
      else if (method == 2) then
      	call gauss_seidel(f, i, j, rho, h, omega, maxIter, converged, actualIter)
      end if
      

c     Check if the solver converged
      if (converged) then
c           Write the final potential to a file
            call system('gnuplot final.gnu') ! makes the final frame image

c           Create a movie of the relaxation process
            call system('gnuplot movie.gnu') ! collects the movie frames

c           Call ffmpeg to create the movie
            call system('ffmpeg -framerate 10 -i movie.png -c:v 
     &             libx264 -pix_fmt yuv420p relaxation_movie.mp4')
      else
c           Print a message if the solver did not converge
            write(*,*) "Warning: Solver did not converge. Skipping
     &             plot and movie."
      end if      

c     Close-Out
      deallocate(f)
      deallocate(rho)
      
      write(*,*) "Done."

      END PROGRAM RELAX