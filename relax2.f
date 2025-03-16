c     [PROJECT DESCRIPTION].
c
c     Zane Pennella DATE
c      
c     Compile via
c gfortran -Wall -O -g -ffixed-line-length-100 -o relax relaxation.f
c     fix this later

      PROGRAM RELAX
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     DECLARATIONS
      IMPLICIT none

c     [VARIABLES]
      REAL*8       :: omega
      REAL*8, DIMENSION(:,:), ALLOCATABLE  :: f(:,:), rho(:,:)
      LOGICAL      :: charged, overrelaxed, converged
      INTEGER      :: i, j, maxIter, h, method
      CHARACTER*20 :: outfilename

c     Declare subroutines
      EXTERNAL choices, setup, jacobi, gauss_seidel
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     ASSIGN VALUES
      outfilename = 'relaxation.dat' ! output filename
      h = 1
      maxIter = 1000 ! TEMP FIXED for now change later
      charged = .false.
      overrelaxed = .false.
      converged = .false.   
    
c     end assigning values
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     MAIN BODY
cccccccccccccccccccccccccccc
c     Set-Up
     
      write(*,*) "################################################"
      write(*,*) "PROGRAM: RELAXATION METHODS"
      write(*,*) "################################################"
      write(*,*) "Zane Pennella [DATE]"
      write(*,*) "Output written to file ",outfilename
      write(*,*)
      
c     set intial conditions
      call choices(i, j, method, charged, overrelaxed)
      allocate(f(i,j))
      allocate(rho(i,j))
      call setup(f, rho, i, j, charged, overrelaxed, omega)

      if (method == 1) then
      	call jacobi2(f, i, j, rho, h, omega, maxIter, converged)
      else if (method == 2) then
      	call gauss_seidel2(f, i, j, rho, h, omega, maxIter, converged)
      end if
c     Write the final potential to a file
      call plot(f, i, j, outfilename)

c     Close-Out
      deallocate(f)
      deallocate(rho)
      
      write(*,*) "Done."


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      SUBROUTINE: MOVIE_MAKER
c
c      Zane Pennella [DATE]
      !SUBROUTINE movie_maker([STUFF])
c$$$      [DESCRIPTION] 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     DECLARATIONS
      !IMPLICIT none

c     [VARIABLES]

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     MAIN BODY
c
   
c 
      !return
      !end                       ! subroutine movie_maker
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc      
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      END PROGRAM RELAX