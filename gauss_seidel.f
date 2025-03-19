      SUBROUTINE gauss_seidel(f, i, j, rho, h, omega, maxIter, converged
     &  , actualIter)
c$$$$ [Solves the Poisson equation using the Gauss-Seidel method]
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     DECLARATIONS
      IMPLICIT none

c     [VARIABLES]
      INTEGER, INTENT(in) :: i, j, maxIter
      INTEGER :: iter, a, b
      REAL*8, INTENT(inout) :: f(i,j)
      REAL*8, INTENT(in) :: rho(i,j), h, omega
      LOGICAL, INTENT(out) :: converged
      INTEGER, INTENT(out) :: actualIter ! Actual number of iterations
      REAL*8 :: sum_neighbors, diff, tol, old_f
      REAL*8, PARAMETER :: Pi = 3.141592653589794E0

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     MAIN BODY
c
c     Initialize variables
      tol = 1.0e-3  ! Convergence tolerance
      converged = .false.
      actualIter = 0

c     Open a single file for all iterations
      OPEN(UNIT=20, FILE='all_iterations.dat', STATUS='replace')

c     Open a single file for final iteration
      OPEN(UNIT=30, FILE='final.dat', STATUS='replace')

c     Main iteration loop
      DO iter = 1, maxIter
          diff = 0.0  ! Reset difference for each iteration

c         Update potential using Gauss-Seidel method
          DO a = 2, i-1
              DO b = 2, j-1
c               Store the old value of f(a,b) for comparison
                old_f = f(a,b)

c               Calculate the new value of f(a,b)
                sum_neighbors = f(a+1,b) + f(a-1,b) + f(a,b+1) + f(a,b-1)
                sum_neighbors = sum_neighbors + (4 * Pi * h**2 * rho(a,b))              
                f(a,b) = (1 - omega) * f(a,b) + (omega / 4.0) * sum_neighbors

c               Limit the range of f(a,b)
                f(a,b) = MIN(1.0d10, f(a,b))
                f(a,b) = MAX(-99.9, f(a,b))

c               Accumulate difference
                diff = diff + ABS(f(a,b) - old_f)  
              END DO
          END DO

c         Normalize the difference by the number of interior points
          diff = diff / ((i-2) * (j-2))

c         Write the potential grid to the file
          DO a = 1, i
              DO b = 1, j
                  WRITE(20, '(F8.4, 2X)', ADVANCE='NO') f(a,b)
              END DO
              WRITE(20, *)  ! Newline after each row
          END DO
          WRITE(20, *)  ! Blank line after each iteration

c         Check for convergence
          IF (diff < tol) then
              converged = .true.
              actualIter = iter
              PRINT *, "Converged after", iter, "iterations."
              exit
          END IF
      END DO

      DO a = 1, i
        DO b = 1, j
            WRITE(30, '(F8.4, 2X)', ADVANCE='NO') f(a,b)
        END DO
        WRITE(30, *)  ! Newline after each row
      END DO  

c     Close the files
      CLOSE(20)
      CLOSE(30)

c     If maxIter is reached without convergence
      IF (.not. converged) then
          actualIter = maxIter
          print *, "Warning: Did not converge after", maxIter, "iterations."
      END IF

      return
      END SUBROUTINE gauss_seidel