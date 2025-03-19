      SUBROUTINE jacobi(f, i, j, rho, h, omega, maxIter, converged, actualIter)
c        
c$$$  [Implements the Jacobi method for solving the Poisson equation]
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     DECLARATIONS
      IMPLICIT none

c     [VARIABLES]
      INTEGER, INTENT(IN) :: i, j, maxIter
      INTEGER :: iter, a, b
      REAL*8, INTENT(INOUT) :: f(i,j)
      REAL*8, INTENT(IN) :: rho(i,j), h, omega
      LOGICAL, INTENT(OUT) :: converged
      INTEGER, INTENT(OUT) :: actualIter  ! Actual number of iterations
      REAL*8 :: temp_f(i,j), diff, tol
      REAL*8, PARAMETER :: Pi = 3.141592653589794E0


c     Initialize variables
      tol = 1.0e-3  ! Convergence tolerance
      converged = .false.
      actualIter = 0  ! Initialize actualIter

c     Open a single file for all iterations
      OPEN(UNIT=20, FILE='all_iterations.dat', STATUS='replace')
  
c     Open a single file for final iteration
      OPEN(UNIT=30, FILE='final.dat', STATUS='replace')

c     Main iteration loop
      DO iter = 1, maxIter
          diff = 0.0d0  ! Reset difference for each iteration

c         Update potential using Jacobi method
          DO a = 2, i-1  ! Interior grid points
              DO b = 2, j-1
c                 Calculate the new value of f(a,b) using Jacobi method
                  temp_f(a,b) = (1.0 - omega) * f(a,b) + (omega / 4.0) *
     &              (f(a+1,b) + f(a-1,b) + f(a,b+1) + f(a,b-1) +
     &              (4.0 * Pi * h**2 * rho(a,b)))  ! Add source term
                  temp_f(a,b)=min(1.0d10,temp_f(a,b))
                  temp_f(a,b)=max(-99.9,temp_f(a,b))

c                 Accumulate difference for convergence check
                  diff = diff + ABS(temp_f(a,b) - f(a,b))  
              END DO
          END DO

c         Normalize the difference by the number of updated grid points (interior)
          diff = diff / ((i-2) * (j-2))

c         Print debugging information
          PRINT *, "Iteration:", iter, "Difference:", diff

c         Write the potential grid to the file
          DO a = 1, i
              DO b = 1, j
                  WRITE(20, '(F8.4, 2X)', ADVANCE='NO') f(a,b)
              END DO
              WRITE(20, *)  ! Newline after each row
          END DO
          WRITE(20, *)  ! Blank line after each iteration

c         Check for convergence
          IF (diff < tol) THEN
              converged = .true.
              actualIter = iter  ! Set actualIter to the current iteration
              PRINT *, "Converged after", iter, "iterations."
              EXIT  ! Exit if converged
          END IF

c         Copy temp_f back to f for the next iteration
          f = temp_f

c         Ensure boundary conditions are preserved
c          Top row (set to 1.0 for all columns except the top-right corner)
          DO b = 1, j
              IF (b < j) THEN
                  f(1, b) = 1.0  ! Entire top row is 1.0 except the top-right corner
              END IF
          END DO

c         Left column (set to 1.0 for all rows except the bottom-left corner)
          DO a = 1, i
              IF (a < i) THEN
                  f(a, 1) = 1.0  ! Entire left column is 1.0 except the bottom-left corner
              END IF
          END DO

c         Bottom row (set to 0.0 for all columns except the bottom-left corner)
          DO b = 1, j
              IF (b > 1) THEN
                  f(i, b) = 0.0  ! Entire bottom row is 0.0 except the bottom-left corner
              END IF
          END DO

c         Right column (set to 0.0 for all rows except the top-right corner)
          DO a = 1, i
              IF (a > 1) THEN
                  f(a, j) = 0.0  ! Entire right column is 0.0 except the top-right corner
              END IF
          END DO

c         Set the bottom-left corner (f(i,1)) and top-right corner (f(1,j)) to 1.0
          f(i, 1) = 1.0  ! Bottom-left corner
          f(1, j) = 1.0  ! Top-right corner
      END DO

      DO a = 1, i
          DO b = 1, j
            WRITE(30, '(F8.4, 2X)', ADVANCE='NO') f(a,b)
          END DO
          WRITE(30, *)  ! Newline after each row
        END DO
      
c     Close the file
      CLOSE(20)
      CLOSE(30)

c     If maxIter is reached without convergence
      IF (.NOT. converged) THEN
          actualIter = maxIter  ! Set actualIter to maxIter
          PRINT *, "Warning: Did not converge after", maxIter, "iterations."
      END IF

      return
      END SUBROUTINE jacobi