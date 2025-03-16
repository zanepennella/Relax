      SUBROUTINE gauss_seidel2(f, i, j, rho, h, omega, maxIter, converged)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     DECLARATIONS
      IMPLICIT none

c     [VARIABLES]
      INTEGER :: iter, a, b
      REAL*8, DIMENSION(i,j), INTENT(inout) :: f
      REAL*8, DIMENSION(i,j), INTENT(in) :: rho
      INTEGER, INTENT(in) :: i, j, h, maxIter
      REAL*8, INTENT(in) :: omega
      REAL*8 :: sum_neighbors, diff, tol, old_f
      REAL*8, PARAMETER :: Pi = 3.141592653589794E0
      LOGICAL, INTENT(out) :: converged

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     MAIN BODY
c
      ! Initialize variables
      tol = 1.0e-6  ! Convergence tolerance
      converged = .false.

      ! Main iteration loop
      do iter = 1, maxIter
          diff = 0.0  ! Reset difference for each iteration

          ! Update potential using Gauss-Seidel method
          do a = 2, i-1
              do b = 2, j-1
                ! Store the old value of f(a,b) for comparison
                old_f = f(a,b)

                ! Calculate the new value of f(a,b)
                sum_neighbors = f(a+1,b) + f(a-1,b) + f(a,b+1) + f(a,b-1)
                sum_neighbors = sum_neighbors + (4 * Pi * h**2 *
     &              rho(a,b))
                f(a,b) = (1 - omega) * f(a,b) + (omega / 4.0) * sum_neighbors
                diff = diff + ABS(f(a,b) - old_f)  ! Accumulate difference
              end do
          end do

        ! Write the potential to a file for this iteration
        WRITE(filename, '(A, I0.3, A)') 'potential_iter_', iter, '.dat'
        OPEN(unit=20, file=filename, status='replace')
        DO a = 1, i
            DO b = 1, j
                WRITE(20, '(F8.4)', advance='no') f(a,b)
            END DO
            WRITE(20, *)  ! Newline after each row
        END DO
        CLOSE(20)

          ! Check for convergence
          if (diff < tol) then
              converged = .true.
              print *, "Converged after", iter, "iterations."
              exit
          end if
      end do

      ! If maxIter is reached without convergence
      if (.not. converged) then
          print *, "Warning: Did not converge after", maxIter, "iterations."
      end if

      return
      END SUBROUTINE gauss_seidel2