      SUBROUTINE jacobi2(f, i, j, rho, h, omega, maxIter, converged)
      IMPLICIT none

            ! Declarations
      INTEGER, INTENT(IN) :: i, j, maxIter
      REAL*8, INTENT(INOUT) :: f(i,j)
      REAL*8, INTENT(IN) :: rho(i,j), h, omega
      LOGICAL, INTENT(OUT) :: converged
      REAL*8 :: temp_f(i,j), diff, tol
      INTEGER :: iter, a, b

      ! Initialize variables
      tol = 1.0e-6  ! Convergence tolerance
      converged = .false.

      ! Main iteration loop
      do iter = 1, maxIter
        diff = 0.0  ! Reset difference for each iteration

      ! Update potential using Jacobi method
        do a = 2, i-1
            do b = 2, j-1
              temp_f(a,b) = (1 - omega) * f(a,b) +  
     &  + (omega / 4.0) * (f(a+1,b) + f(a-1,b) + f(a,b+1) + f(a,b-1) + 
     &  (4 * 3.141592653589794E0 * h**2 * rho(a,b)))
              diff = diff + ABS(temp_f(a,b) - f(a,b))  ! Accumulate difference
            END do
        END do
      


        ! Print debugging information
        PRINT *, "Iteration:", iter, "Difference:", diff


        ! Check for convergence
        IF (diff < tol) THEN
            converged = .true.
            Print *, "Converged after", iter, "iterations."
            EXIT
        END IF

        ! Copy temp_f back to f for the next iteration
        f = temp_f
      END do

      ! If maxIter is reached without convergence
      IF (.NOT. converged) THEN
        PRINT *, "Warning: Did not converge after", maxIter, "iterations."
      END IF
      
      RETURN
      END SUBROUTINE jacobi2