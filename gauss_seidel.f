      SUBROUTINE gauss_seidel(f, i, j, rho, h, omega, maxIter, converged)
c$$$      [DESCRIPTION] 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     DECLARATIONS
      IMPLICIT none

c     [VARIABLES]
      INTEGER :: l, a, b
      REAL*8, DIMENSION(i,j), INTENT(inout) :: f
      REAL*8, DIMENSION(i,j), INTENT(in) :: rho
      REAL*8, DIMENSION(i,j) :: old_f
      INTEGER, INTENT(in) :: i, j, h, maxIter
      REAL*8, INTENT(in) :: omega
      REAL*8 :: sum_neighbors
      REAL*8              :: Pi 
      PARAMETER (Pi = 3.141592653589794E0)
      LOGICAL, INTENT(inout) :: converged

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     MAIN BODY
c
      l = 0
      a = i
      b = j
      sum_neighbors = 0
      
      do a = 1, i
      	do b = 1, j
      		old_f(a, b) = f(a,b)
      	end do ! b
      end do ! a
      
      call sleep(1)
      do l = 1, maxIter
      	do a = 2, i-1
      		 do b = 2, j-1
      		 	sum_neighbors = old_f(a+1,b)+f(a-1,b)+old_f(a,b+1)+f(a,b-1)
      		 	sum_neighbors = sum_neighbors + ((4*Pi)*(h**2)*rho(a,b))
      		 	f(a,b) = (1 - omega)*old_f(a,b) + (omega/4) * sum_neighbors
      		 	sum_neighbors = 0
      		 end do ! b
      	end do !
      	do a = 1, i
      		do b = 1, j
      			old_f(a, b) = f(a, b)
      		end do ! b
      	end do ! a
      	! find difference, decide if converged
      	if (converged) then
      		print *, 'Exiting loop...'
      		exit
      	end if
        print *
      	print *, 'iteration:', l
      	do a = 1, i
      		do b= 1, j
      			write (6, "(F8.4)", ADVANCE="NO") f(a, b)
      		end do
      		print *
      	end do
      	call sleep(1)
      end do ! l
      
      print *
      print *, 'Result:'
      do a = 1, i
      	do b= 1, j
      		write (6, "(F8.4)", ADVANCE="NO") f(a, b)
      	end do
      	print *
      end do
c 
      return
      END SUBROUTINE gauss_seidel