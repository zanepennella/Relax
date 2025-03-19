      SUBROUTINE setup(f, rho, i, j, charged, overrelaxed, omega, sigma)
c
c$$$  [Sets up the initial conditions for the relaxation methods]
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     DECLARATIONS
      IMPLICIT none

c     [VARIABLES]
      REAL*8, DIMENSION(i,j), INTENT(inout)  :: f, rho
      REAL*8, INTENT(inout)      :: omega
      REAL*8, INTENT(in) :: sigma ! Width of the Gaussian charge distribution
      INTEGER :: mid_i, mid_j  ! Center indices (must be integers)
      INTEGER, INTENT(in) :: i, j
      LOGICAL, INTENT(in) :: charged
      LOGICAL, INTENT(in) :: overrelaxed
      INTEGER :: temp_i, temp_j, debug_counter
      REAL*8 :: x, y, x0, y0, A ! Variables for Gaussian distribution
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc 
c     MAIN BODY
c
      mid_i = 0
      mid_j = 0
      
c     Initialize arrays to all 0s
      do temp_i = 1, i
      	do temp_j = 1, j
      		f(temp_i, temp_j) = 0.0d0
      		rho(temp_i, temp_j) = 0.0d0
      	end do ! temp_j
      end do ! temp_i
      
c     Apply initial conditions
      do temp_i = 1, i
      	f(temp_i, 1) = 1.0d0  ! Top edge (V = 1)
      end do ! temp_i
      do temp_j = 1, j
      	f(1, temp_j) = 1.0d0  ! Left edge (V = 1)
      end do ! temp_j
      
c     Set over-relaxation parameter
      if (overrelaxed) then
      	omega = 1.8d0  ! Over-relaxation
      else
      	omega = 1.0d0   ! No over-relaxation
      end if
      
c     Place Gaussian charge distribution if requested
      if (charged) then
      	x0 = real(i)/ 2.0d0 ! Center of the grid in x
      	y0 = real(j)/ 2.0d0 ! Center of the grid in y
      	A = -1.0/10.0       ! Amplitude of the charge distribution

      	do temp_i = 1, i
      	    do temp_j = 1, j
                x= real(temp_i)
                y= real(temp_j)
                rho(temp_i, temp_j) = A * exp(-((x-x0)**2 + (y-y0)**2)/(2*sigma**2))
            end do
      	end do
      end if
      
c     Debugging output
      write (*,'(A)', advance="no") '[ '
      do debug_counter = 1, 3
      	write (*,'(A)', advance="no") '. '
      end do
      write (*,'(A)', advance="no") ']'
      print *
      print *, 'Initial array:'
      do temp_i = 1, i
      	do temp_j = 1, j
      		write (6, "(F8.2)", ADVANCE="NO") f(temp_i, temp_j)           
      	end do
      	print *
      end do
      if (charged) then
      	print *
      	print *, 'Initial charge distribution:'
      	do temp_i = 1, i
      	do temp_j = 1, j
      			write (6, "(F8.2)", ADVANCE="NO") rho(temp_i, temp_j)           
      		end do
      		print *
      	end do
      end if
c      
      return
      END SUBROUTINE setup