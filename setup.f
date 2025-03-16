      SUBROUTINE setup(f, rho, i, j, charged, overrelaxed, omega)
c
c$$$  [DESCRIPTION]
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     DECLARATIONS
      IMPLICIT none

c
      REAL*8, DIMENSION(i,j), INTENT(inout)  :: f, rho
      REAL*8, INTENT(inout)      :: omega
      REAL*8 :: mid_i, mid_j
      INTEGER, INTENT(in) :: i, j
      LOGICAL, INTENT(in) :: charged
      LOGICAL, INTENT(in) :: overrelaxed
      INTEGER :: temp_i, temp_j, a
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc 
c     MAIN BODY
c
      mid_i = 0
      mid_j = 0
      
      ! initialize arrays to all 0s
      do temp_i = 1, i
      	do temp_j = 1, j
      		f(temp_i, temp_j) = 0
      		rho(temp_i, temp_j) = 0
      	end do ! temp_j
      end do ! temp_i
      
      ! apply initial conditions
      do temp_i = 1, i
      	f(temp_i, 1) = 1
      end do ! temp_i
      do temp_j = 1, j
      	f(1, temp_j) = 1
      end do ! temp_j
      
      if (overrelaxed) then
      	omega = 1.15 ! Overrelaxation
      else
      	omega = 1.  ! No overrelaxation
      end if
      
      if (charged) then
      	if (mod(i,2) == 0) then
      		mid_i = i/2
      	else
      		mid_i = (i+1)/2
      	end if
      	if (mod(j,2) == 0) then
      		mid_j = j/2
      	else
      		mid_j = (j+1)/2
      	end if
      	rho(int(mid_i),int(mid_j)) = -(1.0/10)
      end if
      
      write (*,'(A)', advance="no") '[ '
      call sleep(1)
      do a = 1, 3
      	write (*,'(A)', advance="no") '. '
      	call sleep(1)
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