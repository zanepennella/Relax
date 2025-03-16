      SUBROUTINE choices(i, j, method, charged, overrelaxed)
c
c$$$  [DESCRIPTION]
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     DECLARATIONS
      IMPLICIT none

c
      INTEGER, INTENT(inout) :: i, j, method
      LOGICAL, INTENT(inout) :: charged
      LOGICAL, INTENT(inout) :: overrelaxed
      INTEGER :: ierror
      CHARACTER(1) :: c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc 
c     MAIN BODY
c
      do
      	print *,'Number of columns in array?'
      	read(*,'(i10)',iostat=ierror) i
      	if (ierror /= 0) then
      		print *, 'Error in input!'
      	else if (i.eq.0) then
      		print *, "Please choose nonzero value."
      	else
      		write(*, '(A, I3, A)') 'Array will have', i, ' columns...'
      		exit
      	end if
      end do
      print *
      do
      	print *,'Number of rows in array?'
      	read(*,'(i10)',iostat=ierror) j
      	if (ierror /= 0) then
      		print *, 'Error in input!'
      	else if (i.eq.0) then
      		print *, "Please choose nonzero value."
      	else
      		write(*, '(A, I3, A)') 'Array will have', j, ' rows...'
      		exit
      	end if
      end do
      print *
      do
      	print *,' Method?'
      	print *,' (1) Jacobi'
      	print *,' (2) Gauss-Seidel'
      	read(*,'(i10)',iostat=ierror) method
      	if (ierror /= 0) then
      		print *, 'Error in input!'
      	else if (method == 1) then
      		print *, 'Using the Jacobi method...'
      		exit
      	else if (method == 2) then
      		print *, 'Using the Gauss-Seidel method...'
      		exit
      	else
      		print *, 'Error in input!'
      		print *
      	end if
      end do
      print *
      do
      	print *,' Charged?'
      	print *,' (Y/N)'
      	read (*, '(A)',iostat=ierror) c
      	if (ierror /= 0) then
      		print *, 'Error in input!'
      	else if (c == 'Y' .or. c == 'y') then
      		print *, 'Starting with a charge at the center...'
      		charged = .true.
      		exit
      	else if (c == 'N' .or. c=='n') then
      		print *, 'Starting with no charge...'
      		charged = .false.
      		exit
      	else
      		print *, 'Error in input!'
      		print *
      	end if
      end do
      print *
      do
      	print *,' Over-relaxation?'
      	print *,' (Y/N)'
      	read (*, '(A)',iostat=ierror) c
      	if (ierror /= 0) then
      		print *, 'Error in input!'
      	else if (c == 'Y' .or. c == 'y') then
      		print *, 'Relaxation parameter introduced...'
      		overrelaxed = .true.
      		exit
      	else if (c == 'N' .or. c=='n') then
      		print *, 'Using ordinary relaxation...'
      		overrelaxed = .false.
      		exit
      	else
      		print *, 'Error in input!'
      		print *
      	end if
      end do   
      print *  
c      
      return
      END SUBROUTINE choices