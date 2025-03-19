      SUBROUTINE choices(i, j, method, charged, overrelaxed, maxIter, sigma)
c
c$$$  [Allows the user choices on the aspects of the problem]
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     DECLARATIONS
      IMPLICIT none

c
      INTEGER, INTENT(inout) :: i, j, method, maxIter
      LOGICAL, INTENT(inout) :: charged, overrelaxed
      REAL*8, INTENT(out) :: sigma ! Width of the Gaussian charge distribution
      INTEGER :: ierror
      CHARACTER(1) :: c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc 
c     MAIN BODY
c
c     Prompt for number of columns
      do
      	print *, 'Number of columns in array? (Must be a positive integer)'
      	read(*, '(i10)', iostat=ierror) i
      	if (ierror /= 0) then
      		print *, 'Error: Please enter a valid integer.'
      	else if (i <= 0) then
      		print *, 'Error: Please enter a positive integer.'
      	else
      		write(*, '(A, I3, A)') 'Array will have ', i, ' columns...'
      		exit
      	end if
      end do
      print *

c     Prompt for number of rows
      do
      	print *, 'Number of rows in array? (Must be a positive integer)'
      	read(*, '(i10)', iostat=ierror) j
      	if (ierror /= 0) then
      		print *, 'Error: Please enter a valid integer.'
      	else if (j <= 0) then
      		print *, 'Error: Please enter a positive integer.'
      	else
      		write(*, '(A, I3, A)') 'Array will have ', j, ' rows...'
      		exit
      	end if
      end do
      print *

c     Prompt for solver method
      do
      	print *, 'Which solver method would you like to use:'
      	print *, ' (1) Jacobi'
      	print *, ' (2) Gauss-Seidel'
      	read(*, '(i10)', iostat=ierror) method
      	if (ierror /= 0) then
      		print *, 'Error: Please enter a valid integer.'
      	else if (method == 1) then
      		print *, 'Using Jacobi method...'
      		exit
      	else if (method == 2) then
      		print *, 'Using Gauss-Seidel method...'
      		exit
      	else
      		print *, 'Error: Please enter 1 or 2.'
      		print *
      	end if
      end do
      print *

c     Prompt for charge presence
      do
      	print *, 'Would you like to include a charge at the center? (Y/N)'
      	read(*, '(A)', iostat=ierror) c
      	if (ierror /= 0) then
      		print *, 'Error: Please enter Y or N.'
      	else if (c == 'Y' .or. c == 'y') then
      		print *, 'Starting with a charge at the center...'
      		charged = .true.
      		exit
      	else if (c == 'N' .or. c == 'n') then
      		print *, 'Starting with no charge...'
      		charged = .false.
      		exit
      	else
      		print *, 'Error: Please enter Y or N.'
      		print *
      	end if
      end do
      print *

c     Prompt for sigma value
      if (charged) then
            do
                print *, 'Enter the width of the Gaussian charge distribution (sigma)'
                print *, '(Must be float between 0.5 and 10.0)'
                read(*, '(F10.2)', iostat=ierror) sigma
                if (ierror /= 0) then
                    print *, 'Error: Please enter a valid real number.'
                else if (sigma < 0.5 .or. sigma > 10.0) then
                    print *, 'Error: Please enter a value between 0.5 and 10.0.'
                else
                    write(*, '(A, F10.2, A)') 'Sigma set to ', sigma, '...'
                    exit
                end if
            end do
            print *
      else
          sigma = 0.0d0 ! Default value (not used if no charge)
      end if
        
c     Prompt for over-relaxation only if Gauss-Seidel is selected
      if (method == 2) then
          do
              print *, 'Over-relaxation? (Y/N)'
              read(*, '(A)', iostat=ierror) c
              if (ierror /= 0) then
                  print *, 'Error: Please enter Y or N.'
              else if (c == 'Y' .or. c == 'y') then
                  print *, 'Using over-relaxation...'
                  overrelaxed = .true.
                  exit
              else if (c == 'N' .or. c == 'n') then
                  print *, 'Using ordinary relaxation...'
                  overrelaxed = .false.
                  exit
              else
                  print *, 'Error: Please enter Y or N.'
                  print *
              end if
          end do
          print *
      else
          overrelaxed = .false.  ! Default to no over-relaxation if Jacobi is selected
      end if

c     Prompt for maximum number of iterations
      do
      	print *, 'Maximum number of iterations? (Must be a positive integer)'
      	read(*, '(i10)', iostat=ierror) maxIter
      	if (ierror /= 0) then
      		print *, 'Error: Please enter a valid integer.'
      	else if (maxIter <= 0) then
      		print *, 'Error: Please enter a positive integer.'
      	else
      		write(*, '(A, I5, A)') 'Maximum iterations set to ', maxIter, '...'
      		exit
      	end if
      end do
      print *

      OPEN(UNIT=40, FILE='maxIter.txt', STATUS='replace')
      WRITE(40, '(I10)') maxIter
      CLOSE(40)
c      
      return
      END SUBROUTINE choices