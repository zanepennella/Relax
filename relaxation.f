c     [PROJECT DESCRIPTION].
c
c     Zane Pennella DATE
c      
c     Compile via
c gfortran -Wall -O -g -ffixed-line-length-100 -o relax relaxation.f
c
      PROGRAM RELAX
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     DECLARATIONS
      IMPLICIT none

c     [VARIABLES]

      REAL*8       :: omega
      REAL*8, DIMENSION(:,:), ALLOCATABLE  :: f(:,:), rho(:,:)
      LOGICAL      :: charged, overrelaxed, converged
      INTEGER      :: i, j, maxIter, h, method
      CHARACTER*20 :: outfilename
cccccccccccccccccccccccccc
c     [SUBROUTINES AND DESCRIPTIONS]
c     
c     end declarations
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     ASSIGN VALUES
      outfilename = 'relaxation.dat' ! output filename
      h = 1
      maxIter = 3 ! TEMP FIXED
      charged = .false.
      overrelaxed = .false.
      converged = .false.   
    
c     end assigning values
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
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
      call setup(f(i,j), rho(i,j), i, j, charged, overrelaxed, omega)

      if (method == 1) then
      	call jacobi(f(i,j), i, j, rho(i,j), h, omega, maxIter, converged)
      else if (method == 2) then
      	call gauss_seidel(f(i,j), i, j, rho(i,j), h, omega, maxIter, converged)
      end if

c      open output file
      !open(unit=13, FILE=outfilename)
      
cccccccccccccccccccccccccccc
c     Main Part

c     write initial condition to file
      
c     close output file
      !close (13)

cccccccccccccccccccccccccccc
c     Close-Out
      
cc    feedback to stdout that program has ended
      deallocate(f)
      deallocate(rho)
      
      write(*,*) "Done."
      end                       ! program RELAX
ccccccccccccccccc END OF MAIN PROGRAM ccccccccccccccccccccccccccccccccccc

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     SUBROUTINE CHOICES
c     Zane Pennella [DATE]

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
      end ! choices
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     SUBROUTINE SETUP
c     Zane Pennella [DATE]

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
      	omega = 1.2 ! TEMPORARY
      else
      	omega = 1.
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
      end ! setup
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      SUBROUTINE: JACOBI METHOD
c
c      Zane Pennella [DATE]
      SUBROUTINE jacobi(f, i, j, rho, h, omega, maxIter, converged)
c$$$      [DESCRIPTION] 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     DECLARATIONS
      IMPLICIT none

c     [VARIABLES]
      INTEGER :: l, a, b
      REAL*8, DIMENSION(i,j), INTENT(inout) :: f
      REAL*8, DIMENSION(i,j), INTENT(in) :: rho
      REAL*8, DIMENSION(i,j) :: temp_f
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
      		temp_f(a, b) = f(a,b)
      	end do ! b
      end do ! a
      
      call sleep(1)
      do l = 1, maxIter
      	do a = 2, i-1
      		 do b = 2, j-1
      		 	sum_neighbors = f(a+1,b)+f(a-1,b)+f(a,b+1)+f(a,b-1)
      		 	sum_neighbors = sum_neighbors + ((4*Pi)*(h**2)*rho(a,b))
      		 	temp_f(a,b) = (1 - omega)*f(a,b) + (omega/4) * sum_neighbors
      		 	sum_neighbors = 0
      		 end do ! b
      	end do !
      	do a = 1, i
      		do b = 1, j
      			f(a, b) = temp_f(a, b)
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
      end                       ! subroutine jacobi
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc      
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      SUBROUTINE: GAUSS-SEIDEL METHOD
c
c      Zane Pennella [DATE]
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
      end                       ! subroutine gauss_seidel
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc      
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      SUBROUTINE: PLOT
c
c      Zane Pennella [DATE]
      !SUBROUTINE plot([STUFF])
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
      !end                       ! subroutine plot
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc      
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

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