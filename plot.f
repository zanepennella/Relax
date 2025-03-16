      SUBROUTINE plot(f, i, j, outfilename)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     DECLARATIONS
      IMPLICIT none

c     [VARIABLES]
      INTEGER, INTENT(in) :: i, j
      REAL*8, DIMENSION(i,j), INTENT(in) :: f
      CHARACTER(LEN=*), INTENT(in) :: outfilename
      INTEGER :: a, b

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     MAIN BODY
c
      ! Write the potential to a file
      open(unit=10, file=outfilename, status='replace')
      do a = 1, i
          do b = 1, j
              write(10, '(F8.4)', advance='no') f(a, b)
          end do
          write(10, *)  ! Newline after each row
      end do
      close(10)
      write(*,*) "Potential written to file '", outfilename, "'."

      ! Call GNUplot to generate plots
      call system('gnuplot plot.gnu')
      write(*,*) "Plots generated using GNUplot."

      return
      END SUBROUTINE plot