      SUBROUTINE plot(f, i, j, outfilename)
c$$$$ [Plotting the potential to a file]
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     DECLARATIONS

c     [VARIABLES]
        IMPLICIT none
        INTEGER, INTENT(IN) :: i, j
        REAL*8, DIMENSION(i,j), INTENT(IN) :: f
        CHARACTER(LEN=*), INTENT(IN) :: outfilename
        INTEGER :: a, b

c       Open the file for writing
        OPEN(UNIT=20, FILE=outfilename, STATUS='replace')

c       Write the potential grid to the file
        DO a = 1, i
            DO b = 1, j
                WRITE(10, '(F8.4, 2X)', ADVANCE='NO') f(a, b)
            END DO
            WRITE(10, *)  ! Newline after each row
        END DO
        CLOSE(10)

      END SUBROUTINE plot