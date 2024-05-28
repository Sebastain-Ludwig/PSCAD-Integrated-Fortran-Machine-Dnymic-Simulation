# 1 "/home/sebastian/Project/POWERSYSDYN/FORTRAN SCRIPTS/MACHINE-DYNAMIC-SIMULATION/MATHBALIB/BAMTDEFA.f90"

module BAMTDEFA
    use iso_fortran_env, only: stdout => output_unit, &
            stdin => input_unit, &
            stderr => error_unit, &
            real64
    implicit none
    contains
    subroutine BAMTDOLU(MAT,L,U)
        real,intent(in)::MAT(:,:)
        real,intent(inout)::L(:,:),U(:,:)

        integer::i,j,MATN(2)

        i=1
        j=1
        L=0.
        U=0.

        MATN(1)=size(MAT,1)
        MATN(2)=size(MAT,2)
        if (MATN(1)/=MATN(2))then
            stop 'LOW LEVEL MATH ERROR:LU FACTORIZE MAT COL NUMBER NOT EQUAL WITH RAW NUMBER!'
        end if

        do i=1,MATN(1)
            U(1,i)=MAT(1, i)
            if (abs(U(1,1))>=1e-5)then
                L(i,1)=MAT(i, 1)/U(1,1)
            else
                stop 'LOW LEVEL MATH ERROR:LU U MATRIX U(1,1) NEAR SINGULAR!'
            end if
        end do

        do i = 2,MATN(1)
            do j=i,MATN(1)
                U(i,j)=MAT(i, j)-sum(L(i, 1:i-1)*U(1:i-1,j))
                if (abs(U(i, i))>=1e-5)then
                    L(j,i)=(MAT(j,i)-sum(L(j,1:i-1)*U(1:i-1,i)))/U(i,i)
                else
                    write(*,*)i
                    stop 'LOW LEVEL MATH ERROR:LU U DIAG ELEMENT U(i,i) NEAR SINGULAR!'
                end if
            end do
        end do
    end subroutine BAMTDOLU

end module BAMTDEFA
