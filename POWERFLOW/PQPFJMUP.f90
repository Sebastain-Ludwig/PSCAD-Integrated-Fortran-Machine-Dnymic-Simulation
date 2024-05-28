module PQPFJMUP
    use iso_fortran_env,only:stdout=>output_unit,&
            stdin=> input_unit,&
            stderr=>error_unit
    use PQPFELUP
    implicit none
contains
    subroutine PQJOMTUP(VV,GMAT,BMAT,BUSN,PVN,JOCMAT)
        real,intent(in)::VV(:),GMAT(:,:),BMAT(:,:)
        integer,intent(in)::BUSN,PVN
        real,intent(inout)::JOCMAT(:,:)

        integer::i, j

        i=2
        j=2

        do while(i<=BUSN)
            j=2
            do while(j<=BUSN)
                if (i/=j)then
                    call PQHMELUP(VV(i),VV(j),BMAT(i, j),JOCMAT(i-1, j-1))
                else
                    call PQHMDEUP(VV(i),BMAT(i, i),JOCMAT(i-1, j-1))
                end if
                j=j+1
            end do
            i=i+1
        end do
        i=2
        j=2

        do while(i<=BUSN)
            j=2
            do while(j<=BUSN)
                if (i/=j)then
                    call PQLMELUP(VV(i),VV(j),BMAT(i, j),JOCMAT(BUSN-1+i-1, BUSN-1+j-1))
                else
                    call PQLMDEUP(VV(i),BMAT(i, i),JOCMAT(BUSN-1+i-1, BUSN-1+j-1))
                end if
                j=j+1
            end do
            i=i+1
        end do
    end subroutine PQJOMTUP
end module PQPFJMUP