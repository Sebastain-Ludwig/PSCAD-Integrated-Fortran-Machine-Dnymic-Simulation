module NLPFJMSV
    use iso_fortran_env,only:stdout=>output_unit,&
            stdin=> input_unit,&
            stderr=>error_unit
    !use f95_precision,only:WP=>SP
    !use lapack95,only:GETRF,gesv
    implicit none

contains
    subroutine NLJOSVUP(DPV,DQV,DTHEV,DVV,BUSN,PVN,JOCMAT)
        real,intent(in)::DPV(:),DQV(:),JOCMAT(:,:)
        integer,intent(in)::BUSN,PVN
        real,intent(inout)::DTHEV(:),DVV(:)

        real::JOCMAT_TEMP(2*BUSN-PVN-2,2*BUSN-PVN-2),EQB_TEMP(2*BUSN-PVN-2)
        integer::i=1

        JOCMAT_TEMP=0.
        EQB_TEMP=0.
        JOCMAT_TEMP=JOCMAT(1:2*BUSN-PVN+1,1:2*BUSN-PVN+1)

        do while(i<=BUSN-1)
            EQB_TEMP(i)=DPV(i)
            i=i+1
        end do
        i=1
        do while(i<=BUSN-PVN-1)
            EQB_TEMP(BUSN-1+i)=DQV(i)
            i=i+1
        end do

        !call gesv(JOCMAT_TEMP,EQB_TEMP)

        i=1
        do while(i<=BUSN-1)
            DTHEV(i)=EQB_TEMP(i)
            i=i+1
        end do
        i=1
        do while(i<=BUSN-PVN-1)
            DVV(i)=EQB_TEMP(BUSN-1+i)
            i=i+1
        end do

    end subroutine NLJOSVUP
end module NLPFJMSV