# 1 "/home/sebastian/Project/POWERSYSDYN/FORTRAN SCRIPTS/MACHINE-DYNAMIC-SIMULATION/NETSOLVERS/LOADUP.f90"
! Created by sebastian on 24-4-30.

module LOADUP
    use iso_fortran_env, only: stdout => output_unit, &
            stdin => input_unit, &
            stderr => error_unit, &
            real64
    implicit none
    private::YMATBRAUP1
    public::LOAD1ADD1
contains
    subroutine YMATBRAUP1(IND1,IND2,A11,A12,A21,A22,A)
        integer,intent(in)::IND1,IND2
        real,intent(in)::A11,A12,A21,A22
        real,intent(inout)::A(:,:)

        integer::INDI,INDJ

        INDI=2*(IND1-1)+1
        INDJ=2*(IND2-1)+1

        A(INDI,INDJ)=A(INDI,INDJ)+A11
        A(INDI,INDJ+1)=A(INDI,INDJ+1)+A12
        A(INDI+1,INDJ)=A(INDI+1,INDJ)+A21
        A(INDI+1,INDJ+1)=A(INDI+1,INDJ+1)+A22
    end subroutine YMATBRAUP1

    subroutine LOAD1INIT1(V0ABS,P0,Q0,G0,B0)
        real,intent(in)::V0ABS,P0,Q0
        real,intent(out)::G0,B0

        complex::S0,Y0

        S0=cmplx(P0,Q0)

        Y0=conjg(S0)/(V0ABS**2)

        G0=real(Y0)
        B0=imag(Y0)
    end subroutine LOAD1INIT1

    subroutine LOAD1ADD1(G,B,INDLOAD,Y_MAT)
        real,intent(in)::G,B
        real,intent(inout)::Y_MAT(:,:)

!IND:INDEX
        integer,intent(in)::INDLOAD

        call YMATBRAUP1(INDLOAD,INDLOAd,G,-B,B,G,Y_MAT)
    end subroutine LOAD1ADD1
end module LOADUP
