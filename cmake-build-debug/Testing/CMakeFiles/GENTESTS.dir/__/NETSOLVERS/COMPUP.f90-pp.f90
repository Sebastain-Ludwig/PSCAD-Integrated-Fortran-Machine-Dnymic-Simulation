# 1 "/home/sebastian/Project/POWERSYSDYN/FORTRAN SCRIPTS/MACHINE-DYNAMIC-SIMULATION/NETSOLVERS/COMPUP.f90"
! Created by sebastian on 24-4-26.

module COMPUP
    use iso_fortran_env, only: stdout => output_unit, &
            stdin => input_unit, &
            stderr => error_unit, &
            real64
    implicit none
    private::YMATBRAUP1,IMATBRAUP1
    public::GEN1PARA1,GEN1INIT1,GEN1ADD1,GEN1XUP1,GEN1NETUP1,GEN1ISUP1
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

    subroutine IMATBRAUP1(IND,IX,IY,I)
        integer::IND,INDI
        real::IX,IY
        real::I(:)

        INDI=2*(IND-1)+1
        I(INDI)=IX
        I(INDI+1)=IY

    end subroutine IMATBRAUP1

    subroutine GEN1PARA1(RA,XQ,XD,PARAMS)
!GENERATOR TYPE 1 -- MACINE PARAMETERS P.U. RA XQ XD -- VAR VASED PARAMETERS

        real,intent(in)::RA,XQ,XD
        real,intent(out)::PARAMS(3)

        PARAMS=-1.
        PARAMS(1)=RA
        PARAMS(2)=XQ
        PARAMS(3)=XD

        if (abs(PARAMS(1)-(-1.))<=1e-5)then
            stop 'GEN1PARA1 ERROR:ILLIGAL PARAM:RA'
        end if
        if (abs(PARAMS(2)-(-1.))<=1e-5)then
            stop 'GEN1PARA1 ERROR:ILLIGAL PARAM:XQ'
        end if
        if (abs(PARAMS(3)-(-1.))<=1e-5)then
            stop 'GEN1PARA1 ERROR:ILLIGAL PARAM:XD'
        end if

    end subroutine GEN1PARA1

    subroutine GEN1INIT1(VABS,DELTV0,P0,Q0,RA,XQ,XD1,EQ0,EQ1,DELT0,W0,PM0,PE0)
        real,intent(in)::VABS,DELTV0,P0,Q0,RA,XQ,XD1
        complex,intent(out)::EQ0,EQ1
        real,intent(out)::DELT0,W0,PM0,PE0

        complex::S0,V0,I0
        real::EQ1ABS,VD0,VQ0,ID0,IQ0,VX0,VY0

        VX0=VABS*cos(DELTV0)
        VY0=VABS*sin(DELTV0)

        S0=cmplx(P0,Q0)
        V0=cmplx(VX0,VY0)
        I0=conjg(S0)/conjg(V0)
        EQ0=V0+cmplx(RA,XQ)*I0
        DELT0=atan(imag(EQ0),real(EQ0))
        W0=1.0

        VD0=sin(DELT0)*VX0-cos(DELT0)*VY0
        VQ0=cos(DELT0)*VX0+sin(DELT0)*VY0

        ID0=sin(DELT0)*real(I0)-cos(DELT0)*imag(I0)
        IQ0=cos(DELT0)*real(I0)+sin(DELT0)*imag(I0)

        EQ1ABS=VQ0+RA*IQ0+XD1*ID0
        EQ1=cmplx(EQ1ABS*cos(DELT0),EQ1ABS*sin(DELT0))
        PM0=P0+I0**2*RA
        PE0=PM0

    end subroutine GEN1INIT1

    subroutine GEN1ADD1(DELT0,G10,G20,B10,B20,RA,XD,XQ,INDGEN,Y_MAT)
        integer,intent(in)::INDGEN
        real,intent(in)::DELT0,RA,XD,XQ
        real,intent(out)::G10,G20,B10,B20
        real,intent(inout)::Y_MAT(:,:)

        real::G1,B1,G2,B2
        real::PARAMS(3)
        real::DEN1,NUM1

        PARAMS=-1.

        call GEN1PARA1(RA,XQ,XD,PARAMS)

        DEN1=(PARAMS(3)-PARAMS(2))*sin(DELT0)*cos(DELT0)
        NUM1=(PARAMS(1)**2+PARAMS(2)*PARAMS(3))
        G10=(PARAMS(1)-DEN1)/NUM1
        B10=(PARAMS(3)*cos(DELT0)**2-PARAMS(2)*sin(DELT0)**2)/NUM1
        B20=(-PARAMS(3)*sin(DELT0)**2-PARAMS(2)*cos(DELT0)**2)/NUM1
        G20=(PARAMS(1)+DEN1)/NUM1

        call YMATBRAUP1(INDGEN,INDGEN,G10,B10,B20,G20,Y_MAT)

    end subroutine GEN1ADD1

    subroutine GEN1XUP1(DELK,WK,PMK,PEK,DELT,H,WS)
!GENERATOR TYPE 1 -- STATE VARS P.U. X(K+1) -- UPDATE EULER METHOD
        real,intent(inout)::DELK,WK
        real,intent(in)::PMK,PEK
        real,intent(in)::DELT

        real,intent(in)::H,WS

        WK=((PMK-PEK)/(2.0*H)-0.01*WK)*DELT+WK
        DELK=(WS*(WK-1.)*DELT+DELK)
    end subroutine GEN1XUP1

    subroutine GEN1NETUP1(DELK,GLAST1,GLAST2,BLAST1,BLAST2,RA,XQ,XD,INDGEN,Y_MAT)
!GENERATOR TYPE 1 -- STATE NETY MATRIX P.U. [G1,B1;B2,G2] -- UPDATE ALGEBRAIC CALCULATE

        real,intent(in)::DELK,RA,XQ,XD
        real,intent(inout)::GLAST1,BLAST1,GLAST2,BLAST2
        integer,intent(in)::INDGEN
        real,intent(inout)::Y_MAT(:,:)

        real::G1,B1,G2,B2
        real::PARAMS(3)
        real::DEN1,NUM1

        PARAMS=-1.

        call GEN1PARA1(RA,XQ,XD,PARAMS)

        DEN1=(PARAMS(3)-PARAMS(2))*sin(DELK)*cos(DELK)
        NUM1=(PARAMS(1)**2+PARAMS(2)*PARAMS(3))
        G1=(PARAMS(1)-DEN1)/NUM1
        B1=(PARAMS(3)*cos(DELK)**2-PARAMS(2)*sin(DELK)**2)/NUM1
        B2=(-PARAMS(3)*sin(DELK)**2-PARAMS(2)*cos(DELK)**2)/NUM1
        G2=(PARAMS(1)+DEN1)/NUM1

        call YMATBRAUP1(INDGEN,INDGEN,G1-GLAST1,B1-BLAST1,B2-BLAST2,G2-GLAST2,Y_MAT)

        GLAST1=G1
        GLAST2=G2
        BLAST1=B1
        BLAST2=B2
    end subroutine GEN1NETUP1

    subroutine GEN1ISUP1(DELK,ED,EQ,RA,XQ,XD,INDI,I)
        real,intent(in)::DELK,ED,EQ,RA,XQ,XD
        real,intent(inout)::I(:)
        integer::INDI

        real::GX,BX,BY,GY,NUM1,IX,IY

        NUM1=RA**2+XQ*XD
        GX=(RA*sin(DELK)-XD*cos(DELK))/NUM1
        BX=(RA*cos(DELK)+XQ*sin(DELK))/NUM1
        BY=(-RA*cos(DELK)-XD*sin(DELK))/NUM1
        GY=(RA*sin(DELK)-XQ*cos(DELK))/NUM1

        Ix=GX*ED+BX*EQ
        Iy=BY*ED+GY*EQ

        call IMATBRAUP1(INDI,Ix,Iy,I)
    end subroutine GEN1ISUP1

    subroutine GEN1PE1(PE,ISX,ISY,VX,VY,G1,G2,B1,B2)
        real,intent(in)::ISX,ISY,VX,VY,G1,G2,B1,B2
        real,intent(out)::PE

        real::ITX,ITY

        ITX=ISX-G1*VX-B1*VY
        ITY=ISY-B2*VX-G2*VY

        PE=ITX*VX+ITY*VY

    end subroutine GEN1PE1
end module COMPUP
