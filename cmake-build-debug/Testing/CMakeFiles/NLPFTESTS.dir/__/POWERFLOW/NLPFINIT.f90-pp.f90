# 1 "/home/sebastian/Project/POWERSYSDYN/FORTRAN SCRIPTS/MACHINE-DYNAMIC-SIMULATION/POWERFLOW/NLPFINIT.f90"
module NLPFINIT
    use iso_fortran_env,only:stdout=>output_unit,&
            stdin=> input_unit,&
            stderr=>error_unit
    implicit none
    contains
    subroutine NLBUTYAD(PV_SET,QV_SET,TV_IN,VV_IN,P,Q,T,V,BUS1,TYPEN)
        real,intent(inout)::PV_SET(:),QV_SET(:),TV_IN(:),VV_IN(:)
        real,intent(in)::P,Q,V,T
        integer,intent(in)::TYPEN,BUS1

        if (TYPEN==1)then!PQ BUS
            PV_SET(BUS1)=P
            QV_SET(BUS1)=Q
            TV_IN(BUS1)=T
            VV_IN(BUS1)=V
        elseif(TYPEN==2)then
            PV_SET(BUS1)=P
            QV_SET(BUS1)=Q
            TV_IN(BUS1)=T
            VV_IN(BUS1)=V
        else
            PV_SET(BUS1)=P
            QV_SET(BUS1)=Q
            TV_IN(BUS1)=T
            VV_IN(BUS1)=V
        end if
    end subroutine NLBUTYAD

    subroutine NLGBRXAD(G_MAT,B_MAT,BUS1,BUS2,R,X,BGR)
        real,intent(in)::R,X,BGR
        real,intent(inout)::G_MAT(:,:),B_MAT(:,:)
        integer::BUS1,BUS2

        real::G,B

        G=R/(R*R+X*X)
        B=-X/(R*R+X*X)

        G_MAT(BUS1,BUS1)=G_MAT(BUS1,BUS1)+G
        G_MAT(BUS2,BUS2)=G_MAT(BUS2,BUS2)+G
        G_MAT(BUS1,BUS2)=G_MAT(BUS1,BUS2)-G
        G_MAT(BUS2,BUS1)=G_MAT(BUS2,BUS1)-G

        B_MAT(BUS1,BUS1)=B_MAT(BUS1,BUS1)+B+BGR/2.
        B_MAT(BUS2,BUS2)=B_MAT(BUS2,BUS2)+B+BGR/2.
        B_MAT(BUS1,BUS2)=B_MAT(BUS1,BUS2)-B
        B_MAT(BUS2,BUS1)=B_MAT(BUS2,BUS1)-B
    end subroutine NLGBRXAD

    subroutine NLBGGEAD(G_MAT,B_MAT,BUS1,XD)
        real,intent(inout)::G_MAT(:,:),B_MAT(:,:)
        integer::BUS1
        real,intent(in)::XD

        G_MAT(BUS1,BUS1)=G_MAT(BUS1,BUS1)
        B_MAT(BUS1,BUS1)=B_MAT(BUS1,BUS1)+1./XD
    end subroutine NLBGGEAD

    subroutine NLBGLDAD(G_MAT,B_MAT,BUS1,RL)
        real,intent(inout)::G_MAT(:,:),B_MAT(:,:)
        integer::BUS1
        real,intent(in)::RL

        G_MAT(BUS1,BUS1)=G_MAT(BUS1,BUS1)+1./RL
        B_MAT(BUS1,BUS1)=B_MAT(BUS1,BUS1)
    end subroutine NLBGLDAD
end module NLPFINIT
