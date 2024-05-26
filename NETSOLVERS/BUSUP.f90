module BUSUP
    use iso_fortran_env,only:stdout=>output_unit,&
            stdin=> input_unit,&
            stderr=>error_unit
    implicit none
    private::YMATBRAUP1
    public::TRAN1ADD1,TRAN1DE1
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

    subroutine TRAN1ADD1(BUS1,BUS2,G,B,Z,Y_MAT)
        integer,intent(in)::BUS1,BUS2
        real,intent(in)::G,B,Z
        real,intent(inout)::Y_MAT(:,:)

        !IND:INDEX
        integer::INDI,INDJ
        INDI=BUS1
        INDJ=BUS2

        call YMATBRAUP1(INDI,INDI,G,-B-Z,B+Z,G,Y_MAT)
        call YMATBRAUP1(INDJ,INDJ,G,-B-Z,B+Z,G,Y_MAT)
        call YMATBRAUP1(INDI,INDJ,-G,B,-B,-G,Y_MAT)
        call YMATBRAUP1(INDJ,INDI,-G,B,-B,-G,Y_MAT)

    end subroutine TRAN1ADD1

    subroutine TRAN1DE1(BUS1,BUS2,G,B,Z,Y_MAT)
        integer,intent(in)::BUS1,BUS2
        real,intent(in)::G,B,Z
        real,intent(inout)::Y_MAT(:,:)

        !IND:INDEX
        integer::INDI,INDJ
        INDI=2*BUS1+1
        INDJ=2*BUS2+1

        call YMATBRAUP1(INDI,INDI,-G,B+Z,-B-Z,-G,Y_MAT)
        call YMATBRAUP1(INDJ,INDJ,-G,B+Z,-B-Z,-G,Y_MAT)
        call YMATBRAUP1(INDI,INDJ,G,-B,B,G,Y_MAT)
        call YMATBRAUP1(INDJ,INDI,G,-B,B,G,Y_MAT)

    end subroutine TRAN1DE1
end module BUSUP