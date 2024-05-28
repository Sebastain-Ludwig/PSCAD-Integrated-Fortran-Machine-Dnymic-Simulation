module PQPFELUP
    use iso_fortran_env,only:stdout=>output_unit,&
            stdin=> input_unit,&
            stderr=>error_unit
    implicit none
    contains
    subroutine PQHMELUP(V1,V2,B,HELE)
        real,intent(in)::V1,V2,B
        real,intent(inout)::HELE

        HELE=V1*V2*B
    end subroutine PQHMELUP

    subroutine PQHMDEUP(V,B,HELED)
        real,intent(in)::V,B
        real,intent(inout)::HELED

        HELED=V*V*B
    end subroutine PQHMDEUP

    subroutine PQLMELUP(V1,V2,B,LELE)
        real,intent(in)::V1,V2,B
        real,intent(inout)::LELE

        LELE=V1*V2*B
    end subroutine PQLMELUP

    subroutine PQLMDEUP(V,B,LELED)
        real,intent(in)::V,B
        real,intent(inout)::LELED

        LELED=V*V*B
    end subroutine PQLMDEUP
end module PQPFELUP