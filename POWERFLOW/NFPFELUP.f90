module NLPFELUP
    use iso_fortran_env,only:stdout=>output_unit,&
            stdin=> input_unit,&
            stderr=>error_unit
    implicit none
    public::NLHMELUP,NLHDELUP,NLNMELUP,NLNDELUP,NLJMELUP,NLJDELUP,NLLMELUP,NLLDELUP,NLPVELUP,NLQVELUP
    contains
    subroutine NLHMELUP(V1,V2,G,B,THE,HELE)
        real,intent(in)::V1,V2,G,B,THE
        real,intent(inout)::HELE

        HELE=-1*V1*V2*(G*sind(THE)-B*cosd(THE))
    end subroutine NLHMELUP

    subroutine NLHDELUP(V,B,Q,HELED)
        real,intent(in)::V,B,Q
        real,intent(inout)::HELED

        HELED=V*V*B+Q
    end subroutine NLHDELUP

    subroutine NLNMELUP(V1,V2,G,B,THE,NELE)
        real,intent(in)::V1,V2,G,B,THE
        real,intent(inout)::NELE

        NELE=-1*V1*V2*(G*cosd(THE)+B*sind(THE))
    end subroutine NLNMELUP

    subroutine NLNDELUP(V,G,P,NELED)
        real,intent(in)::V,G,P
        real,intent(inout)::NELED

        NELED=-V*V*G-P
    end subroutine NLNDELUP

    subroutine NLJMELUP(V1,V2,G,B,THE,JELE)
        real,intent(in)::V1,V2,G,B,THE
        real,intent(inout)::JELE

        JELE=V1*V2*(G*cosd(THE)+B*sind(THE))
    end subroutine NLJMELUP

    subroutine NLJDELUP(V,G,P,JELED)
        real,intent(in)::V,G,P
        real,intent(inout)::JELED

        JELED=V*V*G-P
    end subroutine NLJDELUP

    subroutine NLLMELUP(V1,V2,G,B,THE,LELE)
        real,intent(in)::V1,V2,G,B,THE
        real,intent(inout)::LELE

        LELE=-1*V1*V2*(G*sind(THE)-B*cosd(THE))
    end subroutine NLLMELUP

    subroutine NLLDELUP(V,B,Q,LELED)
        real,intent(in)::V,B,Q
        real,intent(inout)::LELED

        LELED=V*V*B-Q
    end subroutine NLLDELUP

    subroutine NLPVELUP(TV,VV,GMAT,BMAT,BUS1,BUSN,PELE)
        real,intent(in)::TV(:),VV(:),GMAT(:,:),BMAT(:,:)
        integer,intent(in)::BUS1,BUSN
        real,intent(inout)::PELE

        integer::i


        PELE=0.
        i=1

        do while(i<=BUSN)
            PELE=PELE+VV(BUS1)*VV(i)*(GMAT(BUS1, i)*cosd(TV(BUS1)-TV(i))+BMAT(BUS1,i)*sind(TV(BUS1)-TV(i)))
            i=i+1
        end do
    end subroutine NLPVELUP

    subroutine NLQVELUP(TV,VV,GMAT,BMAT,BUS1,BUSN,QELE)
        real,intent(in)::TV(:),VV(:),GMAT(:,:),BMAT(:,:)
        integer,intent(in)::BUS1,BUSN
        real,intent(inout)::QELE

        integer::i=1

        QELE=0.
        i=1

        do while(i<=BUSN)
            QELE=QELE+VV(BUS1)*VV(i)*(GMAT(BUS1, i)*sind(TV(BUS1)-TV(i))-BMAT(BUS1,i)*cosd(TV(BUS1)-TV(i)))
            i=i+1
        end do
    end subroutine NLQVELUP
end module NLPFELUP