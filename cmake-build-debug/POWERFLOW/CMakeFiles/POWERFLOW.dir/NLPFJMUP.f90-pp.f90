# 1 "/home/sebastian/Project/POWERSYSDYN/FORTRAN SCRIPTS/MACHINE-DYNAMIC-SIMULATION/POWERFLOW/NLPFJMUP.f90"
module NLPFJMUP
    use iso_fortran_env,only:stdout=>output_unit,&
            stdin=> input_unit,&
            stderr=>error_unit
    use NLPFELUP
    implicit none
    contains

!subroutine NLJOMTIN(BUSN,PVN,JOMAT)
!    integer,intent(in)::BUSN,PVN
!    real,intent(out)::JOMAT(:,:)

!    JOMAT=zeros
!end subroutine NLJOMTIN

    subroutine NLJOMTUP(PV,QV,TV,VV,GMAT,BMAT,BUSN,PVN,JOCMAT)
        real,intent(in)::PV(:),QV(:),TV(:),VV(:),GMAT(:,:),BMAT(:,:)
        integer,intent(in)::BUSN,PVN
        real,intent(inout)::JOCMAT(:,:)

        integer::i,j
        i=2
        j=2

        do while(i<=BUSN)
            j=2
            do while(j<=BUSN)
                if (i/=j)then
                    call NLHMELUP(VV(i),VV(j),GMAT(i, j),BMAT(i, j),TV(i)-TV(j),JOCMAT(i-1, j-1))
                    call NLNMELUP(VV(i),VV(j),GMAT(i, j),BMAT(i, j),TV(i)-TV(j),JOCMAT(i-1, BUSN-1+j-1))
                else
                    call NLHDELUP(VV(i),BMAT(i, i),QV(i),JOCMAT(i-1, j-1))
                    call NLNDELUP(VV(i),BMAT(i, i),PV(i),JOCMAT(i-1,BUSN-1+j-1))
                end if
                j=j+1
            end do
            i=i+1
        end do
        i=2
        j=2

        do while(i<=BUSN-PVN-1)
            j=2
            do while(j<=BUSN-1)
                if (i/=j)then
                    call NLJMELUP(VV(i),VV(j),GMAT(i, j),BMAT(i, j),TV(i)-TV(j),JOCMAT(BUSN-1+i-1, j-1))
                    call NLLMELUP(VV(i),VV(j),GMAT(i, j),BMAT(i, j),TV(i)-TV(j),JOCMAT(BUSN-1+i-1, BUSN-1+j-1))
                else
                    call NLJDELUP(VV(i),BMAT(i, i),QV(i),JOCMAT(BUSN-1+i-1, j-1))
                    call NLLDELUP(VV(i),BMAT(i, i),PV(i),JOCMAT(BUSN-1+i-1, BUSN-1+j-1))
                end if
                j=j+1
            end do
            i=i+1
        end do
    end subroutine NLJOMTUP

    subroutine NLPVMTUP(TV,VV,GMAT,BMAT,BUSN,PV)
!N-1
        real,intent(in)::TV(:),VV(:),GMAT(:,:),BMAT(:,:)
        integer,intent(in)::BUSN
        real,intent(inout)::PV(:)

        integer::i

        i=1

        do while(i<=BUSN)
            call NLPVELUP(TV,VV,GMAT,BMAT,i,9,PV(i))
            i=i+1
        end do
    end subroutine NLPVMTUP

    subroutine NLQVMTUP(TV,VV,GMAT,BMAT,BUSN,QV)
!N-1
        real,intent(in)::TV(:),VV(:),GMAT(:,:),BMAT(:,:)
        integer,intent(in)::BUSN
        real,intent(inout)::QV(:)

        integer::i

        i=1

        do while(i<=BUSN)
            call NLQVELUP(TV,VV,GMAT,BMAT,i,9,QV(i))
            i=i+1
        end do
    end subroutine NLQVMTUP

    subroutine NLTVMTUP(DTHEV,TV)
        real,intent(in)::DTHEV(:)
        real,intent(inout)::TV(:)

        TV=TV-DTHEV
    end subroutine NLTVMTUP

    subroutine  NLVVMTUP(DVV,VV)
        real,intent(in)::DVV(:)
        real,intent(inout)::VV(:)
        integer::DVVNUM,VVNUM,i

        DVVNUM=size(DVV,1)
        VVNUM=size(VV,1)
        i=1

        do while(i<=VVNUM)
            if (i<=DVVNUM)then
                VV(i)=-DVV(i)*VV(i)+VV(i)
            else
                VV(i)=VV(i)
            end if
            i=i+1
        end do

    end subroutine NLVVMTUP
end module NLPFJMUP
