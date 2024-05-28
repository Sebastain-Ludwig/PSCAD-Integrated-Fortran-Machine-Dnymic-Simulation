module PQPFJMSV
    use iso_fortran_env,only:stdout=>output_unit,&
            stdin=> input_unit,&
            stderr=>error_unit
    !use f95_precision,only:WP=>SP
    !use lapack95,only:GETRF,gesv
    use BAMTFASV
    use BAMTDEFA
    implicit none

contains
    subroutine PQJOSVIN(DPV,DQV,DTHEV,DVV,BUSN,PVN,JOCMAT,LM,UM)
        real,intent(in)::DPV(:),DQV(:),JOCMAT(:,:)
        integer,intent(in)::BUSN,PVN
        real,intent(inout)::DTHEV(:),DVV(:),LM(:,:),UM(:,:)

        real::JOCMAT_TEMP(2*BUSN-PVN-2,2*BUSN-PVN-2),EQB_TEMP(2*BUSN-PVN-2),XX(2*BUSN-PVN+2)
        integer::i=1

        JOCMAT_TEMP=0.
        EQB_TEMP=1.

        JOCMAT_TEMP(1:BUSN-1,1:BUSN-1)=JOCMAT(1:BUSN-1,1:BUSN-1)
        JOCMAT_TEMP(1:BUSN-1,BUSN:2*BUSN-PVN-2)=JOCMAT(1:BUSN-1,BUSN+PVN:2*BUSN)
        JOCMAT_TEMP(BUSN:2*BUSN-PVN-2,1:BUSN-1)=JOCMAT(BUSN+PVN:2*BUSN,1:BUSN-1)
        JOCMAT_TEMP(BUSN:2*BUSN-PVN-2,BUSN:2*BUSN-PVN-2)=JOCMAT(BUSN+PVN:2*BUSN,BUSN+PVN:2*BUSN)

        EQB_TEMP(1:BUSN-1)=DPV(2:BUSN)
        EQB_TEMP(BUSN:2*BUSN-PVN+1)=DQV(2+PVN:BUSN)

        XX=0.

        call BAMTDOLU(JOCMAT_TEMP,LM,UM)
        call BAMTDOSV(LM,UM,EQB_TEMP,XX)
        DTHEV(2:BUSN)=XX(1:BUSN-1)
        DVV(2+PVN:BUSN)=XX(BUSN:2*BUSN-PVN-2)
    end subroutine PQJOSVIN

    subroutine PQJOSVUP(DPV,DQV,DTHEV,DVV,BUSN,PVN,JOCMAT,LM,UM)
        real,intent(in)::DPV(:),DQV(:),JOCMAT(:,:),LM(:,:),UM(:,:)
        integer,intent(in)::BUSN,PVN
        real,intent(inout)::DTHEV(:),DVV(:)

        real::JOCMAT_TEMP(2*BUSN-PVN-2,2*BUSN-PVN-2),EQB_TEMP(2*BUSN-PVN-2),XX(2*BUSN-PVN+2)
        integer::i=1

        JOCMAT_TEMP=0.
        EQB_TEMP=1.

        JOCMAT_TEMP(1:BUSN-1,1:BUSN-1)=JOCMAT(1:BUSN-1,1:BUSN-1)
        JOCMAT_TEMP(1:BUSN-1,BUSN:2*BUSN-PVN-2)=JOCMAT(1:BUSN-1,BUSN+PVN:2*BUSN)
        JOCMAT_TEMP(BUSN:2*BUSN-PVN-2,1:BUSN-1)=JOCMAT(BUSN+PVN:2*BUSN,1:BUSN-1)
        JOCMAT_TEMP(BUSN:2*BUSN-PVN-2,BUSN:2*BUSN-PVN-2)=JOCMAT(BUSN+PVN:2*BUSN,BUSN+PVN:2*BUSN)

        EQB_TEMP(1:BUSN-1)=DPV(2:BUSN)
        EQB_TEMP(BUSN:2*BUSN-PVN+1)=DQV(2+PVN:BUSN)

        XX=0.

        call BAMTDOSV(LM,UM,EQB_TEMP,XX)
        DTHEV(2:BUSN)=XX(1:BUSN-1)
        DVV(2+PVN:BUSN)=XX(BUSN:2*BUSN-PVN-2)
    end subroutine PQJOSVUP
end module PQPFJMSV