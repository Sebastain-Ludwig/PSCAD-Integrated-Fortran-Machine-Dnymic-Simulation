# 1 "/home/sebastian/Project/POWERSYSDYN/FORTRAN SCRIPTS/MACHINE-DYNAMIC-SIMULATION/NETSOLVERS/SOLVENET.f90"
module SOLVENET
    use iso_fortran_env, only: stdout => output_unit, &
            stdin => input_unit, &
            stderr => error_unit, &
            real64
    implicit none

contains
!    subroutine SOLVENET1(Gxy,Ixy,Vxy)
!        !SOLVE DYNAMIC NODE VOLTAGE EQUATION BY LU FACTIZATION METHOD (LAPACK MKL)
!        real(WP)::Gxy(:,:)
!        real(WP)::Ixy(:,:)
!        real(WP)::Vxy(:,:)
!
!        integer::IPIV(10)
!        integer::INFO=0
!
!        call gesv(Gxy,Vxy)
!    end subroutine SOLVENET1
!
!    subroutine DQ2XY(DVAR,QVAR,XVAR,YVAR)
!
!    end subroutine DQ2XY

    subroutine NETYMUP(YMAT,Gx,Gy,Bx,By,NODE_POS)
        real,intent(in)::Gx,Gy,Bx,By
        real,intent(inout)::YMAT(:,:)
        integer,intent(in)::NODE_POS

        integer::Y_SIZE(2)
        integer::MAT_LU_INDEX

        Y_SIZE=shape(YMAT)
        MAT_LU_INDEX=2*NODE_POS+1

        if (Y_SIZE(1)<2 .or. Y_SIZE(2)<2)then
            stop 'UPDATE Y MATRIX ERROR:TOO SMALL Y MATIRX,CHECK PROGRAM AND MAKE SURE THR Y IS CORRECTLY SET'
        end if

        if (NODE_POS<=0)then
            stop 'UPDATE Y MATRIX ERROR:WRONG NODE POSITION INDEX'
        end if

        if (Y_SIZE(1)<2*NODE_POS .or. Y_SIZE(2)<2*NODE_POS)then
            stop 'UPDATE Y MATRIX ERROR:NO CORRECT POSITION FOR CURRENT NODE,CHECK Y OR CHECK THE INDEX OF BUSES'
        end if

        YMAT(MAT_LU_INDEX,MAT_LU_INDEX)=Gx
        YMAT(MAT_LU_INDEX,MAT_LU_INDEX+1)=Gy
        YMAT(MAT_LU_INDEX+1,MAT_LU_INDEX)=Bx
        YMAT(MAT_LU_INDEX+1,MAT_LU_INDEX+1)=By

    end subroutine NETYMUP
end module SOLVENET
