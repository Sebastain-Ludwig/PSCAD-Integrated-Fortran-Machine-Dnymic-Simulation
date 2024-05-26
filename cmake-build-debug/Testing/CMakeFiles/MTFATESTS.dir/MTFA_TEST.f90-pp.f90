# 1 "/home/sebastian/Project/POWERSYSDYN/FORTRAN SCRIPTS/MACHINE-DYNAMIC-SIMULATION/Testing/MTFA_TEST.f90"

program MTFA_TEST
    use iso_fortran_env, only: stdout => output_unit, &
            stdin => input_unit, &
            stderr => error_unit, &
            real64
    use BAMTDEFA
    use BAMTFASV
    implicit none
    real::MAT(3,3),L(3,3),U(3,3),B(3),X(3)

    MAT=reshape((/1.,2.,3.,3.,2.,1.,3.,2.,2./),[3,3])
    B=(/2.,2.,1./)
    X=0.
    call BAMTDOLU(MAT,L,U)
    call BAMTDOSV(L,U,B,X)
    write(stdout,*) MAT,L,U,X
end program MTFA_TEST
