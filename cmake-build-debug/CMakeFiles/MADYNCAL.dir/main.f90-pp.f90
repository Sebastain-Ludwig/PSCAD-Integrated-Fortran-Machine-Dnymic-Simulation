# 1 "/home/sebastian/Project/POWERSYSDYN/FORTRAN SCRIPTS/MACHINE-DYNAMIC-SIMULATION/main.f90"
program main
    use iso_fortran_env, only: stdout => output_unit, &
            stdin => input_unit, &
            stderr => error_unit, &
            real64
    use f95_precision,only:WP=>SP
    use lapack95,only:GETRF,gesv
    use SOLVENET,only:NETYMUP
    use COMPUP,only:GEN1XUP1
    implicit none
    real(WP)::Gxy(2,2)
    real(WP)::Ixy(10,1)
    real(WP)::Vxy(2,1)
    real::DELK,WK,PMK,PEK,DELT,H

    complex::S0,V0

    DELK=0.2
    WK=1
    PMK=1.01
    PEK=1.0
    DELT=0.01
    H=5

    Gxy(1,1)=1.0
    Gxy(1,2)=2.0
    Gxy(2,1)=3.0
    Gxy(2,2)=1.5
    Vxy=1.0

    call gesv(Gxy,Vxy)
    write(*,*) Vxy

!call NETYMUP(Gxy,1.0,1.0,2.0,3.0,1)
!call gesv(Gxy,Vxy)
!write(*,*) Vxy

!call NETYMUP(Gxy,1.0,1.0,2.0,3.0,1)
!call gesv(Gxy,Vxy)
!write(*,*) Vxy
!call NETYMUP(Gxy,1.0,1.0,2.0,3.0,1)
!call gesv(Gxy,Vxy)
!write(*,*) Vxy

!call GEN1XUP1(DELK,WK,PMK,PEK,DELT,H,50*2*3.14)
!write(stdout,*) DELK,WK,PMK,PEK

    S0=cmplx(1.0,2.0)
    V0=cmplx(sqrt(3.),1.)

!write(stdout,*) abs(V0)

end program main
