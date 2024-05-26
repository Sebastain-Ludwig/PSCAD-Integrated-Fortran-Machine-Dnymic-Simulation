# 1 "/home/sebastian/Project/POWERSYSDYN/FORTRAN SCRIPTS/MACHINE-DYNAMIC-SIMULATION/Testing/GEN_test.f90"
program GENTESTS
    use COMPUP,only:GEN1XUP1,GEN1NETUP1,GEN1ISUP1,GEN1INIT1,GEN1ADD1
    use iso_fortran_env,only:stdout => output_unit, &
            stdin => input_unit, &
            stderr => error_unit
    implicit none

!TEST GEN DYNAMIC CALCULATION
!DEGREE 30 C,ANGLE SPEED 1.0 PU,DELT TIME 0.001S
    real::DELK=30*3.14/180,WK=1.0,RA=0.001,XQ=0.0969,XD=0.0608,&
            PMK=1.01,PEK=1.0,DELT=0.001,H=5.,WS=1.0
    real::G1=-1.,G2=-1.,B1=-1.,B2=-1.
    real::ED=0.0,EQ=1.056,Ix=0.,Iy=0.
    real::Y(18,18)
    real:: DELT0=0.,W0=0.,PM0=0.,PE0=0.
    complex::EQ0=cmplx(0.,0.),EQ1=cmplx(0.,0.)

    Y=.0
    call GEN1XUP1(DELK,WK,PMK,PEK,DELT,H,WS)
    write(stdout,*)"GEN1XUP1:1.DELK NEXT 2.WK NEXT:",DELK,WK
!call GEN1NETUP1(DELK,G1,G2,B1,B2,RA,XQ,XD)
!write(stdout,*) "GEN1NETUP1:",G1,G2,B1,B2
!call GEN1ISUP1(DELK,ED,EQ,RA,XQ,XD,Ix,Iy)
!write(stdout,*)"GEN1ISUP1:1.Ix 2.Iy :",Ix,Iy

    call GEN1INIT1(1.0,0.,0.716,0.27,0.0,0.146,0.0608,EQ0,EQ1,DELT0,W0,PM0,PE0)
    write(stdout,*)EQ0,EQ1,DELT0,W0,PM0,PE0
end program GENTESTS
