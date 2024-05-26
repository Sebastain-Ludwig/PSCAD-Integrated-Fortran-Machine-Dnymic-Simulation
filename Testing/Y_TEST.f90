program YTESTS
    use BUSUP,only:TRAN1ADD1,TRAN1DE1
    use iso_fortran_env,only:stdout => output_unit, &
            stdin => input_unit, &
            stderr => error_unit
    implicit none
    real::Y(10,10)
    Y=0.
    call TRAN1ADD1(1,2,0.624,-3.900,0.250,Y)
    call TRAN1ADD1(1,3,0.755,-2.642,0.000,Y)
    !call TRAN1ADD1()

    write(stdout,*) Y
end program YTESTS