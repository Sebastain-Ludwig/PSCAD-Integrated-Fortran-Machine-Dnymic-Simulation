program GENTESTS
    use BUSUP,only:TRAN1ADD1,TRAN1DE1
    use COMPUP,only:GEN1INIT1,GEN1ADD1,GEN1ISUP1,GEN1XUP1,GEN1NETUP1,GEN1PE1
    use LOADUP,only:LOAD1INIT1,LOAD1ADD1
    use iso_fortran_env,only:stdout => output_unit, &
            stdin => input_unit, &
            stderr => error_unit
    use f95_precision,only:WP=>SP
    use lapack95,only:gesv
    implicit none

    !TEST BUS DYNAMIC CALCULATION
    !Y:2n*2n
    real::Y(18,18),Y_TEMP(18,18),I(18),V(18)
    real::DELT0_1,W0_1,PM0_1,PE0_1,G1_1GEN,G2_1GEN,B1_1GEN,B2_1GEN,PE_1GEN,&
            DELT0_2,W0_2,PM0_2,PE0_2,G1_2GEN,G2_2GEN,B1_2GEN,B2_2GEN,PE_2GEN,&
            DELT0_3,W0_3,PM0_3,PE0_3,G1_3GEN,G2_3GEN,B1_3GEN,B2_3GEN,PE_3GEN
    real::DELT_1,DELT_2,DELT_3,W_1,W_2,W_3,PE1,PE2,PE3,PL1,PL2,PL3
    real::G_LOAD1,G_LOAD2,G_LOAD3,B_LOAD1,B_LOAD2,B_LOAD3
    complex::EQ0_1,EQ10_1,EQ0_2,EQ10_2,EQ0_3,EQ10_3
    integer::it,file_num,fault_flag=0
    integer::BUSINC(9)=(/(it,it=1,9)/)
    real::DELT=0.001,TSPAN=200.,TNOW=0.
    real::RA1=0.,RA2=0.,RA3=0.,XD1=0.1460,XD2=0.8958,XD3=1.3125,&
            XD11=0.0608,XD12=0.1190,XD13=0.1813,XQ1=0.0969,XQ2=0.8645,&
            XQ3=1.2578,XQ11=0.0969,XQ12=0.1969,XQ13=0.2500
    logical::RECORD_EXIT

    Y=0.
    I=0.
    V=0.

    call GEN1INIT1(1.04,0.,0.716,0.2705,0.0,XD11,XD11,EQ0_1,EQ10_1,DELT0_1,W0_1,PM0_1,PE0_1)
    call GEN1ISUP1(DELT0_1,0.,abs(EQ10_1),0.0,XD11,XD11,1,I)
    call GEN1INIT1(1.025,9.28*3.14/180,1.63,0.0665,0.0,XD12,XD12,EQ0_2,EQ10_2,DELT0_2,W0_2,PM0_2,PE0_2)
    call GEN1ISUP1(DELT0_2,0.,abs(EQ10_2),0.0,XD12,XD12,2,I)
    call GEN1INIT1(1.025,4.6648*3.14/180,0.85,-0.1086,0.0,XD13,XD13,EQ0_3,EQ10_3,DELT0_3,W0_3,PM0_3,PE0_3)
    call GEN1ISUP1(DELT0_3,0.,abs(EQ10_3),0.0,XD13,XD13,3,I)

    call LOAD1INIT1(0.9956,1.2500,0.5000,G_LOAD1,B_LOAD1)
    call LOAD1INIT1(1.0137,0.9000,0.3000,G_LOAD2,B_LOAD2)
    call LOAD1INIT1(1.0159,1.0000,0.3500,G_LOAD3,B_LOAD3)

    call GEN1ADD1(DELT0_1,G1_1GEN,G2_1GEN,B1_1GEN,B2_1GEN,0.,XD11,XD11,1,Y)
    call GEN1ADD1(DELT0_2,G1_2GEN,G2_2GEN,B1_1GEN,B2_2GEN,0.,XD12,XD12,2,Y)
    call GEN1ADD1(DELT0_3,G1_3GEN,G2_3GEN,B1_3GEN,B2_3GEN,0.,XD13,XD13,3,Y)
    call TRAN1ADD1(4,5,1.365,-10.411,0.088,Y)
    call TRAN1ADD1(4,6,1.942,-10.511,0.079,Y)
    call TRAN1ADD1(5,7,1.187,-5.975,0.153,Y)
    call TRAN1ADD1(6,9,1.282,-5.588,0.179,Y)
    call TRAN1ADD1(7,8,1.617,-13.698,0.0745,Y)
    call TRAN1ADD1(8,9,1.155,-9.784,0.1045,Y)
    call TRAN1ADD1(1,4,0.000,-17.361,0.,Y)
    call TRAN1ADD1(2,7,0.000,-16.000,0.,Y)
    call TRAN1ADD1(3,9,0.000,-17.065,0.,Y)
    call LOAD1ADD1(G_LOAD1,B_LOAD1,5,Y)
    call LOAD1ADD1(G_LOAD2,B_LOAD2,6,Y)
    call LOAD1ADD1(G_LOAD3,B_LOAD3,8,Y)

    DELT_1=DELT0_1
    DELT_2=DELT0_2
    DELT_3=DELT0_3

    W_1=W0_1
    W_2=W0_2
    W_3=W0_3
    inquire(file="1.dat",exist=RECORD_EXIT)
    if (RECORD_EXIT)then
        open(newunit=file_num,file="1.dat")
        rewind(file_num)
        write(stdout,*)W_1,W_2,W_3
    else
        open(newunit=file_num, file="1.dat", status="new", action="write")
        rewind(file_num)
        write(stdout,*)W_1,W_2,W_3
    end if

    do while(TNOW<=TSPAN)
        if (TNOW>10 .and. fault_flag==0)then
            call TRAN1DE1(5,7,1.187,-5.975,0.,Y)
            fault_flag=1
        end if

        V=I
        Y_TEMP=Y
        call gesv(Y_TEMP,V)


        call GEN1PE1(PE_1GEN,I(1),I(2),V(1),V(2),G1_1GEN,G2_1GEN,B1_1GEN,B2_1GEN)
        call GEN1PE1(PE_2GEN,I(3),I(4),V(3),V(4),G1_2GEN,G2_2GEN,B1_2GEN,B2_2GEN)
        call GEN1PE1(PE_3GEN,I(5),I(6),V(5),V(6),G1_3GEN,G2_3GEN,B1_3GEN,B2_3GEN)
        call GEN1XUP1(DELT_1,W_1,0.7164,PE_1GEN,DELT,47.,2*3.14*50.)
        call GEN1XUP1(DELT_2,W_2,1.0-(W_2-1.0),PE_2GEN,DELT,5.,2*3.14*50.)
        call GEN1XUP1(DELT_3,W_3,0.8500,PE_3GEN,DELT,6.,2*3.14*50.)
        call GEN1NETUP1(DELT_1,G1_1GEN,G2_1GEN,B1_1GEN,B2_1GEN,0.,XD11,XQ1,1,Y)
        call GEN1NETUP1(DELT_2,G1_2GEN,G2_2GEN,B1_2GEN,B2_2GEN,0.,XD12,XQ2,2,Y)
        call GEN1NETUP1(DELT_3,G1_3GEN,G2_3GEN,B1_3GEN,B2_3GEN,0.,XD13,XQ3,3,Y)
        call GEN1ISUP1(DELT_1,0.,abs(EQ10_1),0.,XD11,XD11,1,I)
        call GEN1ISUP1(DELT_2,0.,abs(EQ10_2),0.,XD12,XD12,2,I)
        call GEN1ISUP1(DELT_3,0.,abs(EQ10_3),0.,XD13,XD13,3,I)

        TNOW=TNOW+DELT


        write(file_num,'(4(f15.7,x))')TNOW,W_1, W_2, W_3
    end do

    write(stdout,'(3(f12.7,x))')W_1, W_2, W_3
    close(file_num)

    !write(file_num,'(18(f8.4,x))')Y

end program GENTESTS