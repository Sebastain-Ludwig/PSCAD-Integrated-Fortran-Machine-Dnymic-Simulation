program NLPF_TEST
        use NLPFJMUP
        use NLPFJMSV
        use NLPFINIT

        use iso_fortran_env,only:stdout => output_unit, &
                stdin => input_unit, &
                stderr => error_unit
        use BAMTDEFA
        use BAMTFASV
       ! use f95_precision,only:WP=>SP
       ! use lapack95,only:gesvx
    implicit none
        real,dimension(9)::BR_MAT(9),BX_MAT(9),P_SET(9),Q_SET(9),V_INIT(9),&
        T_INIT(9),PV(9),QV(9),DPV(9),DQV(9),DTHEV(9),DVV(9),TV(9),VV(9),XX(14),&
                LM(14,14),UM(14,14)
        real::G_MAT(9,9),B_MAT(9,9),JOCMAT(16,16)=0.,J_TEMP(14,14),BEQ_TEMP(14)
        integer,dimension(9)::BUS_TYPE=(/3,2,2,1,1,1,1,1,1/)
        integer::ITERN

        BR_MAT=(/0.,0.017,0.039,0.,0.0119, 0.0085, 0., 0.032, 0.01/)
        BX_MAT=(/0.0576, 0.092, 0.17, 0.0586, 0.1008, 0.072, 0.0625, 0.161, 0.085/)
        P_SET=0.
        Q_SET=0.
        G_MAT=0.
        B_MAT=0.
        DPV=0.
        DQV=0.
        DTHEV=0.
        DVV=0.
        ITERN=0
        J_TEMP=0.
        BEQ_TEMP=0.

        call NLBUTYAD(P_SET,Q_SET,T_INIT,V_INIT,0.0000,0.0000,0.0000,1.0400,1,3)
        call NLBUTYAD(P_SET,Q_SET,T_INIT,V_INIT,1.6300,0.0654,0.0000,1.0250,2,2)
        call NLBUTYAD(P_SET,Q_SET,T_INIT,V_INIT,0.8500,-0.110,0.0000,1.0250,3,2)
        call NLBUTYAD(P_SET,Q_SET,T_INIT,V_INIT,0.0000,0.0000,0.0000,1.0000,4,1)
        call NLBUTYAD(P_SET,Q_SET,T_INIT,V_INIT,-0.9000,-0.3000,0.0000,1.0000,5,1)
        call NLBUTYAD(P_SET,Q_SET,T_INIT,V_INIT,0.0000,0.0000,0.0000,1.0000,6,1)
        call NLBUTYAD(P_SET,Q_SET,T_INIT,V_INIT,-1.0000,-0.3500,0.0000,1.0000,7,1)
        call NLBUTYAD(P_SET,Q_SET,T_INIT,V_INIT,0.0000,0.0000,0.0000,1.0000,8,1)
        call NLBUTYAD(P_SET,Q_SET,T_INIT,V_INIT,-1.2500,-0.5000,0.0000,1.0000,9,1)

        call NLGBRXAD(G_MAT,B_MAT,1,4,BR_MAT(1),BX_MAT(1),0.0000)
        call NLGBRXAD(G_MAT,B_MAT,4,5,BR_MAT(2),BX_MAT(2),0.1580)
        call NLGBRXAD(G_MAT,B_MAT,5,6,BR_MAT(3),BX_MAT(3),0.3580)
        call NLGBRXAD(G_MAT,B_MAT,3,6,BR_MAT(4),BX_MAT(4),0.0000)
        call NLGBRXAD(G_MAT,B_MAT,6,7,BR_MAT(5),BX_MAT(5),0.2090)
        call NLGBRXAD(G_MAT,B_MAT,7,8,BR_MAT(6),BX_MAT(6),0.1490)
        call NLGBRXAD(G_MAT,B_MAT,8,2,BR_MAT(7),BX_MAT(7),0.0000)
        call NLGBRXAD(G_MAT,B_MAT,8,9,BR_MAT(8),BX_MAT(8),0.3060)
        call NLGBRXAD(G_MAT,B_MAT,9,4,BR_MAT(9),BX_MAT(9),0.1760)


        call NLPVMTUP(T_INIT,V_INIT,G_MAT,B_MAT,9,PV)
        call NLQVMTUP(T_INIT,V_INIT,G_MAT,B_MAT,9,QV)
        call NLJOMTUP(PV, QV, T_INIT,V_INIT,G_MAT,B_MAT,9,2,JOCMAT)
        DPV=P_SET-PV
        DQV=Q_SET-QV
        TV=T_INIT
        VV=V_INIT


        call NLJOSVUP(DPV,DQV,DTHEV,DVV,9,2,JOCMAT)

        call NLTVMTUP(DTHEV,TV)
        call NLVVMTUP(DVV,VV)

        do while(norm2(DPV(2:9))>=1e-1)
            call NLPVMTUP(TV,VV,G_MAT,B_MAT,9,PV)
            call NLQVMTUP(TV,VV,G_MAT,B_MAT,9,QV)
            call NLJOMTUP(PV, QV, TV,VV,G_MAT,B_MAT,9,2,JOCMAT)
            DPV=P_SET-PV
            DQV=Q_SET-QV
            call NLJOSVUP(DPV(2:9),DQV(4:9),DTHEV(2:9),DVV(4:9),9,2,JOCMAT)

            call NLJOSVUP(DPV,DQV,DTHEV,DVV,9,2,JOCMAT)

            call NLTVMTUP(DTHEV,TV)
            call NLVVMTUP(DVV,VV)
            write(*,*)DPV
            ITERN=ITERN+1
            write(*, *)ITERN
        end do
        write(*,*)VV
end program NLPF_TEST