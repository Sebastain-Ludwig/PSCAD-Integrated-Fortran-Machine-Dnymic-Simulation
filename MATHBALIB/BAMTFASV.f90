module BAMTFASV
    use iso_fortran_env, only: stdout => output_unit, &
            stdin => input_unit, &
            stderr => error_unit, &
            real64
    implicit none
contains
    subroutine BAMTDOSV(L,U,B,X)
        real,intent(in)::L(:,:),U(:,:),B(:)
        real,intent(inout)::X(:)

        integer::XN,i

        X=0.
        XN=size(B, 1)
        i=1

        X(1)=B(1)/L(1,1)

        do i = 2,XN
            X(i)=(B(i)-sum(L(i, 1:i-1)*X(1:i-1)))/L(i, i)
        end do

        X(XN)=X(XN)/U(XN,XN)
        do i = XN-1,1,-1
            X(i)=(X(i)-sum(U(i, i+1:XN)*X(i+1:XN)))/U(i, i)
        end do
    end subroutine BAMTDOSV
end module BAMTFASV