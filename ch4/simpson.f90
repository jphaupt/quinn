program simpson
    !! exercise 4.12
    !! this program uses Simpson's rule to compute pi then prints it out
    !! TODO
    ! [ ] a. parallelise with f(x)=4/(1+x^2), a=0, b=1, n=50
    ! [ ] b. benchmark on various numbers of processors
    use iso_fortran_env, only: rp=>real64
    implicit none
    real(rp) :: area
    integer :: i
    integer, parameter :: n=50
    area = f(0) - f(n) ! TODO
    do i=1,n/2
        area = area + 4._rp*f(2*i-1) + 2*f(2*i)
    enddo
    area = area/(3._rp*n)
    print*, "pi~", area

    contains

    pure real(rp) function f(i) result(retVal)
        implicit none
        integer, intent(in) :: i
        real(rp) :: x
        x = real(i,rp)/real(n,rp)
        retVal = 4._rp/(1._rp + x*x)
    end function f

end program simpson