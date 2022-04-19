program midpoint
    ! exercise 4.11
    !! TODO I must first install (Open)MPI -- and then put in path etc
    !! TODO 
    ! [ ] a. parallelise the code with MPI
    ! [ ] b. benchmark program on various numbers of processors (use gnuplot)
    ! this program computes and prints pi using the rectangle (midpoint) rule
    use iso_fortran_env, only: rp => real64
    implicit none
    integer, parameter :: MAX_INTERVALS = 1000000
    real(rp) :: area, ysum, xi
    integer :: i

    ysum = 0
    do i=0,MAX_INTERVALS
        ! print*, i
        xi = (i+0.5_rp)/MAX_INTERVALS
        ysum = ysum + 4._rp/(1._rp + xi*xi)
    enddo
    area = ysum/MAX_INTERVALS
    print*, 'area=', area
    
end program midpoint