program simpson
    !! exercise 4.12
    !! this program uses Simpson's rule to compute pi then prints it out
    ! [x] a. parallelise with f(x)=4/(1+x^2), a=0, b=1, n=50
    ! [x] b. benchmark on various numbers of processors
    ! note this still uses very ugly preprocessors which would be better put in
    ! another file, like my_mpi.F90 and then simply used here
    ! seems this scales very badly for some reason
    use iso_fortran_env, only: rp => real64
#ifdef ENABLE_MPI
    use mpi_f08
#endif
    implicit none
    real(rp) :: area, area_loc
    integer :: i
    integer, parameter :: n=5000 ! only get speed up if this is huge
    integer :: ierr, my_rank, nproc
    real(rp) :: time
#ifdef ENABLE_MPI
    call MPI_Init_f08(ierr)
    time = -MPI_Wtime()
    call MPI_Comm_rank_f08(MPI_COMM_WORLD, my_rank, ierr)
    call MPI_Comm_size_f08(MPI_COMM_WORLD, nproc, ierr)
#else
    real(rp) :: time2
    my_rank = 0
    nproc = 1
    call cpu_time(time)
#endif
    area_loc = 0
    do i=my_rank+1,n/2,nproc
        area_loc = area_loc + 4._rp*f(2*i-1) + 2*f(2*i)
    enddo
#ifdef ENABLE_MPI
    call MPI_Reduce_f08(area_loc, area, 1, MPI_DOUBLE_PRECISION, MPI_SUM, 0, MPI_COMM_WORLD, ierr)
    time = time + MPI_Wtime()
#else
    area = area_loc
    call cpu_time(time2)
    time = time - time2
#endif
    if(my_rank==0) then
        area = area + f(0) - f(n)
        area = area/(3._rp*n)
        ! print*, "pi~", area
        print '(1x,i0,4x,g0)', nproc, time
    endif

#ifdef ENABLE_MPI
    call MPI_Finalize_f08(ierr)
#endif

    contains

    pure real(rp) function f(i) result(retVal)
        implicit none
        integer, intent(in) :: i
        real(rp) :: x
        x = real(i,rp)/real(n,rp)
        retVal = 4._rp/(1._rp + x*x)
    end function f

end program simpson