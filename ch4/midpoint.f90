program midpoint
    ! exercise 4.11
    ! [x] a. parallelise the code with MPI
    ! [x] b. benchmark program on various numbers of processors (use gnuplot)
    ! [ ] my extra: implement with CMake + preprocessor (#ifdef MPI etc)
    ! this program computes and prints pi using the rectangle (midpoint) rule
    use iso_fortran_env, only: rp => real64
    use mpi_f08
    implicit none
    integer, parameter :: MAX_INTERVALS = 1000000
    real(rp) :: area, ysum=0, xi, ysum_loc
    integer :: i
    ! variables for MPI subroutines
    integer :: ierr, my_rank, nproc
    real(rp) :: time

    call MPI_Init(ierr)
    time = - MPI_WTime()
    call MPI_Comm_rank_f08(MPI_COMM_WORLD, my_rank, ierr)
    call MPI_Comm_size_f08(MPI_COMM_WORLD, nproc, ierr)


    ysum_loc = 0
    ! do i=0,MAX_INTERVALS
    do i=my_rank,MAX_INTERVALS,nproc
        xi = (i+0.5_rp)/MAX_INTERVALS
        ysum_loc = ysum_loc + 4._rp/(1._rp + xi*xi)
    enddo
    ! NOTE below MPI_DOUBLE_PRECISION works but not MPI_REAL (!!!)
    call MPI_Reduce_f08(ysum_loc, ysum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, 0, MPI_COMM_WORLD, ierror=ierr)
    time = time + MPI_Wtime()
    ! print '(1x,a,i0,a)', "Process ", my_rank, " completed."
    if(my_rank==0) then
        area = ysum/MAX_INTERVALS
        print*, 'area=', area
        print '(1x,i0,4x,g0)', nproc, time
    endif
    call MPI_Finalize(ierr)

end program midpoint