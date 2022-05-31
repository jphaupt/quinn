
!    // "pp_defs": {
!    //     "ENABLE_MPI": "ON"
!    // },
    ! "ext_source_dirs": [
    !     "/home/phaupt/software/openmpi_src/openmpi-4.1.3/ompi/mpi/fortran/use-mpi-f08",
    !     "/home/phaupt/software/openmpi_src/openmpi-4.1.3/ompi/mpi/fortran/use-mpi-tkr",
    !     "/home/phaupt/software/openmpi_src/openmpi-4.1.3/ompi/mpi/fortran/base"
    !     ]
! TODO universal config file!
program midpoint
    ! exercise 4.11
    ! [x] a. parallelise the code with MPI
    ! [x] b. benchmark program on various numbers of processors (use gnuplot)
    ! [x] my extra: implement with CMake + preprocessor (#ifdef MPI etc)
    ! [ ] MPI wrapper so that e.g. timer does something else if serial
    ! this program computes and prints pi using the rectangle (midpoint) rule
    use iso_fortran_env, only: rp => real64
    use mpi_f08
#ifdef ENABLE_MPI
#endif
    implicit none
    integer, parameter :: MAX_INTERVALS = 1000000
    real(rp) :: area, ysum=0, xi, ysum_loc
    integer :: i
    ! variables for MPI subroutines
    integer :: ierr, my_rank, nproc
    real(rp) :: time
    integer :: testvar ! unused if ENABLE_MPI not defined
    integer :: testvar2
    ! TODO seems to be the only way to define variables :/
#define ANOTHER_ONE

#ifdef ENABLE_MPI
    ! TODO make VS Code recognise preprocessor usage (here, ierr is supposedly unused)
    ! TODO for some reason VS Code keeps uninstalling Modern Fortran extension
    ! intellisense not applied here :/
    call MPI_Init(ierr)
    time = - MPI_WTime()
    call MPI_Comm_rank_f08(MPI_COMM_WORLD, my_rank, ierr)
    call MPI_Comm_size_f08(MPI_COMM_WORLD, nproc, ierr)
#else
    testvar = 1
    time = 0
    my_rank = 0
    nproc = 1
#endif
#ifdef ANOTHER_ONE
    ! TODO delete
    testvar2 = 2
#endif
    print*, 'nproc=', nproc

    ysum_loc = 0
    ! do i=0,MAX_INTERVALS
    do i=my_rank,MAX_INTERVALS,nproc
        xi = (i+0.5_rp)/MAX_INTERVALS
        ysum_loc = ysum_loc + 4._rp/(1._rp + xi*xi)
    enddo
    ! NOTE below MPI_DOUBLE_PRECISION works but not MPI_REAL (!!!)
#ifdef ENABLE_MPI
    call MPI_Reduce_f08(ysum_loc, ysum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, 0, MPI_COMM_WORLD, ierror=ierr)
    time = time + MPI_Wtime()
#endif
    ! print '(1x,a,i0,a)', "Process ", my_rank, " completed."
    if(my_rank==0) then
        area = ysum/MAX_INTERVALS
        print*, 'area=', area
        print '(1x,i0,4x,g0)', nproc, time
    endif
#ifdef ENABLE_MPI
    call MPI_Finalize(ierr)
#endif

end program midpoint