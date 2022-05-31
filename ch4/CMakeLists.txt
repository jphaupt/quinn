cmake_minimum_required(VERSION 3.13)

project(quin_ch4 Fortran)
enable_language(Fortran)

# midpoint is a minimal example of a CMake Fortran MPI program
add_executable(midpoint midpoint.F90)

option(PARALLEL "Using MPI" ON)
if(PARALLEL)
    set(MPI_FLAG ENABLE_MPI)
    find_package(MPI REQUIRED COMPONENTS Fortran)
    target_compile_definitions(midpoint PUBLIC ENABLE_MPI)
    target_link_libraries(midpoint MPI::MPI_Fortran)
endif()

add_executable(simpson simpson.f90)
