!! author: Oscar Garcia-Cabrejo
!! date: 05/01/2021
!! version: 0.1
!! This module defines the random_number_generator class that is used to generate random numbers 
!! in several procedures across ATALIB.
module random_number_generator_utilities
!! This module defines the random_number_generator class that is used to generate random numbers 
!! in several procedures across ATALIB.
use precision_utilities, only: wp,i64;
use mt19937_64, only: init_genrand64,init_by_array64,genrand64_real3;
!
implicit none;
!
private;
!
type random_number_generator
    private
!! The random_number_generator class is used to encapsulate a generator or random numbers
!! An object of this class is defined through the specification of the random seed or seeds
        integer(i64) :: seed
        integer(i64),allocatable :: seed_array(:) 
    contains
        procedure,private :: create_random_number_grator_single
        procedure,private :: create_random_number_grator_array 
        generic,public :: create => create_random_number_grator_single,create_random_number_grator_array
        procedure,public :: destroy => destroy_random_number_grator
        procedure,public :: generate
end type random_number_generator
!
public :: random_number_generator
!
contains 
!========================================================================================
    subroutine create_random_number_grator_single(grator,iseed)
!========================================================================================
!! Class Constructor 1
        class(random_number_generator) :: grator 
!! A `random_number_generator` object to be defined
        integer(i64) :: iseed 
!! An integer value with the seed of the random_number_generator

!!    program main
!!        use random_number_generator_utilities;
!!        ...
!!        integer :: iseed
!!        type(random_number_generator) :: my_grator
!!        ...
!!        iseed=12345;
!!        call my_grator%create(iseed);
!!        ...
!!    end program main
        grator%seed=iseed;
        call init_genrand64(grator%seed);
!
    end subroutine create_random_number_grator_single
!========================================================================================
    subroutine create_random_number_grator_array(grator,iseed)
!========================================================================================
!! Class Destructor 2
        class(random_number_generator) :: grator
!! A `random_number_generator` object to be defined 
        integer(i64),dimension(:),intent(inout) :: iseed
!! An integer array with the seeds of the random_number_generator

!!    program main
!!        use random_number_generator_utilities;
!!        ...
!!        integer :: i
!!        integer,dimension(5) :: iseed
!!        type(random_number_generator) :: my_grator
!!        ...
!!        do i=1,5
!!            iseed(i)=12345+i;
!!        enddo
!!        call my_grator%create(iseed);
!!        ...
!!    end program main

        allocate(grator%seed_array,source=iseed);
        call init_by_array64(grator%seed_array);
!
    end subroutine create_random_number_grator_array
!========================================================================================
    subroutine destroy_random_number_grator(grator)
!========================================================================================
!! Class Destructor
        class(random_number_generator) :: grator 
!! A `random_number_generator` object to be destroyed

!!    program main
!!        use random_number_generator_utilities;
!!        ...
!!        integer :: iseed
!!        type(random_number_generator) :: my_grator
!!        ...
!!        iseed=12345;
!!        call my_grator%create(iseed);
!!        ...
!!        call my_grator%destroy();
!!        ...
!!    end program main

        grator%seed=0;
        if(allocated(grator%seed_array)) then 
            deallocate(grator%seed_array);
        endif
!
    end subroutine destroy_random_number_grator
!========================================================================================
    function generate(grator) result(rn)
!========================================================================================
!! Function used to generate random numbers
        class(random_number_generator) :: grator
!! A `random_number_generator` object         
        real(wp) :: rn 
!! A real value with the random number generated

!!    program main
!!        use random_number_generator_utilities;
!!        ...
!!        integer :: iseed
!!        real(wp) :: rnum
!!        type(random_number_generator) :: my_grator
!!        ...
!!        iseed=12345;
!!        call my_grator%create(iseed);
!!        rnum=my_grator%generate();
!!        write(*,*) 'Random number= ',rnum;
!!        ...
!!    end program main
        rn=genrand64_real3();
!
    end function generate
!     
end module random_number_generator_utilities