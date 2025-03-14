module test_random
    use testdrive, only: new_unittest, unittest_type, error_type,check
    use precision_utilities, only: wp;
    use rkiss05_generator_utilities, only: rkiss05_generator;
    implicit none;
    private;
    type(rkiss05_generator) :: my_grator
    integer :: seed
    public :: collect_random
contains
    subroutine collect_random(testsuite)
        type(unittest_type),allocatable,intent(out) :: testsuite(:)
        testsuite=[new_unittest("RKISS", test_rkiss)]
    end subroutine collect_random
    !
    subroutine test_rkiss(error)
        type(error_type),allocatable,intent(out) :: error
        real(kind=wp),allocatable :: rnumbers(:),rnumbers1(:)
        integer :: i
        seed=12345;
        call my_grator%create(seed);
        rnumbers=(/(dble(i),i=1,5)/);
        rnumbers1=(/(dble(i),i=1,5)/);    
        do i=1,5;
            rnumbers(i)=my_grator%generate();
        enddo
        call my_grator%destroy();
        call my_grator%create(seed);
        do i=1,5;
            rnumbers1(i)=my_grator%generate();
        enddo
        do i=1,5;
            call check(error,rnumbers(i),rnumbers1(i));
        enddo
        if(allocated(error)) return;
        call my_grator%destroy();
    end subroutine test_rkiss
!
end module test_random

program tester
    use iso_fortran_env, only: error_unit;
    use precision_utilities, only: wp;
    use testdrive, only: run_testsuite, new_testsuite, testsuite_type
    use test_random, only: collect_random;
    implicit none;
    type(testsuite_type),allocatable :: testsuites(:);
    character(len=*),parameter :: fmt = '("#", *(1x, a))';
    integer :: stat,is
    !
    stat=0;
    !
    testsuites=[new_testsuite("random", collect_random)];
    !
    do is=1,size(testsuites);
        write(error_unit,fmt) "Testing: ", testsuites(is)%name;
        call run_testsuite(testsuites(is)%collect, error_unit, stat);
    end do
    !

    if(stat > 0) then
        write(error_unit, '(i0, 1X, a)') stat, "test(s) failed";
        error stop 
    end if
!
end program tester    