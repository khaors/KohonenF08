module test_constants
    !! Test constant values included in the library
    use testdrive, only: new_unittest, unittest_type, error_type,check
    use precision_utilities, only: wp;
    use constants_utilities, only: PI,NUMCHAR,DEG2RAD;
    implicit none;
    private;
    public :: collect_constants
contains
    subroutine collect_constants(testsuite)
        type(unittest_type),allocatable,intent(out) :: testsuite(:)
        testsuite=[new_unittest("NUMCHAR", test_numchar), &
                   new_unittest("PI", test_pi), &
                   new_unittest("DEG2PI", test_deg2pi)];

    end subroutine collect_constants
    !
    subroutine test_numchar(error)
        type(error_type),allocatable,intent(out) :: error
        call check(error, NUMCHAR, 128);
        if(allocated(error)) return;
    end subroutine test_numchar
    !
    subroutine test_pi(error)
        type(error_type),allocatable,intent(out) :: error
        call check(error,PI,3.1415926535897931_wp);
        if(allocated(error)) return;
    end subroutine test_pi
    !
    subroutine test_deg2pi(error)
        type(error_type),allocatable,intent(out) :: error
        call check(error,DEG2RAD,PI/180.0_wp);
        if(allocated(error)) return;
    end subroutine test_deg2pi


end module test_constants

program tester
    use iso_fortran_env, only: error_unit;
    use precision_utilities, only: wp;
    use testdrive, only: run_testsuite, new_testsuite, testsuite_type
    use test_constants, only: collect_constants;
    implicit none;
    type(testsuite_type),allocatable :: testsuites(:);
    character(len=*),parameter :: fmt = '("#", *(1x, a))';
    integer :: stat,is
    !
    stat=0;
    !
    testsuites=[new_testsuite("constants", collect_constants)];
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