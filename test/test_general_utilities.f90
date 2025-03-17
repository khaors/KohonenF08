module test_general_utilities
    !! Test functions in general_utilities module
    use iso_fortran_env, only: output_unit, real64, int64;
    use general_utilities, only: mean,std,variance,correlation_coefficient;
    use testdrive, only: new_unittest, unittest_type, error_type,check
    use precision_utilities, only: wp;
    use mt19937_64, only: mt19937;
    implicit none;
    private;
    !
    type(mt19937) :: random_grator
    !
    public :: collect_utilities
    !
    contains
        subroutine collect_utilities(testsuite)
            type(unittest_type),allocatable,intent(out) :: testsuite(:)
            testsuite=[new_unittest("mean", test_mean),&
                new_unittest("std", test_std),&
                new_unittest("variance", test_variance),&
                new_unittest("correlation coefficient", test_correlation)]
        end subroutine collect_utilities
        !
        subroutine test_mean(error)
            type(error_type),allocatable,intent(out) :: error
            !
            integer(int64) :: seed,ierr,i
            real(kind=wp),allocatable :: x(:)
            !
            seed=12345;
            call random_grator%initialize(seed);
            allocate(x(1000),stat=ierr);
            do i=1,size(x);
                x(i)=random_grator%genrand64_real1();
            end do
            call check(error,mean(x),0.49253084172707717_wp);
            if(allocated(error)) return;
            !
            do i=1,size(x);
                x(i)=random_grator%genrand64_real2();
            end do
            x=x+0.5_wp;
            call check(error,mean(x),0.99417640052962808_wp);
            if(allocated(error)) return;
            deallocate(x);
            !
        end subroutine test_mean
        !
        subroutine test_std(error)
            type(error_type),allocatable,intent(out) :: error
            !
            integer(int64) :: seed
            integer :: i,ierr
            real(kind=wp),allocatable :: x(:)
            !
            seed=123456;
            call random_grator%initialize(seed);
            allocate(x(5000),stat=ierr);
            do i=1,size(x);
                x(i)=random_grator%genrand64_real3();
            end do
            call check(error,std(x), 0.28644144876168942_wp);
            !
            seed=54321;
            call random_grator%initialize(seed);
            do i=1,size(x);
                x(i)=random_grator%genrand64_real1();
            end do
            call check(error,std(x),  0.28757959365998970_wp);    
            deallocate(x);
            !
        end subroutine test_std
        !
        subroutine test_variance(error)
            type(error_type),allocatable,intent(out) :: error
            integer(int64) :: seed
            integer :: i,ierr
            real(kind=wp),allocatable :: x(:)
            !
            seed=123456;
            call random_grator%initialize(seed);
            allocate(x(5000),stat=ierr);
            do i=1,size(x);
                x(i)=random_grator%genrand64_real3();
            end do
            call check(error,variance(x),  0.82048703568695536E-1_wp);
            deallocate(x);
            !
        end subroutine test_variance
        !
        subroutine test_correlation(error)
            type(error_type),allocatable,intent(out) :: error
            !
            integer(int64) :: seed
            integer :: i,ierr
            real(kind=wp),allocatable :: x(:),y(:)
            !
            seed=123456;
            call random_grator%initialize(seed);
            allocate(x(5000),stat=ierr);
            allocate(y(5000),stat=ierr);
            do i=1,size(x);
                x(i)=random_grator%genrand64_real1();
                y(i)=random_grator%genrand64_real2();
            end do
            call check(error,correlation_coefficient(x,y),0.35200423002143366E-2_wp);
            deallocate(x,y);    
        end subroutine test_correlation
        !       
end module test_general_utilities


program tester
    use iso_fortran_env, only: error_unit;
    use precision_utilities, only: wp;
    use testdrive, only: run_testsuite, new_testsuite, testsuite_type
    use test_general_utilities, only: collect_utilities;
    implicit none;
    type(testsuite_type),allocatable :: testsuites(:);
    character(len=*),parameter :: fmt = '("#", *(1x, a))';
    integer :: stat,is
    !
    stat=0;
    !
    testsuites=[new_testsuite("utilities", collect_utilities)];
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