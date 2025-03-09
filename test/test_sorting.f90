module test_sorting
    !! Test constant values included in the library
    use testdrive, only: new_unittest, unittest_type, error_type,check
    use precision_utilities, only: wp;
    use quicksort_utilities, only: quicksort;
    implicit none;
    private;
    public :: collect_sorting
contains
    subroutine collect_sorting(testsuite)
        type(unittest_type),allocatable,intent(out) :: testsuite(:)
        testsuite=[new_unittest("Sort1", test_sort1),&
                   new_unittest("Sort2", test_sort2)];

    end subroutine collect_sorting
    !
    subroutine test_sort1(error)
        type(error_type),allocatable,intent(out) :: error
        !
        real(kind=wp),allocatable :: x(:)
        integer,allocatable :: order(:),order1(:)
        integer :: i
        type(quicksort) :: qsort
        !
        x=[1601, 801, 300, 132, 57, 10, 1];
        order=(/(i,i=1,7)/);
        !write(*,*) order; !(i,i=1,7)
        call qsort%sort(x,order);
        !write(*,*) order;
        order1=(/(i,i=7,1,-1)/);
        !write(*,*) order1;
        call check(error,order1(1),7,'Initial value of array is not equal to the expected value');
        if(allocated(error)) return;
        call check(error,order(7),1,'Final value of array is not equal to the expected value');
        if(allocated(error)) return;

    end subroutine test_sort1 
!
    subroutine test_sort2(error)
        type(error_type),allocatable,intent(out) :: error
        !
        real(kind=wp),allocatable :: x(:)
        integer,allocatable :: order(:),order1(:)
        integer :: i
        type(quicksort) :: qsort
        !
        x=[887, 510, 377, 233, 144, 89, 55, 34, 21, 13, 8, 5, 3, 2, 2, 1];
        order=(/(i,i=1,16)/);
        !write(*,*) order; !(i,i=1,7)
        call qsort%sort(x,order);
        !write(*,*) order;
        order1=(/(i,i=16,1,-1)/);
        !write(*,*) order1;
        call check(error,order1(1),16,'Initial value of array is not equal to the expected value');
        if(allocated(error)) return;
        call check(error,order(16),1,'Final value of array is not equal to the expected value');
        if(allocated(error)) return;

    end subroutine test_sort2 
!
end module test_sorting

program tester
    use iso_fortran_env, only: error_unit;
    use precision_utilities, only: wp;
    use testdrive, only: run_testsuite, new_testsuite, testsuite_type
    use test_sorting, only: collect_sorting;
    implicit none;
    type(testsuite_type),allocatable :: testsuites(:);
    character(len=*),parameter :: fmt = '("#", *(1x, a))';
    integer :: stat,is
    !
    stat=0;
    !
    testsuites=[new_testsuite("sorting", collect_sorting)];
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