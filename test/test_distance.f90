module test_distance
    !! Test constant values included in the library
    use iso_fortran_env, only: output_unit, real64, int64;
    use general_utilities, only: mean,std;
    use testdrive, only: new_unittest, unittest_type, error_type,check
    use precision_utilities, only: wp;
    use distance_base_utilities, only: distance_base;
    use factory_distance_utilities, only: factory_distance;
    use mt19937_64, only: mt19937;
    implicit none;
    private;
    !
    class(distance_base),allocatable :: current_dist
    type(factory_distance) :: my_factory
    type(mt19937) :: random_grator
    !
    public :: collect_distances
contains
    subroutine collect_distances(testsuite)
        type(unittest_type),allocatable,intent(out) :: testsuite(:)
        testsuite=[new_unittest("Euclidean", test_euclidean),&
            new_unittest("Manhattan", test_manhattan),&
            new_unittest("Correlation", test_correlation),&
            new_unittest("Direction_Cosine", test_direction_cosine)]

    end subroutine collect_distances
!
    subroutine test_euclidean(error)
        type(error_type),allocatable,intent(out) :: error
        !
        real(kind=wp),allocatable :: x1(:,:),x2(:,:)
        integer :: i,ierr
        !
        allocate(x1(10,10),stat=ierr);
        allocate(x2(10,10),stat=ierr);
        x1=1.0_wp;
        x2=0.0_wp;
        !
        call my_factory%create_distance('euclidean',current_dist);
        !
        write(*,*) 'dist= ',current_dist%calculate(x1,x2);
        !
        call check(error,current_dist%calculate(x1,x2),100.0_wp);
        deallocate(x1,x2,current_dist);
        if(allocated(error)) return;

    end subroutine test_euclidean
!
    subroutine test_manhattan(error)
        type(error_type),allocatable,intent(out) :: error
        !
        real(kind=wp),allocatable :: x1(:,:),x2(:,:)
        integer :: i,ierr
        !
        allocate(x1(10,10),stat=ierr);
        allocate(x2(10,10),stat=ierr);
        x1=1.5_wp;
        x2=0.0_wp;
        !
        call my_factory%create_distance('manhattan',current_dist);
        !
        write(*,*) 'dist= ',current_dist%calculate(x1,x2);
        call check(error,current_dist%calculate(x1,x2),150.0_wp);
        deallocate(x1,x2,current_dist);
        if(allocated(error)) return;        
    end subroutine test_manhattan
!
    subroutine test_correlation(error)
        type(error_type),allocatable,intent(out) :: error
        !
        real(kind=wp),allocatable :: x1(:,:),x2(:,:),v1(:),v2(:)
        integer :: i,j,ierr
        integer(int64) :: seed
        !
        allocate(x1(10,10),stat=ierr);
        allocate(x2(10,10),stat=ierr);
        allocate(v1(100));
        allocate(v2(100));
        seed=12345;
        call random_grator%initialize(seed);
        do i=1,10;
            do j=1,10;
                x1(i,j)=random_grator%genrand64_real1();
                x2(i,j)=random_grator%genrand64_real2();
            end do
        end do
        v1=reshape(x1,[10*10]);
        v2=reshape(x2,[10*10]);
        !write(*,*) 'mean= ',sum(v1-mean(v1)),sum(v2-mean(v2)),sum((v1-mean(v1))*(v2-mean(v2)));
        !write(*,*) 'std= ',std(v1),std(v2);   
        !
        call my_factory%create_distance('correlation',current_dist);
        !
        write(*,*) 'dist= ',current_dist%calculate(x1,x2);
        call check(error,current_dist%calculate(x1,x2), 7.2324989614911850E-002_wp);
        deallocate(x1,x2,current_dist);
        if(allocated(error)) return;        
        !
    end subroutine test_correlation
!
    subroutine test_direction_cosine(error)
        type(error_type),allocatable,intent(out) :: error
        !
        real(kind=wp),allocatable :: x1(:,:),x2(:,:)
        integer :: i,j,ierr
        integer(int64) :: seed
        !
        allocate(x1(10,10),stat=ierr);
        allocate(x2(10,10),stat=ierr);
        seed=12345;
        call random_grator%initialize(seed);
        do i=1,10;
            do j=1,10;
                x1(i,j)=random_grator%genrand64_real1();
                x2(i,j)=random_grator%genrand64_real2();
            end do
        end do
        !
        call my_factory%create_distance('direction_cosine',current_dist);
        !
        write(*,*) 'dist= ',current_dist%calculate(x1,x2);
        call check(error,current_dist%calculate(x1,x2), 0.10241886007537213E-1_wp);
        deallocate(x1,x2,current_dist);
        if(allocated(error)) return;        
        !
    end subroutine test_direction_cosine

end module test_distance

program tester
    use iso_fortran_env, only: error_unit;
    use precision_utilities, only: wp;
    use testdrive, only: run_testsuite, new_testsuite, testsuite_type
    use test_distance, only: collect_distances;
    implicit none;
    type(testsuite_type),allocatable :: testsuites(:);
    character(len=*),parameter :: fmt = '("#", *(1x, a))';
    integer :: stat,is
    !
    stat=0;
    !
    testsuites=[new_testsuite("distances", collect_distances)];
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