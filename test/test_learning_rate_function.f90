module test_learning_rate_function
    !! Test constant values included in the library
        use iso_fortran_env, only: output_unit, real64, int64;
        use testdrive, only: new_unittest, unittest_type, error_type,check
        use precision_utilities, only: wp;
        use learning_rate_function_base_utilities, only: learning_rate_function_base;
        use factory_learning_rate_function_utilities, only: factory_learning_rate_function;
        use mt19937_64, only: mt19937;
        !
        implicit none;
        !
        private;
        !
        class(learning_rate_function_base),allocatable :: current_fn
        type(factory_learning_rate_function) :: my_factory
        type(mt19937) :: random_grator
        !
        public :: collect_rate_function;
        !
        contains
            subroutine collect_rate_function(testsuite)
                type(unittest_type),allocatable,intent(out) :: testsuite(:)
                !
                testsuite=[new_unittest("Linear", test_linear),&
                    new_unittest("Gaussian", test_gaussian),&
                    new_unittest("Exponential", test_exponential)];
                
            end subroutine collect_rate_function
            !
            subroutine test_linear(error)
                type(error_type),allocatable,intent(out) :: error
                !
                real(kind=wp) :: iteration,current_fn_value
                !
                call my_factory%create_learning_rate_fn('linear',current_fn);
                call current_fn%set_parameters(lambda0=0.05_wp,tau=100.0_wp);
                iteration=1;
                current_fn_value=current_fn%calculate(iteration=iteration);
                call check(error,current_fn_value,0.0495_wp);
                if(allocated(error)) return;
                !
                iteration=50.0_wp;
                current_fn_value=current_fn%calculate(iteration=iteration);
                call check(error,current_fn_value,0.025_wp);
                if(allocated(error)) return;
                !
                iteration=99.0_wp;
                current_fn_value=current_fn%calculate(iteration=iteration);
                call check(error,current_fn_value,0.01_wp);
                if(allocated(error)) return;
                deallocate(current_fn);
                !
            end subroutine test_linear
            !
            subroutine test_gaussian(error)
                type(error_type),allocatable,intent(out) :: error
                !
                real(kind=wp) :: iteration,current_fn_value
                        !
                call my_factory%create_learning_rate_fn('gaussian',current_fn);
                call current_fn%set_parameters(lambda0=0.05_wp,tau=100.0_wp);
                iteration=1;
                current_fn_value=current_fn%calculate(iteration=iteration);
                call check(error,current_fn_value, 0.049997500062498958_wp);
                if(allocated(error)) return;
                !
                iteration=50.0_wp;
                current_fn_value=current_fn%calculate(iteration=iteration);
                call check(error,current_fn_value, 0.044124845129229776_wp);
                if(allocated(error)) return;
                !
                iteration=99.0_wp;
                current_fn_value=current_fn%calculate(iteration=iteration);
                call check(error,current_fn_value, 0.030629788181523857_wp);
                if(allocated(error)) return;
                deallocate(current_fn);
                !
            end subroutine test_gaussian
            !
            subroutine test_exponential(error)
                type(error_type),allocatable,intent(out) :: error
                !
                real(kind=wp) :: iteration,current_fn_value
                        !
                call my_factory%create_learning_rate_fn('exponential',current_fn);
                call current_fn%set_parameters(lambda0=0.05_wp,tau=100.0_wp);
                iteration=1;
                current_fn_value=current_fn%calculate(iteration=iteration);
                call check(error,current_fn_value, 0.05_wp*exp(-iteration/100.0_wp));
                if(allocated(error)) return;
                !
                iteration=50.0_wp;
                current_fn_value=current_fn%calculate(iteration=iteration);
                call check(error,current_fn_value, 0.05_wp*exp(-iteration/100.0_wp));
                if(allocated(error)) return;
                !
                iteration=99.0_wp;
                current_fn_value=current_fn%calculate(iteration=iteration);
                call check(error,current_fn_value, 0.05_wp*exp(-iteration/100.0_wp));
                if(allocated(error)) return;
                deallocate(current_fn);
                !
            end subroutine test_exponential
            !       
end module test_learning_rate_function



program tester
    use iso_fortran_env, only: error_unit;
    use precision_utilities, only: wp;
    use testdrive, only: run_testsuite, new_testsuite, testsuite_type
    use test_learning_rate_function, only: collect_rate_function;
    implicit none;
    type(testsuite_type),allocatable :: testsuites(:);
    character(len=*),parameter :: fmt = '("#", *(1x, a))';
    integer :: stat,is
    !
    stat=0;
    !
    testsuites=[new_testsuite("learning_rate_fn", collect_rate_function)];
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