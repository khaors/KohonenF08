!! author: Oscar Garcia-Cabrejo
!! date: 03/17/2025
!! version: 0.1
!! This module defines a class to define learning rate functions
module exponential_learning_rate_function_utilities
    use precision_utilities, only: wp;
    use learning_rate_function_base_utilities, only: learning_rate_function_base;
    !
    implicit none;
    !
    private;
    !
    type,extends(learning_rate_function_base) :: exponential_learning_rate_function
        private
            real(kind=wp) :: lambda0,tau
        contains
            procedure,public :: set_parameters
            procedure,public :: calculate => calculate_exponential_function
    end type exponential_learning_rate_function
    !
    public :: exponential_learning_rate_function;
!
contains
    subroutine set_parameters(learning_rate_fn,lambda0,tau)
        class(exponential_learning_rate_function) :: learning_rate_fn
        real(kind=wp) :: lambda0,tau
        !
        learning_rate_fn%lambda0=lambda0;
        learning_rate_fn%tau=tau;

    end subroutine set_parameters
    !
    function calculate_exponential_function(learning_rate_fn,iteration) result(alpha)
        class(exponential_learning_rate_function) :: learning_rate_fn
        !! An `learning_rate_function_base` object
        real(kind=wp),intent(inout) :: iteration
        !! A real variable with the current iteration
        real(kind=wp) :: alpha
        !! A real variable with the learning rate
        real(kind=wp) :: time_factor
        !
        time_factor=-(iteration/learning_rate_fn%tau);
        alpha=max(learning_rate_fn%lambda0*dexp(time_factor),0.01_wp);
!
    end function calculate_exponential_function
!
end module exponential_learning_rate_function_utilities