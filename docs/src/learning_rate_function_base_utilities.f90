!! author: Oscar Garcia-Cabrejo
!! date: 03/17/2025
!! version: 0.1
!! This module defines an abstract class to define learning rate functions
module learning_rate_function_base_utilities
!! This module defines an abstract class to define learning rate functions
    use precision_utilities, only: wp;
    implicit none;
    !
    type,abstract :: learning_rate_function_base
    !! Abstract class used to derive classes that calculates the learning rate
        contains
            procedure(learning_rate_set_parameters),deferred :: set_parameters
            procedure(learning_rate_calculate),deferred :: calculate
    end type learning_rate_function_base
!
    abstract interface
        subroutine learning_rate_set_parameters(learning_rate_fn,lambda0,tau)
            !! Template for a function to define the parameters of the learning rate function
            import :: learning_rate_function_base
            import :: wp 
            !!
            class(learning_rate_function_base) :: learning_rate_fn
            real(kind=wp) :: lambda0,tau
            !
        end subroutine learning_rate_set_parameters
        !
        function learning_rate_calculate(learning_rate_fn,iteration) result(alpha)
            !! Template for a function that calculates the learning rate
            import :: learning_rate_function_base
            import :: wp
            !! Import section
            class(learning_rate_function_base) :: learning_rate_fn
            !! An `learning_rate_function_base` object
            real(kind=wp),intent(inout) :: iteration
            !! A real variable with the current iteration
            real(kind=wp) :: alpha
            !! A real variable with the learning rate
        end function learning_rate_calculate
    end interface
    !
end module learning_rate_function_base_utilities