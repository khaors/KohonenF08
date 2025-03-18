! author: Oscar Garcia-Cabrejo
!! date: 03/18/2025
!! version: 0.1
!! This module defines a factory to create learning rate functions 
module factory_learning_rate_function_utilities
    !! This module defines a factory to create learning rate functions 
    use error_handling, only: error_stop;
    use precision_utilities, only: wp;
    use constants_utilities, only: NUMCHAR;
    use learning_rate_function_base_utilities, only: learning_rate_function_base;
    use linear_learning_rate_function_utilities, only: linear_learning_rate_function;
    use gaussian_learning_rate_function_utilities, only: gaussian_learning_rate_function;
    use exponential_learning_rate_function_utilities, only: exponential_learning_rate_function;
    !
    implicit none;
    !
    private;
    !
    type :: factory_learning_rate_function
    !! Class that represents a factory of learning rate functions
        contains
            procedure,public :: create_learning_rate_fn
    end type factory_learning_rate_function
    !
    public :: factory_learning_rate_function
!
    contains
        subroutine create_learning_rate_fn(factory,type_,learning_rate_fn)
            !! Subroutine to create the instances of learning rate functions
            class(factory_learning_rate_function) :: factory
            !! A `factory_learning_rate_function` object
            character(len=*) :: type_
            !! A character string with the type of learning rate function to be instantiated
            class(learning_rate_function_base),allocatable :: learning_rate_fn
            !! A `learning_rate_function_base` object
            integer :: ierr
            character(len=NUMCHAR) :: base_message,message;
            !
            base_message='FACTORY LEARNING RATE FUNCTION ERROR';
            if(allocated(learning_rate_fn)) deallocate(learning_rate_fn);
            select case(trim(type_));
                case('linear')
                    allocate(linear_learning_rate_function :: learning_rate_fn,stat=ierr);
                    if(ierr /= 0) then
                        message=trim(base_message)//' while allocating memory for linear rate function';
                        call error_stop(message);
                    end if
                case('gaussian')
                    allocate(gaussian_learning_rate_function :: learning_rate_fn,stat=ierr);
                    if(ierr /= 0) then
                        message=trim(base_message)//' while allocating memory for gaussian rate function';
                        call error_stop(message);
                    end if   
                case('exponential')
                    allocate(exponential_learning_rate_function :: learning_rate_fn,stat=ierr);
                    if(ierr /= 0) then
                        message=trim(base_message)//' while allocating memory for exponential rate function';
                        call error_stop(message);
                    end if   
                case default
                    write(*,*) 'ERROR: the requested distance is not defined';
                    stop
            end select

        end subroutine create_learning_rate_fn
!
end module factory_learning_rate_function_utilities