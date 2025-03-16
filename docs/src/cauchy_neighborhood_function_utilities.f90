!! author: Oscar Garcia-Cabrejo
!! date: 03/16/2025
!! version: 0.1
!! This module defines the Cauchy neighborhood function
module cauchy_neighborhood_function_utilities
!! This module defines the Cauchy neighborhood function
    use precision_utilities, only: wp;
    use neighborhood_function_base_utilities, only: neighborhood_function_base;
    implicit none;
    !
    private;
    !
    type,extends(neighborhood_function_base) :: cauchy_neighborhood_function
    !! Class that implements the Cauchy Neighborhood Function
        private
            real(kind=wp) :: sigma,p
        contains
            procedure,public :: create => create_cauchy_neighborhood
            procedure,public :: calculate => calculate_cauchy_neighborhood
    end type cauchy_neighborhood_function
    !
    public :: cauchy_neighborhood_function;
!
contains
!
    subroutine create_cauchy_neighborhood(my_neigh_fn,parameters)
    !! Class Constructor
        class(cauchy_neighborhood_function) :: my_neigh_fn
    !! A `cauchy_neighborhood_function` object
        real(kind=wp),dimension(:),intent(in) :: parameters
    !! A real array with the paramters sigma and p
        my_neigh_fn%sigma=parameters(1);
        my_neigh_fn%p=parameters(2);

    end subroutine create_cauchy_neighborhood
!
    function calculate_cauchy_neighborhood(my_neigh_fn,geometric_distance) result(n)
    !! Function to calculate the value of the cauchy neighborhood
        class(cauchy_neighborhood_function) :: my_neigh_fn
    !! A `cauchy_neighborhood_function` object
        real(kind=wp),intent(inout) :: geometric_distance
    !! A real variable with the geometric distnace
        real(kind=wp) :: n
    !! A real value
        n=1.0_wp/(1.0_wp+(geometric_distance/my_neigh_fn%sigma)**my_neigh_fn%p);
    !
    end function calculate_cauchy_neighborhood
!
end module cauchy_neighborhood_function_utilities