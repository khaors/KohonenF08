!! author: Oscar Garcia-Cabrejo
!! date: 03/16/2025
!! version: 0.1
!! This module defines the Gaussian neighborhood function
module gaussian_neighborhood_utilities
    !! This module defines the Gaussian neighborhood function
    use precision_utilities, only: wp;
    use neighborhood_function_base_utilities, only: neighborhood_function_base;
    !
    implicit none;
    !
    private;
    !
    type,extends(neighborhood_function_base) :: gaussian_neighborhood_utilities
        private
            real(kind=wp) :: mean,sigma
        contains
            procedure,public :: create => create_gaussian_neighborhood
            procedure,public :: calculate => calculate_gaussian_neighborhood
    end type gaussian_neighborhood
!
contains
!
    subroutine create_gaussian_neighborhood(my_neigh_fn,parameters)
        class(gaussian_neighborhood) :: my_neigh_fn
        real(kind=wp),dimension(:),intent(in) :: parameters
        !
        my_neigh_fn%mean=parameters(1);
        my_neigh_fn%sigma=parameters(2);

    end subroutine create_gaussian_neighborhood
!
    s



end module gaussian_neighborhood_utilities