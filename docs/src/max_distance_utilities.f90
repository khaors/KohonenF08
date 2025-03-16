!! author: Oscar Garcia-Cabrejo
!! date: 03/16/2025
!! version: 0.1
!! This module defines a class to calculate the Max distance between kohonen prototypes 
module max_distance_utilities
    !! This module defines a class to calculate the Max distance between kohonen prototypes
    use precision_utilities, only: wp;
    use distance_base_utilities, only: distance_base;
    !
    implicit none;
    !
    private;
    !
    type,extends(distance_base) :: max_distance
    !! Class to calculate the Max distance
        contains
            procedure,public :: calculate => calculate_max_distance
    end type max_distance
    !
    public :: max_distance;
    !
    contains 
    !========================================================================================
        function calculate_max_distance(distance,vector1,vector2) result(d)
    !========================================================================================
    !! Function to calculate the Max distance between vectors
            class(max_distance) :: distance
    !! A `Max_distance` object
            real(kind=wp),dimension(:,:),intent(inout) :: vector1,vector2
    !! A real vector
            real(kind=wp) :: d
    !! A real vector
            d=maxval(vector1-vector2);
    !! A real variable with the distance
        end function calculate_manhattan_distance
    ! 
end module max_distance_utilities