!! author: Oscar Garcia-Cabrejo
!! date: 03/16/2025
!! version: 0.1
!! This module defines a class to calculate the direction cosine distance between kohonen prototypes
module direction_cosine_distance_utilities
    !! This module defines a class to calculate the direction cosine distance between kohonen prototypes
    use precision_utilities, only: wp;
    use distance_base_utilities, only: distance_base;
    !
    implicit none;
    !
    private;
    !
    type,extends(distance_base) :: direction_cosine_distance
    !! Class to calculate the direction cosine distance
        contains
            procedure,public :: calculate => calculate_direction_cosine_distance
    end type direction_cosine_distance
    !
    public :: direction_cosine_distance;
    !
    contains 
    !========================================================================================
        function calculate_direction_cosine_distance(distance,vector1,vector2) result(d)
    !========================================================================================
    !! Function to calculate the Manhattan distance between vectors
            class(direction_cosine_distance) :: distance
    !! A `Manhattan_distance` object
            real(kind=wp),dimension(:,:),intent(inout) :: vector1,vector2
    !! A real vector
            real(kind=wp) :: d
    !! A real vector
            real(kind=wp) :: v1norm,v2norm,prod
            !
            prod=sum(sum(vector1*vector2,dim=1),dim=1);
            v1norm=sum(sum(dsqrt(vector1*vector1),dim=1),dim=1);
            v2norm=sum(sum(dsqrt(vector2*vector2),dim=1),dim=1);
            d=prod/(v1norm*v2norm);
    !! A real variable with the distance
        end function calculate_direction_cosine_distance

end module direction_cosine_distance_utilities