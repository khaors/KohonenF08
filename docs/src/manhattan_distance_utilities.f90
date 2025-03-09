!! author: Oscar Garcia-Cabrejo
!! date: 12/04/2024
!! version: 0.1
!! This module defines a class to calculate the Manhattan distance between kohonen prototypes 
module manhattan_distance_utilities
!! This module defines a class to calculate the Manhattan distance between kohonen prototypes
use precision_utilities, only: wp;
use distance_base_utilities, only: distance_base;
!
implicit none;
!
private;
!
type,extends(distance_base) :: manhattan_distance
!! Class to calculate the Manhattan distance
    contains
        procedure,public :: calculate => calculate_manhattan_distance
end type manhattan_distance
!
public :: manhattan_distance;
!
contains 
!========================================================================================
    function calculate_manhattan_distance(distance,vector1,vector2) result(d)
!========================================================================================
!! Function to calculate the Manhattan distance between vectors
        class(manhattan_distance) :: distance
!! A `Manhattan_distance` object
        real(kind=wp),dimension(:,:),intent(inout) :: vector1,vector2
!! A real vector
        real(kind=wp) :: d
!! A real vector
        d=sum(dabs(vector1-vector2));
!! A real variable with the distance
    end function calculate_manhattan_distance
!
end module manhattan_distance_utilities