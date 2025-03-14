!! author: Oscar Garcia-Cabrejo
!! date: 12/04/2024
!! version: 0.1
!! This module defines a class to calculate the Euclidean distance between kohonen prototypes 
module euclidean_distance_utilities
!! This module defines a class to calculate the Euclidean distance between kohonen prototypes 
use precision_utilities, only: wp;
use distance_base_utilities, only: distance_base;
!
implicit none
!   
type,extends(distance_base) :: euclidean_distance 
!! Class to calculate the euclidean distance 
    contains
        procedure,public :: calculate => calculate_euclidean_distance
!
end type euclidean_distance
!
contains
!========================================================================================
    function calculate_euclidean_distance(distance,vector1,vector2) result(d)
!========================================================================================
!! Function to calculate euclidean distance between vectors
        class(euclidean_distance) :: distance
!! A `euclidean_distance` object
        real(kind=wp),dimension(:,:),intent(inout) :: vector1
!! A real vector
        real(kind=wp),dimension(:,:),intent(inout) :: vector2
!! A real vector
        real(kind=wp) :: d
!! A real variable with the distance
        d=sum((vector1-vector2)**2);
!
    end function calculate_euclidean_distance
!
end module euclidean_distance_utilities