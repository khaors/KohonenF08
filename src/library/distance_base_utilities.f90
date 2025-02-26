!! author: Oscar Garcia-Cabrejo
!! date: 12/04/2024
!! version: 0.1
!! This module defines an abstract class called `distance_base` used as a template to 
!! derive classes used in the calculation of distances between input patterns
module distance_base_utilities
!
use precision_utilities, only: wp;
!!   This module defines an abstract class to represent an abstract function to calculate distance
implicit none
!
type,abstract :: distance_base
!! Abstract class `distance_base`
    contains
        procedure(distance_function1),deferred :: calculate
end type distance_base
!*****
abstract interface
!========================================================================================
    function distance_function1(distance,vector1,vector2) result(d)
!========================================================================================
!! Template for the calculate function
        import :: distance_base
        import :: wp
!! Import section
        class(distance_base) :: distance
!! A `distance_base` object
        real(kind=wp),dimension(:,:),intent(inout) :: vector1
!! A real vector
        real(kind=wp),dimension(:,:),intent(inout) :: vector2
!! A real vector
        real(kind=wp) :: d
!! A real variable with the distance
  end function distance_function1
!
end interface

end module distance_base_utilities