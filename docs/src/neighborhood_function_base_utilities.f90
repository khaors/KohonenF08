!! author: Oscar Garcia-Cabrejo
!! date: 03/16/2025
!! version: 0.2
!! This module defines an abstract class to define neighborhood functions
module neighborhood_function_base_utilities
!! This module defines an abstract class to define neighborhood functions
use precision_utilities, only: wp;
implicit none;
!
type,abstract :: neighborhood_function_base
!! Abstract class used to derive classes that calculates the effect of the input on the 
!! units of a SOM (neighborhood function)
    contains
        procedure(neighborhood_function_calculate),deferred :: calculate
end type neighborhood_function_base
!
abstract interface
    function neighborhood_function_calculate(my_neigh_fn,geometric_distance) result(n)
!! Subroutine template to calculate function
        import :: neighborhood_function_base
        import :: wp
!! Import section
        class(neighborhood_function_base) :: my_neigh_fn
!! A `neighborhood_function_base` object
        real(kind=wp),intent(inout) :: geometric_distance
!! A real variable with the geometric or grid distance of a SOM unit
        real(kind=wp) :: n
!! A real variable with the value of the neighborhood function
    end function neighborhood_function_calculate
!
end interface
!
end module neighborhood_function_base_utilities