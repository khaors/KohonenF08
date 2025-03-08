!! author: Oscar Garcia-Cabrejo
!! date: 12/04/2024
!! version: 0.1
!! This module defines an abstract class for kohonen maps
module kohonen_map_base_utilities
!! This module defines an abstract class for kohonen maps
use precision_utilities, only: wp;
use kohonen_layer_parameters_utilities, only: kohonen_layer_parameters;
use kohonen_pattern_utilities, only: kohonen_pattern;
!
implicit none
!
type,abstract :: kohonen_map_base
!! Abstract Class to represent a template for a kohonen map
  contains
    procedure(kohonen_map_constructor),public,deferred :: create
    procedure(kohonen_map_destructor),public,deferred :: destroy
    procedure(kohonen_map_function1),public,deferred :: train 
    procedure(kohonen_map_function2),public,deferred :: predict
end type kohonen_map_base
!!
abstract interface
!========================================================================================
  subroutine kohonen_map_constructor(kohonen_map,training_parameters)
!========================================================================================
!! Template function for the constructor of a kohonen map
    import :: kohonen_map_base
    import :: kohonen_layer_parameters
!! Import section
    class(kohonen_map_base) :: kohonen_map
!! A `kohonen_map_base` object
    type(kohonen_layer_parameters),dimension(:) :: training_parameters
!! A `kohonen_layer_parameters` object
  end subroutine kohonen_map_constructor
!========================================================================================
  subroutine kohonen_map_destructor(kohonen_map)
!========================================================================================
!! Template function for the destructor of a kohonen map
    import :: kohonen_map_base
!! Import section
    class(kohonen_map_base) :: kohonen_map
!! A `kohonen_map_base` object
  end subroutine kohonen_map_destructor
!========================================================================================
  subroutine kohonen_map_function1(kohonen_map,input_data)
!========================================================================================
!!   Template function for the training function of a kohonen map
    import :: kohonen_map_base
    import :: kohonen_pattern
!! import section
    class(kohonen_map_base) :: kohonen_map
!! A `kohonen_map_base` object
    type(kohonen_pattern),dimension(:),intent(inout) :: input_data
!! An array of `kohonen_pottern` objects
  end subroutine kohonen_map_function1
!========================================================================================
  subroutine kohonen_map_function2(kohonen_map,input_data,map_output)
!========================================================================================
!!   Template function for the prediction function of a kohonen map
    import :: kohonen_map_base
    import :: kohonen_pattern
!! import section
    class(kohonen_map_base) :: kohonen_map
!! A `kohonen_map_base` object
    type(kohonen_pattern),dimension(:),intent(inout) :: input_data
!! An array of `kohonen_pottern` objects
    integer,dimension(:,:),intent(out) :: map_output
!! An integer array
  end subroutine kohonen_map_function2
!
end interface
!
end module kohonen_map_base_utilities