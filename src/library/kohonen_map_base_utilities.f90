!****h* Kohonen/kohonen_map_base_utilities
!
! NAME
!  MODULE kohonen_map_base_utilities
!
! PURPOSE
!  This module defines an abstract class for kohonen maps 
!
! AUTHOR
! Oscar Garcia-Cabrejo
!$Author$
! NOTES 
!$Rev$
!$HeadURL$
! MODIFICATION HISTORY
!$LastChangedDate$
!$LastChangedRevision$
!$LastChangedBy$
!*****
module kohonen_map_base_utilities
!
use precision_utilities, only: wp;
use kohonen_layer_parameters_utilities, only: kohonen_layer_parameters;
use kohonen_pattern_utilities, only: kohonen_pattern;
!
implicit none
!****c* kohonen_map_base_utilities/kohonen_map_base
! NAME
!   kohonen_map_base
! PURPOSE
!   Abstract Class to represent a template for a kohonen map
type,abstract :: kohonen_map_base 
  contains
!  
!  METHODS
!
    procedure(kohonen_map_constructor),public,deferred :: create
    procedure(kohonen_map_destructor),public,deferred :: destroy
    procedure(kohonen_map_function1),public,deferred :: train 
    procedure(kohonen_map_function2),public,deferred :: predict
end type kohonen_map_base
!*****
abstract interface
!****f* kohonen_map_base_utilities/kohonen_map_constructor
! NAME
!   kohonen_map_constructor
! PURPOSE
!   Template function for the constructor of a kohonen map
! SYNOPSIS
!========================================================================================
  subroutine kohonen_map_constructor(kohonen_map,training_parameters)
!========================================================================================
    import :: kohonen_map_base
    import :: kohonen_layer_parameters
    class(kohonen_map_base) :: kohonen_map
    type(kohonen_layer_parameters),dimension(:) :: training_parameters
!*****    
  end subroutine kohonen_map_constructor
!****f* kohonen_map_base_utilities/kohonen_map_destructor
! NAME
!   kohonen_map_destructor
! PURPOSE
!   Template function for the destructor of a kohonen map
! SYNOPSIS
!========================================================================================
  subroutine kohonen_map_destructor(kohonen_map)
!========================================================================================
    import :: kohonen_map_base
    class(kohonen_map_base) :: kohonen_map
!*****    
  end subroutine kohonen_map_destructor
!****f* kohonen_map_base_utilities/kohonen_map_function1
! NAME
!   kohonen_map_function1
! PURPOSE
!   Template function for the training function of a kohonen map
! SYNOPSIS
!========================================================================================
  subroutine kohonen_map_function1(kohonen_map,input_data)
!========================================================================================
    import :: kohonen_map_base
    import :: kohonen_pattern
    class(kohonen_map_base) :: kohonen_map
    type(kohonen_pattern),dimension(:),intent(inout) :: input_data
!    real(kind=wp),dimension(:,:),intent(inout),optional :: distances
!*****    
  end subroutine kohonen_map_function1
!****f* kohonen_map_base_utilities/kohonen_map_function2
! NAME
!   kohonen_map_function2
! PURPOSE
!   Template function for the prediction function of a kohonen map
! SYNOPSIS
!========================================================================================
  subroutine kohonen_map_function2(kohonen_map,input_data,map_output)
!========================================================================================
    import :: kohonen_map_base
    import :: kohonen_pattern
    class(kohonen_map_base) :: kohonen_map
    type(kohonen_pattern),dimension(:),intent(inout) :: input_data
    integer,dimension(:,:),intent(out) :: map_output
!*****
  end subroutine kohonen_map_function2
!
end interface
!
end module kohonen_map_base_utilities