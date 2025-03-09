!! author: Oscar Garcia-Cabrejo
!! date: 06/10/2023
!! version: 0.1
!!  Define an abstract class to represent a generic sort procedure.
module sort_base_utilities
!
use precision_utilities, only: wp;
!
implicit none;
!
type, abstract :: sort_base
!! Abstract class to represent a generic sort algoritm
    contains
    ! METHODS
    !   * sort
        procedure (sort_procedure), deferred, pass :: sort
end type sort_base
!
abstract interface
!===============================================================================================================
    subroutine sort_procedure(my_sort,list,order)
!===============================================================================================================
!! Subroutine to sort an array
        import  :: sort_base
        import  :: wp
!! import sort_base
        class(sort_base), intent(inout) :: my_sort
!! my_sort: A sort_base object
        real(kind=wp),dimension(:),intent(inout) :: list
!! list: A real vector
        integer, dimension(:), intent(inout) :: order
!! order: An integer vector
    end subroutine sort_procedure
!
end interface
!
end module sort_base_utilities