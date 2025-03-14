!! author: Oscar Garcia-Cabrejo
!! date: 12/04/2024
!! version: 0.1
!! This module defines a class called `kohonen_pattern` to store the input patterns 
module kohonen_pattern_utilities
!! This module defines a class called `kohonen_pattern` to store the input patterns
use error_handling, only: error_t,error_stop;
use precision_utilities, only: wp;
use constants_utilities, only: NUMCHAR;
use kohonen_prototype_utilities, only: kohonen_prototype;
!
implicit none
!
type kohonen_pattern
!!  Class to represent a container for input data to  a kohonen map
   private
      type(kohonen_prototype) :: pattern
      character(len=NUMCHAR) :: pattern_name 
   contains
      procedure,public :: create => kohonen_pattern_create
      procedure,public :: destroy => kohonen_pattern_destroy
      procedure,public :: get => kohonen_pattern_accessor
      procedure,public :: set => kohonen_pattern_mutator
      procedure,public :: print => kohonen_pattern_print
      procedure,public :: get_nrow => kohonen_pattern_nrow
      procedure,public :: get_ncol => kohonen_pattern_ncol
!
end type kohonen_pattern
!
contains
!========================================================================================
   subroutine kohonen_pattern_create(current_pattern,input,name)
!========================================================================================
!!   Kohonen pattern constructor
      class(kohonen_pattern) :: current_pattern
!! A `kohonen_pattern` object
      real(kind=wp),dimension(:,:),intent(inout) :: input
!! A real array
      character(len=*),optional :: name
!! A character string with the name of the pattern   
      call current_pattern%pattern%create(input);
      if(present(name)) then
         current_pattern%pattern_name=trim(name);
      else
         current_pattern%pattern_name="";
      endif
!   
 end subroutine kohonen_pattern_create
!========================================================================================
   subroutine kohonen_pattern_destroy(current_pattern)
!========================================================================================
!!   Kohonen pattern destructor
      class(kohonen_pattern) :: current_pattern
!! A `kohonen_pattern` object
      call current_pattern%pattern%destroy();
!   
   end subroutine kohonen_pattern_destroy
!========================================================================================
   subroutine kohonen_pattern_accessor(current_pattern,pattern_value)
!========================================================================================
!! Kohonen pattern accessor
      class(kohonen_pattern) :: current_pattern
!! A `kohonen_pattern` object
      type(kohonen_prototype),intent(inout) :: pattern_value
!
      pattern_value=current_pattern%pattern;
!
   end subroutine kohonen_pattern_accessor
!========================================================================================
   subroutine kohonen_pattern_mutator(current_pattern,pattern_value)
!========================================================================================
!!   kohonen_pattern_mutator
      class(kohonen_pattern) :: current_pattern
!! A `kohonen_pattern` object
      type(kohonen_prototype),intent(inout) :: pattern_value
!
      current_pattern%pattern=pattern_value;
!
   end subroutine kohonen_pattern_mutator
!========================================================================================
   subroutine kohonen_pattern_print(current_pattern,unit_)
!========================================================================================
!! Subroutine to print a Kohonen pattern
      class(kohonen_pattern) :: current_pattern
!! A `kohonen_pattern` object
      integer,intent(inout),optional :: unit_
!! An integer variable with the number of the unit where the patterns will be printed
      if(present(unit_)) then
         write(unit_,*)
         write(unit_,*) 'PATTERN: ',trim(current_pattern%pattern_name);
         write(unit_,*)
         call current_pattern%pattern%print(unit_);
      else
         write(*,*)
         write(*,*) 'PATTERN: ',trim(current_pattern%pattern_name);
         write(*,*) 
         call current_pattern%pattern%print();
      endif
 !  
   end subroutine kohonen_pattern_print
!========================================================================================
   function kohonen_pattern_nrow(current_pattern) result(nr)
!========================================================================================
!! Function to calculate the number of rows (samples) in a pattern
      class(kohonen_pattern) :: current_pattern
!! A `kohonen_pattern` object
      integer :: nr
!! An integer with the number of rows (samples) in a pattern
      nr=current_pattern%pattern%get_nrow();
!
   end function kohonen_pattern_nrow
!========================================================================================
   function kohonen_pattern_ncol(current_pattern) result(nc)
!========================================================================================
!! Function to calculate the number of columns (variables) in a pattern
      class(kohonen_pattern) :: current_pattern
!! A `kohonen_pattern` object
      integer :: nc
!! An integer with the number of columns (variables) in a pattern
      nc=current_pattern%pattern%get_ncol();
!
   end function kohonen_pattern_ncol
!
end module kohonen_pattern_utilities