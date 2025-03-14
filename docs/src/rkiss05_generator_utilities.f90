!! author: Oscar Garcia-Cabrejo
!! date: 06/06/2023
!! version: 0.1
!!  This module includes the definition of a class called rkiss05_generator 
!!  that is used to generate random numbers with an uniform disitribution
!!  using the rkiss approach
!!
module rkiss05_generator_utilities
!!  Define the class rkiss05_generator that represents a random number generator
!!  based on the rkiss method
    use precision_utilities, only: wp;
    use random_generator_base_utilities,only: random_generator_base;
    !
    implicit none;
    !
    private;

    type,extends(random_generator_base) :: rkiss05_generator
      private
!! Class to represent a random number generator that implements the rkiss method
        integer :: seed
        integer :: x,y,w,z
      contains
    !  * constructor  
        procedure,public :: create => create_rkiss05_generator
    !  * destructor    
        procedure,public :: destroy => destroy_rkiss05_generator
    !  * generate    
        procedure,public :: generate => generate_rkiss05_generator
    !
    end type rkiss05_generator
    !*****
    public :: rkiss05_generator;
    !
    contains
!========================================================================================
     subroutine create_rkiss05_generator(generator,iseed)
!========================================================================================
!! Constructor of the rkiss05_generator class. In this class random seed is assigned to the 
!! random number generator and the internal state of the corresponding generator  is 
!! initialized.
        class(rkiss05_generator) :: generator
!!  generator: A rkiss05_generator object
     integer,intent(in),optional :: iseed
!! iseed: An integer with the seed of the random number generator
     if(present(iseed)) then
       generator%seed=iseed;
     else 
       generator%seed=12345;
     endif
     generator%x=iseed;
     generator%y=iseed+1;
     generator%z=iseed+2;
     generator%w=iseed+3;   
    ! 
    end subroutine create_rkiss05_generator
!========================================================================================
    subroutine destroy_rkiss05_generator(generator)
!======================================================================================== 
!! Destructor of the rkiss05_generator class. In this class, the random seed is set to 0 
        class(rkiss05_generator) :: generator
!! generator: A rkiss05_generator object
     if(generator%seed > 0) then
       generator%seed=0;
     endif
    !  
    end subroutine destroy_rkiss05_generator
!========================================================================================
    function generate_rkiss05_generator(generator) result(r)
!========================================================================================
!!  Function used to generate realizations of the random numbers with the rkiss05_generator class.       
        class(rkiss05_generator) :: generator
!! generator: A rkiss05_generator object
        real(kind=wp),parameter    :: am=4.656612873077392578d-10       ! multiplier 1/2^31
!! Multiplier
        real(kind=wp) :: r
!! r: A real variable with the generated random number     
          real(kind=wp)             :: rkiss05  
          integer          :: kiss
!          integer          :: x,y,z,w              ! working variables for the four generators
          !common /kisscom/x,y,z,w 
    !      
          generator%x = 69069 * generator%x + 1327217885
          generator%y= ieor (generator%y, ishft (generator%y, 13)); 
          generator%y= ieor (generator%y, ishft (generator%y, -17)); 
          generator%y= ieor (generator%y, ishft (generator%y, 5))
          generator%z = 18000 * iand (generator%z, 65535) + ishft (generator%z, - 16)
          generator%w = 30903 * iand (generator%w, 65535) + ishft (generator%w, - 16)
          kiss = ishft(generator%x + generator%y + ishft (generator%z, 16) + generator%w , -1)
          rkiss05=kiss*am
          r=rkiss05
    
     end function generate_rkiss05_generator
    !
    end module rkiss05_generator_utilities