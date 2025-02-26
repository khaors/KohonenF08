!! author: Oscar Garcia-Cabrejo
!! date: 06/06/2023
!! version: 0.1
!! Define an abstract class random_generator_base to be used to derive different types of 
!!  random number generators to be use with ATALIB03
module random_generator_base_utilities
!
use precision_utilities, only: wp;
!
implicit none;
!
     type,abstract :: random_generator_base
!! Abstract class to derive random number generator classes 
       contains
    !  * constructor  
         procedure(random_generator_constructor),deferred :: create
    !  * destructor     
         procedure(random_generator_destructor),deferred :: destroy
    !  * generate     
         procedure(random_generator_generate),deferred :: generate
    !     procedure(random_generator_initialize),deferred :: initialize
     end type random_generator_base
!
abstract interface
!========================================================================================
    subroutine random_generator_constructor(generator, iseed)
!======================================================================================== 
!! Template of the constructor of the classes derived from the random_generator class.
        import :: random_generator_base
!! random_generator_base
        class(random_generator_base) :: generator
!! generator: A random_generator_base object
        integer,intent(in),optional :: iseed
!! iseed: An integer with the seed of the random generator.
    end subroutine random_generator_constructor
!========================================================================================
    subroutine random_generator_destructor(generator)
!========================================================================================
!! Template of the destructor of the classes derived from the random_generator class.
        import :: random_generator_base
!! random_generator_base
        class(random_generator_base) :: generator
!! generator: A random_generator_base object
    end subroutine random_generator_destructor
!========================================================================================
    function random_generator_generate(generator) result(r)
!========================================================================================
!! Template of the function used to generate realizations of the random numbers in the 
!! classes derived from the random_generator class.
        import :: random_generator_base
        import :: wp
!! random_generator_base 
    class(random_generator_base) :: generator
!! generator: A random_generator_base object
    real(kind=wp) :: r
!! r: A real number 
    end function random_generator_generate

end interface
!    !
end module random_generator_base_utilities