!! author: Oscar Garcia-Cabrejo
!! date: 03/16/2025
!! version: 0.2
!! This module defines a factory to create distance objects 
module factory_distance_utilities
!! This module defines a factory to create distance objects 
use error_handling, only: error_stop;
use constants_utilities, only: NUMCHAR;
use distance_base_utilities, only: distance_base;
use euclidean_distance_utilities, only: euclidean_distance;
use manhattan_distance_utilities, only: manhattan_distance;
use max_distance_utilities, only: max_distance;
use correlation_distance_utilities, only: correlation_distance;
use direction_cosine_distance_utilities, only: direction_cosine_distance;
!
implicit none;
!
type factory_distance 
!!  Class to represent a distance factory 
    contains
        procedure,public :: create_distance
end type factory_distance
!
contains
!======================================================================================== 
    subroutine create_distance(factory,type_,dist)
!========================================================================================
!! Class constructor
        class(factory_distance) :: factory
!! A `factory_distance` object
        character(len=*) :: type_
!! A character string with the type of distance to be instantiated
        class(distance_base),allocatable :: dist
!! An allocatable `distance_base` object
        integer :: ierr
        character(len=NUMCHAR) :: base_message,message
!
        base_message='FACTORY DISTANCE ERROR';
        if(allocated(dist)) deallocate(dist);
        select case(trim(type_))
          case('euclidean')
            allocate(euclidean_distance :: dist,stat=ierr);
            if(ierr /= 0) then
                message=trim(base_message)//' while allocating memory for euclidean distance';
                call error_stop(message);
            end if
          case('manhattan')
            allocate(manhattan_distance :: dist,stat=ierr);
            if(ierr /= 0) then
                message=trim(base_message)//' while allocating memory for manhattan distance';
                call error_stop(message);
            end if
        case('max')
            allocate(max_distance :: dist,stat=ierr);
            if(ierr /= 0) then
                message=trim(base_message)//' while allocating memory for max distance';
                call error_stop(message);
            end if
        case('correlation')
            allocate(correlation_distance :: dist,stat=ierr);
            if(ierr /= 0) then
                message=trim(base_message)//' while allocating memory for correlation distance';
                call error_stop(message);
            end if
        case('direction_cosine')
            allocate(direction_cosine_distance :: dist,stat=ierr);
            if(ierr /= 0) then
                message=trim(base_message)//' while allocating memory for direction cosine distance';
                call error_stop(message);
            end if
          case default
            write(*,*) 'ERROR: the requested distance is not defined'
            stop
        end select
!
    end subroutine create_distance
!  
end module factory_distance_utilities