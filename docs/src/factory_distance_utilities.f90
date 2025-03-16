!! author: Oscar Garcia-Cabrejo
!! date: 03/16/2025
!! version: 0.2
!! This module defines a factory to create distance objects 
module factory_distance_utilities
!! This module defines a factory to create distance objects 
use distance_base_utilities, only: distance_base;
use euclidean_distance_utilities, only: euclidean_distance;
use manhattan_distance_utilities, only: manhattan_distance;
use max_distance_utilities, only: max_distance;
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
        select case(trim(type_))
          case('euclidean')
            !write(*,*) 'Euclidean distance';
            if(allocated(dist)) deallocate(dist);
            allocate(euclidean_distance :: dist);
          case('manhattan')
    !         !write(*,*) 'Manhattan distance allocated';
            if(allocated(dist)) deallocate(dist);
            allocate(manhattan_distance :: dist);
        case('max')
            if(allocated(dist)) deallocate(dist);
            allocate(max_distance :: dist);
          case default
            write(*,*) 'ERROR: the requested distance is not defined'
            stop
        end select
!
    end subroutine create_distance
!  
end module factory_distance_utilities