!! author: Oscar Garcia-Cabrejo
!! date: 12/04/2024
!! version: 0.1
!!  This module defines a class for kohonen prototype (units inside kohonen layers) 
module kohonen_prototype_utilities
!
use error_handling, only: error_t,error_stop;
use precision_utilities, only: wp;
use constants_utilities, only: NUMCHAR;
use distance_base_utilities, only: distance_base;
!
implicit none
!
private;
!
type kohonen_prototype
!!   Class to store a prototype inside a Kohonen map
    private
        real(kind=wp),allocatable :: data_(:,:)
        integer :: number_rows,number_columns
    contains
!
        procedure :: create => kohonen_prototype_constructor
        procedure :: destroy => kohonen_prototype_destructor
        procedure :: get_prototype => kohonen_prototype_accessor
        procedure :: set_prototype => kohonen_prototype_mutator
        procedure :: print => kohonen_prototype_print
        procedure :: distance => kohonen_prototype_distance
        procedure :: get_nrow => kohonen_prototype_nrow
        procedure :: get_ncol => kohonen_prototype_ncol
!
end type kohonen_prototype
!
public :: kohonen_prototype;
!
 contains
!========================================================================================
    subroutine kohonen_prototype_constructor(prototype,input_data)
!========================================================================================
!! Class Constructor
        class(kohonen_prototype) :: prototype
!! A `kohonen_prototype` object
        real(kind=wp),dimension(:,:) :: input_data
!! A real array
        integer :: ierr
        character(len=NUMCHAR) :: message
        !
        prototype%number_rows=size(input_data,1);
        prototype%number_columns=size(input_data,2);
        allocate(prototype%data_(prototype%number_rows,prototype%number_columns),stat=ierr);
        prototype%data_=input_data;
!
    end subroutine kohonen_prototype_constructor
!========================================================================================
    subroutine kohonen_prototype_destructor(prototype)
!========================================================================================
!! Class Destructor
        class(kohonen_prototype),intent(inout) :: prototype
!! A `kohonen_prototype` object
        if(allocated(prototype%data_)) then
!           write(*,*) 'Prototype, ',allocated(prototype%data_),size(prototype%data_,1),size(prototype%data_,2)
            deallocate(prototype%data_);
!           write(*,*) 'Prototype release'
        endif
    end subroutine kohonen_prototype_destructor
!========================================================================================
    subroutine kohonen_prototype_accessor(prototype,d)
!========================================================================================
!! Acccessor
        class(kohonen_prototype) :: prototype
!! A `kohonen_prototype` object
        real(kind=wp),dimension(prototype%number_rows,prototype%number_columns) :: d
!! A real variable with the value of the prototype
        if(allocated(prototype%data_)) then
!           write(*,*) 'ACCESOR'
            d=prototype%data_;
        else 
            stop
        endif
!   
    end subroutine kohonen_prototype_accessor
!========================================================================================
    subroutine kohonen_prototype_mutator(prototype,new_data)
!========================================================================================
!! Mutator
        class(kohonen_prototype) :: prototype
!! A `kohonen_prototype` object
        real(kind=wp),dimension(:,:),intent(inout) :: new_data
!
        if( (size(new_data,1) == prototype%number_rows) .and. &
            (size(new_data,2) == prototype%number_columns) ) then
            prototype%data_=new_data;
        endif
!
    end subroutine kohonen_prototype_mutator
!========================================================================================
    subroutine kohonen_prototype_print(prototype,unit_)
!========================================================================================
!!  Function to print a kohonen prototype
        class(kohonen_prototype) :: prototype
!! A `kohonen_prototype` object
        integer,intent(inout),optional :: unit_
!! An integer variable with the number of the unit
        integer :: ix,iy
!
        if(present(unit_)) then
            write(unit_,*) 'Prototype'
            if(size(prototype%data_,2) /= 1) then
                do ix=1,size(prototype%data_,1)
                    write(unit_,*) (prototype%data_(ix,iy),iy=1,size(prototype%data_,2));
                enddo!ix
            else
                write(unit_,*) (prototype%data_(ix,1),ix=1,size(prototype%data_,1));
            endif
        else
            write(*,*) 'Prototype',size(prototype%data_,1),size(prototype%data_,2);
            do ix=1,size(prototype%data_,1)
                write(*,*) (prototype%data_(ix,iy),iy=1,size(prototype%data_,2));
            enddo!ix
        endif
!
    end subroutine kohonen_prototype_print
!========================================================================================
    function kohonen_prototype_distance(prototype,prototype1,f) result(d)
!========================================================================================
!! Function to calculate the distance between two prototypes
       class(kohonen_prototype) :: prototype
!! A `kohonen_prototype` object   
       type(kohonen_prototype) :: prototype1
!! A `kohonen_prototype` object
       class(distance_base),allocatable :: f
!!
       real(kind=wp)  :: d
!! A real variable with the distance between prototypes
       real(kind=wp),dimension(prototype%number_rows,prototype%number_columns) :: prot1,prot2
       d=0.0_wp;
       call prototype%get_prototype(prot1);
       call prototype1%get_prototype(prot2);
       d=f%calculate(prot1,prot2);
!
    end function kohonen_prototype_distance
!========================================================================================
    function kohonen_prototype_nrow(prototype) result(nr)
!========================================================================================
!! Function to get the number of rows of the prototype
        class(kohonen_prototype) :: prototype
!! A `kohonen_prototype` object 
        integer :: nr
!! Integer variable with the number of rows
        nr=size(prototype%data_,1);
!
    end function kohonen_prototype_nrow
!========================================================================================
    function kohonen_prototype_ncol(prototype) result(nc)
!========================================================================================
!! Function to get the number of columns of the prototype
        class(kohonen_prototype) :: prototype
!! A `kohonen_prototype` object
        integer :: nc
!! Integer variable with the number of columns
        nc=size(prototype%data_,2);
!
    end function kohonen_prototype_ncol
!
end module kohonen_prototype_utilities