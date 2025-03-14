!! author: Oscar Garcia-Cabrejo
!! date: 06/10/2023
!! version: 0.1
!!  This module includes the definition of a class called quicksort
!!  that is used to encapsulate the quicksorting algorithm. This 
!!  algorithm is highly efficient.
module quicksort_utilities
!
use precision_utilities, only: wp;
use sort_base_utilities, only: sort_base;
!
implicit none;
!
private;
!
type, extends(sort_base) :: quicksort
!!  class that encapsulates the quicksort algorithm
    contains
        procedure, pass :: sort => quick_sort
end type quicksort
!
public :: quicksort;
    !
contains
!===============================================================================================================
    recursive subroutine quick_sort(my_sort,list, order)
!===============================================================================================================
!! main subroutine of the quick sort algorithm. 
        implicit none
        class(quicksort),intent(inout) :: my_sort
!! A quicksort object
        real(kind=wp), dimension (:), intent(inout)  :: list
!! A real vector with the values to be sorted.
        integer, dimension (:), intent(inout)  :: order
!! An integer vector with the positions of the ordered samples.
!! quick sort routine from:
!! brainerd, w.s., goldberg, c.h. & adams, j.c. (1990) "programmer's guide to
!! fortran 90", mcgraw-hill  isbn 0-07-000248-7, pages 149-150.
!! modified by alan miller to include an associated integer array which gives
!! the positions of the elements in the original order.
    
! local variable
        integer :: i
        !write(*,*) size(list)
        do i = 1, size(list)
            order(i) = i
        end do
    
        call quick_sort_1(1, size(list))
    
        contains
    
            recursive subroutine quick_sort_1(left_end, right_end)
    
                integer, intent(in) :: left_end, right_end
                !*****
                !     local variables
                integer             :: i, j, itemp
                real(kind=wp)        :: reference, temp
                integer, parameter  :: max_simple_sort_size = 6
    
                if (right_end < left_end + max_simple_sort_size) then
                ! use interchange sort for small lists
                    call interchange_sort(left_end, right_end)
    
                else
                ! use partition ("quick") sort
                    reference = list((left_end + right_end)/2)
                    i = left_end - 1; j = right_end + 1
    
                    do
                    ! scan list from left end until element >= reference is found
                        do
                            i = i + 1
                            if (list(i) >= reference) exit
                        end do
                        ! scan list from right end until element <= reference is found
                        do
                            j = j - 1
                            if (list(j) <= reference) exit
                        end do
    
    
                        if (i < j) then
                        ! swap two out-of-order elements
                            temp = list(i); list(i) = list(j); list(j) = temp
                            itemp = order(i); order(i) = order(j); order(j) = itemp
                        else if (i == j) then
                            i = i + 1
                            exit
                        else
                            exit
                        end if
                    end do
                    if (left_end < j) call quick_sort_1(left_end, j)
                    if (i < right_end) call quick_sort_1(i, right_end)
                end if
    
            end subroutine quick_sort_1
!===============================================================================================================
            subroutine interchange_sort(left_end, right_end)
!===============================================================================================================
!! subroutine to interchange elements
                integer, intent(in) :: left_end, right_end
!! left_end, right_end: integer variables with the positiions to be interchanged
  !     local variables
                integer             :: i, j, itemp
                real(kind=wp)        :: temp
    
                do i = left_end, right_end - 1
                  do j = i+1, right_end
                    if (list(i) > list(j)) then
                      temp = list(i); list(i) = list(j); list(j) = temp
                      itemp = order(i); order(i) = order(j); order(j) = itemp
                    end if
                  end do
                end do
    
            end subroutine interchange_sort
    !
    end subroutine quick_sort
    !
end module quicksort_utilities