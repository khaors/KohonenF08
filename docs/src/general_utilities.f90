!! author: Oscar Garcia-Cabrejo
!! date: 12/04/2024
!! version: 0.1
!! This module includes general purpose functions used in several parts of the library
module general_utilities
!! This module includes general purpose functions used in several parts of the library
use precision_utilities, only: wp;  
!
implicit none;
!
private
!
interface maximum
    module procedure  maximum_only,maximum_position
end interface 
!
public :: minimum,maximum,mean,variance,std
public :: coefficient_of_variation,correlation_coefficient
!
contains
!===================================================================================================
    function minimum(a) result(mn)
!===================================================================================================
!! Function to calculate the minimum of an array
        real(kind=wp),dimension(:) :: a
!! Real array
        real(kind=wp) :: mn
!! Real value with the minimum
        integer :: i,number_elements
        mn=a(1)
        number_elements=size(a)
        do i=2,number_elements
          if(a(i) .lt. mn) then
            mn=a(i)
          endif
        enddo
!
     end function minimum
!===================================================================================================
    subroutine maximum_only(a,mx)
!===================================================================================================
!! Subroutine to calculate the maximum of a real array
        real(kind=wp),dimension(:) :: a
!!
        real(kind=wp) :: mx
!!
        integer :: i,number_elements
        mx=a(1)
        number_elements=size(a)
        do i=2,number_elements
          if(a(i) .gt. mx) then
            mx=a(i)
          endif
        enddo
!
    end subroutine maximum_only
!=============================================================================================
    subroutine maximum_position(a,mx,p)
!=============================================================================================
!! Subroutine to find the position of the maximum value of a real array
        real(kind=wp),dimension(:) :: a
!! A real array
        real(kind=wp) :: mx
!! A real value with the maximum
        integer :: p
!! An integer value with the position of the maximum
        integer :: i,number_elements
        mx=a(1)
        number_elements=size(a)
        do i=2,number_elements
          if(a(i) .gt. mx) then
            mx=a(i);p=i;
          endif
        enddo
        !
    end subroutine maximum_position
!=============================================================================================
    function mean(a) result(mn)
!=============================================================================================
!! Function to calculate the mean of a real array
        real(kind=wp),dimension(:),intent(inout) :: a
!! A real array
        real(kind=wp) :: mn
!! A real value with the calculated mean
        mn=sum(a)/float(size(a));
!
    end function mean
!=============================================================================================
    function variance(a) result(v)
!=============================================================================================
!! Function to calculate the variance of a real array
        real(kind=wp),dimension(:),intent(inout) :: a
!! A real array
        real(kind=wp) :: v
!! A real variable with the calculated variance 
        real(kind=wp),dimension(size(a)) :: a2
!
        a2=a**2;
        v=mean(a2)-(mean(a))**2;
!
    end function variance
!=============================================================================================
    function std(a) result(s)
!=============================================================================================
!! Function to calculate the standard deviation of a real array
        real(kind=wp),dimension(:),intent(inout) :: a
!! A real array
        real(kind=wp) :: s
!! A real variable with the calculated standard deviation
        real(kind=wp) :: v
        !
        v=variance(a);
        s=sqrt(v);
!
    end function std
!============================================================================================
    function coefficient_of_variation(a) result(cov)
!============================================================================================
!! Function to calculate the coefficient of variation of a real array
        real(kind=wp),dimension(:),intent(inout) :: a
!! A real array
        real(kind=wp) :: cov
!! A real variable with the calculated coefficient of variation
        cov=std(a)/mean(a);

    end function coefficient_of_variation
!============================================================================================
    function correlation_coefficient(x,y) result(cor)
!============================================================================================
!! Function to calculate the correlation coefficient between two real vectors
        real(kind=wp),dimension(:),intent(inout) :: x,y
!! Real arrays
        real(kind=wp) :: cor 
!! Real value with the correlation coefficient
        integer :: ndat
        !
        ndat=size(x);
        cor=(sum((x-mean(x))*(y-mean(y)))/dble(ndat))/(std(x)*std(y));
    end function correlation_coefficient
!
end module general_utilities
