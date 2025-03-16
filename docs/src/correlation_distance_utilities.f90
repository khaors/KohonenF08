!! author: Oscar Garcia-Cabrejo
!! date: 03/16/2025
!! version: 0.1
!! This module defines a class to calculate the correlation distance between kohonen prototypes 
module correlation_distance_utilities
!! This module defines a class to calculate the correlation distance between kohonen prototypes 
    use precision_utilities, only: wp;
    use distance_base_utilities, only: distance_base;
    use general_utilities, only: correlation_coefficient
    !
    implicit none;
    !
    private;
    !
    type,extends(distance_base) :: correlation_distance
    !! Class to calculate the correlation distance
        contains
            procedure,public :: calculate => calculate_correlation_distance
    end type correlation_distance
    !
    public :: correlation_distance;
    !
    contains 
    !========================================================================================
        function calculate_correlation_distance(distance,vector1,vector2) result(d)
    !========================================================================================
    !! Function to calculate the Manhattan distance between vectors
            class(correlation_distance) :: distance
    !! A `Manhattan_distance` object
            real(kind=wp),dimension(:,:),intent(inout) :: vector1,vector2
    !! A real vector
            real(kind=wp) :: d
    !! A real vector
            real(kind=wp),dimension(size(vector1,1)*size(vector1,2)) :: v1,v2
            real(kind=wp) :: mx,my,sdx,sdy
            integer :: nrow,ncol
            !
            nrow=size(vector1,1);
            ncol=size(vector1,2);
            v1=reshape(vector1,[nrow*ncol]);
            v2=reshape(vector2,[nrow*ncol]);
            d=correlation_coefficient(v1,v2);
    !! A real variable with the distance
        end function calculate_correlation_distance

end module correlation_distance_utilities