!! author: Oscar Garcia-Cabrejo
!! date: 01/24/2020
!! version: 0.1
!! This module defines several numerical constants used in the ATALIB library. These constants 
!! can be imported in any module of the library. This module is in constant evolution and new 
!! constants are being added to the library.
module constants_utilities
!    
use precision_utilities, only: wp, EP;
!
implicit none;
!
public;
!
real(wp), parameter :: PI =    4.0_wp*atan(1.0_wp);
real(wp), parameter :: LN2 = log(2.0_wp);
!
real(wp), parameter :: PIEP =  4.0_EP*atan(1.0_EP)
real(wp), parameter :: INVPIEP = 0.25_EP/atan(1.0_EP)
real(wp), parameter :: TWOPIEP = 8.0_EP*atan(1.0_EP)
real(wp), parameter :: PIOV2EP = 2.0_EP*atan(1.0_EP)
real(EP), parameter :: PIOV4EP = atan(1.0_EP)
complex(wp), parameter :: EYE = (0.0_wp,1.0_wp)
complex(wp), parameter :: I_ = (0.0_wp, 1.0_wp)
real(wp), parameter :: E = exp(1.0_EP)
real(wp), parameter :: SQRT2 = sqrt(2.0_EP)
real(wp), parameter :: UNEST=-999.999999_EP
real(wp), parameter :: EPSILON=1.0d-10
real(wp),parameter :: TOLERANCE=1.0d-6
real(wp), parameter :: DEG2RAD=PI/180.0_wp
!
! length of filenames
!
integer, parameter :: NUMCHAR = 128;
integer, parameter :: MAXNST = 10;

end module constants_utilities