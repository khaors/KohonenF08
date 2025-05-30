!! author: M. Matsumoto and T. Nishimura. Modified by Oscar Garcia-Cabrejo
!! date: 03/16/2025
!! version: 0.1
!! This module defines a class that encapsulates the mersenne-twister random number generator
!!-------------------------------------------------------------------------------
!!   This is a Fortran translation of the 64-bit version of
!!   the Mersenne Twister pseudorandom number generator
!!
!!   Before using, initialize the state by using
!!       call init_genrand64(seed)
!!   or
!!       call init_by_array64(init_key)
!!
!!   Translated from C-program for MT19937-64 (2004/9/29 version)
!!   originally coded by Takuji Nishimura and Makoto Matsumoto
!!   http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/emt64.html
!!
!!   Fortran translation by Rémi Piatek
!!   The University of Copenhagen
!!   Department of Economics
!!   email: {first}.{last}@econ.ku.dk
!!
!!-------------------------------------------------------------------------------
!!   A C-program for MT19937-64 (2004/9/29 version).
!!   Coded by Takuji Nishimura and Makoto Matsumoto.
!!
!!   This is a 64-bit version of Mersenne Twister pseudorandom number
!!   generator.
!!
!!   Before using, initialize the state by using init_genrand64(seed)  
!!   or init_by_array64(init_key, key_length).
!!
!!   Copyright (C) 2004, Makoto Matsumoto and Takuji Nishimura,
!!   All rights reserved.                          
!!
!!   Redistribution and use in source and binary forms, with or without
!!   modification, are permitted provided that the following conditions
!!   are met:
!!
!!     1. Redistributions of source code must retain the above copyright
!!        notice, this list of conditions and the following disclaimer.
!!
!!     2. Redistributions in binary form must reproduce the above copyright
!!        notice, this list of conditions and the following disclaimer in the
!!        documentation and/or other materials provided with the distribution.
!!
!!     3. The names of its contributors may not be used to endorse or promote 
!!        products derived from this software without specific prior written 
!!        permission.
!!
!!   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
!!   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
!!   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
!!   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
!!   OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
!!   EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
!!   PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
!!   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
!!   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
!!   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
!!   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
!!
!!   References:
!!   T. Nishimura, ``Tables of 64-bit Mersenne Twisters''
!!     ACM Transactions on Modeling and 
!!     Computer Simulation 10. (2000) 348--357.
!!   M. Matsumoto and T. Nishimura,
!!     ``Mersenne Twister: a 623-dimensionally equidistributed
!!       uniform pseudorandom number generator''
!!     ACM Transactions on Modeling and 
!!     Computer Simulation 8. (Jan. 1998) 3--30.
!!
!!   Any feedback is very welcome.
!!   http://www.math.hiroshima-u.ac.jp/~m-mat/MT/emt.html
!!   email: m-mat @ math.sci.hiroshima-u.ac.jp (remove spaces)
!!
!! Modified by Oscar Garcia-Cabrejo from the original Fortran source code
!! 03-16-2025
!-------------------------------------------------------------------------------

module mt19937_64
!! This module defines a class that encapsulates the mersenne-twister random number generator
  use iso_fortran_env, only: output_unit, real64, int64
implicit none

private

! NOTE: genrand64_int64 is kept private, as it generates different numbers
!       compared to the original C code. This is because the original C code
!       uses unsigned integers, while Fortran relies on signed integers.
!       This, however, has no impact on the generation of real numbers
!       (they are identical to those produced by the original C code).

integer, parameter :: r64 = real64
integer, parameter :: i64 = int64

integer(i64), parameter :: nn       = 312_i64
integer(i64), parameter :: mm       = 156_i64
integer(i64), parameter :: seed_def = 5489_i64
integer(i64), parameter :: matrix_a = -5403634167711393303_i64
integer(i64), parameter :: um       = -2147483648_i64 ! most significant 33 bits
integer(i64), parameter :: lm       =  2147483647_i64 ! least significant 31 bits

real(r64),    parameter :: pi253_1  = 1._r64/(2._r64**53 - 1._r64)
real(r64),    parameter :: pi253    = 1._r64/(2._r64**53)
real(r64),    parameter :: pi252    = 1._r64/(2._r64**52)

integer(i64) :: mt(nn)       ! array for the state vector
integer     :: mti = nn+1   ! mti==nn+1 means mt(nn) is not initialized

type :: mt19937
    !! Class that encapsulates the mersenne-twister random number generator
    private
        integer(i64) :: mt(nn) =0_i64       ! array for the state vector
        integer     :: mti = nn+1   ! mti==nn+1 means mt(nn) is not initialized
    contains
        procedure,public :: init_genrand64
        procedure,public :: init_by_array64
        generic,public :: initialize => init_genrand64,init_by_array64
        procedure,public :: genrand64_real1
        procedure,public :: genrand64_real2
        procedure,public :: genrand64_real3
        procedure,public :: genrand64_int64

end type mt19937

public :: mt19937

contains


!-----------------------------------------------------------------------------
! Initializes mt(nn) with a seed

subroutine init_genrand64(mt,seed)
!! Initializes mt(nn) with a seed
  implicit none
  class(mt19937) :: mt
!! A `mt19937` object
  integer(i64), intent(in) :: seed
!! An integer variable with the random seed
  integer :: i

  mt%mt(1) = seed
  do i = 1, nn-1
    mt%mt(i+1) = 6364136223846793005_i64 * ieor(mt%mt(i), ishft(mt%mt(i), -62)) + i
  end do

  mt%mti = nn

end subroutine init_genrand64
!
subroutine init_by_array64(mt,init_key)
!! Initializes by an array with array-length
!!   init_key is the array for initializing keys
  implicit none
  class(mt19937) :: mt
!! A `mt19937` object 
  integer(i64), intent(in) :: init_key(:)
!! An integer array with random seeds
  integer(i64), parameter  :: c1 = 3935559000370003845_i64
  integer(i64), parameter  :: c2 = 2862933555777941757_i64
  integer(i64) :: i, j, k, kk, key_length

  call mt%init_genrand64(19650218_i64)
  key_length = size(init_key)
  i = 1_i64; j = 0_i64
  k = max(nn, key_length)

  do kk = 1, k
    mt%mt(i+1) = ieor(mt%mt(i+1), c1 * ieor(mt%mt(i), ishft(mt%mt(i), -62))) &
                + init_key(j+1) + j
    i = i+1; j = j+1
    if(i >= nn) then
      mt%mt(1) = mt%mt(nn)
      i = 1
    end if
    if(j >= key_length) j = 0
  end do

  do kk = 1, nn-1
    mt%mt(i+1) = ieor(mt%mt(i+1), c2 * ieor(mt%mt(i), ishft(mt%mt(i), -62))) - i
    i = i+1
    if(i >= nn) then
      mt%mt(1) = mt%mt(nn)
      i = 1
    end if
  end do

  mt%mt(1) = ishft(1_i64, 63)  ! MSB is 1; assuring non-zero initial array

end subroutine init_by_array64
!
integer(r64) function genrand64_int64(mt)
!! Generates a random number on [-2^63, 2^63-1]-interval
  implicit none
  class(mt19937) :: mt
!! A `mt19937` object
  integer(i64) :: mag01(0:1) = (/0_i64, matrix_a/)
  integer(i64) :: x
  integer     :: i

  if(mt%mti >= nn) then ! generate nn words at one time

    ! if init_genrand64() has not been called, a default initial seed is used
    if(mt%mti == nn+1) call mt%init_genrand64(seed_def)

    do i = 1, nn-mm
      x = ior(iand(mt%mt(i),um), iand(mt%mt(i+1), lm))
      mt%mt(i) = ieor(ieor(mt%mt(i+mm), ishft(x, -1)), mag01(iand(x, 1_i64)))
    end do

    do i = nn-mm+1, nn-1
      x = ior(iand(mt%mt(i), um), iand(mt%mt(i+1), lm))
      mt%mt(i) = ieor(ieor(mt%mt(i+mm-nn), ishft(x, -1)), mag01(iand(x, 1_i64)))
    end do

    x = ior(iand(mt%mt(nn), um), iand(mt%mt(1), lm))
    mt%mt(nn) = ieor(ieor(mt%mt(mm), ishft(x, -1)), mag01(iand(x, 1_i64)))

    mt%mti = 0

  end if

  mt%mti = mt%mti + 1
  x = mt%mt(mt%mti)

  x = ieor(x, iand(ishft(x,-29), 6148914691236517205_i64))
  x = ieor(x, iand(ishft(x, 17), 8202884508482404352_i64))
  x = ieor(x, iand(ishft(x, 37),   -2270628950310912_i64))
  x = ieor(x, ishft(x, -43))

  genrand64_int64 = x

end function genrand64_int64
!
real(r64) function genrand64_real1(mt)
!! Generates a random number on [0,1]-real-interval
  implicit none
  class(mt19937) :: mt
!! A `mt19937` object
  genrand64_real1 = real(ishft(mt%genrand64_int64(), -11), kind=r64) * pi253_1

end function genrand64_real1


real(r64) function genrand64_real2(mt)
!! Generates a random number on [0,1)-real-interval  
  implicit none
  class(mt19937) :: mt
!! A `mt19937` object
  genrand64_real2 = real(ishft(mt%genrand64_int64(), -11), kind=r64) * pi253

end function genrand64_real2


real(r64) function genrand64_real3(mt)
!! Generates a random number on (0,1)-real-interval
  implicit none
  class(mt19937) :: mt
!! A `mt19937` object
  genrand64_real3 = real(ishft(mt%genrand64_int64(), -12), kind=r64)
  genrand64_real3 = (genrand64_real3 + 0.5_r64) * pi252

end function genrand64_real3


end module mt19937_64


