!! author: Oscar Garcia-Cabrejo
!! date: 12/23/2020
!! version: 0.1
!! This module defines the precision constants used in all modules of the library.
module precision_utilities
! i8=>int8, i16=>int16, i32=>int32, i64=>int64
use,intrinsic :: iso_fortran_env, only: real32,real64,int8,int16,int32,int64, wp => real64
!
implicit none;
!
integer,parameter :: sp = real32;
!! Single precision
integer,parameter :: dp = wp;
!! Double precision
integer,parameter :: ep = wp;
!! Extended precision
integer,parameter :: i8 = int8
!! Integer (short)
integer,parameter :: i16 = int16
!! Integer (short)
integer,parameter :: i32 = int32 
!! Integer (long)
integer,parameter :: i64 = int64
!! Integer (long)

end module precision_utilities
