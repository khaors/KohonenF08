!! author: Arjen Markus. Modified by Oscar Garcia-Cabrejo
!! date: 01/06/2021
!! version: 0.1
!! This module defines a class called `fortran_unit_test` which is used to represent the 
!! test to be applied to another class or a specific procedure.
module fortran_unit_test_utilities
!! The fortran_unit_test class is used to create simple unit tests for functions and 
!! subroutines contained in fortran modules.
use precision_utilities, only: wp;
!
implicit none;
!
type fortran_unit_test
    private
!! Class designed to be used in the testing phase of the development cycle. 
        character(len=100) :: entity_name!function / subroutine /module
        integer :: last_test!Last test that was started
        integer :: testno !Current test number
        integer :: nofails!Number of assertions that failed
        integer :: noruns!Number of runs so far              
        integer :: previous_unit
        logical :: call_final
    contains
        procedure,public :: create => create_fortran_unit_test
        procedure,public :: destroy => destroy_fortran_unit_test
        procedure,private :: assert_equal_int
        procedure,private :: assert_equal_int1d
        procedure,private :: assert_equal_char
        procedure,private :: assert_equal_char1d
        generic,public :: assert_equal => assert_equal_int, assert_equal_int1d, &
                          assert_equal_char, assert_equal_char1d!, assert_comparable_int2d
        procedure, private :: assert_comparable_real
        procedure, private :: assert_comparable_real1d
        procedure, private :: assert_comparable_real2d
        generic,public :: assert_comparable => assert_comparable_real,&
                          assert_comparable_real1d,assert_comparable_real2d
        procedure,public :: assert_true
        procedure,public :: assert_false
        procedure,public :: test
        procedure,private :: get_available_unit
        procedure,public :: file_exists   
end type fortran_unit_test
!
contains
!========================================================================================
    subroutine create_fortran_unit_test(my_test,name)
!========================================================================================
!! Class constructor. This subroutine requires as input the name of the test. This information 
!! will be printed on the screen during the actual tests
        class(fortran_unit_test) :: my_test
!! A fortran_unit_test class
        character(len=*),optional :: name
!! A character string with the name of the test  

!!     program main
!!        use fortran_unit_test_utilities;
!!        ...
!!        type(fortran_unit_test) :: current_test
!!        ...
!!        call current_test%create('Phantom Module');
!!        ...
!!     end program main

        if(present(name)) then
            my_test%entity_name='Unit Test: '//trim(name);
            write(*,'(a)') trim(my_test%entity_name)//' Started'
        else 
            write(*,'(a)') 'Unit Test Started';
        endif
        my_test%last_test = 0;
        my_test%testno = 0;
        my_test%nofails = 0;
        my_test%noruns = 0;
        my_test%previous_unit=10;
        my_test%call_final=.FALSE.;
!  
    end subroutine create_fortran_unit_test
!========================================================================================
    subroutine destroy_fortran_unit_test(my_test)
!========================================================================================
!! Class destructor
        class(fortran_unit_test) :: my_test
!! A fortran_unit_test object

!!     program main
!!        use fortran_unit_test_utilities;
!!        ...
!!        type(fortran_unit_test) :: current_test
!!        ...
!!        call current_test%create('Phantom Module');
!!        ...
!!        call current_test%destroy();
!!        ...
!!     end program main

        if(my_test%entity_name(1:2)  /= ' ') then
            write(*,'(a)') trim(my_test%entity_name)//' Finished'; 
        else 
            write(*,'(a)') 'Unit Test Finished'
        endif

        my_test%call_final=.TRUE.;
!      if ( my_test%file_exists("ftnunit.run") ) then
        write(*,'(a)') 'Report' 
        write(*,'(a,i5)') 'Total number of tests:                      ', my_test%testno
        write(*,'(a,i5)') 'Number of failed assertions:                ', my_test%nofails
        write(*,'(a,i5)') 'Number of runs needed to complete the tests:', my_test%noruns
        !call ftnunit_remove_file( "ftnunit.lst" )
!        stop
!    endif
!  
    end subroutine destroy_fortran_unit_test
!========================================================================================
    subroutine assert_equal_int( my_test, value1, value2, text )
!========================================================================================
!! Function to assert equality of int
        class(fortran_unit_test) :: my_test
!! A fortran_unit_test object
        integer, intent(in)          :: value1
!! An integer value
        integer, intent(in)          :: value2
!! An integer value
        character(len=*), intent(in) :: text
!! Character string with the text to be printed when the test fails.
        character(len=70) :: msg
        character(len=10) :: val1,val2
        if ( value1 /= value2) then
            my_test%nofails = my_test%nofails + 1
            msg='Values not equal: "'//trim(text)//'" - assertion failed'
            write(*,'(A)') trim(msg)
            write(val1, "(f10.5)") value1
            write(val2, "(f10.5)") value2 
            msg='Values: '// trim(val1)//' and '//trim(val2)
            write(*,'(A)') trim(msg) 
        endif
    end subroutine assert_equal_int
!========================================================================================
    subroutine assert_equal_int1d( my_test, array1, array2, text )
!========================================================================================
!! Function to assert equality of 1D array of int
        class(fortran_unit_test) :: my_test
!! A fortran_unit_test object 
    integer, dimension(:), intent(in) :: array1
    integer, dimension(:), intent(in) :: array2
    character(len=*), intent(in)      :: text

    integer                           :: i
    integer                           :: count
    character(len=70) :: msg

    if ( size(array1) /= size(array2) ) then
        my_test%nofails = my_test%nofails + 1
        msg='Arrays have different sizes: "'//trim(text)//'" - assertion failed';
        write(*,*) trim(msg)
    else
        if ( any( array1 /= array2 ) ) then
            my_test%nofails = my_test%nofails + 1
            msg='One or more values different: "'//trim(text)//'" - assertion failed';
            write(*,*) trim(msg)
            count = 0
            do i = 1,size(array1)
                if ( array1(i) /= array2(i) ) then
                    count = count + 1
                    write(msg,'(3a10)')    '    Index', '     First', '    Second';
                    write(*,*) trim(msg)
                    if ( count < 50 ) then
                        write(msg,'(3i10)')    i, array1(i), array2(i)
                    endif
                    write(*,*) 'Number of differences: ', count
                endif
            enddo
        endif
    endif
!    
end subroutine assert_equal_int1d
!========================================================================================
subroutine assert_equal_char( my_test, char1, char2, text )
!========================================================================================
!! subroutine to assert if two char variables are equal
    class(fortran_unit_test) :: my_test
!! A fortran_unit_test object
    character(len=*), intent(in)             :: char1
    character(len=*), intent(in)             :: char2
    character(len=*), intent(in) :: text

    character(len=70) :: msg
    if(trim(char1) /= trim(char2)) then
        my_test%nofails = my_test%nofails + 1
        msg='Character not equal: "'//trim(text)//'" - assertion failed'
        write(*,'(A)') trim(msg)
        msg='Character: '// trim(char1)//' and '//trim(char2)
        write(*,'(A)') trim(msg)    
    endif
!
end subroutine assert_equal_char
!========================================================================================
subroutine assert_equal_char1d( my_test, array1, array2, text )
!========================================================================================
!! Subroutine to assert equality of 1D array of characters
    class(fortran_unit_test) :: my_test
!! A fortran_unit_test object
    character(len=*), dimension(:), intent(in) :: array1
    character(len=*), dimension(:), intent(in) :: array2
    character(len=*), intent(in)      :: text

    integer                           :: i
    integer                           :: count
    character(len=70) :: msg

    if ( size(array1) /= size(array2) ) then
        my_test%nofails = my_test%nofails + 1
        msg='Arrays have different sizes: "'//trim(text)//'" - assertion failed';
        write(*,*) trim(msg)
    else
        if ( any( array1 /= array2 ) ) then
            my_test%nofails = my_test%nofails + 1
            msg='One or more values different: "'//trim(text)//'" - assertion failed';
            write(*,*) trim(msg)
            count = 0
            do i = 1,size(array1)
                if ( array1(i) /= array2(i) ) then
                    count = count + 1
                    write(msg,'(3a10)')    '    Index', '     First', '    Second';
                    write(*,*) trim(msg)
                    if ( count < 50 ) then
                        write(msg,'(3i10)')    i, array1(i), array2(i)
                    endif
                    write(*,*) 'Number of differences: ', count
                endif
            enddo
        endif
    endif
!    
end subroutine assert_equal_char1d
!========================================================================================
subroutine assert_comparable_real( my_test, value1, value2, margin, text )
!========================================================================================
!! subroutine to assert if two real values are equal to a given margin
    class(fortran_unit_test) :: my_test
!! A fortran_unit_test object
    real(wp), intent(in)             :: value1
    real(wp), intent(in)             :: value2
    real(wp), intent(in)             :: margin
    character(len=*), intent(in) :: text

    if ( abs(value1-value2) > 0.5 * margin * (abs(value1)+abs(value2)) ) then
        my_test%nofails = my_test%nofails + 1
        write(*,*) '    Values not comparable: "',trim(text), '" - assertion failed'
        write(*,*) '    Values: ', value1, ' and ', value2
    endif
end subroutine assert_comparable_real
!========================================================================================
subroutine assert_comparable_real1d( my_test, array1, array2, margin, text )
!========================================================================================
!! Subroutine to assert if two arrays of real are equal to a given margin
    class(fortran_unit_test) :: my_test
!! A fortran_unit_test object
    real(wp), dimension(:), intent(in)    :: array1
    real(wp), dimension(:), intent(in)    :: array2
    real(wp), intent(in)                  :: margin
    character(len=*), intent(in)      :: text

    integer                           :: i
    integer                           :: count
    character(len=70) :: msg

    if ( size(array1) /= size(array2) ) then
        my_test%nofails = my_test%nofails + 1
        msg='Arrays have different sizes: "'//trim(text)//'" - assertion failed';
        write(*,*) trim(msg);
    else
        if ( any( abs(array1-array2) > 0.5 * margin * (abs(array1)+abs(array2)) ) ) then
            my_test%nofails = my_test%nofails + 1
            msg='One or more values different: "'//trim(text)//'" - assertion failed';
            write(*,*) trim(msg)
            count = 0
            do i = 1,size(array1)
                if ( abs(array1(i)-array2(i)) > &
                         0.5 * margin * (abs(array1(i))+abs(array2(i))) ) then
                    count = count + 1
                    write(*,'(a10,2a15)')    '    Index', '          First', '         Second'
                    if ( count < 50 ) then
                        write(*,'(i10,2e15.5)')    i, array1(i), array2(i)
                    endif
                    write(*,*) 'Number of differences: ', count
                endif
            enddo
        endif
    endif
end subroutine assert_comparable_real1d
!========================================================================================
subroutine assert_comparable_real2d( my_test, array1, array2, margin, text )
!========================================================================================
!! Subroutine to assert if two arrays of real are equal to a given margin
    class(fortran_unit_test) :: my_test
!! A fortran_unit_test object
    real(wp), dimension(:,:), intent(in)    :: array1
    real(wp), dimension(:,:), intent(in)    :: array2
    real(wp), intent(in)                  :: margin
    character(len=*), intent(in)      :: text

    integer                           :: i,j
    integer                           :: count
    character(len=70) :: msg

    if ( size(array1,1) /= size(array2,1) .and. size(array1,2) /= size(array2,2) ) then
        my_test%nofails = my_test%nofails + 1
        msg='Arrays have different sizes: "'//trim(text)//'" - assertion failed';
        write(*,*) trim(msg);
    else
        if ( any( abs(array1-array2) > 0.5 * margin * (abs(array1)+abs(array2)) ) ) then
            my_test%nofails = my_test%nofails + 1
            msg='One or more values different: "'//trim(text)//'" - assertion failed';
            write(*,*) trim(msg)
            count = 0
            do i = 1,size(array1,1)
               do j = 1,size(array1,2)
                if ( abs(array1(i,j)-array2(i,j)) > &
                         0.5 * margin * (abs(array1(i,j))+abs(array2(i,j))) ) then
                    count = count + 1
                    write(*,'(a10,2a15)')    '    Index', '          First', '         Second'
                    if ( count < 50 ) then
                        write(*,'(i10,2e15.5)')    i, array1(i,j), array2(i,j)
                    endif
                    write(*,*) 'Number of differences: ', count
                endif
              enddo
            enddo
        endif
    endif
end subroutine assert_comparable_real2d
!========================================================================================
subroutine test( my_test, proc, text )
!========================================================================================
!! Subroutine to run a test specified in proc external call 
    class(fortran_unit_test) :: my_test
!! A fortran_unit_test object
    !external          :: proc
    interface 
        subroutine proc()

        end subroutine
    end interface 
    character(len=*)  :: text

    integer           :: lun
    integer           :: ierr

    !
    ! Check if the test should run
    !
    my_test%testno = my_test%testno + 1
    if ( my_test%testno <= my_test%last_test ) then
        return
    endif

    !
    ! Record the fact that we started the test
    !
    call my_test%get_available_unit( lun )
    open( lun, file = 'ftnunit.lst' )
    write( lun, '(A)') 'Number Tests: Failed    NoRun';
    write( lun, * ) my_test%testno, my_test%nofails, my_test%noruns
    close( lun )

    !
    ! Run the test
    !
    write( *, '(2a)' ) 'Test: ', trim(text)

    call proc

    !
    ! No runtime error or premature end of
    ! the program ...
    !
    call my_test%get_available_unit( lun )
    open( lun, file = 'fortran_unit_test.lst' )
    write( lun, * ) my_test%testno, my_test%nofails, my_test%noruns
    close( lun )

end subroutine test
!========================================================================================
subroutine runtests( my_test,testproc )
!========================================================================================
!! Subroutine to run a test specified in proc external call
    class(fortran_unit_test) :: my_test
!! A fortran_unit_test object
    interface
        subroutine testproc
        end subroutine testproc
    end interface

    integer :: lun
    integer :: ierr

    my_test%last_test = 0
    my_test%nofails   = 0
    my_test%noruns    = 0
    my_test%testno    = 0

    if ( my_test%file_exists("fortran_unit_test.run") ) then
        if ( my_test%file_exists("fortran_unit_test.lst") ) then
            call my_test%get_available_unit( lun )
            open( lun, file = "fotran_unit_test.lst", iostat = ierr )
            if ( ierr == 0 ) then
                read( lun, *, iostat = ierr ) my_test%last_test, my_test%nofails, my_test%noruns
                if ( ierr /= 0 ) then
                    my_test%last_test = 0
                    my_test%nofails   = 0
                    my_test%noruns    = 0
                endif
                close( lun )
            endif
        endif

        my_test%noruns = my_test%noruns + 1

        call testproc

!        if ( my_test%call_final ) then
!            call runtests_final
!        endif

    endif

end subroutine runtests
!========================================================================================
subroutine assert_true( my_test, cond, text )
!========================================================================================
!! Subroutine to check if an assertion is true
    class(fortran_unit_test) :: my_test
!! A fortran_unit_test object
    logical, intent(in)          :: cond
    character(len=*), intent(in) :: text

    character(len=70) :: msg
    if ( .not. cond ) then
        my_test%nofails = my_test%nofails + 1
        msg='Condition "'//trim(text)//'" failed';
        write(*,*) msg
        msg='It should have been true';
        write(*,*) msg
    endif
end subroutine assert_true
!========================================================================================
subroutine assert_false( my_test, cond, text )
!========================================================================================
!! Subroutine to check if an assertion is false
    class(fortran_unit_test) :: my_test
!! A fortran_unit_test object
    logical, intent(in)          :: cond
    character(len=*), intent(in) :: text

    character(len=70) :: msg
    if ( cond ) then
        my_test%nofails = my_test%nofails + 1;
        msg='Condition "'//trim(text)//'" failed'
        write(*,*) trim(msg)
        msg='It should have been false'
        write(*,*) trim(msg)
    endif
end subroutine assert_false
!========================================================================================
subroutine get_available_unit(my_test, un)
!========================================================================================
!! Subroutine to get the least available unit to open a file
  class(fortran_unit_test) :: my_test
!! A fortran_unit_test object
  integer,intent(inout) :: un

  logical       :: check_unit
  integer :: iunit
!
  if(my_test%previous_unit /= 10) then
    inquire(unit = un, opened = check_unit);
    if(.not. check_unit) then
       un = my_test%previous_unit;
       return;
    endif
  else
    do iunit=10,99
       inquire(unit = iunit, opened = check_unit)
       if(.not. check_unit) then
         un=iunit;
         return;
       endif
    enddo
  endif
  !  
!
    end subroutine get_available_unit
!========================================================================================
    function file_exists(my_test,fname) result(fe)
!========================================================================================
!! Function to check if a file exists
        logical :: fe
!! A logical variable
        class(fortran_unit_test) :: my_test
!! A fortran_unit_test object 
        character(len=*) :: fname
!! The name of the file 

!!     program main
!!     end program main

        inquire(file = trim(fname), exist = fe)
!  
    end function file_exists
!
end module fortran_unit_test_utilities
