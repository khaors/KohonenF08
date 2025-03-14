!! author: Arjen Markus, Oscar Garcia-Cabrejo
!! date: 06/09/2023
!! version: 0.1
!! This module includes the definition of a class called logger that is used 
!! to log messages on the screen during the development or running of a given 
!! application
module logger_utilities
!
use constants_utilities, only: NUMCHAR;
!
implicit none;
!
private;
!
type logger 
    private
!! The Logger class is used to store all the variables related to the units of 
!! files used to store or print messages during the development (debugging ) or 
!! running
        integer :: fileunit,stdout
        logical :: activate_screen,activate_file,timestamp
        logical :: initialized,stoponerror
        character(len=NUMCHAR) :: level_string_volume,level_string_chapter,level_string_section
        character(len=NUMCHAR) :: level_string_subsection 
    contains
        procedure,public :: create => create_logger
        procedure,public :: destroy => destroy_logger
        procedure,public :: startup
        procedure,public :: shutdown
        procedure,public :: is_initialized
        procedure,public :: message
        procedure,public :: write
        procedure,private :: get_available_unit
        procedure,private :: configure_logical
        procedure,private :: configure_integer
        procedure,private :: configure_character
        generic,public :: configure => configure_logical,configure_integer,configure_character
!        generic,public :: get 
        procedure,public :: get_unit
        procedure,public :: delimiter
        procedure,public :: get_delimiter
        procedure,public :: reset
        procedure,public :: error
end type logger
!
type(logger) :: global_logger;
!
public :: logger,global_logger
!
contains 
!========================================================================================
    subroutine create_logger(current_log)
!========================================================================================
!! logger constructor
        class(logger) :: current_log
!! a logger object
        current_log%initialized=.FALSE.;
        current_log%activate_screen=.TRUE.;
        current_log%activate_file=.TRUE.;
        current_log%timestamp=.FALSE.;
        current_log%fileunit=10;
        current_log%stdout=-1;
        current_log%stoponerror=.TRUE.;
!
    end subroutine create_logger
!========================================================================================
    subroutine destroy_logger(current_log)
!========================================================================================
!! Logger destructor  
        class(logger) :: current_log
!! A logger object
        current_log%level_string_volume=''
        current_log%level_string_chapter=''
        current_log%level_string_section=''
!
    end subroutine destroy_logger
!========================================================================================
    subroutine startup(current_log,log_file,append_)
!========================================================================================
!! Subroutine to initialize a logger object 
        class(logger) :: current_log
!! A logger object
        character(len=*) :: log_file
!! A character variable with the name of the file associated to the logger
        logical,intent(in),optional :: append_
!! A logical (optional) variable to indicate if appending to an existing file 
!!          is desired
!
        logical :: append_real
!
        if(present(append_)) then
            append_real=append_;
        else 
            append_real=.TRUE.;
        endif
        if(current_log%initialized) then 
            call current_log%error('Logger not initialized');
        else 
            current_log%fileunit=current_log%get_available_unit();
            if(append_real) then
                open(current_log%fileunit,file=log_file,status='unknown',action='write',position='append');
            else
                open(current_log%fileunit,file=log_file,status='unknown',action='write',form='formatted'); 
            endif
            current_log%initialized=.TRUE.; 
        endif
!
    end subroutine startup
!========================================================================================
    subroutine shutdown(current_log)
!========================================================================================
!! Subroutine to turn-off the logger
        class(logger) :: current_log
!! A logger object
!
        current_log%initialized=.FALSE.;
!
    end subroutine shutdown
!========================================================================================
    function is_initialized(current_log) result(initialized)
!========================================================================================
!! Function to check if a logger is initialized
        class(logger) :: current_log
!! A logger object
        logical :: initialized
!!  A logical variable 
        initialized=current_log%initialized;
!
    end function is_initialized
!========================================================================================
    subroutine reset(current_log)
!========================================================================================
!! Subroutine to reset the logger
        class(logger) :: current_log
!! A logger object
        current_log%activate_screen=.TRUE.;
        current_log%activate_file=.TRUE.;
        current_log%timestamp=.FALSE.;
!
    end subroutine reset
!========================================================================================
    subroutine message(current_log,msg)
!========================================================================================
!! Subroutine to send a message to the logger
        class(logger) :: current_log
!! A logger object
        character(len=*) :: msg
!! A character variable with the message to send to the logger  
!
        character(len=NUMCHAR) :: date_string
        character(len=NUMCHAR) :: time_string
        character(len=NUMCHAR) :: stamp
!
        if(current_log%timestamp) then 
            call date_and_time(date = date_string, time = time_string);
            write(stamp, '(11A)') date_string(1:4), '-', date_string(5:6), '-', date_string(7:8), ' ',&
            time_string(1:2), ':', time_string(3:4), ':', time_string(5:6);
        else 
            stamp = ' ';
        endif 
        if(current_log%activate_screen) then 
            if(current_log%timestamp) then 
                call current_log%write(current_log%stdout, trim(stamp) // ' ' // trim(msg));
            else 
                call current_log%write(current_log%stdout,trim(msg));
            endif
        endif
        if(current_log%activate_file) then 
            if(current_log%timestamp) then 
                call current_log%write(current_log%fileunit, trim(stamp) // ' ' // trim(msg));
            else 
                call current_log%write(current_log%fileunit,trim(msg));
            endif
        endif
!
    end subroutine message
!========================================================================================
    subroutine write(current_log,unit_,msg)
!========================================================================================
!! Subroutine to write a message in a file associated with the logger
        class(logger) :: current_log
!! A logger object
        integer,intent(in) :: unit_ 
!! An integer variable with the value of the output unit
        character(len=*) :: msg
!! A character variable with the message to be written in the output unit
        character(len=NUMCHAR) :: filename
        integer :: unit1
!
        unit1=unit_;
        if(unit1 == -1) then 
            write(*,'(A)') trim(msg);
        else 
            write(unit1,'(A)') trim(msg);
            inquire(unit1, name = filename);
            close(unit1);
            open(unit1,file=trim(filename),action='write',status='unknown',position='append');
        endif
!
    end subroutine write 
!========================================================================================
    function get_available_unit(current_log) result(un)
!========================================================================================
!! Function to get the next available unit
        class(logger) :: current_log
!! A logger object
        integer :: un
!! An integer variable with the number of the next available unit
!
        logical :: check_unit
        integer :: iunit
!
        if(current_log%fileunit /= 10) then
            inquire(unit = un, opened = check_unit);
            if(.not. check_unit) then
                un = current_log%fileunit;
                return;
            endif
        else
            do iunit=10,99
                inquire(unit = iunit, opened = check_unit)
                if(.not. check_unit) then
                    un=iunit;
                    current_log%fileunit=un;
                    return;
                endif
            enddo
        endif
!
    end function get_available_unit
!========================================================================================
    subroutine configure_logical(current_log,option,value)
!========================================================================================
!! Subroutine to define the logger state
        class(logger) :: current_log
!! A logger object
        character(len=*),intent(in) :: option 
!! A character variable with the name of the state to be defined 
        logical,intent(in) :: value
!! A logical variable 
!
        character(len=NUMCHAR) :: message
!
        select case ( option )
            case ( "timestamp" )
                current_log%timestamp = value;
            case ( "writeonstdout" )
                current_log%activate_screen = value;
            case ( "writeonlogfile" )
                current_log%activate_file = value;
            case ( "stoponerror" )
                current_log%stoponerror = value;
            case default
                write (message,"(A,A,A,l5,A)") "Unknown option ", option, &
                        " for value ", value, " in log_configure_logical"
                call current_log%error(message);
    end select
!
    end subroutine configure_logical
!========================================================================================
    subroutine configure_integer(current_log,option,value)
!========================================================================================
!! Subroutine to define a logger integer state 
        class(logger) :: current_log
!! A logger object
        character(len=*),intent(in) :: option
!! A character variable with the name of the state to be defined 
        integer,intent(in) :: value 
!! An integer variable 
        character(len=NUMCHAR) :: message
!
        select case ( option )
            case ( "logfileunit" )
                current_log%fileunit = value
            case default
                write (message,"(A,A,A,I5,A)") "Unknown option ", option, &
                    " for value ", value, " in log_configure_integer"
            call current_log%error(message);
    end select
!
    end subroutine configure_integer
!========================================================================================
    subroutine configure_character(current_log,option,value)
!========================================================================================
!! Subroutine to define a logger character state 
        class(logger) :: current_log
!! A logger object
        character(len=*),intent(in) :: option 
!! A character variable with the name of the state to be defined 
        character(len=*),intent(in) :: value
!! A character variable
        character (len = NUMCHAR) :: message
!        
        select case ( option )
            case ( "volume" )
                current_log%level_string_volume = value;
            case ( "chapter" )
                current_log%level_string_chapter = value;
            case ( "section" )
                current_log%level_string_section = value;
            case ( "subsection" )
                current_log%level_string_subsection = value;
            case default
                write (message,"(A,A,A,A,A)") "Unknown option ", option, &
                    " for value ", value, " in log_configure_character";
            call current_log%error(message);
        end select
!
    end subroutine configure_character
!========================================================================================
    subroutine delimiter(current_log,level)
!========================================================================================
!! Subroutine that defines the delimiter in a logger repport
        class(logger) :: current_log
!! A logger object        
        character(len=*),optional :: level
!! A character variable with the definition of the delimiter 
!
        character(len=NUMCHAR) :: used_level
        character(len=NUMCHAR) :: msg
!
        if(present(level)) then 
            used_level=level;
        else 
            used_level='volume';
        endif
        call current_log%get_delimiter(used_level,msg);
        call current_log%message(msg(1:30));
!
    end subroutine delimiter
!========================================================================================
    subroutine get_delimiter(current_log,level,msg)
!========================================================================================
!! Subroutine to get the delimiter text
        integer,parameter :: LOG_LEVEL_DELIMITER_LENGTH = 30
        character(len=LOG_LEVEL_DELIMITER_LENGTH),parameter :: log_level_string_volume = "==============================";
        character(len=LOG_LEVEL_DELIMITER_LENGTH),parameter :: log_level_string_chapter = "------------------------------";
        character(len=LOG_LEVEL_DELIMITER_LENGTH),parameter :: log_level_string_section = "******************************";
        character(len=LOG_LEVEL_DELIMITER_LENGTH),parameter :: log_level_string_subsection = "++++++++++++++++++++++++++++++";
        class(logger) :: current_log
!! A logger object   
        character(len=*) :: level 
!! A character variable
        character(len=100) :: msg
!! A character variable
        select case (level)
            case ('volume')
                write(msg,*) trim(log_level_string_volume);
            case ('chapter')
                write(msg,*) trim(log_level_string_chapter);
            case ('section')
                write(msg,*) trim(log_level_string_section);
            case ('subsection')
                write(msg,*)  trim(log_level_string_subsection);
            case default
       ! NOTE :
       ! We do not use m_exception here to limit the dependencies of
       ! such a low level utility.
                write(*,*) "Bad value for the message level:" , level
                write(*,*)
                stop
        end select
!
    end subroutine get_delimiter
!========================================================================================
    function get_unit(current_log) result ( logger_unit )
!========================================================================================
!! Function to get the logger unit
        class(logger) :: current_log
!! A logger object        
        integer :: logger_unit
!! An integer variable with the logger unit
        logger_unit = current_log%fileunit
!
    end function get_unit
!========================================================================================
    subroutine error(current_log,message)
!========================================================================================
!! Subroutine to print an error message
        class(logger) :: current_log
!! A logger object     
        character (len=*), intent(in) :: message
!! A character varaible with the error message        
        write ( 6, "(A)" ) "Error in m_logger."
        write ( 6 , "(A)" ) trim(message)
        stop;
!
    end subroutine error 
!    
end module logger_utilities