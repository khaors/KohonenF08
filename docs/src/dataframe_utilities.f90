!! author: Oscar Garcia-Cabrejo
!! date: 03/52/2025
!! version: 0.1
!! This module defines a data structure called dataframe
module dataframe_utilities
!! This module defines a data structure called dataframe
    use error_handling, only: error
    use precision_utilities, only: wp;
    use constants_utilities, only: NUMCHAR;
    implicit none;
    private;
    !
    type :: variable
        integer :: data_type !1=real,2=integer,3=character
        integer :: number_samples
        character(len=NUMCHAR) :: name
        real(kind=wp),allocatable :: real_data(:)
        integer,allocatable :: integer_data(:)
        character(len=:),allocatable :: character_data(:)
    end type variable
    !
    type :: dataframe
        private
            character(len=NUMCHAR) :: base_message
            integer :: number_variables
            type(variable),allocatable :: variables(:)
            character(len=:),allocatable :: variable_names(:)
            logical :: initialized,has_variable_names
        contains
            procedure,public :: create => create_dataframe
            procedure,public :: destroy => destroy_dataframe
            procedure,public :: add_variable
            ! procedure,public :: remove_variable 
            ! procedure,public :: is_initialized
            ! procedure,public :: summary
            ! procedure,public :: nrow
            ! procedure,public :: ncol
            ! procedure,public :: size => size_dataframe
            ! procedure,public :: normalize
    end type dataframe

    public :: dataframe
!
    contains
        !========================================================================================
        subroutine create_dataframe(df,data_,varnames)
        !========================================================================================
        !! Class constructor
            class(dataframe) :: df
        !! A `dataframe` object
            real(kind=wp),dimension(:,:),intent(inout) :: data_
            character(len=:),dimension(:),intent(inout) :: varnames
        !
            integer :: ivar,ierr,nvar,ndat
            character(len=NUMCHAR) :: message
            real(kind=wp),allocatable :: tmp_data(:)
        !
            df&base_message='DATAFRAME ERROR';
            allocate(df%variable_names,source=varnames,stat=ierr);
            if(ierr /= 0) then
                message=trim(df%base_message)//' while allocating memory for variable_names array';
                call error(message);
            endif
            ndat=size(data_,1);
            nvar=size(data_,2);
            allocate(tmp_data(ndat),stat=ierr);
            if(ierr /= 0) then
                message=trim(df%base_message)//' while allocating memory for tmp_data array';
                call error(message);
            endif
            do ivar=1,nvar;
                tmp_data(1:ndat)=data_(:,ivar:ivar);
                call df%add_variable(tmp_data);
            end do
            deallocate(tmp_data);
        !
        end subroutine create_dataframe
        !========================================================================================
        subroutine destroy_dataframe(df)
        !========================================================================================
        !! Class destructor
            class(dataframe) :: df
        !! A `dataframe` object
            integer :: i,current_type 
        !
            if(allocated(df%variable_names)) deallocate(df%variable_names);
            do i=1,size(df%variables);
                if(allocated(df%variables(i))) then
                    current_type=df%variables(i)%data_type;
                    select case(current_type)
                        case(1)
                            deallocate(df%variables(i)%real_data);
                        case(2)
                            deallocate(df%variables(i)%integer_data);
                        case(3)
                            deallocate(df%variables(i)%character_data);
                    end select
                    df%variables(i)%name='';
                    df%variables(i)%number_samples=0;
                    deallocate(df%variables(i));
                endif 
            end do

        end subroutine destroy_dataframe
        !========================================================================================
        subroutine add_variable(df,data_,varname_)
        !========================================================================================
            class(dataframe) :: df
            class(*) :: data_
            character(len=*) :: varname_

        end subroutine add_variable

end module dataframe_utilities
