program two_level_som_train
    use precision_utilities, only: wp;
    use constants_utilities, only: NUMCHAR;
    use two_level_som_train_variables, only: train_two_level_som;
    !
    implicit none;
    !
    real(kind=wp),parameter :: version=0.1_wp;
    character(len=*),parameter :: name='TWO_LEVEL_SOM_TRAIN'
    character(len=NUMCHAR) :: arg,parfl
    integer :: number_arguments,i        
    !
    write(*,*)
    write(*,'(A,A,f10.5)') name,' version: ',version
    write(*,*)
    write(*,*) 'Developed by: Oscar Garcia-Cabrejo'
    write(*,*)


    parfl=''; ! Initialize to ensure that checking for empty string
    number_arguments = command_argument_count();
    do i=1,number_arguments;
        call get_command_argument(i,arg);
        select case(arg)
            case('-v', '--version')
                write(*,'(A,A,f10.5)') trim(name),' version: ',version;
                stop;
            case('-h', '--help')
                call print_help();
                stop;
            case default
                if(arg(1:1) /= '-' .or. arg(1:2) /= '--') then
                    parfl=trim(adjustl(arg));
                endif
                !write(*,*) 'parfl= ',trim(parfl);
        end select
    enddo
    !write(*,*) 'parfl= ',trim(parfl),len(parfl),parfl(1:1) .eq. ' ';
    if(parfl(1:1) .eq. ' ') then 
        write(*,*) 'Parameter file?'
        read(*,'(A)') parfl
        if(parfl(1:1) .eq. ' ') parfl='som_train.par';
    endif
    write(*,*) 'Parameter file= ',trim(parfl)        
    !
    call train_two_level_som(parfl);
    !
    write(*,*)
    write(*,'(A,A,f10.5,2X,A)') name,' version: ',version,'Finished'
    write(*,*)
!
contains
    subroutine print_help()
        write(*,'(a, /)') 'command-line options:'
        write(*,'(a)')    '  -v, --version     print version information and exit'
        write(*,'(a)') '  -h, --help        print usage information and exit'
        write(*,'(a, /)') '  tweo_level_som_train file.par  run the som_train program with the parameter file file.par'

    end subroutine print_help    

end program two_level_som_train