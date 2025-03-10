program som_predict
!
    use precision_utilities, only:wp;
    use constants_utilities, only: NUMCHAR;
    use som_predict_variables, only: initialize_variables,release_variables,parfl,&
        my_som,input_patterns,map_output,prototype_file,som_parameters;
    use self_organized_map_utilities;
!
    implicit none;
!
    real(kind=wp),parameter :: version=0.2d0
    character(len=NUMCHAR),parameter :: name='SOM_PREDICT';
    character(len=NUMCHAR) :: arg
    integer :: i,number_arguments
!
    write(*,*)
    write(*,'(A,f10.5)') 'SOM_PREDICT version: ',version
    write(*,*)
    write(*,*) 'Developed by: Oscar Garcia-Cabrejo'
    write(*,*)
    !
    parfl='';
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
            

    if(parfl(1:1) .eq. ' ') then 
        write(*,*) 'Parameter file?'
        read(*,'(A)') parfl
        if(parfl(1:1) .eq. ' ') parfl='som_train.par';
    endif
    write(*,*) 'Parameter file= ',trim(parfl)
    !
    call initialize_variables(parfl);
    !
    call my_som%create(som_parameters);
    !
    call my_som%read(prototype_file); !create(som_parameters);
    !
    call my_som%predict(input_patterns,map_output);
    !
    call my_som%destroy();
    !
    call release_variables();
!
!
    write(*,*)
    write(*,'(A,A,f10.5,2X,A)') trim(name),' version: ',version,'Finished'
    write(*,*)
    stop
    !
    contains
        subroutine print_help()
            write(*,'(a, /)') 'command-line options:'
            write(*,'(a)')    '  -v, --version     print version information and exit'
            write(*,'(a)') '  -h, --help        print usage information and exit'
            write(*,'(a, /)') '  som_predict file.par  run the som_predict program with the parameter file file.par'
    
        end subroutine print_help
!
end program som_predict