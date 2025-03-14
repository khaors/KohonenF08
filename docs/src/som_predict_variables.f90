module som_predict_variables
    !
    use error_handling, only: error_stop,error_t;
    use precision_utilities, only: wp;
    use constants_utilities, only: NUMCHAR;
    use kohonen_layer_parameters_utilities, only: kohonen_layer_parameters;
    use kohonen_map_base_utilities, only: kohonen_map_base;
    use kohonen_prototype_utilities, only: kohonen_prototype;
    use self_organizing_map_utilities, only: self_organizing_map;
    use kohonen_pattern_utilities, only: kohonen_pattern;
    !
    implicit none
    !
     type(self_organizing_map),save :: my_som
     type(kohonen_layer_parameters),dimension(1) :: som_parameters
     type(kohonen_pattern),allocatable :: input_patterns(:)
     integer,allocatable :: map_output(:,:)
     character(len=NUMCHAR) :: parfl,som_parameter_file,prototype_file
    !
     contains
    ! 
     subroutine initialize_variables(par_file)
    ! 
       character(len=*) :: par_file
    !
       logical :: testfl
       integer :: ipar,ipat,isom,iout,ierr,number_patterns,train_option
       integer :: ipattern,nvar1,nvar2,ivar,i,j
       character(len=NUMCHAR) :: input_file,output_file,current_line
       character(len=NUMCHAR) :: current_file
       real(kind=wp),allocatable :: var(:,:)
       character(len=NUMCHAR),allocatable :: pattern_files(:)
    !
       ipar=1;ipat=2;isom=3;
    !
       inquire(file=trim(par_file),exist=testfl);
       if(.not. testfl) then
         stop 'ERROR: the som predict parameter file does not exist'
       endif
       open(ipar,file=trim(par_file),status='unknown',action='read',access='sequential');
       write(*,*) 'Reading parameter file...'
       current_line='';
       do while(trim(current_line) .ne. 'SOM_PREDICT_PARAMETERS')
         read(ipar,'(A)') current_line
       enddo
       read(ipar,*,err=90) train_option
       read(ipar,'(A40)',err=90) som_parameter_file
       read(ipar,'(A40)',err=90) prototype_file
       read(ipar,'(A40)',err=90) input_file
       read(ipar,*,err=90) number_patterns
       read(ipar,*,err=90) nvar1,nvar2
       read(ipar,'(A40)',err=90) output_file
       close(ipar);
       write(*,*) 'Reading parameter file...finished!'
    !
       inquire(file=trim(som_parameter_file),exist=testfl);
       if(.not. testfl) then
          stop 'ERROR the som parameter file does not exist';
       endif
       write(*,*) 'Reading SOM parameter file...';
       open(isom,file=trim(som_parameter_file),status='unknown',action='read',access='sequential');
       call som_parameters(1)%read_parameters(isom);
       close(isom);
       write(*,*) 'Reading SOM parameter file...OK!';
    !
       som_parameters(1)%idbg=10;
       som_parameters(1)%iout=11;
       som_parameters(1)%iindex=12;
       som_parameters(1)%iprot=13;
       som_parameters(1)%ihit=14;
       som_parameters(1)%idist=15;
       som_parameters(1)%iumat=16;
       som_parameters(1)%ipar=17;
    !
       inquire(file=trim(prototype_file),exist=testfl);
       if(.not. testfl) then
          stop 'ERROR the prototype file does not exist';
       endif
       allocate(input_patterns(number_patterns),stat=ierr);
       allocate(map_output(number_patterns,3),stat=ierr);
       allocate(var(nvar1,nvar2),stat=ierr);
    !
       if(train_option .eq. 0) then
         write(*,*) 'Reading input file...';
         inquire(file=trim(input_file),exist=testfl);
         if(.not. testfl) then
           stop 'ERROR the input pattern file does not exist';
         endif
         open(ipat,file=trim(input_file),status='unknown',action='read',access='sequential');
           do ipattern=1,number_patterns
             read(ipat,*,err=91) (var(ivar,1),ivar=1,nvar1*nvar2)
             call input_patterns(ipattern)%create(var);
           enddo!ipatterh
         close(ipar)
         write(*,*) 'Reading input file...finished!';
       elseif(train_option .eq. 1) then
         allocate(pattern_files(number_patterns),stat=ierr);
         write(*,*) 'Reading input file names...';
         inquire(file=trim(input_file),exist=testfl);
         if(.not. testfl) then
           stop 'ERROR the input pattern file with names does not exist';   
         endif
         open(ipar,file=trim(input_file),status='unknown',action='read',access='sequential');
         do ipattern=1,number_patterns
            read(ipar,'(A)',err=91) pattern_files(ipattern);
         enddo
         close(ipar);
         !
         do ipattern=1,number_patterns
            open(ipat,file=trim(pattern_files(ipattern)),status='unknown',action='read',access='sequential');
              do i=1,nvar1
                 read(ipat,*,err=92) (var(i,j),j=1,nvar2);
              enddo
            close(ipat);
         enddo
         write(*,*) 'Reading input file names...finished!';
       endif
    !
       current_file=trim(output_file);
       open(som_parameters(1)%iout,file=trim(current_file),status='unknown',&
            action='write',access='sequential');
    !
      deallocate(var);
      if(allocated(pattern_files)) deallocate(pattern_files)
    !
     return
    90 stop 'ERROR while reading parameter file'
    91 stop 'ERROR while reading pattern file'
    92 stop 'ERROR while reading a input pattern file'
    !
     end subroutine initialize_variables 
    !
     subroutine release_variables()
    !
      integer :: i
      logical :: testop
    !  
      if(allocated(input_patterns)) then
        do i=1,size(input_patterns)
           call input_patterns(i)%destroy();
        enddo
        deallocate(input_patterns);
      endif
    !
      if(allocated(map_output)) then
        !do i=1,size(map_output)
        !   call map_output(i)%destroy();
        !enddo
        deallocate(map_output);
      endif
    !
      inquire(unit=som_parameters(1)%iout,opened=testop);
      if(testop) then
        close(som_parameters(1)%iout)
      endif
    !
     end subroutine release_variables
    
end module som_predict_variables