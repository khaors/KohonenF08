module som_train_variables
    !
    use error_handling, only: error_stop,error_t;
    use precision_utilities, only: wp;
    use constants_utilities, only: NUMCHAR;
    use logger_utilities, only: global_logger;
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
    real(kind=wp),allocatable :: distance_matrix(:,:)
    integer :: ipattern
    character(len=NUMCHAR) :: parfl
    !
     contains
    !
    ! Outputs:
    !  - neuron indices for each input pattern
    !  - neuron prototypes
    !  - neuron hit count
    !  - distance matrix between neuron prototypes (for clustering)
    !  - u-matrix
    !
    !============================================================================== 
     subroutine initialize_variables(par_file)
    !============================================================================== 
        character(len=*) :: par_file
    !
        integer :: idata,ipattern,ivar,ierr,number_variables,i,j,ipar,toroidal
        integer :: layer_ind,ferr
        real(kind=wp),allocatable:: var(:,:)
        logical :: testfl,testop
        character(len=NUMCHAR) :: current_line
        character(len=NUMCHAR) :: current_file    
        !integer,allocatable :: column_var(:)
        character(len=NUMCHAR),allocatable :: pattern_files(:)
        character(len=NUMCHAR) :: base_message,message
        !
        idata=1;ipar=2;
        base_message="SOM_TRAIN_VARIABLES ERROR";
        call global_logger%create();
        call global_logger%startup('som_train.log',append_=.false.);
        write(*,*) 'logger initialized= ',global_logger%is_initialized();
        call global_logger%configure('timestamp',.true.);
        call global_logger%delimiter('volume');
        call global_logger%message('SOM_TRAIN program started');
        !
        inquire(file=trim(par_file),exist=testfl);
        if(.not. testfl) then
            message=trim(base_message)//"The parameter file does not exist";
            call global_logger%message(message);
            call global_logger%delimiter('volume');
            call error_stop(message);
        endif
        open(ipar,file=trim(par_file),status='unknown',access='sequential',&
            action='read',iostat=ferr);
        if(ferr /= 0) then
            message=trim(base_message)//"while opening "//trim(par_file)//" file";
            call global_logger%message(message);
            call global_logger%delimiter('volume');        
            call error_stop(message);
        endif
        call global_logger%message('Start to read parameter file');
        write(*,*) 'Reading parameter file...'
        current_line='';
        do while(trim(current_line) .ne. 'SOM_TRAIN_PARAMETERS')
            read(ipar,'(A)') current_line
        enddo
        read(ipar,*) som_parameters(1)%train_option
        write(*,*) 'Train option= ',som_parameters(1)%train_option
        read(ipar,'(A40)') som_parameters(1)%pattern_file
        write(*,*) 'Pattern file= ',trim(som_parameters(1)%pattern_file)
        read(ipar,*) som_parameters(1)%number_patterns;
        write(*,*) 'Number patterns= ',som_parameters(1)%number_patterns;
        read(ipar,*) som_parameters(1)%number_variables1
        som_parameters(1)%number_variables2=1;
        number_variables=som_parameters(1)%number_variables1*&
                            som_parameters(1)%number_variables2;
        som_parameters(1)%number_variables=number_variables;                 
        write(*,*) 'nvar= ',som_parameters(1)%number_variables;
        allocate(var(som_parameters(1)%number_variables1,som_parameters(1)%number_variables2),stat=ierr);
        if(ierr /= 0) then
            message=trim(base_message)//"while allocating memory for var array";
            call global_logger%message(message);
            call global_logger%delimiter('volume');        
            call error_stop(message);
        endif
        allocate(som_parameters(1)%column_var(number_variables),stat=ierr);
        if(ierr /= 0) then
            message=trim(base_message)//"while allocating memory for column_var array";
            call global_logger%message(message);
            call global_logger%delimiter('volume');        
            call error_stop(message);
        endif
        if(som_parameters(1)%train_option .eq. 0) then
            if(number_variables .le. 10) then
            read(ipar,*) (som_parameters(1)%column_var(ivar),ivar=1,number_variables);
            write(*,*) 'columns= ',(som_parameters(1)%column_var(ivar),ivar=1,number_variables);
            else
            read(ipar,*)
            write(*,*) 'Columns read'
            do ivar=1,number_variables
                som_parameters(1)%column_var(ivar)=ivar;
            enddo
            write(*,*) som_parameters(1)%column_var(1:number_variables);
            endif
        else
            read(ipar,*)
        endif
        read(ipar,'(A40)') som_parameters(1)%som_type
        write(*,*) 'SOM type= ',trim(som_parameters(1)%som_type);
        read(ipar,*) som_parameters(1)%number_nodes_nx,&
                        som_parameters(1)%number_nodes_ny,&
                        som_parameters(1)%number_nodes_nz
        write(*,*) 'number nodes= ',som_parameters(1)%number_nodes_nx,&
                        som_parameters(1)%number_nodes_ny,&
                        som_parameters(1)%number_nodes_nz              
        read(ipar,*) som_parameters(1)%number_epochs
        write(*,*) 'number epochs= ',som_parameters(1)%number_epochs
        read(ipar,*) som_parameters(1)%learning_rate
        write(*,*) 'learning rate= ',som_parameters(1)%learning_rate
        read(ipar,*) som_parameters(1)%random_seed_
        write(*,*) 'random seed= ',som_parameters(1)%random_seed_
        read(ipar,'(A40)') som_parameters(1)%distance_type
        write(*,*) 'distance type= ',trim(som_parameters(1)%distance_type)
        read(ipar,'(A40)') som_parameters(1)%node_type
        write(*,*) 'node type= ',trim(som_parameters(1)%node_type)
        read(ipar,'(A40)') som_parameters(1)%neighborhood_type
        write(*,*) 'neighborhood type= ',trim(som_parameters(1)%neighborhood_type)
        read(ipar,*) som_parameters(1)%debug_level
        write(*,*) 'debug level= ',som_parameters(1)%debug_level
        read(ipar,'(A40)') som_parameters(1)%debug_file
        write(*,*) 'debug file= ',trim(som_parameters(1)%debug_file);
        read(ipar,'(A40)') som_parameters(1)%output_file
        write(*,*) 'output file base= ',trim(som_parameters(1)%output_file)
        read(ipar,*) toroidal
        write(*,*) 'Toroidal grid= ',toroidal
        if(toroidal .eq. 1) then 
            som_parameters(1)%toroidal_grid=.true.;
        else 
            som_parameters(1)%toroidal_grid=.false.;
        endif
        write(*,*) 'Reading parameter file...finished'
        close(ipar);
        call global_logger%message('Reading parameter file finished');
        !
        inquire(file=trim(som_parameters(1)%pattern_file),exist=testfl)
        if(.not. testfl) then
            message=trim(base_message)//'the file '//trim(som_parameters(1)%pattern_file)//' does not exist';
            !call global_logger%message(message);
            !call global_logger%delimiter('volume');
            call error_stop(message);
        endif
        !
        som_parameters(1)%idbg=10;!debugging
        som_parameters(1)%iout=11;!output
        som_parameters(1)%iindex=12;!SOM output by index
        som_parameters(1)%iprot=13;!SOM prototpyes
        som_parameters(1)%ihit=14;!SOM neuron_hit
        som_parameters(1)%idist=15;!neuron_distances
        som_parameters(1)%iumat=16;! umatrix
        som_parameters(1)%ipar=17;
        som_parameters(1)%isam=18;!
        som_parameters(1)%idisto=19;!save distortion
        !
        !   call random_seed
        !
        allocate(input_patterns(som_parameters(1)%number_patterns),stat=ierr);
        if(ierr /= 0) then
            message=trim(base_message)//"while allocating memory for input_patterns array";
            !call global_logger%message(message);
            !call global_logger%delimiter('volume');        
            call error_stop(message);
        endif
        if(som_parameters(1)%train_option .eq. 0) then   
        !
            call global_logger%message('Reading patterns');
            write(*,*) 'Reading patterns...';
            open(idata,file=trim(som_parameters(1)%pattern_file),status='unknown',&
            access='sequential',action='read',iostat=ferr);
            if(ferr /= 0) then
                message=trim(base_message)//" while opening the "//trim(som_parameters(1)%pattern_file)//" file";
                call error_stop(message);
            endif
            do ipattern=1,som_parameters(1)%number_patterns
                read(idata,*,err=90) (var(ivar,1),ivar=1,number_variables);
                !write(*,*) (var(ivar,1),ivar=1,number_variables);
                call input_patterns(ipattern)%create(var);
            enddo!ipatterns
            close(idata)
            write(*,*) 'Reading patterns...OK!!!';
            call global_logger%message('Reading patterns finished');
        elseif(som_parameters(1)%train_option .eq. 1) then
            allocate(pattern_files(som_parameters(1)%number_patterns),stat=ierr);
            if(ierr /= 0) then 
                message=trim(base_message)//"while allocating memory for pattern_files array";
                call error_stop(message);
            endif
            call global_logger%message('Reading pattern files');
            write(*,*) 'Reading pattern files...'
            open(idata,file=trim(som_parameters(1)%pattern_file),status='unknown',&
            access='sequential',action='read',iostat=ferr);
            if(ferr /= 0) then
                message=trim(base_message)//" while opening the "//trim(som_parameters(1)%pattern_file)//" file";
                call error_stop(message);
            endif
            do ipattern=1,som_parameters(1)%number_patterns
                read(idata,'(A)',err=90) pattern_files(ipattern);
            enddo!ipattern
            write(*,*) 'Reading pattern files...finished!';
            call global_logger%message('Reading pattern files finished');
            !
            call global_logger%message('Reading patterns');
            write(*,*) 'Reading patterns...';
            do ipattern=1,som_parameters(1)%number_patterns
                inquire(file=trim(pattern_files(ipattern)),exist=testfl);
                if(.not. testfl) then
                    message=trim(base_message)//"the file "//trim(pattern_files(ipattern))//' does not exist';
                    call error_stop(message);
                endif
                write(*,*) 'Currently reading ',trim(pattern_files(ipattern));
                open(idata,file=trim(pattern_files(ipattern)),status='unknown',action='read',&
                    access='sequential',iostat=ferr);
                if(ferr /= 0) then
                    message=trim(base_message)//" while opening the "//trim(pattern_files(ipattern))//' file';
                    call error_stop(message);
                endif
                do i=1,size(var,1)
                    read(idata,*,err=91) (var(i,j),j=1,size(var,2));
                enddo!ix
                close(idata);
                write(*,*) 'Currently reading ',trim(pattern_files(ipattern)),' finished';
                call input_patterns(ipattern)%create(var);
            enddo!ipattern
            write(*,*) 'Reading patterns...finished!';
            call global_logger%message('Reading patterns finished');
        !
        elseif(som_parameters(1)%train_option .eq. 2) then
            write(*,*) 'Reading patterns...'
            open(idata,file=trim(som_parameters(1)%pattern_file),status='unknown',&
                access='sequential',action='read',iostat=ferr);
            if(ferr /= 0) then
                message=trim(base_message)//" while opening the "//trim(som_parameters(1)%pattern_file)//' file';
                call error_stop(message);
            endif
            do ipattern=1,som_parameters(1)%number_patterns
                read(idata,*,err=90) (var(ivar,1),ivar=1,number_variables);
                !write(*,*) (var(ivar,1),ivar=1,number_variables);
                call input_patterns(ipattern)%create(var);
            enddo!ipatterns
            close(idata)
            write(*,*) 'Reading patterns...OK!!!'
        !   
            write(*,*) 'Reading distance matrix....';
            allocate(distance_matrix(som_parameters(1)%number_patterns,som_parameters(1)%number_patterns),stat=ierr);
            if(ierr /= 0) then
                message=trim(base_message)//"while allocating memory for distance_matrix array";
                call error_stop(message);
            endif
            open(idata,file=trim(som_parameters(1)%pattern_file),status='unknown',&
                access='sequential',action='read',iostat=ferr);
            if(ferr /= 0) then
                message=trim(base_message)//" while opening the "//trim(som_parameters(1)%pattern_file)//' file';
                call error_stop(message);
            endif
            do i=1,som_parameters(1)%number_patterns
                read(idata,*) (distance_matrix(i,j),j=1,som_parameters(1)%number_patterns);
            enddo
            write(*,*) 'Reading distance matrix....finished!';
        endif
        !
        if(som_parameters(1)%debug_level .gt. 0) then
            open(som_parameters(1)%idbg,file=trim(som_parameters(1)%debug_file),&
                status='unknown',access='sequential',action='write',iostat=ferr);
            if(ferr /= 0) then
                message=trim(base_message)//" while opening the "//trim(som_parameters(1)%debug_file)//' file';
                call error_stop(message);
            endif
        endif
        !
        if(som_parameters(1)%train_option < 3) then
            write(*,*) 'Opening output files...';
            ! parameter file
            current_file=trim(som_parameters(1)%output_file)//'_parameters.som';
            open(som_parameters(1)%ipar,file=trim(current_file),status='unknown',&
                    action='write',access='sequential',iostat=ferr);
            if(ferr /= 0) then
                message=trim(base_message)//" while opening the "//trim(som_parameters(1)%output_file)//'_parameter.som file';
                call error_stop(message);
            endif
            layer_ind=1;     
            call som_parameters(1)%print(layer_ind,som_parameters(1)%ipar);     
            close(som_parameters(1)%ipar)     
            ! neuron indices   
            current_file=trim(som_parameters(1)%output_file)//'_neuron_indices.out';
            open(som_parameters(1)%iindex,file=trim(current_file),status='unknown',&
                    action='write',access='sequential',iostat=ferr);
            if(ferr /= 0) then
                message=trim(base_message)//" while opening the "//trim(som_parameters(1)%output_file)//'_neuron_indices.out file';
                call error_stop(message);
            endif
            !   write(som_parameters(1)%iindex,'(A)') 'KOHONEN MAP PATTERN INDICES'
            !   write(som_parameters(1)%iindex,'(A17,1X,2I6)') 'Number Patterns= ',&
            !         som_parameters(1)%number_patterns,3;
                write(som_parameters(1)%iindex,'(A22)') 'PatternNumber ix iy iz'         
            ! neuron prototypes        
            current_file=trim(som_parameters(1)%output_file)//'_prototypes.out';
            open(som_parameters(1)%iprot,file=trim(current_file),status='unknown',&
                    action='write',access='sequential',iostat=ferr);
            if(ferr /= 0) then
                message=trim(base_message)//" while opening the "//trim(som_parameters(1)%output_file)//'_prototypes.out file';
                call error_stop(message);
            endif
            write(som_parameters(1)%iprot,'(A)') 'KOHONEN MAP PROTOTYPES'
            write(som_parameters(1)%iprot,'(A17,1X,3I6)') 'number of nodes= ',&
                    som_parameters(1)%number_nodes_nx,som_parameters(1)%number_nodes_ny,&
                    som_parameters(1)%number_nodes_nz
            write(som_parameters(1)%iprot,'(A21,1X,2I6)') 'number of variables= ',&
                    som_parameters(1)%number_variables1,som_parameters(1)%number_variables2
            !rectangular
            !hexagonal
            write(som_parameters(1)%iprot,'(A25,1X,A11,1X,L4)') 'node_type,toroidal_grid= ',&
                    trim(som_parameters(1)%node_type),som_parameters(1)%toroidal_grid
                    
                    
            ! neuron hit        
            current_file=trim(som_parameters(1)%output_file)//'_neuron_hit.out';
            open(som_parameters(1)%ihit,file=trim(current_file),status='unknown',&
                    action='write',access='sequential',iostat=ferr);
            if(ferr /= 0) then
                message=trim(base_message)//" while opening the "//trim(som_parameters(1)%output_file)//'_neuron_hits.out file';
                call error_stop(message);
            endif
            write(som_parameters(1)%ihit,'(A)') 'KOHONEN MAP NEURON HITS'
            write(som_parameters(1)%ihit,'(A17,1X,3I6)') 'number of nodes= ',&
                    som_parameters(1)%number_nodes_nx,som_parameters(1)%number_nodes_ny,&
                    som_parameters(1)%number_nodes_nz     
            ! neuron distances        
            current_file=trim(som_parameters(1)%output_file)//'_neuron_distances.out';
            open(som_parameters(1)%idist,file=trim(current_file),status='unknown',&
                    action='write',access='sequential',iostat=ferr);
            if(ferr /= 0) then
                message=trim(base_message)//" while opening the "//trim(som_parameters(1)%output_file)//'_neuron_distances.out file';
                call error_stop(message);
            endif
            write(som_parameters(1)%idist,'(A)') 'KOHONEN MAP DISTANCE MATRIX'
            write(som_parameters(1)%idist,'(A17,1X,2I6)') 'number of nodes= ',&
                    som_parameters(1)%number_nodes_nx*som_parameters(1)%number_nodes_ny*&
                    som_parameters(1)%number_nodes_nz,&
                    som_parameters(1)%number_nodes_nx*som_parameters(1)%number_nodes_ny*&
                    som_parameters(1)%number_nodes_nz
            ! u-matrix        
            current_file=trim(som_parameters(1)%output_file)//'_u-matrix.out';
            open(som_parameters(1)%iumat,file=trim(current_file),status='unknown',&
                    action='write',access='sequential',iostat=ferr);
            if(ferr /= 0) then
                message=trim(base_message)//" while opening the "//trim(som_parameters(1)%output_file)//'_u_matrix.out file';
                call error_stop(message);
            endif
            write(som_parameters(1)%iumat,'(A)') 'KOHONEN MAP U-MATRIX'
            write(som_parameters(1)%iumat,'(A17,1X,3I6)') 'number of nodes= ',&
                    2*som_parameters(1)%number_nodes_nx-1,2*som_parameters(1)%number_nodes_ny-1,&
                    2*som_parameters(1)%number_nodes_nz-1     
            ! map_samples
            current_file=trim(som_parameters(1)%output_file)//'_map_samples.out';
            open(som_parameters(1)%isam,file=trim(current_file),status='unknown',&
                    action='write',access='sequential',iostat=ferr);
            if(ferr /= 0) then
                message=trim(base_message)//" while opening the "//trim(som_parameters(1)%output_file)//'_map_samples.out file';
                call error_stop(message);
            endif
            write(som_parameters(1)%isam,'(A)') 'KOHONEN MAP SAMPLE LOCATION'
            write(som_parameters(1)%isam,'(A17,1X,3I6)') 'number of nodes= ',&
                    som_parameters(1)%number_nodes_nx,som_parameters(1)%number_nodes_ny,&
                    som_parameters(1)%number_nodes_nz     
            ! SOM distortion
            current_file=trim(som_parameters(1)%output_file)//'_distortion.out';
            open(som_parameters(1)%idisto,file=trim(current_file),status='unknown',&
                action='write',access='sequential',iostat=ferr);
            if(ferr /= 0) then
                message=trim(base_message)//" while opening the "//trim(som_parameters(1)%output_file)//'_distortion.out file';
                call error_stop(message);
            endif
            write(som_parameters(1)%idisto,'(A)') 'KOHONEN MAP DISTORTION'
            !      
            write(*,*) 'Opening output files...finished!!!';
        endif
        !
        deallocate(var);
        return
        !
        90 stop 'ERROR while reading pattern file'
        91 stop 'ERROR while reading pattern sample file'
    !
    end subroutine initialize_variables
    !==============================================================================
    subroutine release_variables()
    !==============================================================================
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
        if(allocated(distance_matrix)) then
            deallocate(distance_matrix);
        endif
        !
        if(allocated(som_parameters(1)%column_var)) then
            deallocate(som_parameters(1)%column_var)
        endif
        !
        inquire(unit=som_parameters(1)%iindex,opened=testop);
        if(testop) then
            close(som_parameters(1)%iindex);
        endif
        !
        inquire(unit=som_parameters(1)%iprot,opened=testop);
        if(testop) then
            close(som_parameters(1)%iprot);
        endif
        !
        inquire(unit=som_parameters(1)%ihit,opened=testop);
        if(testop) then
            close(som_parameters(1)%ihit);
        endif
        !
        inquire(unit=som_parameters(1)%idist,opened=testop);
        if(testop) then
            close(som_parameters(1)%idist);
        endif
        !
        inquire(unit=som_parameters(1)%iumat,opened=testop);
        if(testop) then
            close(som_parameters(1)%iumat);
        endif
        !
        inquire(unit=som_parameters(1)%isam,opened=testop);
        if(testop) then
            close(som_parameters(1)%isam);
        endif
        
        !
        inquire(file=trim(som_parameters(1)%debug_file),opened=testop);
        if(testop) then
            close(som_parameters(1)%idbg);
        endif
        !
        inquire(unit=som_parameters(1)%idisto,opened=testop);
        if(testop) then
            close(som_parameters(1)%idisto);
        endif
        !
        call global_logger%shutdown();
        call global_logger%destroy();
    !
    end subroutine release_variables
    !
end module som_train_variables