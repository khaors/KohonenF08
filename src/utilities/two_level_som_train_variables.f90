module two_level_som_train_variables
    !
    use precision_utilities, only: wp;
    use constants_utilities, only: NUMCHAR;
    use kohonen_layer_parameters_utilities, only: kohonen_layer_parameters;
    use kohonen_map_base_utilities, only: kohonen_map_base;
    use kohonen_prototype_utilities, only: kohonen_prototype;
    use kohonen_pattern_utilities, only: kohonen_pattern;
    use two_level_self_organizing_map_utilities, only: two_level_self_organizing_map;
    !
    implicit none;
    !
     type(two_level_self_organizing_map),save :: my_som
     type(kohonen_layer_parameters),dimension(2) :: som_parameters
     type(kohonen_pattern),allocatable :: input_patterns(:)
     integer :: ipattern
     character(len=NUMCHAR) :: parfl
    !
     contains
    !============================================================================== 
     subroutine initialize_variables(par_file)
    !============================================================================== 
      character(len=*) :: par_file
    !
       integer :: idata,ipattern,ivar,ierr,number_variables,i,j,ipar,train_option
       integer :: K,iprint,itoroidal,layer_ind   
       real(kind=wp),allocatable:: var(:,:)
       logical :: testfl,testop
       character(len=NUMCHAR) :: current_line
       character(len=NUMCHAR) :: current_file    
       character(len=NUMCHAR),allocatable :: pattern_files(:) 
    !  
       idata=1;ipar=2;
       !
       inquire(file=trim(par_file),exist=testfl);
       if(.not. testfl) then
          stop 'ERROR: parameter file does not exist'
       endif
       open(ipar,file=trim(par_file),status='unknown',access='sequential',action='read');
       write(*,*) 'Reading parameter file...'
       current_line='';
       do while(trim(current_line) .ne. 'TWO_LEVEL_SOM_TRAIN_PARAMETERS')
         read(ipar,'(A)') current_line
       enddo
       do while(trim(current_line) .ne. 'LAYER1')
         read(ipar,'(A)') current_line
       enddo
       write(*,*)
       write(*,*) 'LAYER 1: Reading parameters...';
       write(*,*)
       read(ipar,*) train_option
       write(*,*) 'Train option= ',train_option
       som_parameters(1)%train_option=train_option
       read(ipar,'(A40)') som_parameters(1)%pattern_file
       write(*,*) 'Pattern file= ',trim(som_parameters(1)%pattern_file)
       read(ipar,*) som_parameters(1)%number_patterns;
       write(*,*) 'Number patterns= ',som_parameters(1)%number_patterns;
       read(ipar,*) som_parameters(1)%number_variables1!,&
       som_parameters(1)%number_variables2=1;
       number_variables=som_parameters(1)%number_variables1*&
                        som_parameters(1)%number_variables2;
       som_parameters(1)%number_variables=number_variables;                 
       write(*,*) 'nvar1,nvar2,nvar= ',som_parameters(1)%number_variables1,&
                                       som_parameters(1)%number_variables2,&
                                       number_variables;
       allocate(var(som_parameters(1)%number_variables1,som_parameters(1)%number_variables2),stat=ierr);
       allocate(som_parameters(1)%column_var(number_variables),stat=ierr);
       if(train_option .eq. 0) then
         if(number_variables .le. 10) then
           read(ipar,*) (som_parameters(1)%column_var(ivar),ivar=1,number_variables);
           write(*,*) 'columns= ',(som_parameters(1)%column_var(ivar),ivar=1,number_variables);
         else
           read(ipar,*)
           write(*,*) 'WARNING: Assigning columns internally'
           do ivar=1,number_variables
              som_parameters(1)%column_var(ivar)=ivar;
           enddo
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
       read(ipar,*) som_parameters(1)%random_seed_(1)
       write(*,*) 'random seed= ',som_parameters(1)%random_seed_(1)
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
       read(ipar,*) iprint
       som_parameters(1)%view_flag=.FALSE.
       if(iprint .eq. 1)   som_parameters(1)%view_flag=.TRUE.;
       write(*,*) 'Print Training Results= ',iprint
       read(ipar,*) itoroidal
       som_parameters(1)%toroidal_grid=.FALSE.;
       if(itoroidal .eq. 1) som_parameters(1)%toroidal_grid=.TRUE.;
       write(*,*) 'Toroidal grid= ',itoroidal
       write(*,*)
       write(*,*) 'LAYER 1: Reading parameters...finished';
       write(*,*)
       current_line='';
       do while(trim(current_line) .ne. 'LAYER2')
         read(ipar,'(A)') current_line
       enddo
       write(*,*)
       write(*,*) 'LAYER 2: Reading parameters...';
       write(*,*)
    
       read(ipar,*) som_parameters(2)%number_nodes_nx,&
                    som_parameters(2)%number_nodes_ny,&
                    som_parameters(2)%number_nodes_nz
       write(*,*) 'number nodes= ',som_parameters(2)%number_nodes_nx,&
                    som_parameters(2)%number_nodes_ny,&
                    som_parameters(2)%number_nodes_nz              
       read(ipar,*) som_parameters(2)%number_epochs
       write(*,*) 'number epochs= ',som_parameters(2)%number_epochs
       read(ipar,*) som_parameters(2)%learning_rate
       write(*,*) 'learning rate= ',som_parameters(2)%learning_rate
       read(ipar,*) som_parameters(2)%random_seed_
       write(*,*) 'random seed= ',som_parameters(2)%random_seed_
       read(ipar,'(A40)') som_parameters(2)%distance_type 
       write(*,*) 'distance type= ',trim(som_parameters(2)%distance_type)
       read(ipar,'(A40)') som_parameters(2)%node_type
       write(*,*) 'node type= ',trim(som_parameters(2)%node_type)
       read(ipar,'(A40)') som_parameters(2)%neighborhood_type
       write(*,*) 'neighborhood type= ',trim(som_parameters(2)%neighborhood_type)
       write(*,*)
       write(*,*) 'LAYER 2: Reading parameters...finished';
       write(*,*)
    !
    ! 
    !
       write(*,*) 'Reading parameter file...finished'  
       close(ipar);
       inquire(file=trim(som_parameters(1)%pattern_file),exist=testfl)
       if(.not. testfl) then
         stop 'ERROR: input file does not exist'
       endif
    !
    ! Assigning units to output files
    !
       som_parameters(1)%idbg=10;
       som_parameters(1)%iout=11;
       som_parameters(1)%iindex=12;
       som_parameters(1)%iprot=13;
       som_parameters(1)%ihit=14;
       som_parameters(1)%idist=15;
       som_parameters(1)%iumat=16;
       som_parameters(1)%ipar=17;
       som_parameters(1)%isam=18;
       som_parameters(1)%iclus=19;
       som_parameters(1)%icen=20;
       som_parameters(1)%iclus1=21;
    !   som_parameters(1)%view_flag=.TRUE.;
    !
    !   K=1;
    !   call random_seed(size=K);
    !   call random_seed(put=som_parameters(1)%random_seed_(1:K));
       
    !
       allocate(input_patterns(som_parameters(1)%number_patterns),stat=ierr);
       if(train_option .eq. 0) then   
       !
         write(*,*) 'Reading patterns...'
         open(idata,file=trim(som_parameters(1)%pattern_file),status='unknown',&
         access='sequential',action='read');
           do ipattern=1,som_parameters(1)%number_patterns
             read(idata,*,err=90) (var(ivar,1),ivar=1,number_variables);
             !write(*,*) (var(ivar,1),ivar=1,number_variables);
             call input_patterns(ipattern)%create(var);
           enddo!ipatterns
         close(idata)
         write(*,*) 'Reading patterns...OK!!!'
       elseif(train_option .eq. 1) then
         allocate(pattern_files(som_parameters(1)%number_patterns),stat=ierr);
         write(*,*) 'Reading pattern files...'
         open(idata,file=trim(som_parameters(1)%pattern_file),status='unknown',&
         access='sequential',action='read');
         do ipattern=1,som_parameters(1)%number_patterns
            read(idata,'(A)',err=90) pattern_files(ipattern);
         enddo!ipattern
         write(*,*) 'Reading pattern files...finished!';
         !
         write(*,*) 'Reading patterns...';
         do ipattern=1,som_parameters(1)%number_patterns
            inquire(file=trim(pattern_files(ipattern)),exist=testfl);
            if(.not. testfl) then
              write(*,*) 'ERROR: the file ',trim(pattern_files(ipattern)),' does not exist'
              stop
            endif
            if(som_parameters(1)%view_flag) then
               write(*,*) 'Currently reading ',trim(pattern_files(ipattern));
            endif
            open(idata,file=trim(pattern_files(ipattern)),status='unknown',action='read',access='sequential');
            do i=1,size(var,1)
               read(idata,*,err=91) (var(i,j),j=1,size(var,2));
            enddo!ix
            close(idata);
            if(som_parameters(1)%view_flag) then
               write(*,*) 'Currently reading ',trim(pattern_files(ipattern)),' finished';
            endif
            call input_patterns(ipattern)%create(var);
         enddo!ipattern
         write(*,*) 'Reading patterns...finished!';
       endif
    !
       if(som_parameters(1)%debug_level .gt. 0) then
         open(som_parameters(1)%idbg,file=trim(som_parameters(1)%debug_file),&
              status='unknown',access='sequential',action='write');
       endif
    !
       write(*,*) 'Opening output files...';
    ! parameter file
       current_file=trim(som_parameters(1)%output_file)//'_parameters.som';
       open(som_parameters(1)%ipar,file=trim(current_file),status='unknown',&
            action='write',access='sequential')
       layer_ind=1;
       call som_parameters(1)%print(layer_ind,som_parameters(1)%ipar);
    !   
       som_parameters(2)%output_file='NOFILE';
       som_parameters(2)%pattern_file='NOFILE';
       som_parameters(2)%debug_file='NOFILE';
       som_parameters(2)%som_type='NOTYPE';
    !
       layer_ind=2;
       call som_parameters(2)%print(layer_ind,som_parameters(1)%ipar);
       close(som_parameters(1)%ipar)     
    ! neuron indices   
       current_file=trim(som_parameters(1)%output_file)//'_neuron_indices.out';
       open(som_parameters(1)%iindex,file=trim(current_file),status='unknown',&
            action='write',access='sequential');
    !    write(som_parameters(1)%iindex,'(A)') 'KOHONEN MAP PATTERN INDICES'
    !    write(som_parameters(1)%iindex,'(A17,1X,2I6)') 'Number Patterns= ',&
    !          som_parameters(1)%number_patterns,3;
          write(som_parameters(1)%iindex,'(A22)') 'PatternNumber ix iy iz'         
    ! neuron prototypes        
       current_file=trim(som_parameters(1)%output_file)//'_prototypes.out';
       open(som_parameters(1)%iprot,file=trim(current_file),status='unknown',&
            action='write',access='sequential');
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
            action='write',access='sequential');
       write(som_parameters(1)%ihit,'(A)') 'KOHONEN MAP NEURON HITS'
       write(som_parameters(1)%ihit,'(A17,1X,3I6)') 'number of nodes= ',&
            som_parameters(1)%number_nodes_nx,som_parameters(1)%number_nodes_ny,&
            som_parameters(1)%number_nodes_nz     
    ! neuron distances        
       current_file=trim(som_parameters(1)%output_file)//'_neuron_distances.out';
       open(som_parameters(1)%idist,file=trim(current_file),status='unknown',&
            action='write',access='sequential');
       write(som_parameters(1)%idist,'(A)') 'KOHONEN MAP DISTANCE MATRIX'
       write(som_parameters(1)%idist,'(A17,1X,2I6)') 'number of nodes= ',&
            som_parameters(1)%number_nodes_nx*som_parameters(1)%number_nodes_ny*&
            som_parameters(1)%number_nodes_nz,&
            som_parameters(1)%number_nodes_nx*som_parameters(1)%number_nodes_ny*&
            som_parameters(1)%number_nodes_nz
    ! u-matrix        
       current_file=trim(som_parameters(1)%output_file)//'_u-matrix.out';
       open(som_parameters(1)%iumat,file=trim(current_file),status='unknown',&
            action='write',access='sequential');
       write(som_parameters(1)%iumat,'(A)') 'KOHONEN MAP U-MATRIX'
       write(som_parameters(1)%iumat,'(A17,1X,3I6)') 'number of nodes= ',&
            2*som_parameters(1)%number_nodes_nx-1,&
            2*som_parameters(1)%number_nodes_ny-1,&
            2*som_parameters(1)%number_nodes_nz-1     
    !
       current_file=trim(som_parameters(1)%output_file)//'_map_samples.out';
       open(som_parameters(1)%isam,file=trim(current_file),status='unknown',&
            action='write',access='sequential');
       write(som_parameters(1)%isam,'(A)') 'KOHONEN MAP SAMPLE LOCATION'
       write(som_parameters(1)%isam,'(A17,1X,3I6)') 'number of nodes= ',&
            som_parameters(1)%number_nodes_nx,som_parameters(1)%number_nodes_ny,&
            som_parameters(1)%number_nodes_nz     
    !
       current_file=trim(som_parameters(1)%output_file)//'_clusters.out';
       open(som_parameters(1)%iclus,file=trim(current_file),status='unknown',&
            action='write',access='sequential');
       write(som_parameters(1)%iclus,'(A)') 'KOHONEN MAP CLUSTERS'
       write(som_parameters(1)%iclus,'(A17,1X,3I6)') 'number of nodes= ',&
            som_parameters(1)%number_nodes_nx,som_parameters(1)%number_nodes_ny,&
            som_parameters(1)%number_nodes_nz     
    !
       current_file=trim(som_parameters(1)%output_file)//'_cluster_centers.out';
       open(som_parameters(1)%icen,file=trim(current_file),status='unknown',&
            action='write',access='sequential');
       write(som_parameters(1)%icen,'(A)') 'KOHONEN MAP CLUSTER CENTERS'
       write(som_parameters(1)%icen,'(A17,1X,3I6)') 'number of nodes= ',&
            som_parameters(1)%number_nodes_nx,som_parameters(1)%number_nodes_ny,&
            som_parameters(1)%number_nodes_nz
       write(som_parameters(2)%iprot,'(A21,1X,2I6)') 'number of variables= ',&
            som_parameters(2)%number_variables1,som_parameters(2)%number_variables2        
    !
       current_file=trim(som_parameters(1)%output_file)//'_cluster_samples.out';
       open(som_parameters(1)%iclus1,file=trim(current_file),status='unknown',&
            action='write',access='sequential');
       write(som_parameters(1)%iclus1,'(A)') 'KOHONEN MAP CLUSTER SAMPLES'
    !   write(som_parameters(1)%icen,'(A17,1X,3I6)') 'number of nodes= ',&
    !        som_parameters(1)%number_nodes_nx,som_parameters(1)%number_nodes_ny,&
    !        som_parameters(1)%number_nodes_nz
       write(som_parameters(1)%iclus1,'(2I6)') som_parameters(1)%number_patterns,5
    
    !
       write(*,*) 'Opening output files...finished!!!';
    !
       deallocate(var);
    !
       return;
    !
    90 stop 'ERROR while reading pattern file'
    91 stop 'ERROR while reading pattern sample file'
    !
     end subroutine initialize_variables
    
     subroutine release_variables()
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
      inquire(unit=som_parameters(1)%iclus,opened=testop);
      if(testop) then
        close(som_parameters(1)%iclus);
      endif
    !
      inquire(unit=som_parameters(1)%icen,opened=testop);
      if(testop) then
        close(som_parameters(1)%icen);
      endif
    !
      inquire(unit=som_parameters(1)%iclus1,opened=testop);
      if(testop) then
        close(som_parameters(1)%iclus1);
      endif
    
    !
      inquire(file=trim(som_parameters(1)%debug_file),opened=testop);
      if(testop) then
        close(som_parameters(1)%idbg);
      endif
    ! 
     end subroutine release_variables
    
    end module two_level_som_train_variables