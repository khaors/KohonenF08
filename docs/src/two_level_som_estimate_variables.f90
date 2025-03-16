module two_level_som_estimate_variables
    !
    use precision_utilities, only: wp;
    use constants_utilities, only: NUMCHAR;
    use kohonen_layer_parameters_utilities, only: kohonen_layer_parameters;
    use kohonen_map_base_utilities, only: kohonen_map_base;
    use kohonen_prototype_utilities, only: kohonen_prototype;
    use kohonen_pattern_utilities, only: kohonen_pattern;
    use two_level_self_organizing_map_utilities, only: two_level_self_organizing_map;
    use rkiss05_generator_utilities, only: rkiss05_generator;
    !
    implicit none;
    !
    type(two_level_self_organizing_map),save :: my_som
    type(kohonen_layer_parameters),dimension(2) :: som_parameters
    type(kohonen_pattern),allocatable :: input_patterns(:)
    integer :: ipattern,number_clusters_evaluations,iseed,ic,current_cluster,neval,i,j
    character(len=NUMCHAR) :: parfl
    character(len=NUMCHAR) :: matrix_fl
    integer,allocatable :: seeds(:),clusters(:)
    integer,dimension(3) :: min_nodes,max_nodes
    real(kind=wp),dimension(3) :: results
! total_results: declared as fixed size array due to a weird memory leakage 
! real(kind=wp),dimension(10,10,3):: total_results
    real(kind=wp),allocatable :: total_results(:,:,:),association_matrix(:,:)
    integer :: number_nodes,p,min_cluster,number_clusters;
    type(rkiss05_generator) :: rgrator
    !
     contains
    !
     subroutine initialize_variables(par_file)
    !
      character(len=*) :: par_file
    !
       integer :: idata,ipattern,ivar,ierr,number_variables,i,j,ipar,train_option,view_option
       integer :: random_seeds_option
       real(kind=wp),allocatable:: var(:,:)
       logical :: testfl,testop
       character(len=NUMCHAR) :: current_line
       character(len=NUMCHAR) :: current_file    
       integer,dimension(1000) :: column_var
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
       do while(trim(current_line) .ne. 'TWO_LEVEL_SOM_ESTIMATE_PARAMETERS')
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
       read(ipar,'(A40)') som_parameters(1)%pattern_file
       write(*,*) 'Pattern file= ',trim(som_parameters(1)%pattern_file)
       read(ipar,*) som_parameters(1)%number_patterns;
       write(*,*) 'Number patterns= ',som_parameters(1)%number_patterns;
       read(ipar,*) som_parameters(1)%number_variables1,&
                    som_parameters(1)%number_variables2;
       number_variables=som_parameters(1)%number_variables1*&
                        som_parameters(1)%number_variables2;
       write(*,*) 'nvar1,nvar2,nvar= ',som_parameters(1)%number_variables1,&
                                       som_parameters(1)%number_variables2,&
                                       number_variables;
       allocate(var(som_parameters(1)%number_variables1,som_parameters(1)%number_variables2),stat=ierr);
       allocate(association_matrix(som_parameters(1)%number_patterns,som_parameters(1)%number_patterns),stat=ierr);
       association_matrix=0.0d0;
       allocate(clusters(som_parameters(1)%number_patterns),stat=ierr);
       if(train_option .eq. 0) then
         if(number_variables .le. 10) then
           read(ipar,*) (column_var(ivar),ivar=1,number_variables);
           write(*,*) 'columns= ',(column_var(ivar),ivar=1,number_variables);
         else
           read(ipar,*)
           write(*,*) 'WARNING: Assigning columns internally'
           do ivar=1,number_variables
              column_var(ivar)=ivar;
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
    !   read(ipar,*) som_parameters(1)%random_seed_(1)
    !   write(*,*) 'random seed= ',som_parameters(1)%random_seed_(1)
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
       read(ipar,*) view_option
       if(view_option .eq. 1) then
         som_parameters(1)%view_flag=.TRUE.;
       else
         som_parameters(1)%view_flag=.FALSE.;
       endif
       write(*,*) 'Print training results= ',som_parameters(1)%view_flag
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
    
       som_parameters(2)%number_nodes_nx=0;
       som_parameters(2)%number_nodes_ny=0;
       som_parameters(2)%number_nodes_nz=0;
       read(ipar,*) (min_nodes(i),i=1,3);
       write(6,*) 'Min number of nodes= ',(min_nodes(i),i=1,3);
       read(ipar,*) (max_nodes(i),i=1,3);
       write(6,*) 'Max number of nodes= ',(max_nodes(i),i=1,3)
    !    write(*,*) 'number nodes= ',som_parameters(2)%number_nodes_nx,&
    !                 som_parameters(2)%number_nodes_ny,&
    !                 som_parameters(2)%number_nodes_nz              
       read(ipar,*) som_parameters(2)%number_epochs
       write(*,*) 'number epochs= ',som_parameters(2)%number_epochs
       read(ipar,*) som_parameters(2)%learning_rate
       write(*,*) 'learning rate= ',som_parameters(2)%learning_rate
    !   read(ipar,*) som_parameters(2)%random_seed_
    !   write(*,*) 'random seed= ',som_parameters(2)%random_seed_
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
       current_line='';
       do while(current_line .ne. 'RANDOM_SEEDS')
          read(ipar,'(A)') current_line
       enddo
       write(6,*) ''
       write(6,*) 'RANDOM SEEDS: Reading parameters...'
       write(6,*) ''
    !
      read(ipar,*) random_seeds_option;
      write(6,*) 'random seeds option= ',random_seeds_option;
      read(ipar,*) number_clusters_evaluations;
      write(6,*) 'Number random seeds= ',number_clusters_evaluations;
      allocate(seeds(number_clusters_evaluations),stat=ierr);
      if(random_seeds_option .eq. 1) then 
         read(ipar,*) (seeds(iseed),iseed=1,number_clusters_evaluations);
         write(6,*) 'Seeds= ',(seeds(iseed),iseed=1,number_clusters_evaluations);
      else 
         read(ipar,*) seeds(1);
        call rgrator%create(seeds(1));
         !call sgrnd(seeds(1));
         do i=1,number_clusters_evaluations
            !seeds(i)=int(1.0e7*grnd());
            seeds(i)=int(1.0e7*rgrator%generate());
         enddo
        call rgrator%destroy();
      endif
    !
      allocate(total_results(max_nodes(1)-min_nodes(1),number_clusters_evaluations,3),stat=ierr);
    !
       write(6,*) ''
       write(6,*) 'RANDOM SEEDS: Reading parameters...finished'
       write(6,*) ''
    !   stop
    !
    !   allocate(my_som(number_clusters_evaluations),stat=ierr);
    !
       write(*,*) 'Reading parameter file...finished'  
       close(ipar);
       inquire(file=trim(som_parameters(1)%pattern_file),exist=testfl)
       if(.not. testfl) then
         stop 'ERROR: input file does not exist'
       endif
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
    !
       som_parameters(1)%iout1=22;
       som_parameters(1)%imeas=23;
    !
    !
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
            write(*,*) 'Currently reading ',trim(pattern_files(ipattern));
            open(idata,file=trim(pattern_files(ipattern)),status='unknown',action='read',access='sequential');
            do i=1,size(var,1)
               read(idata,*,err=91) (var(i,j),j=1,size(var,2));
            enddo!ix
            close(idata);
            write(*,*) 'Currently reading ',trim(pattern_files(ipattern)),' finished';
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
    ! output file
       current_file=trim(som_parameters(1)%output_file)//'_evaluation_output.out'
       open(som_parameters(1)%iout1,file=trim(current_file),status='unknown',&
            action='write',access='sequential');
    !    write(som_parameters(1)%iout1,'(A)') 'KOHONEN MAP EVALUATION RESULTS';
    !    write(som_parameters(1)%iout1,'(A6,1X,6A12)') 'clust.','H Ind.','KL','CH','Ball','Silhouette','Friedman'
    !
      current_file=trim(som_parameters(1)%output_file)//'_association_matrix.out';
      matrix_fl=trim(current_file);
      open(som_parameters(1)%imeas,file=trim(current_file),status='unknown',&
           action='write',access='sequential');
    !  write(som_parameters(1)%imeas,'(A)') 'KOHONEN MAP - ASSOCIATION MATRIX'
    !  write(som_parameters(1)%imeas,*) som_parameters(1)%number_patterns,som_parameters(1)%number_patterns
            
    ! ! parameter file
    !
    !    current_file=trim(som_parameters(1)%output_file)//'_parameters.som';
    !    open(som_parameters(1)%ipar,file=trim(current_file),status='unknown',&
    !         action='write',access='sequential')
    !    call som_parameters(1)%print(som_parameters(1)%ipar);
    !    som_parameters(2)%output_file='NOFILE';
    !    som_parameters(2)%pattern_file='NOFILE';
    !    som_parameters(2)%debug_file='NOFILE';
    !    call som_parameters(2)%print(som_parameters(1)%ipar);
    !    close(som_parameters(1)%ipar)     
    ! ! neuron indices   
    !    current_file=trim(som_parameters(1)%output_file)//'_neuron_indices.out';
    !    open(som_parameters(1)%iindex,file=trim(current_file),status='unknown',&
    !         action='write',access='sequential');
    !    write(som_parameters(1)%iindex,'(A)') 'KOHONEN MAP PATTERN INDICES'
    !    write(som_parameters(1)%iindex,'(A17,1X,2I6)') 'Number Patterns= ',&
    !          som_parameters(1)%number_patterns,3;
    !       write(som_parameters(1)%iindex,'(A21)') 'Pattern Number,ix,iy '         
    ! ! neuron prototypes        
    !    current_file=trim(som_parameters(1)%output_file)//'_prototypes.out';
    !    open(som_parameters(1)%iprot,file=trim(current_file),status='unknown',&
    !         action='write',access='sequential');
    !    write(som_parameters(1)%iprot,'(A)') 'KOHONEN MAP PROTOTYPES'
    !    write(som_parameters(1)%iprot,'(A17,1X,3I6)') 'number of nodes= ',&
    !         som_parameters(1)%number_nodes_nx,som_parameters(1)%number_nodes_ny,&
    !         som_parameters(1)%number_nodes_nz
    !    write(som_parameters(1)%iprot,'(A21,1X,2I6)') 'number of variables= ',&
    !         som_parameters(1)%number_variables1,som_parameters(1)%number_variables2        
    ! ! neuron hit        
    !    current_file=trim(som_parameters(1)%output_file)//'_neuron_hit.out';
    !    open(som_parameters(1)%ihit,file=trim(current_file),status='unknown',&
    !         action='write',access='sequential');
    !    write(som_parameters(1)%ihit,'(A)') 'KOHONEN MAP NEURON HITS'
    !    write(som_parameters(1)%ihit,'(A17,1X,3I6)') 'number of nodes= ',&
    !         som_parameters(1)%number_nodes_nx,som_parameters(1)%number_nodes_ny,&
    !         som_parameters(1)%number_nodes_nz     
    ! ! neuron distances        
    !    current_file=trim(som_parameters(1)%output_file)//'_neuron_distances.out';
    !    open(som_parameters(1)%idist,file=trim(current_file),status='unknown',&
    !         action='write',access='sequential');
    !    write(som_parameters(1)%idist,'(A)') 'KOHONEN MAP DISTANCE MATRIX'
    !    write(som_parameters(1)%idist,'(A17,1X,2I6)') 'number of nodes= ',&
    !         som_parameters(1)%number_nodes_nx*som_parameters(1)%number_nodes_ny*&
    !         som_parameters(1)%number_nodes_nz,&
    !         som_parameters(1)%number_nodes_nx*som_parameters(1)%number_nodes_ny*&
    !         som_parameters(1)%number_nodes_nz
    ! ! u-matrix        
    !    current_file=trim(som_parameters(1)%output_file)//'_u-matrix.out';
    !    open(som_parameters(1)%iumat,file=trim(current_file),status='unknown',&
    !         action='write',access='sequential');
    !    write(som_parameters(1)%iumat,'(A)') 'KOHONEN MAP U-MATRIX'
    !    write(som_parameters(1)%iumat,'(A17,1X,3I6)') 'number of nodes= ',&
    !         som_parameters(1)%number_nodes_nx,som_parameters(1)%number_nodes_ny,&
    !         som_parameters(1)%number_nodes_nz     
    ! !
    !    current_file=trim(som_parameters(1)%output_file)//'_map_samples.out';
    !    open(som_parameters(1)%isam,file=trim(current_file),status='unknown',&
    !         action='write',access='sequential');
    !    write(som_parameters(1)%isam,'(A)') 'KOHONEN MAP SAMPLE LOCATION'
    !    write(som_parameters(1)%isam,'(A17,1X,3I6)') 'number of nodes= ',&
    !         som_parameters(1)%number_nodes_nx,som_parameters(1)%number_nodes_ny,&
    !         som_parameters(1)%number_nodes_nz     
    ! !
    !    current_file=trim(som_parameters(1)%output_file)//'_clusters.out';
    !    open(som_parameters(1)%iclus,file=trim(current_file),status='unknown',&
    !         action='write',access='sequential');
    !    write(som_parameters(1)%iclus,'(A)') 'KOHONEN MAP CLUSTERS'
    !    write(som_parameters(1)%iclus,'(A17,1X,3I6)') 'number of nodes= ',&
    !         som_parameters(1)%number_nodes_nx,som_parameters(1)%number_nodes_ny,&
    !         som_parameters(1)%number_nodes_nz     
    ! !
    !    current_file=trim(som_parameters(1)%output_file)//'_cluster_centers.out';
    !    open(som_parameters(1)%icen,file=trim(current_file),status='unknown',&
    !         action='write',access='sequential');
    !    write(som_parameters(1)%icen,'(A)') 'KOHONEN MAP CLUSTER CENTERS'
    !    write(som_parameters(1)%icen,'(A17,1X,3I6)') 'number of nodes= ',&
    !         som_parameters(1)%number_nodes_nx,som_parameters(1)%number_nodes_ny,&
    !         som_parameters(1)%number_nodes_nz     
    ! !
    !    current_file=trim(som_parameters(1)%output_file)//'_cluster_samples.out';
    !    open(som_parameters(1)%iclus1,file=trim(current_file),status='unknown',&
    !         action='write',access='sequential');
    !    write(som_parameters(1)%iclus1,'(A)') 'KOHONEN MAP CLUSTER SAMPLES'
    ! !   write(som_parameters(1)%icen,'(A17,1X,3I6)') 'number of nodes= ',&
    ! !        som_parameters(1)%number_nodes_nx,som_parameters(1)%number_nodes_ny,&
    ! !        som_parameters(1)%number_nodes_nz
    !    write(som_parameters(1)%iclus1,'(2I6)') som_parameters(1)%number_patterns,5
    
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
      if(allocated(seeds)) then
        deallocate(seeds);
      endif
    !
      if(allocated(total_results)) then
        deallocate(total_results);
      endif
    !
     if(allocated(association_matrix)) then
       deallocate(association_matrix);
     endif
    ! 
     if(allocated(clusters)) then
       deallocate(clusters);
     endif
    !
      inquire(unit=som_parameters(1)%iout1,opened=testop);
      if(testop) then
        close(som_parameters(1)%iout1);
      endif
    !
      inquire(unit=som_parameters(1)%imeas,opened=testop);
      if(testop) then
        close(som_parameters(1)%imeas);
      endif
    !   inquire(unit=som_parameters(1)%iindex,opened=testop);
    !   if(testop) then
    !     close(som_parameters(1)%iindex);
    !   endif
    ! !
    !   inquire(unit=som_parameters(1)%iprot,opened=testop);
    !   if(testop) then
    !     close(som_parameters(1)%iprot);
    !   endif
    ! !
    !   inquire(unit=som_parameters(1)%ihit,opened=testop);
    !   if(testop) then
    !     close(som_parameters(1)%ihit);
    !   endif
    ! !
    !   inquire(unit=som_parameters(1)%idist,opened=testop);
    !   if(testop) then
    !     close(som_parameters(1)%idist);
    !   endif
    ! !
    !   inquire(unit=som_parameters(1)%iumat,opened=testop);
    !   if(testop) then
    !     close(som_parameters(1)%iumat);
    !   endif
    ! !
    !   inquire(unit=som_parameters(1)%isam,opened=testop);
    !   if(testop) then
    !     close(som_parameters(1)%isam);
    !   endif
    ! !
    !   inquire(unit=som_parameters(1)%iclus,opened=testop);
    !   if(testop) then
    !     close(som_parameters(1)%iclus);
    !   endif
    ! !
    !   inquire(unit=som_parameters(1)%icen,opened=testop);
    !   if(testop) then
    !     close(som_parameters(1)%icen);
    !   endif
    ! !
    !   inquire(unit=som_parameters(1)%iclus1,opened=testop);
    !   if(testop) then
    !     close(som_parameters(1)%iclus1);
    !   endif
    ! 
    ! !
    !   inquire(file=trim(som_parameters(1)%debug_file),opened=testop);
    !   if(testop) then
    !     close(som_parameters(1)%idbg);
    !   endif
    ! 
     end subroutine release_variables
    
    end module two_level_som_estimate_variables