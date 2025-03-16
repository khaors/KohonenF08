!! author: Oscar Garcia-Cabrejo
!! date: 03/12/2025
!! version: 0.1
!! This module defines a class that represents a two layer self_organizing_map  for clustering 
module two_level_self_organizing_map_utilities
!! This module defines a class that represents a two layer self_organizing_map  for clustering
   use precision_utilities, only: wp;
   use kohonen_layer_parameters_utilities;
   use kohonen_map_base_utilities;
   use kohonen_prototype_utilities;
   use kohonen_pattern_utilities;
   use distance_base_utilities;
   use factory_distance_utilities;
   use random_generator_base_utilities, only: random_generator_base;
   use rkiss05_generator_utilities, only: rkiss05_generator;
   !
   implicit none;
   !
   type,extends(kohonen_map_base) :: two_level_self_organizing_map
   private
!!   Class to represent a two level self_organized_map
   type(kohonen_prototype),allocatable :: grid(:,:,:),cluster_layer(:)
      real(kind=wp),allocatable :: coordinates(:,:)
      integer,allocatable :: number_patterns(:,:,:),cells_index(:,:)
      integer,allocatable :: cluster_number_patterns(:),cluster_cells_index(:,:)
      integer,allocatable :: grid_cluster(:,:,:),cluster_samples(:)
      real(kind=wp),allocatable :: u_matrix(:,:,:),distance(:,:),cells_distances(:,:)
      integer,allocatable :: number_cluster_samples(:),index_cluster_samples(:,:)
      type(kohonen_layer_parameters),dimension(2) :: parameters
      type(factory_distance) :: factory
      class(distance_base),allocatable :: distance_function
      type(rkiss05_generator),dimension(2) :: rnumber_grator
      integer :: seed1,seed2
      integer :: number_variables,number_variables1,number_variables2,number_clusters
      integer :: number_nodes
   contains
      procedure,public :: create => create_2lsom
      procedure,public :: destroy => destroy_2lsom
      procedure,private :: create_random_sample
      procedure,public :: train => train_2lsom
      procedure,public :: predict => predict_2lsom
      procedure,public :: train_grid_layer
      procedure,public :: train_cluster_layer
      procedure,public :: print => print_2lsom
      procedure,public :: query => query_2lsom
      procedure,public :: set_cluster_layer
      procedure,public :: set_parameters
   !    procedure,public :: read => read_som
      procedure,private :: query_2lsom
      procedure,public :: read_som_layer
      procedure,private :: calculate_u_matrix
      procedure,private :: find_best_match_unit
      procedure,private :: update_weights
      procedure,private :: calculate_distance_between_prototypes
      procedure,private :: assign_input_to_clusters
      !procedure,public :: get_count => get_count_2lsom
      !procedure,public :: get_index => get_index_som
      !procedure,public :: get_u_matrix => get_u_matrix_som
      procedure,public :: calculate_sum2_clusters_samples => evaluate_2lsom
      procedure,public :: get_cluster_samples
      procedure,public :: calculate_sum2_clusters_grid
      procedure,nopass,private :: calculate_distance_matrix
      procedure,nopass,private :: calculate_coordinates
   !    
      procedure,nopass,public :: external_train_map
   !    procedure,nopass,public :: external_predict_map    
   !*****   
   end type two_level_self_organizing_map
    
   contains
   !========================================================================================
   subroutine create_2lsom(kohonen_map,training_parameters)
   !========================================================================================
!!   Constructor of a two_level self_organized_map class 
      class(two_level_self_organizing_map) :: kohonen_map
!! A `two_level_self_organizing_map` object
      type(kohonen_layer_parameters),dimension(:) :: training_parameters
!! A `kohonen_layer_parameters` structure
      integer :: ierr,nx,ny,nz,ix,iy,iz,nvar1,nvar2,number_clusters,ivar1,ivar2,current_index
      real(kind=wp),allocatable :: input(:,:)
      integer :: seed
   !
      kohonen_map%parameters=training_parameters(1:2);
      nx=training_parameters(1)%number_nodes_nx;
      ny=training_parameters(1)%number_nodes_ny;
      nz=training_parameters(1)%number_nodes_nz;
      nvar1=training_parameters(1)%number_variables1;
      nvar2=training_parameters(1)%number_variables2;
      number_clusters=training_parameters(2)%number_nodes_nx*&
                     training_parameters(2)%number_nodes_ny*&
                     training_parameters(2)%number_nodes_nz;
      kohonen_map%number_clusters=number_clusters;
      kohonen_map%number_variables=nvar1*nvar2;
      kohonen_map%number_variables1=nvar1;
      kohonen_map%number_variables2=nvar2;
      kohonen_map%number_nodes=nx*ny*nz;
      allocate(kohonen_map%grid(nx,ny,nz),stat=ierr);
      allocate(kohonen_map%coordinates(nx*ny*nz,3),stat=ierr);
      kohonen_map%coordinates=0;
      allocate(input(nvar1,nvar2),stat=ierr);
      input=0.0_wp;
      allocate(kohonen_map%number_patterns(nx,ny,nz),stat=ierr);
      allocate(kohonen_map%cells_index(training_parameters(1)%number_patterns,3),stat=ierr);
      kohonen_map%number_patterns=0;
      kohonen_map%cells_index=0;
      allocate(kohonen_map%u_matrix(2*nx-1,2*ny-1,2*nz-1),stat=ierr);
      kohonen_map%u_matrix=0.0_wp;
      allocate(kohonen_map%distance(nx*ny*nz,nx*ny*nz),stat=ierr);
   !  cluster layer arrays   
      allocate(kohonen_map%cluster_layer(number_clusters),stat=ierr);
      allocate(kohonen_map%cluster_cells_index(nx*ny*nz,4),stat=ierr);
      kohonen_map%cluster_cells_index=0;
      allocate(kohonen_map%cluster_number_patterns(number_clusters),stat=ierr);
      kohonen_map%cluster_number_patterns=0;
      allocate(kohonen_map%grid_cluster(nx,ny,nz),stat=ierr);
      kohonen_map%grid_cluster=0;
      allocate(kohonen_map%cluster_samples(training_parameters(1)%number_patterns),stat=ierr);
      allocate(kohonen_map%number_cluster_samples(number_clusters),stat=ierr);
      !Lack of initialization was causing problems during execution
      kohonen_map%number_cluster_samples=0;
      allocate(kohonen_map%index_cluster_samples(number_clusters,&
               training_parameters(1)%number_patterns),stat=ierr);
      kohonen_map%index_cluster_samples=0         
   !
      call kohonen_map%factory%create_distance(training_parameters(1)%distance_type,&
         kohonen_map%distance_function);
   !
      kohonen_map%seed1=training_parameters(1)%random_seed_(1)+100;
      call kohonen_map%rnumber_grator(1)%create(kohonen_map%seed1);
      !call sgrnd(seed)
      write(*,*) 'TWO LEVEL SOM: Initializing grid...',seed
      do iz=1,nz
         do iy=1,ny
            do ix=1,nx
               call kohonen_map%create_random_sample(input);
               !call grnd_array(input);
   !            write(*,*) ix,iy,input(1:2,1)
               call kohonen_map%grid(ix,iy,iz)%create(input); 
               !call kohonen_map%grid(ix,iy)%print();
               current_index=position2index(ix,iy,iz,nx,ny);
               kohonen_map%coordinates(current_index,1)=dble(ix);
               kohonen_map%coordinates(current_index,2)=dble(iy);
               kohonen_map%coordinates(current_index,3)=dble(iz);
   !            write(*,*) ix+(iy-1)*nx+(iz-1)*nx*ny
               if(trim(training_parameters(1)%node_type) .eq. 'hexagonal') then
   !              write(*,*) 'hexagonal'
                  kohonen_map%coordinates(current_index,1)=kohonen_map%coordinates(current_index,1)+&
                                          .5_wp*(mod(kohonen_map%coordinates(current_index,2),2.0_wp));
                  kohonen_map%coordinates(current_index,2)=(dsqrt(3.0_wp)/2._wp)*kohonen_map%coordinates(current_index,2);
               endif
            enddo!iz
         enddo !iy
      enddo !ix
   !
   !
   !
      allocate(kohonen_map%cells_distances(nx*ny*nz,nx*ny*nz),stat=ierr);
      call kohonen_map%calculate_distance_matrix(kohonen_map%coordinates,&
         kohonen_map%cells_distances,training_parameters(1)%node_type,&
         training_parameters(1)%toroidal_grid);
   !
   !
   !
      kohonen_map%seed2=training_parameters(2)%random_seed_(1);
      !call sgrnd(seed);
      call kohonen_map%rnumber_grator(2)%create(kohonen_map%seed2);
   !
      do ix=1,number_clusters
         !call grnd_array(input);
         call kohonen_map%create_random_sample(input);
         call kohonen_map%cluster_layer(ix)%create(input);
      enddo
   !   
      deallocate(input);
   !
      write(*,*) 'TWO LEVEL SOM: Initializing grid...OK'
   !   
   end subroutine create_2lsom
   !========================================================================================
   subroutine destroy_2lsom(kohonen_map)
   !========================================================================================
!!   Destructor of a two_level self_organized_map 
      class(two_level_self_organizing_map) :: kohonen_map
!! A `two_level_self_organizing_map` object 
      integer :: ix,iy,iz
   !
      write(*,*) 'TWO LEVEL SOM: Releasing memory...'
      if(allocated(kohonen_map%grid)) then
      do ix=1,size(kohonen_map%grid,1)
         do iy=1,size(kohonen_map%grid,2)
            do iz=1,size(kohonen_map%grid,3);
               call kohonen_map%grid(ix,iy,iz)%destroy();
            enddo
         enddo
      enddo
      deallocate(kohonen_map%grid);
      endif
   !
      if(allocated(kohonen_map%coordinates)) then
      deallocate(kohonen_map%coordinates);
      endif
   !
      if(allocated(kohonen_map%number_patterns)) then
      deallocate(kohonen_map%number_patterns);
      endif
   !
      if(allocated(kohonen_map%cells_index)) then
      deallocate(kohonen_map%cells_index);
      endif
   !
      if(allocated(kohonen_map%u_matrix)) then
      deallocate(kohonen_map%u_matrix);
      endif
   !
      if(allocated(kohonen_map%distance)) then
         deallocate(kohonen_map%distance);
      endif
   !
      if(allocated(kohonen_map%distance_function)) then
      deallocate(kohonen_map%distance_function);
      endif
   !
   ! Deallocate cluster layers arrays
   !
      do ix=1,size(kohonen_map%cluster_layer)
         call kohonen_map%cluster_layer(ix)%destroy();
      enddo
      deallocate(kohonen_map%cluster_layer)
   !
      if(allocated(kohonen_map%cluster_cells_index)) then
      deallocate(kohonen_map%cluster_cells_index);
      endif
   !
   if(allocated(kohonen_map%cluster_number_patterns)) then
      deallocate(kohonen_map%cluster_number_patterns);
   endif
   !
   if(allocated(kohonen_map%grid_cluster)) then
      deallocate(kohonen_map%grid_cluster);
   endif
   !
   if(allocated(kohonen_map%cluster_samples)) then
      deallocate(kohonen_map%cluster_samples);
   endif
   !
   if(allocated(kohonen_map%number_cluster_samples)) then
      deallocate(kohonen_map%number_cluster_samples);
   endif
   !
   if(allocated(kohonen_map%index_cluster_samples)) then
      deallocate(kohonen_map%index_cluster_samples);
   endif
   !
      write(*,*) 'TWO LEVEL SOM: Releasing memory...OK!'
   !
   end subroutine destroy_2lsom
   !========================================================================================
   subroutine create_random_sample(kohonen_map,input)
   !========================================================================================
   !! Subroutine to generate random values that serve as inputs to the SOM
      class(two_level_self_organizing_map) :: kohonen_map
   !! A `self_organizing_map` object
      real(kind=wp),dimension(:,:),intent(out) :: input 
   !! A real array with the initial values of the prototypes
      integer :: nvar1,nvar2,i,j
   !
      nvar1=size(input,1);
      nvar2=size(input,2);
      do i=1,nvar1;
         do j=1,nvar2;
            input(i,j)=kohonen_map%rnumber_grator(1)%generate();
         end do
      end do
!    
      end subroutine create_random_sample         
   !========================================================================================
   subroutine train_2lsom(kohonen_map,input_data)
   !========================================================================================
!!   Subroutine to train a two_level self_organized_map
      class(two_level_self_organizing_map) :: kohonen_map
!! A `two_level_self_organizing_map` object
      type(kohonen_pattern),dimension(:),intent(inout) :: input_data
!! A `kohonen_pattern` array
      call train_grid_layer(kohonen_map,input_data);
   !
      call train_cluster_layer(kohonen_map);
   !
      call kohonen_map%assign_input_to_clusters(input_data);
   !
   end subroutine train_2lsom
   !========================================================================================
   subroutine predict_2lsom(kohonen_map,input_data,map_output)
   !========================================================================================
!! Subroutine to make a prediction from a trained two_level self_organized_map 
      class(two_level_self_organizing_map) :: kohonen_map
!! A `two_level_self_organizing_map` object
      type(kohonen_pattern),dimension(:),intent(inout) :: input_data
!! A `kohonen_pattern` array
      integer,dimension(:,:),intent(out) :: map_output
!! An integer array
      integer :: ipattern,ihit,jhit,khit,ix,iy,iz,number_variables,ic
      real(kind=wp) :: dist_hit,dist
      type(kohonen_prototype) :: current_prototype,predict_grid_prototype,predict_cluster_prototype
      real(kind=wp),dimension(kohonen_map%number_variables1,kohonen_map%number_variables2) :: current_values
      real(kind=wp),dimension(kohonen_map%number_clusters) :: distance_units
   !
      number_variables=kohonen_map%number_variables1*kohonen_map%number_variables2;
   !
         write(*,*)
         write(*,*) ' TWO LEVEL SOM: Prediction starting...';
         write(*,*)
   !       write(*,*) '                Prediction for Grid Layer in progress'
         do ipattern = 1, size(input_data,1)
            ihit = 0;
            jhit = 0;
            khit = 0;
            dist_hit = 100000.0;
            call input_data(ipattern)%get(current_prototype);
            !call current_prototype%get_prototype(current_values);         
            do iz = 1, size(kohonen_map%grid,3);  
               do iy = 1, size(kohonen_map%grid,2);
                  do ix = 1,size(kohonen_map%grid,1);
                     dist = 0.0_wp;
                     dist=kohonen_map%grid(ix,iy,iz)%distance(current_prototype,kohonen_map%distance_function);
                     dist = dist/float(number_variables);
                     if (dist .lt. dist_hit) then
                        dist_hit = dist
                        ihit = ix;
                        jhit = iy;
                        khit = iz;
                     endif
                  enddo!ix
               enddo!iy
            enddo!iz
            !call kohonen_map%grid(ihit,jhit,khit)%get_prototype(current_values);
            predict_grid_prototype=kohonen_map%grid(ihit,jhit,khit);
            ihit=0;
            dist_hit=10.0e4;
            do ic=1,size(kohonen_map%cluster_layer)
               dist=kohonen_map%cluster_layer(ic)%distance(predict_grid_prototype,kohonen_map%distance_function);
               distance_units(ic)=dist;
               if(dist .lt. dist_hit) then
                  dist_hit=dist;
                  ihit=ic;
               endif            
            enddo
   !         write(*,*) ipattern,ihit
            predict_cluster_prototype=kohonen_map%cluster_layer(ihit);
            call predict_cluster_prototype%get_prototype(current_values);
            map_output(ipattern,1)=ihit
         enddo !ipattern
         write(*,*) 'TWO LEVEL SOM: Prediction finished';
   
   !
   end subroutine predict_2lsom
   !========================================================================================
   subroutine train_grid_layer(kohonen_map,input_data)
   !========================================================================================
!! Subroutine to train the grid layer of a two_level self_organized_map
      class(two_level_self_organizing_map) :: kohonen_map
!! A `two_level_self_organizing_map` object
      type(kohonen_pattern),dimension(:),intent(inout) :: input_data
!! A `kohonen_pattern` array
      integer :: iteration,iepoch,ipattern,ix,iy,iz,jhit,ihit,khit,ineigh,jneigh,kneigh
      integer :: idbg,number_variables,max_pattern,ierr,nx,ny,nz,ic,current_pos
      integer :: cx,cy,cz,i,j,k,number_nodes,debug_option,pos,pos1,ix1,iy1,iz1
      real(kind=wp) :: distortion,dist,dist_hit,maximum_radius,minimum_radius,current_radius,alpha
      real(kind=wp) :: sigma2,geometric_distance2,h_neighborhood,real_distance,lambda,current_distance,d1,d2,d3
      real(kind=wp) :: term3,distance_ratio
      type(kohonen_prototype) :: current_prototype,current_prototype1
      real(kind=wp),dimension(kohonen_map%number_variables1,&
                  kohonen_map%number_variables2) :: current_values,prototype_values,winner_values,term1,term2
      integer, allocatable :: pattern_index(:,:,:,:)
      logical :: testop
      integer :: unit_out
         
         nx=kohonen_map%parameters(1)%number_nodes_nx;
         ny=kohonen_map%parameters(1)%number_nodes_ny;
         nz=kohonen_map%parameters(1)%number_nodes_nz;
         idbg=kohonen_map%parameters(1)%idbg;
         debug_option=kohonen_map%parameters(1)%debug_level;
         if(debug_option .gt. 0) then
         open(idbg,file=trim(kohonen_map%parameters(1)%debug_file),status='unknown');
         endif
         iteration = 0
         distortion = 0.0_wp
         number_variables=kohonen_map%number_variables; 
         maximum_radius=real(max(kohonen_map%parameters(1)%number_nodes_nx,kohonen_map%parameters(1)%number_nodes_ny,&
                                 kohonen_map%parameters(1)%number_nodes_nz));
         lambda=2.0_wp*(1.0_wp/maximum_radius);
         minimum_radius=1.0_wp;
         if(kohonen_map%parameters(1)%view_flag) then
         write(*,*) 'TWO LEVEL SOM - Grid Layer: Training starting...'
         endif
         do iepoch = 1,kohonen_map%parameters(1)%number_epochs
            if(kohonen_map%parameters(1)%view_flag) then
               write(6,*) ' Starting epoch -- distortion',iepoch,' -- ',distortion
            endif
            distortion = 0.0_wp
            do ipattern = 1, kohonen_map%parameters(1)%number_patterns
               iteration = iteration + 1
   !          det best match grid unit
   
               ihit = 0;
               jhit = 0;
               khit = 0;
               dist_hit = 1.0e7
               call input_data(ipattern)%get(current_prototype);
               call current_prototype%get_prototype(current_values);
               !call kohonen_map%find_best_match_unit(current_prototype,ihit,jhit,khit,dist_hit);
               !call current_prototype%print(unit_out);
               !write(*,*) 'check= ',ipattern,ihit,jhit,khit,dist_hit
               if(debug_option .gt. 0) then
               write(idbg,*) 'Epoch,Current Pattern',iepoch,ipattern;
               call current_prototype%print(idbg);
               endif
               !
               do iz = 1, size(kohonen_map%grid,3)  
                  do iy = 1, size(kohonen_map%grid,2)
                     do ix=1, size(kohonen_map%grid,1)
                        dist = 0.0_wp
                        ! calculate distance
                        !call kohonen_map%grid(ix,iy,iz)%print();
                        dist=kohonen_map%grid(ix,iy,iz)%distance(current_prototype,&
                           kohonen_map%distance_function);
                        !write(*,*) ix,iy,iz,dist
                        !stop
                     if(debug_option .gt. 0) then
                        call kohonen_map%grid(ix,iy,iz)%print(idbg);
                        write(idbg,*) ix,iy,iz,dist
                     endif
                     dist = dist/float(number_variables);
                     if (dist .lt. dist_hit) then
                        dist_hit = dist
                        ihit = ix;
                        jhit = iy;
                        khit = iz;
                     endif
                  enddo!iz
               enddo!iy
            enddo!ix
            !write(*,*) 'epoch,ipat,i,j,k,d= ',iepoch,ipattern,ihit,jhit,dist_hit
   !            if(kohonen_map%parameters(1)%ireal == 7) then
   !               write(unit_out,*) 'current_prototype'
   !               call current_prototype%print(unit_out)
   !               write(unit_out,*) 'bmu ',ipattern,ihit,jhit,dist_hit
   !               call kohonen_map%grid(ihit,jhit,khit)%print(unit_out)           
   !            endif
   !           kohonen_map%number_patterns(ihit,jhit)=kohonen_map%number_patterns(ihit,jhit)+1;
               distortion = distortion + dist_hit;
            ! define radius
            current_radius = max(maximum_radius*real(1001-iteration)/1000.0 + 0.9999999999,1.0_wp);
            ! define learning rate
            alpha = max(kohonen_map%parameters(1)%learning_rate*(1.0_wp-real(iteration)/1000.0),0.01_wp);
            sigma2=current_radius**2
            !max(0.2*maximum_radius*(1.0_wp-real(iteration)/1000.0),1.0_wp);
            ! update prototypes
            if(debug_option .gt. 0) then
               write(idbg,*) 'Neighborhood,alpha= ',alpha
            endif
   !
   !           call kohonen_map%update_weights(current_values,ihit,jhit,khit,maximum_radius,iteration)
   !
            do ic=1,size(kohonen_map%coordinates,1)
               current_pos=position2index(ihit,jhit,khit,nx,ny);
               current_distance=kohonen_map%cells_distances(current_pos,ic)
               if(current_distance .lt. current_radius) then
                  geometric_distance2=current_distance**2;
                  call index2position(ic,nx,ny,nz,ineigh,jneigh,kneigh);
                  !write(*,*) ic,ineigh,jneigh,kneigh,ihit,jhit,khit
                  select case(trim(kohonen_map%parameters(1)%neighborhood_type))
                     case('gaussian')
                        h_neighborhood=alpha*exp(-0.5*geometric_distance2/sigma2);
                     case('bubble')
                        h_neighborhood=alpha;
                  end select
                  if(debug_option .gt. 0) then
                        write(idbg,*) ihit,jhit,khit,ineigh,jneigh,kneigh
                  endif
                  select case(trim(kohonen_map%parameters(1)%som_type));
                     case('normal_som');
                        call kohonen_map%grid(ineigh,jneigh,kneigh)%get_prototype(prototype_values);
                        prototype_values=prototype_values+h_neighborhood*(current_values-prototype_values);
                        call kohonen_map%grid(ineigh,jneigh,kneigh)%set_prototype(prototype_values);
                     case('visom');
                        call kohonen_map%grid(ineigh,jneigh,kneigh)%get_prototype(prototype_values);
                        call kohonen_map%grid(ihit,jhit,khit)%get_prototype(winner_values);
                        real_distance=sum((winner_values-prototype_values)**2);
                        if( (ineigh .eq. ihit) .and. (jneigh .eq. jhit) .and. (kneigh .eq. khit) ) then                           
                           prototype_values=prototype_values+h_neighborhood*(current_values-prototype_values);
                        else
                           distance_ratio=dsqrt(real_distance)/(dsqrt(geometric_distance2)*lambda);
                           term1=(current_values-winner_values);
                           term2=(winner_values-prototype_values);
                           !eps=max(1.0_wp*time_factor,0.0_wp);
                           term3=1.0_wp;!((1.0_wp-eps)+eps)
                           prototype_values=prototype_values+h_neighborhood*(term1+term2*(distance_ratio-1.0_wp)*term3);
                        endif
                        call kohonen_map%grid(ineigh,jneigh,kneigh)%set_prototype(prototype_values);                         
                     end select                     
               endif
            enddo
   !
            enddo !ipattern
            !if(kohonen_map%parameters(1)%ireal == 7) stop
         enddo!iepoch
         if(kohonen_map%parameters(1)%view_flag) then
            write(*,*) 'TWO LEVEL SOM - Grid Layer: Training finished'
         endif
   ! Print prototypes
   !     print prototypes
         inquire(unit=kohonen_map%parameters(1)%iprot,opened=testop);
         if(testop) then
         do iz=1,size(kohonen_map%grid,3);
            write(kohonen_map%parameters(1)%iprot,'(A,I4)') 'Layer ',iz
            do iy=1,size(kohonen_map%grid,2);
               do ix=1,size(kohonen_map%grid,1);               
                  write(kohonen_map%parameters(1)%iprot,'(A6,1X,3I4)') 'node= ',ix,iy,iz            
                  call kohonen_map%grid(ix,iy,iz)%print(kohonen_map%parameters(1)%iprot);
               enddo
            enddo
         enddo!ix
         endif
   !     calculate and print distance matrix
         inquire(unit=kohonen_map%parameters(1)%idist,opened=testop);
         if(testop) then
         call kohonen_map%calculate_distance_between_prototypes()
   !        
   !         do ix=1,size(kohonen_map%grid,1);
   !           do iy=1,size(kohonen_map%grid,2);
   !              do iz=1,size(kohonen_map%grid,3);
   !                  current_prototype=kohonen_map%grid(ix,iy,iz);
   !                  pos=ix+(iy-1)*kohonen_map%parameters(1)%number_nodes_ny+&
   !                    (iz-1)*kohonen_map%parameters(1)%number_nodes_nx*kohonen_map%parameters(1)%number_nodes_ny;
   !                  do ix1=1,size(kohonen_map%grid,1);
   !                     do iy1=1,size(kohonen_map%grid,2);
   !                        do iz1=1,size(kohonen_map%grid,3);
   !                           pos1=ix1+(iy1-1)*kohonen_map%parameters(1)%number_nodes_ny+&
   !                              (iz1-1)*kohonen_map%parameters(1)%number_nodes_nx*&
   !                              kohonen_map%parameters(1)%number_nodes_ny;
   !                           current_prototype1=kohonen_map%grid(ix1,iy1,iz1);
   !                           kohonen_map%distance(pos,pos1)=current_prototype1%distance(current_prototype,&
   !                                                         kohonen_map%distance_function);
   !                        enddo!iz1        
   !                     enddo!iy1
   !                  enddo!ix1
   !               enddo!iz
   !            enddo!iy         
   !         enddo!ix
   ! !
   !         do ix=1,size(kohonen_map%distance,1)
   !            write(kohonen_map%parameters(1)%idist,*) (kohonen_map%distance(ix,iy),iy=1,size(kohonen_map%distance,2));
   !         enddo!ix
         endif
   !
   !     final best match
         if(kohonen_map%parameters(1)%view_flag) then
         write(6,*) 
         write(6,*) 'TWO LEVEL SOM - Grid Layer: Find Best Match Unit...'
         write(6,*)
         endif
         max_pattern=0;
         do ipattern = 1, kohonen_map%parameters(1)%number_patterns
            ihit = 0;
            jhit = 0;
            khit = 0;
            dist_hit = 100000.0;
            call input_data(ipattern)%get(current_prototype);
            !call current_prototype%get_prototype(current_values);
   !         call kohonen_map%find_best_match_unit(current_prototype,ihit,jhit,khit,dist_hit);
            !write(*,*) 'bmu=',ipattern,ihit,jhit,khit
            do iz = 1, size(kohonen_map%grid,3);  
               do iy = 1, size(kohonen_map%grid,2);
                  do ix = 1,size(kohonen_map%grid,1)
                     dist = 0.0_wp;
                     dist=kohonen_map%grid(ix,iy,iz)%distance(current_prototype,kohonen_map%distance_function);
                     dist = dist/float(number_variables);
                     if (dist .lt. dist_hit) then
                        dist_hit = dist
                        ihit = ix;
                        jhit = iy;
                        khit = iz;
                     endif
                  enddo
               enddo
            enddo
            kohonen_map%number_patterns(ihit,jhit,khit)=kohonen_map%number_patterns(ihit,jhit,khit)+1;
            if(kohonen_map%number_patterns(ihit,jhit,khit) .gt. max_pattern) then 
            max_pattern=kohonen_map%number_patterns(ihit,jhit,khit);
            endif
            kohonen_map%cells_index(ipattern,1)=ihit;
            kohonen_map%cells_index(ipattern,2)=jhit;
            kohonen_map%cells_index(ipattern,3)=khit;
            
            if(debug_option .gt. 0) then
            write(idbg,*) ipattern,ihit,jhit,khit
            endif
            inquire(unit=kohonen_map%parameters(1)%iindex,opened=testop);
            if(testop) then
            write(kohonen_map%parameters(1)%iindex,*) ipattern,ihit,jhit,khit
            endif
   !          mtchx(i) = ihit
   !          mtchy(i) = jhit
   !          ioutrep(ihit,jhit) = i
   
         enddo !ipattern
         if(kohonen_map%parameters(1)%debug_level .gt. 0) then
         close(idbg)
         endif
         allocate(pattern_index(size(kohonen_map%grid,1),size(kohonen_map%grid,2),&
                  size(kohonen_map%grid,3),&
                  max_pattern),stat=ierr);
         pattern_index=-1;
         do ipattern=1,kohonen_map%parameters(1)%number_patterns
            ix=kohonen_map%cells_index(ipattern,1);
            iy=kohonen_map%cells_index(ipattern,2);
            iz=kohonen_map%cells_index(ipattern,3);
            do i=1,max_pattern
               if(pattern_index(ix,iy,iz,i) .lt. 0) then
               pattern_index(ix,iy,iz,i)=ipattern;
               exit
               endif
            enddo
         enddo!ipattern
         inquire(unit=kohonen_map%parameters(1)%isam,opened=testop);
         if(testop) then
         do iz1=1,size(kohonen_map%grid,3);
            do iy1=1,size(kohonen_map%grid,2);
               do ix1=1,size(kohonen_map%grid,1);
                  write(kohonen_map%parameters(1)%isam,'(A,3I4)') 'Node= ',ix1,iy1,iz1
                  if(kohonen_map%number_patterns(ix1,iy1,iz1) .gt. 0) then
                     write(kohonen_map%parameters(1)%isam,'(A,100I4)') 'Sample ID= ',&
                     pattern_index(ix1,iy1,iz1,1:kohonen_map%number_patterns(ix1,iy1,iz1));
                  else
                     write(kohonen_map%parameters(1)%isam,'(A,I4)') 'Sample ID= ',0
                  endif
               enddo
            enddo
         enddo
         deallocate(pattern_index);
         endif
   !     print hit counter
      inquire(unit=kohonen_map%parameters(1)%ihit,opened=testop);
      if(testop) then
         do iz=1,size(kohonen_map%grid,3)
            write(kohonen_map%parameters(1)%ihit,'(A,I4)') 'Layer ',iz
            do ix=1,size(kohonen_map%grid,1);
               write(kohonen_map%parameters(1)%ihit,'(100I6)') (kohonen_map%number_patterns(ix,iy,iz),iy=1,size(kohonen_map%grid,2));
            enddo!ix
         enddo!iz
      endif
   
   !     calculate u_matrix
         inquire(unit=kohonen_map%parameters(1)%iumat,opened=testop);
         if(testop) then
         call kohonen_map%calculate_u_matrix();
   !       do iz=1,size(kohonen_map%grid,3);
   !          do iy=1,size(kohonen_map%grid,2);
   !             do ix=1,size(kohonen_map%grid,1);
   !                dist=0.0_wp;
   !                number_nodes=0;
   !                do k=-1,1
   !                   do j=-1,1
   !                      do i=-1,1
   !                         cx=ix+i;cy=iy+j;cz=iz+k;
   !                         if( (cx .ge. 1 .and. cx .le. size(kohonen_map%grid,1)) .and. &
   !                           (cy .ge. 1 .and. cy .le. size(kohonen_map%grid,2)) .and. &
   !                           (cz .ge. 1 .and. cz .le. size(kohonen_map%grid,3))) then
   !                               !write(*,*) ix,iy,cx,cy
   !                               dist=dist+kohonen_map%grid(ix,iy,iz)%distance(kohonen_map%grid(cx,cy,cz),&
   !                                     kohonen_map%distance_function);
   !                               number_nodes=number_nodes+1;
   !                         endif
   !                      enddo
   !                   enddo!j
   !                enddo!i
   !                kohonen_map%u_matrix(ix,iy,iz)=dist/real(number_nodes);
   !             enddo!iz
   !          enddo!iy
   !       enddo!ix
   !       do iz=1,size(kohonen_map%grid,3)
   !          write(kohonen_map%parameters(1)%iumat,'(A,I4)') 'Layer ',iz
   !          do ix=1,size(kohonen_map%grid,1)
   !             write(kohonen_map%parameters(1)%iumat,*) (kohonen_map%u_matrix(ix,iy,iz),iy=1,size(kohonen_map%grid,2)); 
   !          enddo
   !       enddo
         endif
         if(kohonen_map%parameters(1)%view_flag) then
         write(6,*) 
         write(6,*) 'TWO LEVEL SOM - Grid Layer: Find Best Match Unit finished'
         write(6,*)
         endif
         
   end subroutine train_grid_layer
   !========================================================================================
   subroutine train_cluster_layer(kohonen_map)
   !========================================================================================
!! Subroutine to train the cluster layer of a two_level self_organized_map
      class(two_level_self_organizing_map) :: kohonen_map
!! A `two_level_self_organizing_map` object
      integer :: ix,iy,iz,iepoch,ihit,ic,number_variables,iteration,ineigh,current_pos,ipattern,ipos,pos
      type(kohonen_prototype) :: current_prototype
      real(kind=wp) :: dist_hit,dist,distortion,maximum_radius,minimum_radius,current_radius
      real(kind=wp) :: alpha,h_neighborhood,sigma2,geometric_distance2
      real(kind=wp),dimension(kohonen_map%number_variables1,&
                              kohonen_map%number_variables2) :: current_values,prototype_values
      real(kind=wp),dimension(kohonen_map%number_variables1*kohonen_map%number_variables2) :: centers
      real(kind=wp),dimension(kohonen_map%number_clusters) :: distance_units
      real(kind=wp),dimension(kohonen_map%number_variables1*kohonen_map%number_variables2,&
                              kohonen_map%number_clusters) :: centers1
      logical :: testop
   !
      number_variables=kohonen_map%number_variables1*kohonen_map%number_variables2;
      !maximum_radius=real(max(kohonen_map%parameters%number_nodes_nx,kohonen_map%parameters%number_nodes_ny));
      maximum_radius=real(kohonen_map%number_clusters);
      minimum_radius=1.0_wp;
   
   !
      iteration = 0
      distortion=0.0_wp;
      if(kohonen_map%parameters(1)%view_flag) then
      write(*,*) 'TWO LEVEL SOM - Cluster Layer: Training starting...'
      endif
   !
      do iepoch=1,kohonen_map%parameters(2)%number_epochs
         if(kohonen_map%parameters(1)%view_flag) then
            write(6,*) ' Starting epoch -- distortion',iepoch,' -- ',distortion
         endif
         distortion=0.0_wp;
         do iz=1,size(kohonen_map%grid,3);
            do iy=1,size(kohonen_map%grid,2);
               do ix=1,size(kohonen_map%grid,1);
               iteration = iteration + 1
               ihit = 0;
               dist_hit = 100000.0;
               current_prototype=kohonen_map%grid(ix,iy,iz);
               call current_prototype%get_prototype(current_values);
               do ic=1,size(kohonen_map%cluster_layer)
                  dist=kohonen_map%cluster_layer(ic)%distance(current_prototype,kohonen_map%distance_function);
                  dist = dist/float(number_variables);                 
                  if (dist .lt. dist_hit) then
                        dist_hit = dist
                        ihit = ic;
                  endif 
               enddo!ic
               distortion = distortion + dist_hit;
               current_radius = max(maximum_radius*real(1001-iteration)/1000.0 + 0.9999999999,0.49_wp);
               sigma2=current_radius**2;
               !current_radius=0.0_wp;
               ! define learning rate
               alpha = max(kohonen_map%parameters(2)%learning_rate*&
                        (1.0_wp-real(iteration)/1000.0),0.01_wp);
               do ineigh = ihit-int(current_radius), ihit+int(current_radius)
                  if(ineigh .ge. 1 .and. ineigh .le. size(kohonen_map%cluster_layer)) then
                     select case(trim(kohonen_map%parameters(2)%neighborhood_type))
                        case('gaussian');
                        geometric_distance2=(ihit-ineigh)**2;
                        h_neighborhood=alpha*exp(-0.5*geometric_distance2/sigma2);
                        case('bubble');
                        h_neighborhood=alpha;
                     end select
                     call kohonen_map%cluster_layer(ineigh)%get_prototype(prototype_values);
                     prototype_values=prototype_values+h_neighborhood*(current_values-prototype_values);
                     call kohonen_map%cluster_layer(ineigh)%set_prototype(prototype_values);
                  endif
               enddo!ineigh
   
               enddo!iz
            enddo!iy
         enddo!ix
      enddo!iepoch
      if(kohonen_map%parameters(1)%view_flag) then
      write(*,*) 'TWO LEVEL SOM - Cluster Layer: Training finished';
   !
      write(6,*) 
      write(6,*) 'TWO LEVEL SOM - Cluster Layer: Find Best Match Unit...'
      write(6,*)
      endif
   !
      current_pos=0;
      do iz = 1,size(kohonen_map%grid,3);
         do iy=1,size(kohonen_map%grid,2);
            do ix=1,size(kohonen_map%grid,1);
               current_pos=current_pos+1;
               ihit = 0;
               dist_hit = 100000.0;
               current_prototype=kohonen_map%grid(ix,iy,iz);
               call current_prototype%get_prototype(current_values);
               do ic = 1, size(kohonen_map%cluster_layer);
                  dist=kohonen_map%cluster_layer(ic)%distance(current_prototype,kohonen_map%distance_function);
                  dist = dist/float(number_variables);
                  distance_units(ic)=dist;
                  if (dist .lt. dist_hit) then
                     dist_hit = dist
                     ihit = ic;
                     !write(*,*) 'hit= ',ix,iy,iz,ihit
                  endif
               enddo
               kohonen_map%cluster_cells_index(current_pos,1)=ix;
               kohonen_map%cluster_cells_index(current_pos,2)=iy;
               kohonen_map%cluster_cells_index(current_pos,3)=iz;
               kohonen_map%cluster_cells_index(current_pos,4)=ihit;
               !write(6,*) 'hit= ',ix,iy,iz,ihit
   !            if(kohonen_map%number_patterns(ix,iy,iz) .ge. 1) then
                  kohonen_map%grid_cluster(ix,iy,iz)=ihit;
   !            endif
               
   !             if(debug_option .gt. 0) then
   !               write(idbg,*) ix,iy,iz,ihit
   !             endif
            enddo !iz
         enddo!iy
      enddo!ix
   ! Print grid cluster
      inquire(unit=kohonen_map%parameters(1)%iclus,opened=testop);
      if(testop) then
      do iz=1,size(kohonen_map%grid_cluster,3);
            write(kohonen_map%parameters(1)%iclus,*) 'Layer ',iz
            do ix=1,size(kohonen_map%grid_cluster,1);
               write(kohonen_map%parameters(1)%iclus,'(100I5)') (kohonen_map%grid_cluster(ix,iy,iz),&
                     iy=1,size(kohonen_map%grid_cluster,2))
            enddo!ix
         enddo!iz
      endif
   !
   !     do ipattern=1,48
   !        write(*,*) kohonen_map%cells_index(ipattern,:)
   !     enddo
      do ipattern=1,kohonen_map%parameters(1)%number_patterns
         ix=kohonen_map%cells_index(ipattern,1);
         iy=kohonen_map%cells_index(ipattern,2);
         iz=kohonen_map%cells_index(ipattern,3);
         !write(*,*) 'pre= ',ipattern,ix,iy,iz,kohonen_map%parameters(1)%number_patterns
         do ipos=1,size(kohonen_map%cluster_cells_index,1)
            if(kohonen_map%cluster_cells_index(ipos,1) .eq. ix .and. &
               kohonen_map%cluster_cells_index(ipos,2) .eq. iy .and. &
               kohonen_map%cluster_cells_index(ipos,3) .eq. iz) then
               pos=ipos;
               exit
            endif
         enddo!ipos
         ic=kohonen_map%cluster_cells_index(pos,4);
         kohonen_map%cluster_samples(ipattern)=ic;
         !write(*,*) 'clust-train = ',pos,ipattern, ic
         inquire(unit=kohonen_map%parameters(1)%iclus1,opened=testop);
         if(testop) then
            write(kohonen_map%parameters(1)%iclus1,*) ipattern,ix,iy,iz,ic;
         endif
      enddo!ipattern
   !
      do ic=1,size(kohonen_map%cluster_layer);
         !write(unit1,*) 'Cluster= ',ic
         !call kohonen_map%cluster_layer(ic)%print(unit1);
         call kohonen_map%cluster_layer(ic)%get_prototype(centers)
         centers1(:,ic)=centers(:)
      enddo!ic
      inquire(unit=kohonen_map%parameters(1)%icen,opened=testop);
      if(testop) then
      do ix=1,size(centers1,1)
         write(kohonen_map%parameters(1)%icen,*) ix,(centers1(ix,ic),ic=1,size(centers1,2))
      enddo
      endif
   !
      if(kohonen_map%parameters(1)%view_flag) then
      write(6,*) 
      write(6,*) 'TWO LEVEL SOM - Cluster Layer: Find Best Match Unit finished'
      write(6,*)
      endif
   !
   end subroutine train_cluster_layer
   !========================================================================================
   subroutine print_2lsom(kohonen_map,unit_)
   !========================================================================================
!!   Subroutine to print the layers of a two_level self_organized_map 
      class(two_level_self_organizing_map) :: kohonen_map
!! A `two_level_self_organizing_map` object
      integer,optional :: unit_
!! An integer variable with the unit
      integer :: unit1,ix,iy,iz,ic
      real(kind=wp),dimension(kohonen_map%number_variables1*kohonen_map%number_variables2) :: centers
      real(kind=wp),dimension(kohonen_map%number_variables1*kohonen_map%number_variables2,&
                              kohonen_map%number_clusters) :: centers1
                        
   !
      if(.not. present(unit_)) then
         unit1=6;
      else
         unit1=unit_;
      endif
   !
   write(unit1,*) 'TWO LEVEL SOM: Results';
   write(unit1,*)
   call kohonen_map%parameters(1)%print(unit1);
   ! write(unit1,*) 'After'
   write(unit1,*)
   write(unit1,*) 'TWO LEVEL SOM: Grid nodes';
   write(unit1,*)
   do iz=1,size(kohonen_map%grid,3);
   do ix=1,size(kohonen_map%grid,1);
      do iy=1,size(kohonen_map%grid,2);
         write(unit1,*) ix,iy,iz
         call kohonen_map%grid(ix,iy,iz)%print(unit1);
      enddo!iy
   enddo!ix
   enddo
   write(unit1,*)
   write(unit1,*) 'TWO LEVEL SOM: Grid Hit count';
   write(unit1,*)
   write(unit1,*) 'Pattern Numbers';
   do iz=1,size(kohonen_map%number_patterns,3)
      write(unit1,*)
      write(unit1,*) 'Layer ',iz
      write(unit1,*)
      do ix=1,size(kohonen_map%number_patterns,1)
         write(unit1,'(100I5)') (kohonen_map%number_patterns(ix,iy,iz),iy=1,size(kohonen_map%number_patterns,2))
      enddo
   enddo
   write(unit1,*)
   write(unit1,*) 'TWO LEVEL SOM: Grid Pattern index'
   write(unit1,*)
   write(unit1,*)
   write(unit1,*) 'Pattern #, ix   ,iy';
   
   do ix=1,size(kohonen_map%cells_index,1)
      write(unit1,'(100I5)') ix, (kohonen_map%cells_index(ix,iy),iy=1,size(kohonen_map%cells_index,2))
   enddo
   write(unit1,*)
   write(unit1,*) 'TWO LEVEL SOM: Cluster nodes';
   write(unit1,*)
   open(2,file='cluster_centers.out',status='unknown')
   do ic=1,size(kohonen_map%cluster_layer);
         write(unit1,*) 'Cluster= ',ic
         call kohonen_map%cluster_layer(ic)%print(unit1);
         call kohonen_map%cluster_layer(ic)%get_prototype(centers)
         centers1(:,ic)=centers(:)
   enddo!ic
   do ix=1,size(centers1,1)
      write(2,*) ix,(centers1(ix,ic),ic=1,size(centers1,2))
   enddo
   close(2)
   !
   write(unit1,*)
   write(unit1,*) 'TWO LEVEL SOM: Cluster Pattern index'
   write(unit1,*)
   !
   do ix=1,size(kohonen_map%cluster_cells_index,1)
      write(unit1,*) (kohonen_map%cluster_cells_index(ix,iy),iy=1,size(kohonen_map%cluster_cells_index,2))
   enddo
   !
   do iz=1,size(kohonen_map%grid_cluster,3);
      write(unit1,*) 'Layer ',iz
      do ix=1,size(kohonen_map%grid_cluster,1);
         write(unit1,'(100I5)') (kohonen_map%grid_cluster(ix,iy,iz),iy=1,size(kohonen_map%grid_cluster,2))
      enddo!ix
   enddo!iz
   !
   end subroutine print_2lsom
   !========================================================================================  
   subroutine set_cluster_layer(kohonen_map,seed)
   !========================================================================================  
!!   Subroutine to initialize the cluster layer of a Two Level Self-Organizing Map
   class(two_level_self_organizing_map) :: kohonen_map
!! A `two_level_self_organizing_map` object
   integer :: seed
!! An integer variable with the random seed
   integer :: ix,number_clusters,ierr
   real(kind=wp),allocatable :: input(:,:)
!
      number_clusters=kohonen_map%parameters(2)%number_nodes_nx*&
      kohonen_map%parameters(2)%number_nodes_ny*&
      kohonen_map%parameters(2)%number_nodes_nz;
      write(*,*) 'nc=', number_clusters
      allocate(input(kohonen_map%number_variables1,kohonen_map%number_variables2),stat=ierr)
      write(*,*) 'nc1= ',size(input,1),size(input,2)
   !
      seed=kohonen_map%parameters(2)%random_seed_(1);
      allocate(kohonen_map%cluster_layer(number_clusters),stat=ierr);
      write(*,*) seed
      call kohonen_map%rnumber_grator(2)%create(seed);
      !call sgrnd(seed);
      do ix=1,number_clusters
         !call grnd_array(input);
         call kohonen_map%create_random_sample(input);
         write(*,*) ix,input
         write(*,*) size(kohonen_map%cluster_layer)
         call kohonen_map%cluster_layer(ix)%create(input);
      enddo
   !
      call kohonen_map%factory%create_distance(kohonen_map%parameters(2)%distance_type,kohonen_map%distance_function);
   !
      deallocate(input);
   !
   end subroutine set_cluster_layer
   !========================================================================================  
   subroutine set_parameters(kohonen_map,training_parameters)
   !========================================================================================  
!!   Subroutine to set parameters
   class(two_level_self_organizing_map) :: kohonen_map
!! A `two_level_self_organizing_map` object
   type(kohonen_layer_parameters),dimension(2) :: training_parameters
!A `kohonen_layer_parameters` object
   kohonen_map%parameters=training_parameters;
   !
   end subroutine set_parameters
   !========================================================================================  
   subroutine evaluate_2lsom(kohonen_map,input_data,results)
   !========================================================================================
!!   Subroutine to calculate some clustering statistics of a two-level self_organized_map 
      class(two_level_self_organizing_map) :: kohonen_map
!! A `two_level_self_organizing_map` object
      type(kohonen_pattern),dimension(:),intent(inout) :: input_data
!! A `kohonen_pattern` array
      real(kind=wp),dimension(:),optional :: results
!! A real array
      integer :: ipattern1,ipattern2,current_cluster,ic,is,pos
      integer,dimension(size(input_data)) :: indicator,positions
      integer,dimension(kohonen_map%number_clusters,size(input_data)) :: sample_positions
      integer,dimension(kohonen_map%number_clusters) :: number_samples_cluster
      type(kohonen_pattern) :: current_pattern1,current_pattern2
      type(kohonen_prototype) :: current_prototype1,current_prototype2
      real(kind=wp),dimension(kohonen_map%number_variables1,kohonen_map%number_variables2) :: &
                           current_values1,current_values2
      real(kind=wp) :: current_dissimilarity
      real(kind=wp),dimension(size(input_data)) :: mean_dissimilarity_a,silhouette,min_b,W,B
      real(kind=wp),dimension(size(input_data),kohonen_map%number_clusters) :: mean_dissimilarity_b
   !  Silouette or whatever
   !  find samples in each cluster
      min_b=10.0d8;
      B=0.0_wp;W=0.0_wp;
      do is=1,size(input_data) 
         positions(is)=is;
      enddo
   !   
      do ic=1,kohonen_map%number_clusters
         indicator=0;
         pos=0;
         where(kohonen_map%cluster_samples .eq. ic)
         indicator=1;        
         end where
         number_samples_cluster(ic)=sum(indicator);
         if(number_samples_cluster(ic) .eq. 0) then
         write(6,*) 'WARNING: Empty cluster. No cluster evaluation is done'
         if(present(results)) then
            results=0.0_wp;
         endif
         return
         endif
         do ipattern1=1,size(indicator)
            if(indicator(ipattern1) .eq. 1) then
            pos=pos+1;
            sample_positions(ic,pos)=ipattern1;
            endif
         enddo
      enddo!ic
   !
      do ipattern1=1,size(input_data)
         current_cluster=kohonen_map%cluster_samples(ipattern1);
   ! get current prototype
         current_pattern1=input_data(ipattern1);
         call current_pattern1%get(current_prototype1);
         call current_prototype1%get_prototype(current_values1);
         current_dissimilarity=0.0_wp;
         do ipattern2=1,number_samples_cluster(current_cluster);
            if (sample_positions(current_cluster,ipattern2) .ne. ipattern1) then
               current_pattern2=input_data(ipattern2);
               call current_pattern2%get(current_prototype2);
               call current_prototype2%get_prototype(current_values2);
   !	    if(kohonen_map%number_variables1 .eq. 1 .or. kohonen_map%number_variables2 .eq. 1 ) then
               current_dissimilarity=current_dissimilarity+sum((current_values1-current_values2)**2);
   !            else 
   !              current_dissimilarity=current_dissimilarity+sum(sum((current_values1-current_values2)**2));
   !            endif
            endif
         enddo!ipattern2
         mean_dissimilarity_a(ipattern1)=current_dissimilarity/max(1.0,real(number_samples_cluster(current_cluster)-1));
         W(ipattern1)=current_dissimilarity;
         !
         do ic=1,kohonen_map%number_clusters
            current_dissimilarity=0.0_wp;
            if(ic .ne. current_cluster) then
               do ipattern2=1,number_samples_cluster(ic);
                  current_pattern2=input_data(ipattern2);
                  call current_pattern2%get(current_prototype2);
                  call current_prototype2%get_prototype(current_values2);
   !               if(kohonen_map%number_variables1 .eq. 1 .or. kohonen_map%number_variables2 .eq. 1 ) then
                  current_dissimilarity=current_dissimilarity+sum((current_values1-current_values2)**2);
   !               else
   !                 current_dissimilarity=current_dissimilarity+sum(sum((current_values1-current_values2)**2));
   !               endif
               enddo!ipattern2
            endif
            mean_dissimilarity_b(ipattern1,ic)=current_dissimilarity/max(1.0,real(number_samples_cluster(ic)-1));       
            B(ipattern1)=B(ipattern1)+current_dissimilarity;
         enddo!ic
         
         !
         do ic=1,kohonen_map%number_clusters
            if(ic .ne. current_cluster .and. mean_dissimilarity_b(ipattern1,ic) .lt. min_b(ipattern1) ) then
               min_b(ipattern1)=mean_dissimilarity_b(ipattern1,ic)
            endif
         enddo
         !
         if(mean_dissimilarity_a(ipattern1) .lt. min_b(ipattern1)) then
         silhouette(ipattern1)=1.0_wp-(mean_dissimilarity_a(ipattern1)/min_b(ipattern1));
         else
         silhouette(ipattern1)=(min_b(ipattern1)/mean_dissimilarity_a(ipattern1))-1.0_wp;
         endif
         !write(6,*) ipattern1,mean_dissimilarity_a(ipattern1),min_b(ipattern1),silhouette(ipattern1) !(mean_dissimilarity_b(ipattern1,ic),ic=1,3);
      enddo!ipattern1
      write(6,*) ''
      write(6,*) 'CH= ',(sum(B)/dble(kohonen_map%number_clusters-1))&
                  /(sum(W)/(dble(size(input_data)-kohonen_map%number_clusters))); !,sum(B),sum(W),
      write(6,*) 'Silhouette= ',sum(silhouette)/real(size(silhouette))
      write(6,*) ''
   !
      if(present(results)) then
      results(1)=sum(B);
      results(2)=sum(W);
      results(3)=sum(silhouette)/real(size(silhouette));
      endif
   !
   end subroutine evaluate_2lsom
   !========================================================================================
   subroutine calculate_sum2_clusters_grid(kohonen_map,results)
   !========================================================================================
!! Subroutine to calculate some clustering statistics of a two-level self_organized_map 
   class(two_level_self_organizing_map) :: kohonen_map
!! A `two_level_self_organizing_map` object
   real(kind=wp),dimension(:),optional :: results
!! A real array
   integer :: ix1,iy1,iz1,ic,current_cluster,j,ipos,pos,isample
   integer :: nx,ny,nz,cix,ciy,ciz,current_pos,sample_pos
   real(kind=wp),dimension(kohonen_map%number_variables1,&
                  kohonen_map%number_variables2) :: mean_unit
   real(kind=wp),dimension(kohonen_map%number_clusters) :: W,B
   real(kind=wp) :: current_dissimilarity
   real(kind=wp),dimension(kohonen_map%number_variables1*&
                  kohonen_map%number_variables2,1) :: current_values1,current_values2
   type(kohonen_prototype) :: current_prototype1,current_prototype2
   integer,dimension(kohonen_map%number_nodes) :: indicator
   integer,dimension(kohonen_map%number_clusters) :: number_samples_cluster
   integer, dimension(kohonen_map%number_nodes) ::  positions
   integer,dimension(kohonen_map%number_clusters,kohonen_map%number_nodes) :: sample_positions
   real(kind=wp),dimension(kohonen_map%number_nodes) :: mean_dissimilarity_a,min_b,silhouette
   real(kind=wp),dimension(kohonen_map%number_nodes,kohonen_map%number_clusters) :: mean_dissimilarity_b
   
   !  
   positions(1:kohonen_map%number_nodes)=(/(ipos,ipos=1,kohonen_map%number_nodes)/)
   !
   nx=kohonen_map%parameters(1)%number_nodes_nx;
   ny=kohonen_map%parameters(1)%number_nodes_ny;
   nz=kohonen_map%parameters(1)%number_nodes_nz;
   !  find samples in each cluster
      min_b=10.0d8;
   !   
      B=0.0_wp;W=0.0_wp;mean_unit=0.0_wp
   !
   do iz1=1,size(kohonen_map%grid,3);
      do iy1=1,size(kohonen_map%grid,2);
         do ix1=1,size(kohonen_map%grid,1);
            current_cluster=kohonen_map%grid_cluster(ix1,iy1,iz1);
            current_prototype1=kohonen_map%grid(ix1,iy1,iz1);
            call current_prototype1%get_prototype(current_values1);
            current_prototype2=kohonen_map%cluster_layer(current_cluster);
            call current_prototype2%get_prototype(current_values2)              
            W(current_cluster)=W(current_cluster)+sum((current_values1-current_values2)**2);
            mean_unit=mean_unit+current_values1;
         enddo!ix
      enddo!iy
   enddo!iz
   mean_unit=mean_unit/dble(nx*ny*nz);
   !
   do ic=1,kohonen_map%number_clusters
      current_prototype1=kohonen_map%cluster_layer(ic);
      call current_prototype1%get_prototype(current_values1);
      B(ic)=B(ic)+sum((current_values1-mean_unit)**2);
   enddo!ic
   !
   if(present(results)) then
      results(1)=sum(W);results(2)=sum(B);
   !  else 
   !     write(6,*) 'Number clusters,W,B= ',kohonen_map%number_clusters,sum(W),sum(B)
   endif
   !
   ! Silhouette for grid layer
   !
   do ic=1,kohonen_map%number_clusters
      indicator=0;
      where(kohonen_map%cluster_cells_index(:,4) == ic)
         indicator=1;
      end where
      number_samples_cluster(ic)=sum(indicator);
      pos=0;
      do isample=1,size(indicator)
         if(indicator(isample) .eq. 1) then
            pos=pos+1;
            sample_positions(ic,pos)=isample;
         endif
      enddo
      !write(6,*) (sample_positions(ic,isample),isample=1,number_samples_cluster(ic));
   enddo
   !
   current_pos=0;
   do iz1=1,size(kohonen_map%grid,3);
      do iy1=1,size(kohonen_map%grid,2);
         do ix1=1,size(kohonen_map%grid,1);
            current_pos=current_pos+1;
            current_cluster=kohonen_map%grid_cluster(ix1,iy1,iz1);
            current_prototype1=kohonen_map%grid(ix1,iy1,iz1);
            call current_prototype1%get_prototype(current_values1);
            current_dissimilarity=0.0_wp;
            do ic=1,number_samples_cluster(current_cluster);
               if(sample_positions(current_cluster,ic) .ne. current_pos ) then
                  sample_pos=sample_positions(current_cluster,ic);
                  cix=kohonen_map%cluster_cells_index(sample_pos,1);
                  ciy=kohonen_map%cluster_cells_index(sample_pos,2);
                  ciz=kohonen_map%cluster_cells_index(sample_pos,3);
                  current_prototype2=kohonen_map%grid(cix,ciy,ciz);
                  call current_prototype2%get_prototype(current_values2);
                  current_dissimilarity=current_dissimilarity+sum((current_values1-current_values2)**2);
               endif
            enddo
            mean_dissimilarity_a(current_pos)=current_dissimilarity/dble(number_samples_cluster(current_cluster));
   ! 
            do ic=1,kohonen_map%number_clusters
               if(ic /= current_cluster) then
                  current_dissimilarity=0.0_wp;
                  do isample=1,number_samples_cluster(ic);
                     sample_pos=sample_positions(ic,isample);
                     cix=kohonen_map%cluster_cells_index(sample_pos,1);
                     ciy=kohonen_map%cluster_cells_index(sample_pos,2);
                     ciz=kohonen_map%cluster_cells_index(sample_pos,3);
                     current_prototype2=kohonen_map%grid(cix,ciy,ciz);
                     call current_prototype2%get_prototype(current_values2);
                     current_dissimilarity=current_dissimilarity+sum((current_values1-current_values2)**2);
                  enddo!isample
                  mean_dissimilarity_b(current_pos,ic)=current_dissimilarity/dble(number_samples_cluster(ic));
               endif              
            enddo!ic
            !
            do ic=1,kohonen_map%number_clusters
               if(ic .ne. current_cluster .and. mean_dissimilarity_b(current_pos,ic) .lt. min_b(current_pos) ) then
                  min_b(current_pos)=mean_dissimilarity_b(current_pos,ic)
               endif
            enddo
   !
            if(mean_dissimilarity_a(current_pos) .lt. min_b(current_pos)) then
               silhouette(current_pos)=1.0_wp-(mean_dissimilarity_a(current_pos)/min_b(current_pos));
            else
               silhouette(current_pos)=(min_b(current_pos)/mean_dissimilarity_a(current_pos))-1.0_wp;
            endif
   !
         enddo!ix1
      enddo!iy1
   enddo!iz1
   write(*,*) 'Number clusters,W,B,sil= ',kohonen_map%number_clusters,sum(W),sum(B),sum(silhouette)/dble(kohonen_map%number_nodes);
   if(present(results)) then
      results(3)=sum(silhouette)/dble(kohonen_map%number_nodes);
   endif
   !  write(*,*) 'b',min_b !mean_dissimilarity_b
   !  write(*,*) 'a',mean_dissimilarity_a
   !  write(*,*) 'a,b',sum(mean_dissimilarity_a)/dble(kohonen_map%number_clusters),&
   !                   sum(mean_dissimilarity_b)/dble(kohonen_map%number_clusters)
   !
   !
   end subroutine calculate_sum2_clusters_grid
   !========================================================================================
   subroutine calculate_cluster_measures(kohonen_map,results)
   !========================================================================================
!! Subroutine to calculate some clustering statistics of a two-level self_organized_map 
   class(two_level_self_organizing_map) :: kohonen_map
!! A `two_level_self_organizing_map` object
   real(kind=wp),dimension(:,:,:) :: results
!! A real array
   logical :: testop
   !
   inquire(unit=kohonen_map%parameters(1)%iout,opened=testop);
   if(.not. testop) then
      write(6,*) 'ERROR: the output file is not opened'
      stop
   endif
   !
   end subroutine calculate_cluster_measures
   !========================================================================================
   subroutine read_som_layer(kohonen_map,som_fl,layer_type)
   !========================================================================================
!! Subroutine to read the prototypes of the first/seconf layer of a two level self_organized_map 
      class(two_level_self_organizing_map) :: kohonen_map
!! A `two_level_self_organizing_map` object
      character(len=*) :: som_fl,layer_type
!! A character variable
      logical :: testfl,toroidal_grid
      integer :: isom,nx,ny,nz,nvar1,nvar2,ierr,ix,iy,iz,ivar,current_index
      character(len=40) :: current_line,node_type
      real(kind=wp),allocatable :: Prototype_value(:,:)
   !
      isom=20;
      inquire(file=trim(som_fl),exist=testfl);
      if(.not. testfl) then
      write(*,*) 'ERROR: the som file does not exist'
      stop
      endif
   !
      write(*,*)
      write(*,*) 'SOM: Reading SOM Prototypes...'
      write(*,*)
      open(isom,file=trim(som_fl),status='unknown',action='read',access='sequential');
      read(isom,'(A)') current_line
      write(*,*) trim(current_line)
      read(isom,'(A17,1X,3I6)') current_line,nx,ny,nz
      write(*,*) current_line,nx,ny,nz
      read(isom,'(A21,1X,2I6)') current_line,nvar1,nvar2
      write(*,*) current_line,nvar1,nvar2
      kohonen_map%number_variables1=nvar1;
      kohonen_map%number_variables2=nvar2;
      kohonen_map%number_variables=nvar1*nvar2;
      read(isom,'(A25,1X,A11,1X,L4)') current_line,node_type,toroidal_grid
      write(*,*) current_line,node_type,toroidal_grid
      allocate(Prototype_value(nvar1*nvar2,1),stat=ierr);
      !
      select case(trim(layer_type))
      case('grid')
         if(allocated(kohonen_map%grid)) then
            do iz=1,size(kohonen_map%grid,3)
               do iy=1,size(kohonen_map%grid,2)
                  do ix=1,size(kohonen_map%grid,1)
                     call kohonen_map%grid(ix,iy,iz)%destroy();             
                  enddo
               enddo
            enddo
            deallocate(kohonen_map%grid);
         endif
         allocate(kohonen_map%grid(nx,ny,nz),stat=ierr);     
         if(allocated(kohonen_map%coordinates)) then
            deallocate(kohonen_map%coordinates);
         endif
         allocate(kohonen_map%coordinates(nx*ny*nz,3),stat=ierr);
         if(allocated(kohonen_map%cells_distances)) then
            deallocate(kohonen_map%cells_distances);
         endif
         allocate(kohonen_map%cells_distances(nx*ny*nz,nx*ny*nz),stat=ierr);
         if(allocated(kohonen_map%cluster_cells_index)) then
            deallocate(kohonen_map%cluster_cells_index);
         endif
         allocate(kohonen_map%cluster_cells_index(nx*ny*nz,4),stat=ierr);
         if(allocated(kohonen_map%grid_cluster)) then
            deallocate(kohonen_map%grid_cluster);
         endif
         allocate(kohonen_map%grid_cluster(nx,ny,nz),stat=ierr);
      case('cluster') 
         if(allocated(kohonen_map%cluster_layer) ) then
            do iz=1,size(kohonen_map%cluster_layer,1)
               call kohonen_map%cluster_layer(ix)%destroy();             
            enddo
            deallocate(kohonen_map%cluster_layer);
         endif
         allocate(kohonen_map%cluster_layer(nx),stat=ierr);
      end select
      do iz=1,nz
         read(isom,'(A)') current_line
         write(*,*) 'Reading ',trim(current_line)
         do iy=1,ny
            do ix=1,nx
               read(isom,'(A)') current_line
   !            write(*,*) current_line
               read(isom,'(A)') current_line
   !            write(*,*) current_line
               read(isom,*) (Prototype_value(ivar,1),ivar=1,nvar1*nvar2)
               !write(*,*) ix,iy,(Prototype_value(ivar,1),ivar=1,nvar1*nvar2)
               select case(trim(layer_type))
               case('grid')
                  call kohonen_map%grid(ix,iy,iz)%create(Prototype_value);
                  !call kohonen_map%grid(ix,iy,iz)%print()
                  current_index=position2index(ix,iy,iz,nx,ny);
                  call calculate_coordinates(current_index,ix,iy,iz,nx,ny,nz,kohonen_map%coordinates,node_type);
               case('cluster')
                  call kohonen_map%cluster_layer(ix)%set_prototype(Prototype_value);
               end select
            enddo
         enddo
      enddo
      close(isom)
   !
      if(trim(layer_type) .eq. 'grid') then
      call calculate_distance_matrix(kohonen_map%coordinates,kohonen_map%cells_distances,node_type,toroidal_grid);    
      endif   
   !
      write(*,*)
      write(*,*) 'SOM: Reading SOM Prototypes...finished'
      write(*,*)
   !
   end subroutine read_som_layer
   !========================================================================================
   subroutine query_2lsom(kohonen_map,input_pattern,sample_index) !,output_patterns)
   !========================================================================================
!!  Function to find the input samples associated with specific vector 
      class(two_level_self_organizing_map) :: kohonen_map
!! A `two_level_self_organizing_map` object
      real(kind=wp),dimension(:,:),intent(inout) :: input_pattern
!! A real array
      integer,allocatable :: sample_index(:)
!! An integer array
      integer :: ix,iy,iz,ihit,jhit,khit,ivar1,ivar2,nvar1,nvar2,number_patterns,ipat,ierr
      integer :: number_selected,i,pos,ic,number_samples
      real(kind=wp),dimension(kohonen_map%parameters(1)%number_variables1,&
      kohonen_map%parameters(1)%number_variables2) ::current_values,centers
      real(kind=wp) :: dist,dist_min
      integer,dimension(size(kohonen_map%cells_index,1)) :: position,real_position
   ! 
      do ic=1,size(kohonen_map%cluster_layer);
         !write(unit1,*) 'Cluster= ',ic
         !call kohonen_map%cluster_layer(ic)%print(unit1);
         call kohonen_map%cluster_layer(ic)%get_prototype(centers);
         !centers1(:,ic)=centers(:)
         dist_min=1.0d5;
         dist=0.0_wp;
         do ix=1,size(centers,1);
            do iy=1,size(centers,2);
               if(input_pattern(ix,iy) > 0.0_wp) then
                  dist=dist+(input_pattern(ix,iy)-centers(ix,iy))**2
               endif
            enddo
         enddo
         if(dist < dist_min) then
            dist_min=dist;
            ihit=ic;
         endif
         number_samples=kohonen_map%number_cluster_samples(ihit);
         if(number_samples .gt. 0) then
            allocate(sample_index(number_samples),stat=ierr);
            sample_index=kohonen_map%index_cluster_samples(ihit,1:number_samples);
         else 
            write(*,*) 'WARNING: Empty query'
            return
         endif
      enddo!ic
   !
   end subroutine query_2lsom
   !======================================================================================== 
   subroutine get_cluster_samples(kohonen_map,clusters)
   !========================================================================================
!! Accessor to cluster results obtained using a two-level self_organized_map
   class(two_level_self_organizing_map) :: kohonen_map
!! A `two_level_self_organizing_map` object
   integer,dimension(:),intent(inout) :: clusters
!! An integer array
   write(*,*) 'size_cluster= ',size(clusters)
   if(size(clusters) .eq. size(kohonen_map%cluster_samples)) then
      clusters=kohonen_map%cluster_samples;
   endif
   !
   end subroutine get_cluster_samples
   !========================================================================================
   function position2index(ix,iy,iz,nx,ny) result(index_)
   !========================================================================================
!!   Function to calculate the index inside a rectangular grid from position ix,iy,iz
      integer,intent(in) :: ix,iy,iz,nx,ny
!! Integer variables ix,iy,iz,nx,ny
      integer ::index_
!! Integer variable with the index
   !*****
      index_=ix+(iy-1)*nx+(iz-1)*nx*ny
   !
   end function position2index
   !========================================================================================
   subroutine index2position(index_,nx,ny,nz,cx,cy,cz)
   !========================================================================================
!!  Subroutine to calculate the position ix,iy,iz inside a rectangular grid from index
   integer,intent(in) :: index_,nx,ny,nz
!! Integer variables, index_,nx,ny,nz
   integer,intent(inout) :: cx,cy,cz
!! Integer variables cx,cy,cz
   !  write(*,*) index_,nx,ny,1+int((index_-1)/(nx*ny))
   cz=min(1+int((index_-1)/(nx*ny)),nz);
   cy=min(1+int((index_-1-(cz-1)*nx*ny)/nx),ny);
   cx=min(index_-(cz-1)*nx*ny-(cy-1)*nx,nx);
   !
   end subroutine index2position
   !========================================================================================
   subroutine calculate_distance_matrix(coordinates,distance_matrix,grid_type,toroidal)
   !========================================================================================
!! Subroutine to calculate the distance between the units inside a kohonen layer  
   real(kind=wp),dimension(:,:),intent(inout) :: coordinates
!! A real array
   real(kind=wp),dimension(:,:),intent(inout) :: distance_matrix
!! A real array
   character(len=*) :: grid_type
!! A character varaible
   logical :: toroidal
!! A logical variableS
   integer :: i,j
   real(kind=wp) :: maxdiffx,maxdiffy,maxdiffz
   real(kind=wp),dimension(3) :: diffs
   !
   maxdiffx=maxval(coordinates(:,1))/2.0_wp;
   maxdiffy=maxval(coordinates(:,2))/2.0_wp;
   maxdiffz=maxval(coordinates(:,3))/2.0_wp;
   !
   if(toroidal) then
      do i=1,size(distance_matrix,1)
         do j=i+1,size(distance_matrix,2)
            diffs=abs(coordinates(j,1:3) - coordinates(i,1:3));
            if (diffs(1) > maxdiffx) diffs(1)=2.0_wp*maxdiffx - diffs(1);
            if (diffs(2) > maxdiffy) diffs(2)=2.0_wp*maxdiffy - diffs(2);
            !if (diffs(3) > maxdiffy) diffs(3)=2*maxdiffz - diffs(3);
            if (trim(grid_type) .eq. "hexagonal") then 
                  distance_matrix(i,j)= sum(diffs**2);
            elseif(trim(grid_type) .eq. "rectangular") then !rectangular 
                  distance_matrix(i,j)=maxval(diffs);
            endif
         enddo
      enddo
   else
      do i=1,size(distance_matrix,1)
         do j=i+1,size(distance_matrix,2)
            diffs=abs(coordinates(j,1:3) - coordinates(i,1:3));
            distance_matrix(i,j)=dsqrt(sum(diffs**2));
         enddo
      enddo   
   endif
   !
   distance_matrix=distance_matrix + transpose(distance_matrix);
   !write(*,'(49f10.4)') distance_matrix(1,:)
   !
   end subroutine calculate_distance_matrix
   !========================================================================================
   subroutine calculate_coordinates(current_index,ix,iy,iz,nx,ny,nz,coordinates,node_type)
   !========================================================================================
!! Subroutine to calculate the coordinates of the units inside a kohonen layer 
   integer,intent(in) :: current_index,ix,iy,iz,nx,ny,nz
!! Integer variables
   real(kind=wp),dimension(:,:),intent(out) :: coordinates
!! Real array
   character(len=*),intent(in) :: node_type
!! Character variable
   coordinates(current_index,1)=dble(ix);
   coordinates(current_index,2)=dble(iy);
   coordinates(current_index,3)=dble(iz);
   if(trim(node_type) .eq. 'hexagonal') then
      coordinates(current_index,1)=coordinates(current_index,1)+&
                  .5_wp*(mod(coordinates(current_index,2),2.0_wp));
      coordinates(current_index,2)=(dsqrt(3.0_wp)/2._wp)*coordinates(current_index,2);
   endif
   !
   end subroutine calculate_coordinates
   !========================================================================================
   subroutine find_best_match_unit(kohonen_map,current_prototype,ihit,jhit,khit,dist_hit)
   !========================================================================================
!!    Subroutine to calculate the best match unit  
      class(two_level_self_organizing_map) :: kohonen_map
!! A `two_level_self_organizing_map` object
      type(kohonen_prototype),intent(inout) :: current_prototype
!! A `kohonen_prototype` object
      integer,intent(out) :: ihit,jhit,khit
!! Integer variables
      real(kind=wp),intent(out) :: dist_hit
!! A real variable
      integer :: debug_option,idbg,ix,iy,iz,number_variables
      real(kind=wp) :: dist
   !
      idbg=kohonen_map%parameters(1)%idbg;
      debug_option=kohonen_map%parameters(1)%debug_level;
      number_variables=kohonen_map%parameters(1)%number_variables1*kohonen_map%parameters(1)%number_variables2
      ihit = 0;
      jhit = 0;
      khit = 0;
      dist_hit = 1.0e6;
      do iz = 1, size(kohonen_map%grid,3)  
         do iy = 1, size(kohonen_map%grid,2)
            do ix = 1,size(kohonen_map%grid,1)
               dist = 0.0_wp;
               dist=kohonen_map%grid(ix,iy,iz)%distance(current_prototype,kohonen_map%distance_function);
               if(debug_option .gt. 0) then
                  call kohonen_map%grid(ix,iy,iz)%print(idbg)
                  write(idbg,*) ix,iy,iz,dist
               endif
               dist = dist/float(number_variables);
               if (dist .lt. dist_hit) then
                  dist_hit = dist
                  ihit = ix
                  jhit = iy
                  khit = iz
                  !write(*,*)' fbmu= ',ihit,jhit,khit,dist
               endif
            enddo!ix
         enddo!iy
      enddo!iz
   !
   return
   !
   end subroutine find_best_match_unit
   !========================================================================================
   subroutine update_weights(kohonen_map,current_values,ihit,jhit,khit,&
               maximum_radius,iteration) 
   !========================================================================================
!!    Subroutine to update the weights
   class(two_level_self_organizing_map) :: kohonen_map
!! A `two_level_self_organizing_map` object
   real(kind=wp),dimension(:,:),intent(inout) :: current_values
!! A real array
   integer,intent(inout) :: ihit,jhit,khit,iteration
!! Integer variables
   real(kind=wp),intent(inout) :: maximum_radius
!! A real variable with the maximum radius
   real(kind=wp),dimension(size(current_values,1),size(current_values,2)) :: prototype_values
   real(kind=wp),dimension(size(current_values,1),size(current_values,2)) :: winner_values,term1,term2
   integer :: nx,ny,nz,debug_option,ic,current_pos,ineigh,jneigh,kneigh,idbg
   real(kind=wp) :: time_factor,current_radius,alpha,sigma2,h_neighborhood,real_distance,term3
   real(kind=wp) :: distance_ratio,geometric_distance2,eps,current_distance,lambda
   !
   nx=kohonen_map%parameters(1)%number_nodes_nx;
   ny=kohonen_map%parameters(1)%number_nodes_ny;
   nz=kohonen_map%parameters(1)%number_nodes_nz;
   debug_option=kohonen_map%parameters(1)%debug_level;
   idbg=kohonen_map%parameters(1)%idbg;
   lambda=2.0_wp*(1.0_wp/maximum_radius);
   time_factor=1.0_wp-dble(iteration)/&
               dble(kohonen_map%parameters(1)%number_epochs*kohonen_map%parameters(1)%number_patterns);
   !current_radius = max(maximum_radius*real(1001-iteration)/1000.0 + 0.9999999999,4.0_wp);
   current_radius = max(maximum_radius*time_factor,4.0_wp);
   !alpha = max(kohonen_map%parameters%learning_rate*(1.0_wp-real(iteration)/1000.0),0.01_wp);
   alpha = max(kohonen_map%parameters(1)%learning_rate*time_factor,0.01_wp);
   sigma2=current_radius**2
   !
   do ic=1,size(kohonen_map%coordinates,1)
      current_pos=position2index(ihit,jhit,khit,nx,ny);
      current_distance=kohonen_map%cells_distances(current_pos,ic)
      if(current_distance .lt. current_radius) then
         geometric_distance2=current_distance**2;
         call index2position(ic,nx,ny,nz,ineigh,jneigh,kneigh);
                  !write(*,*) ic,ineigh,jneigh,kneigh,ihit,jhit,khit
         select case(trim(kohonen_map%parameters(1)%neighborhood_type))
            case('gaussian')
               h_neighborhood=alpha*exp(-0.5*geometric_distance2/sigma2);
            case('bubble')
               h_neighborhood=alpha;
            end select
            if(debug_option .gt. 0) then
               write(idbg,*) ihit,jhit,khit,ineigh,jneigh,kneigh
            endif
            select case(trim(kohonen_map%parameters(1)%som_type))
            case('normal_som')                      
               call kohonen_map%grid(ineigh,jneigh,kneigh)%get_prototype(prototype_values);
               prototype_values=prototype_values+h_neighborhood*(current_values-prototype_values);
               call kohonen_map%grid(ineigh,jneigh,kneigh)%set_prototype(prototype_values)
            case('visom')
               !write(*,*) trim(kohonen_map%parameters%som_type)
               call kohonen_map%grid(ineigh,jneigh,kneigh)%get_prototype(prototype_values);
               call kohonen_map%grid(ihit,jhit,khit)%get_prototype(winner_values);
               real_distance=sum((winner_values-prototype_values)**2);
               if( (ineigh .eq. ihit) .and. (jneigh .eq. jhit) .and. (kneigh .eq. khit) ) then                           
                  prototype_values=prototype_values+h_neighborhood*(current_values-prototype_values);
               else
                  distance_ratio=dsqrt(real_distance)/(dsqrt(geometric_distance2)*lambda);
                  term1=(current_values-winner_values);
                  term2=(winner_values-prototype_values);
                  eps=max(1.0_wp*time_factor,0.0_wp);
                  term3=1.0_wp;!((1.0_wp-eps)+eps)
                  prototype_values=prototype_values+h_neighborhood*(term1+term2*(distance_ratio-1.0_wp)*term3);
                  endif
                        !write(*,*) iteration,dsqrt(real_distance),dsqrt(geometric_distance2)*lambda,distance_ratio
                  call kohonen_map%grid(ineigh,jneigh,kneigh)%set_prototype(prototype_values); 
            end select
      endif
      enddo!ic
   
   end subroutine update_weights
   !========================================================================================
   subroutine calculate_distance_between_prototypes(kohonen_map)
   !========================================================================================
!!    Subroutine to calculate distance between prototypes
   class(two_level_self_organizing_map) :: kohonen_map
!! A `two_level_self_organizing_map` object
   integer :: nx,ny,ix,iy,iz,ix1,iy1,iz1,pos,pos1
   type(kohonen_prototype) :: current_prototype,current_prototype1
   logical :: testop
!! A logical variable
   nx=kohonen_map%parameters(1)%number_nodes_nx;
   ny=kohonen_map%parameters(1)%number_nodes_ny;
   do iz=1,size(kohonen_map%grid,3)
      do iy=1,size(kohonen_map%grid,2);
         do ix=1,size(kohonen_map%grid,1);
            current_prototype=kohonen_map%grid(ix,iy,iz);
            pos=position2index(ix,iy,iz,nx,ny);
            do iz1=1,size(kohonen_map%grid,3);
               do iy1=1,size(kohonen_map%grid,2);
                  do ix1=1,size(kohonen_map%grid,1);
                     pos1=position2index(ix1,iy1,iz1,nx,ny)
                     current_prototype1=kohonen_map%grid(ix1,iy1,iz1);
                     kohonen_map%distance(pos,pos1)=current_prototype1%distance(current_prototype,&
                                 kohonen_map%distance_function);
                     enddo!ix1
                  enddo!iy1  
            enddo!iz1
         enddo!ix
      enddo!iy         
   enddo!iz
   !
   do ix=1,size(kohonen_map%distance,1)
         write(kohonen_map%parameters(1)%idist,*) (kohonen_map%distance(ix,iy),iy=1,size(kohonen_map%distance,2));
      enddo!ix
   ! 
   end subroutine calculate_distance_between_prototypes
   !========================================================================================
   subroutine calculate_u_matrix(kohonen_map)
   !========================================================================================
!!    Subroutine to calculate  the u_matrix
   class(two_level_self_organizing_map) :: kohonen_map
!! A `two_level_self_organizing_map` object 
   character(len=50) :: type_
   integer :: nx,ny,nz,nt,ierr,ix,iy,iz,cx,cy,cz,nxu,nyu,nzu
   real(kind=wp) :: dist,u_temp
   logical :: testop
   !
   type_=kohonen_map%parameters(1)%node_type;
   nx=kohonen_map%parameters(1)%number_nodes_nx;
   ny=kohonen_map%parameters(1)%number_nodes_ny;
   nz=kohonen_map%parameters(1)%number_nodes_nz;
   !
   nxu=size(kohonen_map%u_matrix,1);
   nyu=size(kohonen_map%u_matrix,2);
   nzu=size(kohonen_map%u_matrix,3);
   
   !
   select case(trim(type_))
   ! 
      case('rectangular')
   !   
      do iz=1,size(kohonen_map%grid,3);
         do iy=1,size(kohonen_map%grid,2);
            do ix=1,size(kohonen_map%grid,1);
               ! horizontal
               if(ix<nx) then
                  cx=ix+1;cy=iy;cz=iz;               
                  dist=kohonen_map%grid(ix,iy,iz)%distance(kohonen_map%grid(cx,cy,cz),&
                                    kohonen_map%distance_function);
                  kohonen_map%u_matrix(2*ix,2*iy-1,2*iz-1)=dist;
               endif
               !vertical
               if(iy<ny) then
                  cx=ix;cy=iy+1;cz=iz;               
                  dist=kohonen_map%grid(ix,iy,iz)%distance(kohonen_map%grid(cx,cy,cz),&
                                    kohonen_map%distance_function);
                  kohonen_map%u_matrix(2*ix-1,2*iy,2*iz-1)=dist;              
               endif
               !
               if(iz<nz) then
                  cx=ix;cy=iy;cz=iz+1;               
                  dist=kohonen_map%grid(ix,iy,iz)%distance(kohonen_map%grid(cx,cy,cz),&
                                    kohonen_map%distance_function);
                  kohonen_map%u_matrix(2*ix-1,2*iy-1,2*iz-1)=dist;         
               endif
               ! Diagonal
               if(ix < nx .and. iy < ny) then
                  cx=ix+1;cy=iy+1;cz=iz;
                  dist=kohonen_map%grid(ix,iy,iz)%distance(kohonen_map%grid(cx,cy,cz),&
                                    kohonen_map%distance_function);
                  cx=ix+1;cy=iy+1;cz=iz;
                  dist=dist+kohonen_map%grid(ix,cy,iz)%distance(kohonen_map%grid(cx,iy,cz),&
                                    kohonen_map%distance_function);
                  kohonen_map%u_matrix(2*ix,2*iy,2*iz-1)=dist;         
               endif
            enddo
         enddo
      enddo
   !
      do iz=1,size(kohonen_map%u_matrix,3),2
         do iy=1,size(kohonen_map%u_matrix,2),2
            do ix=1,size(kohonen_map%u_matrix,1),2
                  u_temp=0.0_wp;
                  if(ix>1 .and. ix<size(kohonen_map%u_matrix,1) .and. & 
                     iy>1 .and. iy<size(kohonen_map%u_matrix,2)) then
                     u_temp = kohonen_map%u_matrix(ix-1,iy,iz)+kohonen_map%u_matrix(ix+1,iy,iz)+&
                              kohonen_map%u_matrix(ix,iy-1,iz)+kohonen_map%u_matrix(ix,iy+1,iz);
                     nt=4;
                  elseif(iy==1 .and. ix>1 .and. ix<size(kohonen_map%u_matrix,1)) then
                     u_temp = kohonen_map%u_matrix(ix-1,iy,iz)+kohonen_map%u_matrix(ix+1,iy,iz)+&
                              kohonen_map%u_matrix(ix,iy+1,iz);
                     nt=3;
                  elseif(iy==size(kohonen_map%u_matrix,2) .and. ix>1 .and.&
                        ix<size(kohonen_map%u_matrix,1)) then
                     u_temp = kohonen_map%u_matrix(ix-1,iy,iz)+kohonen_map%u_matrix(ix+1,iy,iz)+&
                              kohonen_map%u_matrix(ix,iy-1,iz);
                     nt=3;
                  elseif(ix==1 .and. iy>1 .and. iy<size(kohonen_map%u_matrix,2)) then
                     u_temp = kohonen_map%u_matrix(ix+1,iy,iz)+&
                              kohonen_map%u_matrix(ix,iy-1,iz)+kohonen_map%u_matrix(ix,iy+1,iz);
                     nt=3;
                  elseif(ix==size(kohonen_map%u_matrix,1) .and. iy>1 .and. iy<size(kohonen_map%u_matrix,2)) then
                     u_temp = kohonen_map%u_matrix(ix-1,iy,iz)+&
                              kohonen_map%u_matrix(ix,iy-1,iz)+kohonen_map%u_matrix(ix,iy+1,iz);
                     nt=3;
                  elseif(ix==1 .and. iy==1) then
                     u_temp = kohonen_map%u_matrix(ix+1,iy,iz)+kohonen_map%u_matrix(ix,iy+1,iz);
                     nt=2;
                  elseif( ix==size(kohonen_map%u_matrix,1) .and. iy==1) then
                     u_temp=kohonen_map%u_matrix(ix-1,iy,iz)+kohonen_map%u_matrix(ix,iy+1,iz);
                     nt=2;
                  elseif(ix==1 .and. iy==size(kohonen_map%u_matrix,2)) then
                     u_temp=kohonen_map%u_matrix(ix+1,iy,iz)+kohonen_map%u_matrix(ix,iy-1,iz);
                     nt=2;
                  elseif( ix==size(kohonen_map%u_matrix,1) .and. iy==size(kohonen_map%u_matrix,2)) then
                     u_temp = kohonen_map%u_matrix(ix-1,iy,iz)+kohonen_map%u_matrix(ix,iy-1,iz);
                     nt=2;
                  else
                     u_temp = 0.0_wp;
                  endif
                  kohonen_map%u_matrix(ix,iy,iz)=u_temp/dble(nt);
            enddo
         enddo
      enddo   
   !
      case('hexagonal')
   !   
      do iz=1,size(kohonen_map%grid,3);
         do iy=1,size(kohonen_map%grid,2);
            do ix=1,size(kohonen_map%grid,1);
               if(ix < nx) then !horizontal
                  cx=ix+1;cy=iy;cz=iz;               
                  dist=kohonen_map%grid(ix,iy,iz)%distance(kohonen_map%grid(cx,cy,cz),&
                                    kohonen_map%distance_function);
                  kohonen_map%u_matrix(2*ix,2*iy-1,2*iz-1)=dist;
               endif
               !
               if(iy < ny) then !diagonals
                  cx=ix;cy=iy+1;cz=iz;               
                  dist=kohonen_map%grid(ix,iy,iz)%distance(kohonen_map%grid(cx,cy,cz),&
                                    kohonen_map%distance_function);
                  kohonen_map%u_matrix(2*ix-1,2*iy,2*iz-1)=dist;
                  if(mod(iy,2)==0 .and. ix < nx) then
                     cx=ix+1;cy=iy+1;cz=iz;
                     dist=kohonen_map%grid(ix,iy,iz)%distance(kohonen_map%grid(cx,cy,cz),&
                                    kohonen_map%distance_function);
                     kohonen_map%u_matrix(2*ix,2*iy,2*iz-1)=dist;               
                  elseif(mod(iy,2)==1 .and. ix>1) then
                     cx=ix-1;cy=iy+1;cz=iz;
                     dist=kohonen_map%grid(ix,iy,iz)%distance(kohonen_map%grid(cx,cy,cz),&
                                    kohonen_map%distance_function);
                     kohonen_map%u_matrix(2*ix-2,2*iy,2*iz-1)=dist;
                  endif
               endif
            enddo
         enddo
      enddo
   !
   do iz=1,nzu,2;
      do iy=1,nyu,2;
         do ix=1,nxu,2;
            u_temp=0.0_wp;
            if(ix>1 .and. iy>1 .and. ix<nxu .and. iy<nyu ) then !middle part of the map
               u_temp = kohonen_map%u_matrix(ix-1,iy,iz) + kohonen_map%u_matrix(ix+1,iy,iz);
               if (mod(iy-1,4)==0) then
                  u_temp = u_temp +  kohonen_map%u_matrix(ix-1,iy-1,iz) + kohonen_map%u_matrix(ix,iy-1,iz) + &
                           kohonen_map%u_matrix(ix-1,iy+1,iz)+ kohonen_map%u_matrix(ix,iy+1,iz);                
               else 
                  u_temp = u_temp+ kohonen_map%u_matrix(ix,iy-1,iz)+ kohonen_map%u_matrix(ix+1,iy-1,iz) +&
                           kohonen_map%u_matrix(ix,iy+1,iz) +  kohonen_map%u_matrix(ix+1,iy+1,iz); 
               endif
               nt=6;
            elseif(iy==1 .and. ix>1 .and. ix<nxu ) then ! upper edge
               u_temp = kohonen_map%u_matrix(ix-1,iy,iz)+kohonen_map%u_matrix(ix+1,iy,iz)+&
                        kohonen_map%u_matrix(ix-1,iy+1,iz) + kohonen_map%u_matrix(ix,iy+1,iz);
               nt=4;
            elseif(iy==nyu .and. ix>1 .and. ix<nxu) then ! lower edge
               u_temp = kohonen_map%u_matrix(ix-1,iy,iz)+ kohonen_map%u_matrix(ix+1,iy,iz);
               if (mod(iy-1,4)==0) then
                  u_temp = u_temp + kohonen_map%u_matrix(ix-1,iy-1,iz) + kohonen_map%u_matrix(ix,iy-1,iz);
               else 
                  u_temp = u_temp + kohonen_map%u_matrix(ix,iy-1,iz) + kohonen_map%u_matrix(ix+1,iy-1,iz); 
               endif
               nt=4;
            elseif( ix==1 .and. iy>1 .and. iy<nyu) then ! left edge
               u_temp = kohonen_map%u_matrix(ix+1,iy,iz);
               if(mod(iy-1,4)==0) then
                  u_temp = u_temp + kohonen_map%u_matrix(ix,iy-1,iz)+ kohonen_map%u_matrix(ix,iy+1,iz);
                  nt=3;
               else 
                  u_temp = u_temp + kohonen_map%u_matrix(ix,iy-1,iz) + kohonen_map%u_matrix(ix+1,iy-1,iz) +&
                           kohonen_map%u_matrix(ix,iy+1,iz) + kohonen_map%u_matrix(ix+1,iy+1,iz); 
                  nt=5;
               endif             
            elseif(ix==nxu .and. iy>1 .and. iy<nyu) then ! right edge
               u_temp = kohonen_map%u_matrix(ix-1,iy,iz);
               if (mod(iy-1,4)==0) then
                  u_temp= u_temp + kohonen_map%u_matrix(ix,iy-1,iz) + kohonen_map%u_matrix(ix-1,iy-1,iz) +&
                           kohonen_map%u_matrix(ix,iy+1,iz) + kohonen_map%u_matrix(ix-1,iy+1,iz);
                  nt=5;        
               else 
                  u_temp = u_temp + kohonen_map%u_matrix(ix,iy-1,iz) + kohonen_map%u_matrix(ix,iy+1,iz);
                  nt=3;
               endif
            elseif(ix==1 .and. iy==1) then ! top left corner
               u_temp = kohonen_map%u_matrix(ix+1,iy,iz) + kohonen_map%u_matrix(ix,iy+1,iz);
               nt=2;
            elseif(ix==nxu .and. iy==1) then ! top right corner
               u_temp = kohonen_map%u_matrix(ix-1,iy,iz) +  kohonen_map%u_matrix(ix-1,iy+1,iz) +&
                        kohonen_map%u_matrix(ix,iy+1,iz);
               nt=3;
            elseif(ix==1 .and. iy==nyu) then ! bottom left corner
               if (mod(iy-1,4)==0) then
                  u_temp = kohonen_map%u_matrix(ix+1,iy,iz) + kohonen_map%u_matrix(ix,iy-1,iz);
                  nt=2;
               else 
                  u_temp = kohonen_map%u_matrix(ix+1,iy,iz) + kohonen_map%u_matrix(ix,iy-1,iz) +&
                           kohonen_map%u_matrix(ix+1,iy-1,iz); 
                  nt=3;
               endif;
            elseif(ix==nxu .and. iy==nyu) then ! bottom right corner
               if (mod(iy-1,4)==0) then
                  u_temp = kohonen_map%u_matrix(ix-1,iy,iz) + kohonen_map%u_matrix(ix,iy-1,iz) +&
                           kohonen_map%u_matrix(ix-1,iy-1,iz);
                  nt=3;
               else 
                  u_temp = kohonen_map%u_matrix(ix-1,iy,iz) + kohonen_map%u_matrix(ix,iy-1,iz);
                  nt=2;
               endif           
            endif
            kohonen_map%u_matrix(ix,iy,iz)=u_temp/dble(nt);
         enddo
      enddo
   enddo
   ! 
   end select
   !
   inquire(unit=kohonen_map%parameters(1)%iumat,opened=testop);
   if(testop) then
      do iz=1,size(kohonen_map%u_matrix,3);
         write(kohonen_map%parameters(1)%iumat,'(A,I4)') 'Layer ',iz 
         do ix=1,size(kohonen_map%u_matrix,1);
            write(kohonen_map%parameters(1)%iumat,'(100f10.5)') (kohonen_map%u_matrix(ix,iy,iz),&
                  iy=1,size(kohonen_map%u_matrix,2));
         enddo
      enddo
   endif
   
   !
   end subroutine calculate_u_matrix
   !======================================================================================== 
   subroutine assign_input_to_clusters(kohonen_map,input_patterns)
   !========================================================================================
!!    Subroutine to assign input to clusters
   class(two_level_self_organizing_map) :: kohonen_map
!! A `two_level_self_organizing_map` object
   type(kohonen_pattern),dimension(:),intent(inout) :: input_patterns
!! A `kohonen_pattern` array
   type(kohonen_prototype) :: current_prototype,current_prototype1
   integer :: ipattern,ic,i_hit,current_pos
   real(kind=wp) :: dist_min,dist
   !
   ! write(*,*) 'assign= ',size(input_patterns)
   do ipattern=1,kohonen_map%parameters(1)%number_patterns
      call input_patterns(ipattern)%get(current_prototype);
   !    write(*,*) 'ipat= ',ipattern
      dist_min=1.0d5;
      do ic=1,size(kohonen_map%cluster_layer)
         current_prototype1=kohonen_map%cluster_layer(ic)
         dist=current_prototype1%distance(current_prototype,kohonen_map%distance_function);
         if(dist < dist_min) then
            i_hit=ic;
            dist_min=dist;
   !         write(*,*) 'ic= ',ipattern,i_hit
         endif
      enddo
      kohonen_map%number_cluster_samples(i_hit)=kohonen_map%number_cluster_samples(i_hit)+1;
      current_pos=kohonen_map%number_cluster_samples(i_hit);
   !    write(*,*)' ic,pos= ',ic,i_hit,current_pos,allocated(kohonen_map%number_cluster_samples),&
   !                         size(kohonen_map%number_cluster_samples)
   !    write(*,*) kohonen_map%number_cluster_samples                    
      kohonen_map%index_cluster_samples(i_hit,current_pos)=ipattern;
   enddo!ipattern1
   !
   !  do ic=1,size(kohonen_map%cluster_layer)
   !     write(*,*) ic,kohonen_map%number_cluster_samples(ic),(kohonen_map%index_cluster_samples(ic,1:5))
   !  enddo
   !
   end subroutine assign_input_to_clusters
   !========================================================================================
   subroutine external_train_map(x,nvar,npat,som_type,nx1,ny1,nepoch1,alpha1,grid_type1,&
               distance_type1,neigh_type1,toroidal1,nx2,nepoch2,alpha2,grid_type2,&
               prot,distortion,u_matrix,coords,number_patterns,&
               node_index) bind(C, name="train_2lsom")
   !========================================================================================
!!    Subroutine to connect the two_level_self_organizing_map module to R o C
   use, intrinsic :: iso_c_binding, only : c_double, c_int, c_char
!! Use iso_c_binding module
   real(kind=wp),parameter :: version=0.1_wp
!! Subroutine Version
   character(len=*),parameter :: program_name="2lsom_train"
!! Subroutine name
   integer(c_int), intent(in) :: nvar,npat,som_type,nx1,ny1,nepoch1,toroidal1
!! Integere variables
   real(c_double),intent(out) :: prot(nx1*ny1,nvar),distortion(nepoch1)
!! Real variables
   real(c_double),intent(out) :: u_matrix(2*nx1-1,2*ny1-1),coords(nx1*ny1,3)
!! Real variables
   integer(c_int),intent(out) :: number_patterns(nx1,ny1),node_index(npat,3)
!! Integer variables
   real(c_double),intent(in) :: x(npat,nvar)
!! Real variables
   real(c_double),intent(in) :: alpha1,alpha2
!! Real variables
   integer(c_int),intent(in) :: grid_type1,distance_type1,neigh_type1
!! Integer variables
   integer(c_int),intent(in) :: nx2,grid_type2,nepoch2 !,distance_type1,neigh_type2
!! Integer variables
   type(two_level_self_organizing_map) :: my_som
   type(kohonen_layer_parameters),dimension(1) :: parameters
   real(kind=wp),dimension(nvar,1) :: var
   integer :: i,j,k,ierr,pos,ihit,jhit,khit,nx1a,ny1a
   type(kohonen_pattern),allocatable :: input_patterns(:)
   real(kind=wp),dimension(nx1*ny1,nvar) :: prototypes
   real(kind=wp),dimension(nvar,1) :: temp
   !
   parameters(1)%train_option=3;
   parameters(1)%number_nodes_nx=nx1;
   parameters(1)%number_nodes_ny=ny1;
   parameters(1)%number_nodes_nz=1;
   parameters(1)%number_variables1=nvar;
   parameters(1)%number_variables2=1;
   parameters(1)%number_variables=nvar;
   parameters(1)%number_patterns=npat;
   parameters(1)%number_epochs=nepoch1;
   parameters(1)%learning_rate=alpha1;
   parameters(1)%random_seed_=12345;
   !
   select case(grid_type1)
      case(0)
      parameters(1)%node_type="rectangular";
      case(1)
      parameters(1)%node_type="hexagonal"; 
   end select
   !  
   parameters(1)%debug_level=0;
   parameters(1)%debug_file="NOFILE";
   parameters(1)%pattern_file="NOFILE";
   parameters(1)%output_file="NOFILE";
   parameters(1)%distance_type="euclidean"; !"euclidean" !euclidean, manhattan, correlation, correlation2
   !
   select case(neigh_type1)
      case(0)
      parameters(1)%neighborhood_type="bubble";
      case(1)
      parameters(1)%neighborhood_type="gaussian";
   end select  
   !
   select case(som_type)
      case(0)
      parameters(1)%som_type="normal_som";!,visom, robust_som
      case(1)
      parameters(1)%som_type="visom";
      case(2)
      parameters(1)%som_type="robust_som";
   end select
   !      
   if(toroidal1 == 1) then
      parameters(1)%toroidal_grid=.TRUE.;
   else
      parameters(1)%toroidal_grid=.FALSE.;
   endif
   ! 
   end subroutine external_train_map
   ! 
   end module two_level_self_organizing_map_utilities