!! author: Oscar Garcia-Cabrejo
!! date: 12/04/2024
!! version: 0.1
!!  This module defines a class for simple self_organizing_map (one kohonen layer) 
module self_organizing_map_utilities
!!  This module defines a class for simple self_organizing_map (one kohonen layer) 
use error_handling, only: error_t,error_stop;
use precision_utilities, only: wp;
use constants_utilities, only: NUMCHAR;
use random_generator_base_utilities, only: random_generator_base;
use rkiss05_generator_utilities, only: rkiss05_generator;
use kohonen_layer_parameters_utilities, only: kohonen_layer_parameters;
use kohonen_map_base_utilities, only: kohonen_map_base;
use kohonen_prototype_utilities, only: kohonen_prototype;
use kohonen_pattern_utilities, only: kohonen_pattern;
use distance_base_utilities, only: distance_base;
use factory_distance_utilities, only: factory_distance;
!use influence_function_utilities;
use quicksort_utilities, only: quicksort;
!
implicit none;
!
type,extends(kohonen_map_base) :: self_organizing_map
!!   Class to represent a self_organizing_map
    private
        character(len=NUMCHAR) :: class_name='self_organizing_map';
        type(kohonen_prototype),allocatable :: grid(:,:,:)
        integer,allocatable :: number_patterns(:,:,:),cells_index(:,:)
        real(kind=wp),allocatable :: u_matrix(:,:,:),distance(:,:)
        real(kind=wp),allocatable :: cells_distances(:,:),coordinates(:,:)
        type(kohonen_layer_parameters) :: parameters
        type(factory_distance) :: factory
        class(distance_base),allocatable :: distance_function
        real(kind=wp),allocatable :: distortion(:)
        type(rkiss05_generator) :: rnumber_grator
        integer :: seed  
        integer,allocatable :: grid_pattern_index(:,:,:),list_node_grid(:,:,:,:)
    contains
        procedure,public :: create => create_som
        procedure,public :: destroy => destroy_som
        procedure,private :: train_som_data
        procedure,public :: train => train_som_data 
        procedure,public :: predict => predict_som
        procedure,public :: print => print_som
        procedure,public :: read => read_som
        procedure,public :: get_count => get_count_som
        procedure,public :: query => query_som
        procedure,public :: get_prototypes
        !procedure,public :: get_index => get_index_som
        procedure,public :: get_u_matrix => get_u_matrix_som
        procedure,private :: find_best_match_unit
        procedure,private :: update_weights
        !procedure,private :: update_weights1
        procedure,private :: find_bmu_grid
        procedure,private :: calculate_u_matrix
        procedure,private :: calculate_u_matrix_hexagonal
        procedure,private :: calculate_u_matrix_rectangular
        procedure,private :: calculate_sigma
        procedure,nopass,private :: position2index
        procedure,nopass,private :: index2position
        procedure,nopass,private :: calculate_distance_matrix
        procedure,nopass,private :: calculate_coordinates
        procedure,private :: calculate_distance_between_prototypes
        procedure,nopass,public :: external_train_map
        procedure,nopass,public :: external_predict_map
!
end type self_organizing_map
!
contains
!========================================================================================
    subroutine create_som(kohonen_map,training_parameters)
!========================================================================================
!!   Constructor for self_organizing_map 
        character(len=NUMCHAR),parameter :: fname = 'create_som'
!! A character variable with the name of the function
        class(self_organizing_map) :: kohonen_map
!! A  `self_organizing_map` object
        type(kohonen_layer_parameters),dimension(:) :: training_parameters
!! A `kohonen_layer_parameters` object
        integer :: ierr,nx,ny,nz,ix,iy,iz,nvar1,nvar2,seed,current_index,nepoch
        integer :: i,j
        real(kind=wp),allocatable :: input(:,:)
        character(len=NUMCHAR) :: base_message,message
!
        base_message=trim(kohonen_map%class_name)//'_'//trim(fname)//'_ERROR';
!
        kohonen_map%parameters=training_parameters(1);
        nx=training_parameters(1)%number_nodes_nx;
        ny=training_parameters(1)%number_nodes_ny;
        nz=training_parameters(1)%number_nodes_nz;
        nvar1=training_parameters(1)%number_variables1;
        nvar2=training_parameters(1)%number_variables2;
        nepoch=training_parameters(1)%number_epochs;
        write(*,*) 'Create= ',nx,ny,nz,nvar1,nvar2,nepoch;
        allocate(kohonen_map%grid(nx,ny,nz),stat=ierr);
        if(ierr /= 0) then
            message = trim(base_message)//'_allocating memory for grid array';
            call error_stop(message);
        endif
!
        allocate(kohonen_map%grid_pattern_index(nx,ny,nz),stat=ierr);
        if(ierr /= 0) then
            message = trim(base_message)//'_allocating memory for grid_pattern_index array';
            call error_stop(message);
        endif
!
        allocate(input(nvar1,nvar2),stat=ierr);
        if(ierr /= 0) then
            message = trim(base_message)//'_allocating memory for input array';
            call error_stop(message);
        endif
!
        allocate(kohonen_map%number_patterns(nx,ny,nz),stat=ierr);
        if(ierr /= 0) then
            message = trim(base_message)//'_allocating memory for number_patterns array';
            call error_stop(message);
        endif
!
        allocate(kohonen_map%cells_index(training_parameters(1)%number_patterns,3),stat=ierr);
        if(ierr /= 0) then
            message = trim(base_message)//'_allocating memory for cell_index array';
            call error_stop(message);
        endif
!
        kohonen_map%number_patterns=0;
        kohonen_map%cells_index=0;
        allocate(kohonen_map%u_matrix(2*nx-1,2*ny-1,2*nz-1),stat=ierr);
        if(ierr /= 0) then
            message = trim(base_message)//'_allocating memory for u_matrix array';
            call error_stop(message);
        endif
        kohonen_map%u_matrix=0.0_wp;
!
        allocate(kohonen_map%distance(nx*ny,nx*ny),stat=ierr);
        if(ierr /= 0) then
            message = trim(base_message)//'_allocating memory for distance array';
            call error_stop(message);
        endif
        kohonen_map%distance=0.0_wp;
!
        allocate(kohonen_map%cells_distances(nx*ny*nz,nx*ny*nz),stat=ierr);
        kohonen_map%cells_distances=0.0d0;
        allocate(kohonen_map%coordinates(nx*ny*nz,3),stat=ierr);
        kohonen_map%coordinates=0.0d0;
        allocate(kohonen_map%distortion(nepoch),stat=ierr);
        kohonen_map%distortion=0.0d0;
!
        call kohonen_map%factory%create_distance(training_parameters(1)%distance_type,&
            kohonen_map%distance_function);
!
        kohonen_map%seed=training_parameters(1)%random_seed_(1);
        call kohonen_map%rnumber_grator%create(kohonen_map%seed);
        do i=1,nvar1;
            do j=1,nvar2;
                input(i,j)=kohonen_map%rnumber_grator%generate();
                !write(*,*) 'input= ',input(i,j);
            enddo
        enddo
!   
    write(*,*) 'SOM: Initializing grid...',kohonen_map%seed;
        do iz=1,nz;
            do iy=1,ny;
                do ix=1,nx;
                    !write(*,*) 'creating ',ix,iy,iz
                    call kohonen_map%grid(ix,iy,iz)%create(input); 
                        current_index=position2index(ix,iy,iz,nx,ny);
                    call calculate_coordinates(current_index,ix,iy,iz,nx,ny,nz,&
                        kohonen_map%coordinates,training_parameters(1)%node_type);
                enddo!ix
            enddo !iy
         enddo !iz
         deallocate(input);
   !
         call calculate_distance_matrix(kohonen_map%coordinates,kohonen_map%cells_distances,&
             training_parameters(1)%node_type,training_parameters(1)%toroidal_grid);
    write(*,*) 'SOM: Initializing grid...OK';
!
    end subroutine create_som
!========================================================================================
    subroutine destroy_som(kohonen_map)
!========================================================================================
!!   Destructor for self_organizing_map 
        class(self_organizing_map) :: kohonen_map
!! A `self_organizing_map` object
   
        integer :: ix,iy,iz
!
!       write(*,*) 'SOM: Releasing memory...'
        if(allocated(kohonen_map%grid)) then
            do iz=1,size(kohonen_map%grid,3)
                do iy=1,size(kohonen_map%grid,2)
                    do ix=1,size(kohonen_map%grid,1);
                        call kohonen_map%grid(ix,iy,iz)%destroy();
                    enddo
                enddo
            enddo
            deallocate(kohonen_map%grid);
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
         if(allocated(kohonen_map%distance_function)) then
             deallocate(kohonen_map%distance_function);
         endif
!
         if(allocated(kohonen_map%distance)) then
             deallocate(kohonen_map%distance);
         endif
!
         if(allocated(kohonen_map%cells_distances)) then
             deallocate(kohonen_map%cells_distances);
         endif
!
         if(allocated(kohonen_map%coordinates)) then
             deallocate(kohonen_map%coordinates);
         endif
!
         if(allocated(kohonen_map%distortion)) then
             deallocate(kohonen_map%distortion)
         endif
   !
         if(allocated(kohonen_map%grid_pattern_index)) then
             deallocate(kohonen_map%grid_pattern_index);
         endif
   !
         if(allocated(kohonen_map%list_node_grid)) then
             deallocate(kohonen_map%list_node_grid);
         endif
         call kohonen_map%rnumber_grator%destroy();
!
!        write(*,*) 'SOM: Releasing memory...OK!'
!
    end subroutine destroy_som
!****f* self_organizing_map_utilities/train_som_data
! NAME
!   train_som_data
! PURPOSE
!!   Training function for self_organizing_map 
! SYNOPSIS
!========================================================================================
   subroutine train_som_data(kohonen_map,input_data)
!========================================================================================
!!   Training function for self_organizing_map 
      class(self_organizing_map) :: kohonen_map
!! A `self_organizing_map` object
      type(kohonen_pattern),dimension(:),intent(inout) :: input_data
!! A `kohonen_pattern` array with the input data
      integer :: iteration,iepoch,ipattern,ix,iy,iz,jhit,ihit,khit,ineigh,jneigh
      integer :: kneigh,idbg,number_variables,idisto !neff,
      integer :: cx,cy,cz,i,j,k,number_nodes,debug_option,ix1,iy1,iz1,pos,pos1,max_pattern
      integer :: ierr,nx,ny,nz,ipos
      integer :: current_pos,ic,itemp
      real(kind=wp) :: distortion,dist,dist_hit,maximum_radius,minimum_radius
      real(kind=wp) :: current_radius,alpha,u_temp
      type(kohonen_prototype) :: current_prototype
      real(kind=wp),dimension(kohonen_map%parameters%number_variables1,&
      kohonen_map%parameters%number_variables2) :: current_values
      integer,allocatable :: pattern_index(:,:,:,:),positions(:)
!
! 
!
      nx=kohonen_map%parameters%number_nodes_nx;
      ny=kohonen_map%parameters%number_nodes_ny;
      nz=kohonen_map%parameters%number_nodes_nz;
      allocate(positions(nx*ny*nz),stat=ierr);
      idbg=kohonen_map%parameters%idbg;
      idisto=kohonen_map%parameters%idisto;
      debug_option=kohonen_map%parameters%debug_level;
      if(debug_option > 0) then
      open(idbg,file=trim(kohonen_map%parameters%debug_file),status='unknown');
      endif
      iteration = 0;
      distortion = 0.0_wp;
      number_variables=kohonen_map%parameters%number_variables1*kohonen_map%parameters%number_variables2;
      maximum_radius=dble(max(kohonen_map%parameters%number_nodes_nx,kohonen_map%parameters%number_nodes_ny));
      minimum_radius=1.0_wp;
      write(*,*) 'SOM: Training starting...'
      do iepoch = 1,kohonen_map%parameters%number_epochs;
         kohonen_map%distortion(iepoch)=distortion;
         write(6,*) ' Starting epoch -- distortion',iepoch,' -- ',distortion;
         write(idisto,*) iepoch,distortion
         distortion = 0.0_wp;
         do ipattern = 1, kohonen_map%parameters%number_patterns;
            iteration = iteration + 1;
            ihit = 0;
            jhit = 0;
            khit = 0;
            dist_hit = 100000.0_wp;
            call input_data(ipattern)%get(current_prototype);
            call current_prototype%get_prototype(current_values);
            call kohonen_map%find_best_match_unit(current_prototype,ihit,jhit,khit,dist_hit);
            !write(*,*) 'Test= ',ipattern,ihit,jhit,khit,dist_hit
            if(debug_option > 0) then
               write(idbg,*) 'Epoch,Current Pattern',iepoch,ipattern;
               call current_prototype%print(idbg);
            endif            
            distortion = distortion + dist_hit;
            if(debug_option > 0) then
               write(idbg,*) 'Neighborhood,alpha= ',alpha;
            endif
            call kohonen_map%update_weights(current_values,ihit,jhit,khit,maximum_radius,iteration);
      !   
         enddo !ipattern
      enddo!iepoch
      !       write(*,*) 'SOM: Training finished'
      !       write(*,*) 'Total number of iterations= ',iteration
      !     print prototypes
      ! if(kohonen_map%parameters%train_option < 3) then
      ! do iz=1,size(kohonen_map%grid,3)
      !    !write(kohonen_map%parameters%iprot,'(A,I4)') 'Layer ',iz
      !    do iy=1,size(kohonen_map%grid,2);
      !       do ix=1,size(kohonen_map%grid,1);
      !          !write(kohonen_map%parameters%iprot,'(A6,1X,3I4)') 'node= ',ix,iy,iz            
      !          call kohonen_map%grid(ix,iy,iz)%print(kohonen_map%parameters%iprot);
      !       enddo
      !    enddo
      ! enddo!ix
      ! endif
      !     calculate and print distance matrix
      call kohonen_map%calculate_distance_between_prototypes();
      !     final best match
      !      call kohonen_map%find_bmu_grid(input_data);
      max_pattern=0;         
      do ipattern = 1, kohonen_map%parameters%number_patterns
         ihit = 0;
         jhit = 0;
         khit = 0;
         dist_hit = 100000.0_wp;
         call input_data(ipattern)%get(current_prototype);
         !call current_prototype%get_prototype(current_values);
         call kohonen_map%find_best_match_unit(current_prototype,ihit,jhit,khit,dist_hit);
         kohonen_map%number_patterns(ihit,jhit,khit)=kohonen_map%number_patterns(ihit,jhit,khit)+1;
         if(kohonen_map%number_patterns(ihit,jhit,khit) > max_pattern) then 
               max_pattern=kohonen_map%number_patterns(ihit,jhit,khit);
         endif
         kohonen_map%cells_index(ipattern,1)=ihit;
         kohonen_map%cells_index(ipattern,2)=jhit;
         kohonen_map%cells_index(ipattern,3)=khit;
         if(debug_option > 0) then
            write(idbg,*) ipattern,ihit,jhit,khit;
         endif
         !if(kohonen_map%parameters%train_option < 3) then
         !   write(kohonen_map%parameters%iindex,*) ipattern,ihit,jhit,khit
         !endif
         !         write(*,*) 'BMU= ',ipattern,ihit,jhit,khit,dist_hit
         !
      enddo !ipattern
      !
      allocate(pattern_index(size(kohonen_map%grid,1),&
         size(kohonen_map%grid,2),size(kohonen_map%grid,3),&
         max_pattern),stat=ierr);
      pattern_index=-1;         
      do ipattern=1,kohonen_map%parameters%number_patterns
         ix=kohonen_map%cells_index(ipattern,1);
         iy=kohonen_map%cells_index(ipattern,2);
         iz=kohonen_map%cells_index(ipattern,3);
         do i=1,max_pattern;
            if(pattern_index(ix,iy,iz,i) < 0) then
               pattern_index(ix,iy,iz,i)=ipattern;
               exit;
            endif
         enddo
      enddo!ipattern
      if(kohonen_map%parameters%train_option < 3) then
         do iz1=1,size(kohonen_map%grid,3);
            do iy1=1,size(kohonen_map%grid,2);
               do ix1=1,size(kohonen_map%grid,1);
                  write(kohonen_map%parameters%isam,'(A,3I4)') 'Node= ',ix1,iy1,iz1
                  if(kohonen_map%number_patterns(ix1,iy1,iz1) > 0) then
                     write(kohonen_map%parameters%isam,'(A,10000I5)') 'Sample ID= ',&
                     pattern_index(ix1,iy1,iz1,1:kohonen_map%number_patterns(ix1,iy1,iz1));
                  else
                     write(kohonen_map%parameters%isam,'(A,I4)') 'Sample ID= ',0
                  endif
               enddo
            enddo
         enddo
         deallocate(pattern_index);
      endif
      !
        if(debug_option .gt. 0) then 
            close(idbg);
        endif
        close(idisto);
    
      !     print hit counter
      if(kohonen_map%parameters%train_option < 3) then
         do iz=1,size(kohonen_map%grid,3)
            do ix=1,size(kohonen_map%grid,1);
               write(kohonen_map%parameters%ihit,'(100I5)') (kohonen_map%number_patterns(ix,iy,iz),&
                  iy=1,size(kohonen_map%grid,2));
            enddo!ix
         enddo
      endif
      call kohonen_map%calculate_u_matrix();
!
   end subroutine train_som_data
!========================================================================================
    subroutine predict_som(kohonen_map,input_data,map_output)
!========================================================================================
!! Function for Prediction of a self_organizing_map 
        class(self_organizing_map) :: kohonen_map
!! A `self_organizing_map` object
        type(kohonen_pattern),dimension(:),intent(inout) :: input_data
!! A `kohonen_pattern` array with the input data        
        integer,dimension(:,:),intent(out) :: map_output
!! An integer array with the map output
        integer :: ipattern,ihit,jhit,khit,ix,iy,iz,number_variables,i,j,k
        real(kind=wp) :: dist_hit,dist
        type(kohonen_prototype) :: current_prototype
        real(kind=wp),dimension(kohonen_map%parameters%number_variables1,&
        kohonen_map%parameters%number_variables2) :: current_values
!
        number_variables=kohonen_map%parameters%number_variables1*&
                        kohonen_map%parameters%number_variables2;
!
!       write(*,*) 'SOM: Prediction starting...';
!       write(*,*) number_variables
        do ipattern = 1, size(input_data,1)
            ihit = 0;
            jhit = 0;
            dist_hit = 100000.0_wp;
            call input_data(ipattern)%get(current_prototype);
            !call current_prototype%print();
            !write(*,*) ihit,jhit,dist_hit
            !call current_prototype%get_prototype(current_values);
            !$OMP parallel do         
            do iz=1,size(kohonen_map%grid,3)
                do iy = 1, size(kohonen_map%grid,2);  
                    do ix = 1, size(kohonen_map%grid,1);
                        dist = 0.0_wp;
                        !write(*,*) ix,iy,dist
                        !call kohonen_map%grid(ix,iy)%print();
                        dist=kohonen_map%grid(ix,iy,iz)%distance(current_prototype,&
                            kohonen_map%distance_function);
                        dist = dist/float(number_variables);
                        if (dist < dist_hit) then
                            dist_hit = dist;
                            ihit = ix;
                            jhit = iy;
                            khit = iz;
                        endif
                     enddo
                enddo
            enddo
            !         
            !$OMP end parallel do
            !         
            call kohonen_map%grid(ihit,jhit,khit)%get_prototype(current_values);
            ! if(size(current_values,2) .eq. 1) then 
            !   write(kohonen_map%parameters%iout,*) (current_values(i,1),&
            !         i=1,size(current_values,1));
            ! else
            !   do i=1,size(current_values,1)
            !      write(kohonen_map%parameters%iout,*) (current_values(i,j),j=1,&
            !            size(current_values,2))
            !   enddo
            ! endif
            !call map_output(ipattern)%create(current_values);
            map_output(ipattern,1)=ihit;
            map_output(ipattern,2)=jhit;
            map_output(ipattern,3)=khit;
            !size(current_values,1),size(current_values,2)
            !write(*,*) current_values
        enddo !ipattern
!       write(*,*) 'SOM: Prediction finished';
!
    end subroutine predict_som
!========================================================================================
    subroutine print_som(kohonen_map,unit_)
!========================================================================================
!!   Print function for self_organizing_map 
        class(self_organizing_map) :: kohonen_map
!!
        integer,intent(inout),optional :: unit_
!!
        integer :: ix,iy,iz,unit1
!
        if(.not. present(unit_)) then 
            unit1=6;
        else
            unit1=unit_;
        endif
        write(unit1,*) 'SOM: Results';
        write(unit1,*)
        call kohonen_map%parameters%print(unit1);
        ! write(unit1,*) 'After'
        write(unit1,*)
        write(unit1,*) 'SOM: Grid nodes';
        write(unit1,*)
        do iz=1,size(kohonen_map%grid,3)
            do iy=1,size(kohonen_map%grid,2);
                do ix=1,size(kohonen_map%grid,1);
                    call kohonen_map%grid(ix,iy,iz)%print(unit1);
                enddo
            enddo!iy
        enddo!ix
        write(unit1,*)
        write(unit1,*) 'SOM: Hit count';
        write(unit1,*)
        write(unit1,*) 'Pattern Numbers';
        do iz=1,size(kohonen_map%number_patterns,3);
            do ix=1,size(kohonen_map%number_patterns,1);
                write(unit1,'(100I5)') (kohonen_map%number_patterns(ix,iy,iz),iy=1,&
                   size(kohonen_map%number_patterns,2));
            enddo
        enddo
        write(unit1,*)
        write(*,*) 'SOM: Pattern index'
        write(unit1,*)
        write(unit1,*)
        write(unit1,*) 'Pattern #, ix   ,iy';

        do ix=1,size(kohonen_map%cells_index,1);
            write(unit1,'(100I5)') ix, (kohonen_map%cells_index(ix,iy),&
                iy=1,size(kohonen_map%cells_index,2));
        enddo
!
    end subroutine print_som
!========================================================================================
    subroutine get_count_som(kohonen_map,count_)
!========================================================================================
!!   Function to get count matrix for self_organizing_map 
        class(self_organizing_map) :: kohonen_map
!!
        integer,dimension(:,:,:),intent(inout) :: count_
!!
        count_=kohonen_map%number_patterns;
!   
    end subroutine get_count_som
!========================================================================================
    subroutine query_som(kohonen_map,input_pattern,sample_index) !,output_patterns)
!========================================================================================
!!   Function to find the input samples associated with specific vector 
        class(self_organizing_map) :: kohonen_map
!!
        real(kind=wp),dimension(:,:),intent(inout) :: input_pattern
!!
        integer,allocatable :: sample_index(:)
!!
        integer :: ix,iy,iz,ihit,jhit,khit,ivar1,ivar2,nvar1,nvar2,number_patterns,ipat,ierr
        integer :: number_selected,i,pos
        real(kind=wp),dimension(kohonen_map%parameters%number_variables1,&
        kohonen_map%parameters%number_variables2) ::current_values
        real(kind=wp) :: dist,dist_min
        integer,dimension(size(kohonen_map%cells_index,1)) :: position,real_position
!
!(real_position(ix)=ix,ix=1,size(real_position))
        do ix=1,size(real_position)
            real_position(ix)=ix;
        enddo
        nvar1=kohonen_map%parameters%number_variables1;
        nvar2=kohonen_map%parameters%number_variables2;
        dist_min=1.0d10;
        !$OMP parallel do   
        do iz=1,size(kohonen_map%grid,3);
             do iy=1,size(kohonen_map%grid,2);
                 do ix=1,size(kohonen_map%grid,1);
                     dist=0.0_wp;
                     call kohonen_map%grid(ix,iy,iz)%get_prototype(current_values);
                     do ivar1=1,nvar1;
                         do ivar2=1,nvar2;
                             if(input_pattern(ivar1,ivar2) > 0.0_wp) then
                                 dist=dist+(input_pattern(ivar1,ivar2)-current_values(ivar1,ivar2))**2;
                             endif
                         enddo
                     enddo
                     if(dist < dist_min) then
                         dist_min=dist;
                         ihit=ix;jhit=iy;khit=iz;               
                     endif
                 enddo
             enddo
         enddo
         !$OMP end parallel do
!         write(*,*) 'BMU'
!         write(*,*) ihit,jhit,khit,dist_min
!
         position=0;
         number_patterns=kohonen_map%number_patterns(ihit,jhit,khit);
         if(number_patterns > 0) then
             where(kohonen_map%cells_index(:,1) == ihit .and. &
                 kohonen_map%cells_index(:,2) == jhit .and. &
                 kohonen_map%cells_index(:,3) == khit )
                 position=1;!real_position;
             end where
             number_selected=sum(position);
             pos=0
             if(number_selected > 0) then
                 allocate(sample_index(number_selected),stat=ierr);
                 do i=1,size(real_position)
                     if(position(i) == 1) then
                         pos=pos+1;
                         sample_index(pos)=real_position(i);
                         !write(*,*) 'Inside= ',i,real_position(i)
                     endif
                 enddo
             endif
             !write(*,*) kohonen_map%cells_index(118,1:3)
         else 
             write(*,*) 'WARNING: Query has returned an empty result'
            return
         endif
!
    end subroutine query_som
!========================================================================================
    subroutine read_som(kohonen_map,som_fl)
!========================================================================================
!! Subroutine to read the prototypes to define a self_organizing_map 
        class(self_organizing_map) :: kohonen_map
!! A `self_organizing_map` object      
        character(len=*) :: som_fl
!! A character variable with the name of the file
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
        read(isom,'(A25,1X,A11,1X,L4)') current_line,node_type,toroidal_grid
        write(*,*) current_line,node_type,toroidal_grid
        allocate(Prototype_value(nvar1*nvar2,1),stat=ierr);
   !
        if(allocated(kohonen_map%grid)) then
            do iz=1,nz
                do iy=1,ny
                    do ix=1,nx
                        call kohonen_map%grid(ix,iy,iz)%destroy();
                    enddo
                enddo
            enddo
            deallocate(kohonen_map%grid);
        endif
        if(allocated(kohonen_map%coordinates)) then
            deallocate(kohonen_map%coordinates);
        endif
        allocate(kohonen_map%grid(nx,ny,nz),stat=ierr);
        allocate(kohonen_map%coordinates(nx*ny*nz,3),stat=ierr);
        allocate(kohonen_map%cells_distances(nx*ny*nz,nx*ny*nz),stat=ierr);
        do iz=1,nz
            read(isom,'(A)') current_line;
            write(*,*) 'Reading ',trim(current_line);
            do iy=1,ny
                do ix=1,nx;
                    read(isom,'(A)') current_line;
!                   write(*,*) current_line
                    read(isom,'(A)') current_line;
!                   write(*,*) current_line
                    read(isom,*) (Prototype_value(ivar,1),ivar=1,nvar1*nvar2);
                    !write(*,*) ix,iy,(Prototype_value(ivar,1),ivar=1,nvar1*nvar2)
                    call kohonen_map%grid(ix,iy,iz)%set_prototype(Prototype_value);
                    current_index=position2index(ix,iy,iz,nx,ny);
                    call calculate_coordinates(current_index,ix,iy,iz,nx,ny,nz,&
                           kohonen_map%coordinates,node_type);
                enddo
            enddo
         enddo
         close(isom)
         !write(*,*) 'Reading done'
         !
         call calculate_distance_matrix(kohonen_map%coordinates,kohonen_map%cells_distances,&
               node_type,toroidal_grid);   
   !
         write(*,*)
         write(*,*) 'SOM: Reading SOM Prototypes...finished'
         write(*,*)
!
     end subroutine read_som
!========================================================================================
    function position2index(ix,iy,iz,nx,ny) result(index_)
!========================================================================================
!! Function to calculate the index inside a rectangular grid from position ix,iy,iz
        integer,intent(in) :: ix,iy,iz,nx,ny
!! Integer variables
        integer ::index_
!! Integer variable with the required index
        index_=ix+(iy-1)*nx+(iz-1)*nx*ny;
!
    end function position2index
!========================================================================================
    subroutine index2position(index_,nx,ny,nz,cx,cy,cz)
!========================================================================================
!! Subroutine to calculate the position ix,iy,iz inside a rectangular grid from index
        integer,intent(in) :: index_
!! Integer variable representing the index
        integer,intent(in) :: nx,ny,nz
!! Integer variables representing the dimensions of the kohonen map
        integer,intent(inout) :: cx,cy,cz
!! Integer variables representing the position of the node
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
!! Real array with the coordinates
        real(kind=wp),dimension(:,:),intent(inout) :: distance_matrix
!! Real array with the distance_matrix
        character(len=*) :: grid_type
!! Character variable with the grid type
        logical :: toroidal
!! Logical variable for toroidal grid
        integer :: i,j
        real(kind=wp) :: maxdiffx,maxdiffy,maxdiffz
        real(kind=wp),dimension(3) :: diffs
!
        maxdiffx=maxval(coordinates(:,1))/2.0_wp;
        maxdiffy=maxval(coordinates(:,2))/2.0_wp;
        maxdiffz=maxval(coordinates(:,3))/2.0_wp;
!
        distance_matrix=0.0d0;
!
        if(toroidal) then
            do i=1,size(distance_matrix,1);
                do j=i+1,size(distance_matrix,2);
                    diffs=dabs(coordinates(j,1:3) - coordinates(i,1:3));
                    if (diffs(1) > maxdiffx) diffs(1)=2.0_wp*maxdiffx - diffs(1);
                    if (diffs(2) > maxdiffy) diffs(2)=2.0_wp*maxdiffy - diffs(2);
                    !if (diffs(3) > maxdiffy) diffs(3)=2*maxdiffz - diffs(3);
                    if (trim(grid_type) == 'hexagonal') then
                        distance_matrix(i,j)=sum(diffs**2);
                    elseif(trim(grid_type) == 'rectangular') then!rectangular
                       distance_matrix(i,j)=maxval(diffs);
                    endif
                    !write(*,*) 'd= ',i,j,diffs(1:3),trim(grid_type)!distance_matrix(i,j)
                enddo
            enddo
        else
            do i=1,size(distance_matrix,1);
                do j=i+1,size(distance_matrix,2);
                   diffs=dabs(coordinates(j,1:3) - coordinates(i,1:3));
                   distance_matrix(i,j)=dsqrt(sum(diffs**2));
                enddo
            enddo
        endif
      !
        distance_matrix=distance_matrix + transpose(distance_matrix);
!
    end subroutine calculate_distance_matrix
!========================================================================================
    subroutine calculate_coordinates(current_index,ix,iy,iz,nx,ny,nz,coordinates,node_type)
!========================================================================================
!!  Subroutine to calculate the coordinates of the units inside a kohonen layer 
        integer,intent(in) :: current_index,ix,iy,iz,nx,ny,nz
!!
        real(kind=wp),dimension(:,:),intent(out) :: coordinates
!!
        character(len=*),intent(in) :: node_type
!!
        coordinates(current_index,1)=dble(ix);
        coordinates(current_index,2)=dble(iy);
        coordinates(current_index,3)=dble(iz);
        !write(*,*) coordinates(current_index,1:3);
        if(trim(node_type) == 'hexagonal') then
            coordinates(current_index,1)=coordinates(current_index,1)+&
                  .5_wp*(mod(coordinates(current_index,2),2.0_wp));
            coordinates(current_index,2)=(dsqrt(3.0_wp)/2.0_wp)*coordinates(current_index,2);
        endif
!
    end subroutine calculate_coordinates
!========================================================================================
    subroutine find_best_match_unit(kohonen_map,current_prototype,ihit,jhit,khit,dist_hit)
!========================================================================================
!! Subroutine to calculate the best match unit
        class(self_organizing_map) :: kohonen_map
!! A `self_organizing_map` object
        type(kohonen_prototype),intent(inout) :: current_prototype
!! A `kohonen_prototype` object
        integer,intent(out) :: ihit,jhit,khit
!! Integer variables for the coordinates of the BMU
        real(kind=wp),intent(out) :: dist_hit
!! Real variable with the distance to the BMU
        integer :: debug_option,idbg,ix,iy,iz,number_variables
        real(kind=wp) :: dist
!
        idbg=kohonen_map%parameters%idbg;
        debug_option=kohonen_map%parameters%debug_level;
        number_variables=kohonen_map%parameters%number_variables1*&
                       kohonen_map%parameters%number_variables2
        ihit = 0;
        jhit = 0;
        khit = 0;
        dist_hit = 1.0e7;
        !$OMP parallel do   
        do iz = 1, size(kohonen_map%grid,3)  
            do iy = 1, size(kohonen_map%grid,2)
                do ix = 1,size(kohonen_map%grid,1)
                    dist = 0.0_wp;
                    dist=kohonen_map%grid(ix,iy,iz)%distance(current_prototype,&
                        kohonen_map%distance_function);
                    !write(*,*) 'dist= ',dist
                    if(debug_option > 0) then
                        call kohonen_map%grid(ix,iy,iz)%print(idbg);
                        write(idbg,*) ix,iy,iz,dist;
                    endif
                    dist = dist/float(number_variables);
                    if (dist < dist_hit) then
                        dist_hit = dist;
                        ihit = ix;
                        jhit = iy;
                        khit = iz;
                    endif
                enddo!ix
            enddo!iy
         enddo!iz
         !$OMP end parallel do   
!
!        write(*,*) 'find= ',ihit,jhit,khit,dist_hit
      return
!
    end subroutine find_best_match_unit
!========================================================================================
    subroutine update_weights(kohonen_map,current_values,ihit,jhit,khit,&
        maximum_radius,iteration) 
!========================================================================================
!!    Subroutine to update the weights   
        class(self_organizing_map) :: kohonen_map
!! A `self_organizing_map` object
        real(kind=wp),dimension(:,:),intent(inout) :: current_values
!! A real array with the values of the current unit
        integer,intent(inout) :: ihit,jhit,khit,iteration
!! Integer variables with the coordinates of the unit (neuron) to be modified
        real(kind=wp),intent(inout) :: maximum_radius
!! Real variable with the maximum radius of the neighborhood 
        real(kind=wp),dimension(size(current_values,1),size(current_values,2)) :: prototype_values
        real(kind=wp),dimension(size(current_values,1),size(current_values,2)) :: winner_values,term1,term2
        integer :: nx,ny,nz,debug_option,ic,current_pos,ineigh,jneigh,kneigh,idbg
        real(kind=wp) :: time_factor,current_radius,alpha,sigma2,h_neighborhood,real_distance,term3
        real(kind=wp) :: distance_ratio,geometric_distance2,eps,current_distance,lambda
        !type(influence_function) :: influence_func
        real(kind=wp),dimension(size(current_values,1),size(current_values,2)) :: v_vector
        real(kind=wp) :: v_vector_norm,r,Psi
        character(len=NUMCHAR) :: m_estimator
!
        nx=kohonen_map%parameters%number_nodes_nx;
        ny=kohonen_map%parameters%number_nodes_ny;
        nz=kohonen_map%parameters%number_nodes_nz;
        debug_option=kohonen_map%parameters%debug_level;
        idbg=kohonen_map%parameters%idbg;
        lambda=2.0_wp*(1.0_wp/maximum_radius);
        time_factor=1.0_wp-dble(iteration)/&
                 dble(kohonen_map%parameters%number_epochs*kohonen_map%parameters%number_patterns);
        !current_radius = max(maximum_radius*real(1001-iteration)/1000.0 + 0.9999999999,4.0d0);
        current_radius = max(maximum_radius*time_factor,4.0_wp);
        !alpha = max(kohonen_map%parameters%learning_rate*(1.0d0-real(iteration)/1000.0),0.01d0);
        alpha = max(kohonen_map%parameters%learning_rate*time_factor,0.01_wp);
        sigma2=current_radius**2;
        !
        m_estimator=trim(kohonen_map%parameters%m_estimator);  
!
        do ic=1,size(kohonen_map%coordinates,1)
            current_pos=position2index(ihit,jhit,khit,nx,ny);
            current_distance=kohonen_map%cells_distances(current_pos,ic)
            if(current_distance < current_radius) then
                geometric_distance2=current_distance**2;
                call index2position(ic,nx,ny,nz,ineigh,jneigh,kneigh);
                !write(*,*) ic,ineigh,jneigh,kneigh,ihit,jhit,khit
                select case(trim(kohonen_map%parameters%neighborhood_type))
                    case('gaussian')
                        h_neighborhood=alpha*dexp(-0.5_wp*geometric_distance2/sigma2);
                    case('bubble')
                        h_neighborhood=alpha;
                end select
                if(debug_option > 0) then
                    write(idbg,*) ihit,jhit,khit,ineigh,jneigh,kneigh
                endif
                select case(trim(kohonen_map%parameters%som_type))
                    case('normal_som')                      
                        call kohonen_map%grid(ineigh,jneigh,kneigh)%get_prototype(prototype_values);
                        prototype_values=prototype_values+h_neighborhood*(current_values-prototype_values);
                        !v_vector=(current_values-prototype_values);
                        !v_vector_norm=dsqrt(sum(v_vector**2));
                        !r=v_vector_norm/sigma;
                        !Psi=influence_func%calculate(m_estimator,r);
                        !prototype_values=prototype_values+sigma*h_neighborhood*Psi*v_vector/v_vector_norm;
                        call kohonen_map%grid(ineigh,jneigh,kneigh)%set_prototype(prototype_values);
                    case('visom')
                        !write(*,*) trim(kohonen_map%parameters%som_type)
                        call kohonen_map%grid(ineigh,jneigh,kneigh)%get_prototype(prototype_values);
                        call kohonen_map%grid(ihit,jhit,khit)%get_prototype(winner_values);
                        real_distance=sum((winner_values-prototype_values)**2);
                        if( (ineigh == ihit) .and. (jneigh == jhit) .and. (kneigh == khit) ) then                           
                             prototype_values=prototype_values+h_neighborhood*(current_values-prototype_values);
                        else
                             distance_ratio=dsqrt(real_distance)/(dsqrt(geometric_distance2)*lambda);
                             term1=(current_values-winner_values);
                             term2=(winner_values-prototype_values);
                             eps=max(1.0_wp*time_factor,0.0_wp);
                             term3=1.0_wp;!((1.0d0-eps)+eps)
                             prototype_values=prototype_values+h_neighborhood*(term1+term2*&
                                         (distance_ratio-1.0_wp)*term3);
                        endif
                        !write(*,*) iteration,dsqrt(real_distance),dsqrt(geometric_distance2)*lambda,distance_ratio
                        call kohonen_map%grid(ineigh,jneigh,kneigh)%set_prototype(prototype_values); 
                    case('robust_som')
                        call kohonen_map%grid(ineigh,jneigh,kneigh)%get_prototype(prototype_values);
                        prototype_values=prototype_values+h_neighborhood*(current_values-prototype_values);
                        ! v_vector=(current_values-prototype_values);
                        ! v_vector_norm=dsqrt(sum(v_vector**2));
                        ! r=v_vector_norm/sigma;
                        ! Psi=influence_func%calculate(m_estimator,r);
                        ! prototype_values=prototype_values+sigma*h_neighborhood*Psi*v_vector/v_vector_norm;
                        call kohonen_map%grid(ineigh,jneigh,kneigh)%set_prototype(prototype_values);
                end select
            endif
        enddo!ic
!
    end subroutine update_weights
!========================================================================================
    subroutine calculate_distance_between_prototypes(kohonen_map)
!========================================================================================
!! Subroutine to calculate the distance between the prototypes
        class(self_organizing_map) :: kohonen_map
!! A `self_organizing_map` object
        integer :: nx,ny,ix,iy,iz,ix1,iy1,iz1,pos,pos1
!
        type(kohonen_prototype) :: current_prototype,current_prototype1
!!
        nx=kohonen_map%parameters%number_nodes_nx;
        ny=kohonen_map%parameters%number_nodes_ny;
        !$OMP parallel do  
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
        !$OMP end parallel do  
!
        if(kohonen_map%parameters%train_option < 3) then
            do ix=1,size(kohonen_map%distance,1)
                write(kohonen_map%parameters%idist,*) (kohonen_map%distance(ix,iy),iy=1,size(kohonen_map%distance,2));
            enddo!ix
        endif
! 
    end subroutine calculate_distance_between_prototypes
!========================================================================================
   subroutine find_bmu_grid(kohonen_map,input_data)
!========================================================================================
!! Subroutine to calculate the best match unit over the grid  
        class(self_organizing_map) :: kohonen_map
!! A `self_organizing_map` object  
        type(kohonen_pattern),dimension(:),intent(inout) :: input_data
!! A `kohonen_pattern` array with the input data
        integer :: nx,ny,nz,ix,iy,iz,ihit,jhit,khit,idat,pat_hit
        type(kohonen_prototype) :: current_prototype
        real(kind=wp) :: dist,dist_min
!
        do idat=1,size(input_data)
            dist_min=1.0e7;ihit=0;jhit=0;khit=0;
            call input_data(idat)%get(current_prototype);
            !$OMP parallel do    
            do iz=1,size(kohonen_map%grid,3)
                do iy=1,size(kohonen_map%grid,2)
                    do ix=1,size(kohonen_map%grid,1)
                        dist=kohonen_map%grid(ix,iy,iz)%distance(current_prototype,kohonen_map%distance_function);
                        dist = dist/float(kohonen_map%parameters%number_variables)
                        if(dist < dist_min) then
                            dist_min=dist;
                            ihit=ix;jhit=iy;khit=iz;pat_hit=idat;
                        endif
                    enddo
                enddo
            enddo
            !$OMP end parallel do    
            kohonen_map%grid_pattern_index(ihit,jhit,khit)=pat_hit;
            kohonen_map%cells_index(idat,1)=ihit;
            kohonen_map%cells_index(idat,2)=jhit;
            kohonen_map%cells_index(idat,3)=khit;
            !    write(*,*) 'BMU= ',idat,ihit,jhit,khit,dist_min
        enddo
!
!
    end subroutine find_bmu_grid
!****f* self_organizing_map_utilities/calculate_u_matrix
! NAME
!   calculate_u_matrix
! PURPOSE
!    Subroutine to calculate  the u_matrix
! SYNOPSIS
!========================================================================================
    subroutine calculate_u_matrix(kohonen_map)
!========================================================================================
!! Subroutine to calculate  the u_matrix
        class(self_organizing_map) :: kohonen_map
!! A `self_organizing_map` object
        character(len=NUMCHAR) :: type_
        integer :: nx,ny,nz,nt,ierr,ix,iy,iz,cx,cy,cz,nxu,nyu,nzu
        real(kind=wp) :: dist,u_temp
!
        type_=trim(kohonen_map%parameters%node_type);
        nx=kohonen_map%parameters%number_nodes_nx;
        ny=kohonen_map%parameters%number_nodes_ny;
        nz=kohonen_map%parameters%number_nodes_nz;
!
        nxu=size(kohonen_map%u_matrix,1);
        nyu=size(kohonen_map%u_matrix,2);
        nzu=size(kohonen_map%u_matrix,3);
!
        select case(trim(type_))
! 
            case('rectangular')
                !call kohonen_map%calculate_u_matrix_rectangular();
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
                             u_temp=0.0d0;
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
                !call kohonen_map%calculate_u_matrix_hexagonal();
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
                        u_temp=0.0d0;
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
        if(kohonen_map%parameters%train_option < 3) then
            do iz=1,size(kohonen_map%u_matrix,3);
                write(kohonen_map%parameters%iumat,'(A,I4)') 'Layer ',iz 
                do ix=1,size(kohonen_map%u_matrix,1);
                    write(kohonen_map%parameters%iumat,'(100f10.5)') (kohonen_map%u_matrix(ix,iy,iz),&
                      iy=1,size(kohonen_map%u_matrix,2));
                enddo
            enddo
        endif
!
    end subroutine calculate_u_matrix
!========================================================================================
    subroutine calculate_u_matrix_hexagonal(kohonen_map)
!========================================================================================
!! Subroutine to calculate the u_matrix for an hexagonal grid
      class(self_organizing_map) :: kohonen_map
!! A `self_organizing_map` object
    
    end subroutine calculate_u_matrix_hexagonal
!========================================================================================
    subroutine calculate_u_matrix_rectangular(kohonen_map)
!========================================================================================
!! Subroutine to calculate the u_matix for a rectangular grid
        class(self_organizing_map) :: kohonen_map 
!! A `self_organizing_map` object        
    end subroutine calculate_u_matrix_rectangular 
!========================================================================================
    subroutine get_u_matrix_som(kohonen_map,u_matrix)
!========================================================================================
!! Subroutine to get the u_matrix from a SOM
        class(self_organizing_map) :: kohonen_map
!! A `self_organizing_map` object 
        real(kind=wp),dimension(:,:,:),intent(out) :: u_matrix
!! A real array to return the u_matrix
        u_matrix=kohonen_map%u_matrix;
!
    end subroutine get_u_matrix_som
!========================================================================================
    subroutine get_prototypes(kohonen_map,prototypes)
!========================================================================================
!! Subroutine to get SOM prototypes
        class(self_organizing_map) :: kohonen_map
!! A `self_organizing_map` object 
        real(kind=wp),dimension(:,:),intent(out) :: prototypes
!! A real array to return the values of the SOM prototypes
        integer :: i,j,k,pos,nvar1,nvar2
        integer,dimension(1) :: nvar
        real(kind=wp),dimension(kohonen_map%parameters%number_variables1,&
                    kohonen_map%parameters%number_variables2) :: current_prototype
        real(kind=wp),dimension(kohonen_map%parameters%number_variables1*&
                    kohonen_map%parameters%number_variables2) :: current_prototype1
      !
        nvar1=kohonen_map%parameters%number_variables1;
        nvar2=kohonen_map%parameters%number_variables2;
        nvar(1)=nvar1*nvar2
        pos=0;
        do k=1,size(kohonen_map%grid,3)
            do j=1,size(kohonen_map%grid,2);
                do i=1,size(kohonen_map%grid,1);
                    pos=pos+1;
                    call kohonen_map%grid(i,j,k)%get_prototype(current_prototype);
                    current_prototype1(1:nvar1*nvar2)=reshape(current_prototype,nvar)
                    prototypes(pos,:)=current_prototype1;
                enddo
            enddo
        enddo
!
    end subroutine get_prototypes
!========================================================================================
    function calculate_sigma(kohonen_map,input_data,seed) result(sigma)
!========================================================================================
!!    Function to calculate the scaling factor sigma
        class(self_organizing_map) :: kohonen_map
!! A `self_organizing_map` object 
        real(kind=wp),dimension(:,:),intent(inout) :: input_data
!! A real array with the input data
        integer,intent(inout),optional :: seed
!! An integer with the random seed
        real(kind=wp) :: sigma
!! A real variable with the value of sigma
        integer :: ndat,nvar,seed1,nx,ny,nz,nxyz,ierr,i,j
        real(kind=wp),allocatable :: sample_pos(:),p_vector(:,:),sigma_table(:,:)
        real(kind=wp),allocatable :: current_sigma(:)
        integer,allocatable :: sample_index(:)
        type(quicksort) :: qsort
!
        if(.not. present(seed)) then 
            seed1=12345;
        else 
            seed1=seed;
        endif
        !
        ndat=size(input_data,1);
        nvar=size(input_data,2);
        !
        !kohonen_map%parameters=training_parameters(1);
        nx=kohonen_map%parameters%number_nodes_nx;
        ny=kohonen_map%parameters%number_nodes_ny;
        nz=kohonen_map%parameters%number_nodes_nz;
        nxyz=nx*ny*nz;
        allocate(sample_pos(ndat),stat=ierr);
        allocate(sample_index(ndat),stat=ierr);
        allocate(p_vector(nxyz,nvar),stat=ierr);
        allocate(sigma_table(ndat,nxyz),stat=ierr);
        allocate(current_sigma(ndat),stat=ierr);
        !call sgrnd(seed1);
        do i=1,size(sample_pos);
            sample_pos(i)=kohonen_map%rnumber_grator%generate();
        enddo
        !call grnd_array(sample_pos);
        do i=1,nxyz
            sample_index(i)=i;
        enddo
        !
        call qsort%sort(sample_pos,sample_index);
        !      
        !  define p vector (See Lopez-Rubio et al, 2015)
        !
        p_vector(1:nxyz,1:nvar)=input_data(sample_index(1:nxyz),1:nvar);
        !   
        !  Calculate the distance between the input data and the selected prototypes
        !
        do i=1,ndat
            do j=1,nxyz
                sigma_table(i,j)=sum((input_data(i,:)-p_vector(j,:))**2);
           enddo
        enddo
      !
        do j=1,nxyz
            current_sigma(1:ndat)=sigma_table(1:ndat,j);
            !(sample_index(i)=i,i=1,ndat)
            call qsort%sort(current_sigma,sample_index);
            !    if(current_sigma(1) > 1d-10) then
            !       current_sigma_value()
        enddo      
        !
        deallocate(sample_pos,sample_index,p_vector,sigma_table,current_sigma);   
!
    end function calculate_sigma 
!****f* self_organizing_map_utilities/external_train_map
! NAME
!   external_train_map
! PURPOSE
! SYNOPSIS
!========================================================================================
    subroutine external_train_map(x,nvar,npat,nx,ny,nepoch,alpha,grid_type,&
       distance_type,neigh_type,toroidal,prot,distortion,&
       u_matrix,coords,number_patterns,node_index) bind(C, name="train_som_")
!========================================================================================
!!    Subroutine to connect the self_organizing_map module to R o C
        use, intrinsic :: iso_c_binding, only : c_double, c_int, c_char
!! Import section
        real(kind=wp),parameter :: version=0.1_wp;
!! Parameter version
        character(len=*),parameter :: program_name="som_train";
!! Parameter name of the function
        integer(c_int), intent(in) :: nvar,npat
!! Integer variables to indicate the number of variables and patterns
        integer(c_int), intent(in) :: nx,ny
!! Integer variables to indicate the number of nodes of the SOM
        integer(c_int), intent(in) :: nepoch
!! Integer variables to indicate the number of epochs for training
        integer(c_int), intent(in) :: toroidal
!! Integer variable to indicate if a toroidal grid is used
        real(c_double),intent(out) :: prot(nx*ny,nvar)
!! Real array for the prototypes
        real(c_double),intent(out) :: distortion(nepoch)
!! Real array for the distortion measure (error during training)
        real(c_double),intent(out) :: u_matrix(2*nx-1,2*ny-1)
!! Real array for the u_matrix 
        real(c_double),intent(out) :: coords(nx*ny,3)
!! Real array for the grid coordinates of the SOM
        integer(c_int),intent(out) :: number_patterns(nx,ny)
!! Integer array with the number of hits for each neuron
        integer(c_int),intent(out) :: node_index(npat,3)
!! Integer array with the index node for all the neurons of the SOM       
        real(c_double),intent(in) :: x(npat,nvar)
!! Real array with the input patterns
        real(c_double),intent(in) :: alpha
!! Real value with the initial learning rate
        integer(c_int),intent(in) :: grid_type
!! Integer variable to indicate the type of grid  
        integer(c_int),intent(in) :: distance_type
!! Integer variable to indicate the distance type
        integer(c_int),intent(in) :: neigh_type
!! Integer variable to indicate the neighborhood type
        type(self_organizing_map) :: my_som
        type(kohonen_layer_parameters),dimension(1) :: parameters
        real(kind=wp),dimension(nvar,1) :: var
        integer :: i,j,k,ierr,pos,ihit,jhit,khit,nx1,ny1
        type(kohonen_pattern),allocatable :: input_patterns(:)
        real(kind=wp),dimension(nx*ny,nvar) :: prototypes
        real(kind=wp),dimension(nvar,1) :: temp
!
        parameters(1)%train_option=3;
        parameters(1)%number_nodes_nx=nx;
        parameters(1)%number_nodes_ny=ny;
        parameters(1)%number_nodes_nz=1;
        parameters(1)%number_variables1=nvar;
        parameters(1)%number_variables2=1;
        parameters(1)%number_variables=nvar;
        parameters(1)%number_patterns=npat;
        parameters(1)%number_epochs=nepoch;
        parameters(1)%learning_rate=alpha;
        parameters(1)%random_seed_=12345;
        if(grid_type == 0) then
             parameters(1)%node_type="rectangular"; !"hexagonal" !rectangular, hexagonal
         elseif(grid_type == 1) then
             parameters(1)%node_type="hexagonal";
         endif
         parameters(1)%debug_level=0;
         parameters(1)%debug_file="NOFILE";
         parameters(1)%pattern_file="NOFILE";
         parameters(1)%output_file="NOFILE";
         parameters(1)%distance_type="euclidean"; !"euclidean" !euclidean, manhattan, correlation, correlation2
         if(neigh_type == 0) then
             parameters(1)%neighborhood_type="bubble";
         elseif(neigh_type == 1) then
             parameters(1)%neighborhood_type="gaussian"; !gaussian,bubble
         endif
         parameters(1)%som_type="normal_som"!,visom
         if(toroidal == 1) then
             parameters(1)%toroidal_grid=.TRUE.;
         else
             parameters(1)%toroidal_grid=.FALSE.;
         endif
         !
         ! ADDED TO AVOID PRINTING UNIT INFO (THE CAUSE IS UNKNONW)
         ! write(*,*) ''
         ! write(*,'(A,A,f10.5)') trim(program_name),' version: ',version
         ! write(*,*) ''
         allocate(input_patterns(npat),stat=ierr);
         do i=1,npat
             var(1:nvar,1) = x(i,1:nvar)
             !write(*,*) i,var
             call input_patterns(i)%create(var);
             !    call input_patterns(i)%print();
         enddo
        ! Create SOM
        call my_som%create(parameters);
        ! Train SOM
        call my_som%train(input_patterns);
        ! Extract results
        pos=0
        k=1
        nx1=nx;ny1=ny;
        do j=1,ny
            do i=1,nx
                pos=position2index(i,j,k,nx1,ny1);
                !write(*,*) i,j,pos,i+(j-1)*nx
                call my_som%grid(i,j,k)%get_prototype(temp);
                !position2index()
                prototypes(pos,1:nvar)=temp(1:nvar,1);
            enddo
        enddo
        !
        ! Get the results in the arrays
        !
        distortion=my_som%distortion
        u_matrix(1:2*nx-1,1:2*ny-1)=my_som%u_matrix(:,:,1);
        !do i=1,size(my_som%coordinates,1);
        !write(*,*) my_som%coordinates(i,1:3);
        !   coords(i,1:3)=my_som%coordinates(i,1:3);
        !enddo
        coords=my_som%coordinates;
        !coords(1:nx*nx,1)=my_som%coordinates(:,1);
        !coords(1:nx*nx,2)=my_som%coordinates(:,2);
        !coords(1:nx*nx,3)=my_som%coordinates(:,3);
        number_patterns=my_som%number_patterns(:,:,1);
        node_index=my_som%cells_index
        prot=prototypes;
        ! 
        call my_som%destroy();
        !
        do i=1,npat
            call input_patterns(i)%destroy();
        enddo
        deallocate(input_patterns);
        !
        !
        ! write(*,*)
        ! write(*,'(A,A,f10.5,2X,A)') trim(program_name),' version: ',version,'Finished'
        ! write(*,*)
        !
    end subroutine external_train_map
!========================================================================================
    subroutine external_predict_map(prot,nx,ny,new_pat,npat,nvar,node_index) & 
        bind(C, name="predict_som_")
!========================================================================================
!!    Subroutine to connect this module to R

        use, intrinsic :: iso_c_binding, only : c_double, c_int
        integer(c_int),intent(in) :: nx,ny,npat,nvar
        real(c_double),intent(in) :: prot(nx*ny,nvar),new_pat(npat,nvar)
        integer(c_int),intent(out) :: node_index(npat,3) 
!!
        type(self_organizing_map) :: my_som
        type(kohonen_layer_parameters),dimension(1) :: parameters
        integer :: ipat,inode,i_hit,nx1,ny1,nz1,cx,cy,cz,ix,iy,iz,pos,ierr
        real(kind=wp) :: dist,dist_hit
        real(kind=wp),dimension(nvar,1) :: temp
        type(kohonen_pattern),dimension(npat) :: input_data
!
        parameters(1)%train_option=3;
        parameters(1)%number_nodes_nx=nx;
        parameters(1)%number_nodes_ny=ny;
        parameters(1)%number_nodes_nz=1;
        parameters(1)%number_variables1=nvar;
        parameters(1)%number_variables2=1;
        parameters(1)%number_variables=nvar;
        parameters(1)%number_patterns=npat;
        parameters(1)%number_epochs=1;
        parameters(1)%learning_rate=0.0d0;
        parameters(1)%random_seed_=12345;
        parameters(1)%node_type="hexagonal"
        parameters(1)%debug_level=0;
        parameters(1)%debug_file="NOFILE"
        parameters(1)%pattern_file="NOFILE"
        parameters(1)%output_file="NOFILE"
        parameters(1)%distance_type="euclidean" !"euclidean" !euclidean, manhattan, correlation, correlation2
        parameters(1)%neighborhood_type="gaussian" !gaussian,bubble
        parameters(1)%som_type="normal_som"!,visom
        parameters(1)%toroidal_grid=.TRUE.
!
! call parameters(1)%print();
!
        call my_som%create(parameters);
!
        pos=0;
        iz=1; 
        do iy=1,ny
            do ix=1,nx
                pos=pos+1;
                temp(1:nvar,1)=prot(pos,1:nvar)
                call my_som%grid(ix,iy,iz)%set_prototype(temp)
            enddo
        enddo
        !
        do ipat=1,npat
            temp(1:nvar,1)=new_pat(ipat,1:nvar);
            call input_data(ipat)%create(temp);
        enddo
        !
        call my_som%predict(input_data,node_index);
        !
        call my_som%destroy();
        !
        do ipat=1,size(input_data);
            call input_data(ipat)%destroy();
        enddo
!
    end subroutine external_predict_map
! 
end module self_organizing_map_utilities