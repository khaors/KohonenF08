!! author: Oscar Garcia-Cabrejo
!! date: 07/15/2026
!! version: 0.2
!!  This module defines a class for simple self_organizing_map (one kohonen layer) 
module self_organizing_map_utilities
!!  This module defines a class for simple self_organizing_map (one kohonen layer)
!$  use omp_lib     
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
        procedure,private :: create_random_sample
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
interface
    ! Train
    module subroutine train_som_data(kohonen_map,input_data)
      class(self_organizing_map) :: kohonen_map
      type(kohonen_pattern),dimension(:),intent(inout) :: input_data
    end subroutine train_som_data
    ! Train
    module subroutine find_best_match_unit(kohonen_map,current_prototype,ihit,&
        jhit,khit,dist_hit)
        class(self_organizing_map) :: kohonen_map
        type(kohonen_prototype),intent(inout) :: current_prototype
        integer,intent(out) :: ihit,jhit,khit
        real(kind=wp),intent(out) :: dist_hit
    end subroutine find_best_match_unit
    ! Train
    module subroutine update_weights(kohonen_map,current_values,ihit,jhit,khit,&
        maximum_radius,iteration) 
        class(self_organizing_map) :: kohonen_map
        real(kind=wp),dimension(:,:),intent(inout) :: current_values
        integer,intent(inout) :: ihit,jhit,khit,iteration
        real(kind=wp),intent(inout) :: maximum_radius
    end subroutine update_weights
    ! Train
    module subroutine calculate_distance_between_prototypes(kohonen_map)
        class(self_organizing_map) :: kohonen_map
    end subroutine calculate_distance_between_prototypes
    ! Train
    module function position2index(ix,iy,iz,nx,ny) result(index_)
        integer,intent(in) :: ix,iy,iz,nx,ny
        integer :: index_
    end function position2index
    ! Train
    module subroutine index2position(index_,nx,ny,nz,cx,cy,cz)
        integer,intent(in) :: index_
        integer,intent(in) :: nx,ny,nz
        integer,intent(inout) :: cx,cy,cz
    end subroutine index2position
    ! Accessor
    module subroutine get_prototypes(kohonen_map,prototypes)
        class(self_organizing_map) :: kohonen_map
        real(kind=wp),dimension(:,:),intent(out) :: prototypes
    end subroutine get_prototypes
    ! Accessor
    module subroutine get_count_som(kohonen_map,count_)
        class(self_organizing_map) :: kohonen_map
        integer,dimension(:,:,:),intent(inout) :: count_
    end subroutine get_count_som
    ! Accessor
    module subroutine print_som(kohonen_map,unit_)
        class(self_organizing_map) :: kohonen_map
        integer,intent(inout),optional :: unit_
    end subroutine print_som
    ! Mutator
    module subroutine read_som(kohonen_map,som_fl)
        class(self_organizing_map) :: kohonen_map
        character(len=*) :: som_fl
    end subroutine read_som
    !
    module subroutine external_train_map(x,nvar,npat,nx,ny,nepoch,alpha,grid_type,&
        distance_type,neigh_type,toroidal,prot,distortion,&
        u_matrix,coords,number_patterns,node_index) bind(C, name="train_som")
        use, intrinsic :: iso_c_binding, only : c_double, c_int, c_char
        real(kind=wp),parameter :: version=0.1_wp;
        character(len=*),parameter :: program_name="som_train";
        integer(c_int), intent(in) :: nvar,npat
        integer(c_int), intent(in) :: nx,ny
        integer(c_int), intent(in) :: nepoch
        integer(c_int), intent(in) :: toroidal
        real(c_double),intent(out) :: prot(nx*ny,nvar)
        real(c_double),intent(out) :: distortion(nepoch)
        real(c_double),intent(out) :: u_matrix(2*nx-1,2*ny-1)
        real(c_double),intent(out) :: coords(nx*ny,3)
        integer(c_int),intent(out) :: number_patterns(nx,ny)
        integer(c_int),intent(out) :: node_index(npat,3)
        real(c_double),intent(in) :: x(npat,nvar)
        real(c_double),intent(in) :: alpha
        integer(c_int),intent(in) :: grid_type
        integer(c_int),intent(in) :: distance_type
        integer(c_int),intent(in) :: neigh_type
    end subroutine external_train_map
    !
    module subroutine external_predict_map(prot,nx,ny,new_pat,npat,nvar,node_index) & 
        bind(C, name="predict_som")
        use, intrinsic :: iso_c_binding, only : c_double, c_int
        integer(c_int),intent(in) :: nx,ny,npat,nvar
        real(c_double),intent(in) :: prot(nx*ny,nvar),new_pat(npat,nvar)
        integer(c_int),intent(out) :: node_index(npat,3) 
        !
        type(self_organizing_map) :: my_som
        type(kohonen_layer_parameters),dimension(1) :: parameters
        integer :: ipat,inode,i_hit,nx1,ny1,nz1,cx,cy,cz,ix,iy,iz,pos,ierr
        real(kind=wp) :: dist,dist_hit
        real(kind=wp),dimension(nvar,1) :: temp
        type(kohonen_pattern),dimension(npat) :: input_data
    end subroutine external_predict_map
end interface
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
!   
        write(*,*) 'SOM: Initializing grid...',kohonen_map%seed;
        do iz=1,nz;
            do iy=1,ny;
                do ix=1,nx;
                    !write(*,*) 'creating ',ix,iy,iz
                    call kohonen_map%create_random_sample(input);
                    call kohonen_map%grid(ix,iy,iz)%create(input); 
                        current_index=position2index(ix,iy,iz,nx,ny);
                    call calculate_coordinates(current_index,ix,iy,iz,nx,ny,nz,&
                        kohonen_map%coordinates,training_parameters(1)%node_type);
                enddo!ix
            enddo !iy
         enddo !iz
         deallocate(input);
   !
         call calculate_distance_matrix(kohonen_map%coordinates,&
            kohonen_map%cells_distances,&
            training_parameters(1)%node_type,&
            training_parameters(1)%toroidal_grid);
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
!========================================================================================
    subroutine create_random_sample(kohonen_map,input)
!========================================================================================
!! Subroutine to generate random values that serve as inputs to the SOM
    class(self_organizing_map) :: kohonen_map
!! A `self_organizing_map` object
    real(kind=wp),dimension(:,:),intent(out) :: input 
!! A real array with the initial values of the prototypes
    integer :: nvar1,nvar2,i,j
!
    nvar1=size(input,1);
    nvar2=size(input,2);
    do i=1,nvar1;
        do j=1,nvar2;
            input(i,j)=kohonen_map%rnumber_grator%generate();
        end do
    end do
!    
    end subroutine create_random_sample
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
            map_output(ipattern,1)=ihit;
            map_output(ipattern,2)=jhit;
            map_output(ipattern,3)=khit;
        enddo !ipattern
!       write(*,*) 'SOM: Prediction finished';
!
    end subroutine predict_som
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
! 
end module self_organizing_map_utilities