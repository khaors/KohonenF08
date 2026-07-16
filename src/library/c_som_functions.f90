submodule (self_organizing_map_utilities) c_som_functions
    implicit none;
    !
    contains
!========================================================================================
    module subroutine external_train_map(x,nvar,npat,nx,ny,nepoch,alpha,grid_type,&
        distance_type,neigh_type,toroidal,prot,distortion,&
        u_matrix,coords,number_patterns,node_index) bind(C, name="train_som")
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
    end subroutine external_train_map
!========================================================================================
    module subroutine external_predict_map(prot,nx,ny,new_pat,npat,nvar,node_index) & 
        bind(C, name="predict_som")
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
end submodule c_som_functions