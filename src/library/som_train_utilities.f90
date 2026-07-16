submodule (self_organizing_map_utilities) som_train_utilities
!
    implicit none;
    !
    contains
!========================================================================================
        module subroutine train_som_data(kohonen_map,input_data)
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
        if(iepoch > 1) write(idisto,*) iepoch,distortion
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
    module subroutine find_best_match_unit(kohonen_map,current_prototype,ihit,jhit,&
        khit,dist_hit)
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
    module subroutine update_weights(kohonen_map,current_values,ihit,jhit,khit,&
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
    module subroutine calculate_distance_between_prototypes(kohonen_map)
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
    module function position2index(ix,iy,iz,nx,ny) result(index_)
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
    module subroutine index2position(index_,nx,ny,nz,cx,cy,cz)
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


end submodule som_train_utilities