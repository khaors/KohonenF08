submodule (self_organizing_map_utilities) som_mutators_utilities
    !
    implicit none
    !
    contains 
!========================================================================================
    module subroutine read_som(kohonen_map,som_fl)
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
     !
end submodule som_mutators_utilities
