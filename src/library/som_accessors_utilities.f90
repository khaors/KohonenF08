submodule (self_organizing_map_utilities) som_accessors_utilities
    !
    implicit none;
    !
    contains
!========================================================================================
    module subroutine get_prototypes(kohonen_map,prototypes)
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
    !
!========================================================================================
    module subroutine get_count_som(kohonen_map,count_)
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
    module subroutine print_som(kohonen_map,unit_)
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

end submodule som_accessors_utilities
