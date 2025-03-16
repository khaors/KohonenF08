program two_level_som_estimate
!
    use precision_utilities, only: wp;
    use constants_utilities, only: NUMCHAR;
    use two_level_som_estimate_variables, only: my_som,parfl,som_parameters,total_results,&
        results,clusters,current_cluster,input_patterns,matrix_fl,min_cluster,initialize_variables,&
        release_variables,seeds,association_matrix,min_nodes,max_nodes;
    use two_level_self_organizing_map_utilities;
    !
    implicit none;
    !
    real(kind=wp),parameter :: version=0.1_wp;
    character(len=*),parameter :: name='TWO_LEVEL_SOM_ESTIMATE'
    character(len=NUMCHAR) :: arg
    integer :: i,ic,iseed,j,neval,number_arguments,number_nodes,p
    !
    write(*,*)
    write(*,'(A,A,f10.5)') name,' version: ',version;
    write(*,*)
    write(*,*) 'Developed by: Oscar Garcia-Cabrejo';
    write(*,*)
!
    parfl=''; ! Initialize to ensure that checking for empty string
    number_arguments = command_argument_count();
    do i=1,number_arguments;
        call get_command_argument(i,arg);
        select case(arg)
            case('-v', '--version')
                write(*,'(A,A,f10.5)') trim(name),' version: ',version;
                stop;
            case('-h', '--help')
                call print_help();
                stop;
            case default
                if(arg(1:1) /= '-' .or. arg(1:2) /= '--') then
                    parfl=trim(adjustl(arg));
                endif
                !write(*,*) 'parfl= ',trim(parfl);
        end select
    enddo
!

    if(parfl(1:1) .eq. ' ') then 
        write(*,*) 'Parameter file?'
        read(*,'(A)') parfl
        if(parfl(1:1) .eq. ' ') parfl='two_level_som_estimate.par';
    endif
    write(*,*) 'Parameter file= ',trim(parfl);
    !
    call initialize_variables(parfl);
    !
    current_cluster=0;
    neval=0;
    do ic=min_nodes(1),max_nodes(1)
    !
    som_parameters(2)%number_nodes_nx=ic;
    som_parameters(2)%number_nodes_ny=1;
    som_parameters(2)%number_nodes_nz=1;
    !
    current_cluster=current_cluster+1;
    !  
    do iseed=1,size(seeds); !number_clusters_evaluations
    !  
        neval=neval+1;
        som_parameters(1)%random_seed_=seeds(iseed);
    !  
        call my_som%create(som_parameters);
    !
        call my_som%train(input_patterns);
    !
        call my_som%calculate_sum2_clusters_grid(results);
    !
    !     write(*,*) results(1:3)
        !write(*,*) size(total_results,1),size(total_results,2),size(total_results,3)
        total_results(current_cluster,iseed,1:3)=results(1:3);
        call my_som%get_cluster_samples(clusters)
        call update_association_matrix(clusters,association_matrix);
    !
        call my_som%destroy();
    !
    enddo !iseed
    !write(*,*) (seeds(iseed),iseed=1,size(seeds));
    !write(som_parameters(1)%iout1,*) ic,(total_results(current_cluster,iseed,1:3),iseed=1,size(seeds))  
    enddo !c
    !
    number_nodes=som_parameters(1)%number_nodes_nx*som_parameters(1)%number_nodes_ny*&
                som_parameters(1)%number_nodes_nz;
    p=som_parameters(1)%number_variables1*som_parameters(1)%number_variables2;
    min_cluster=maxval(min_nodes);
    !
    call calculate_cluster_measures1(total_results,number_nodes,p,min_cluster,som_parameters(1)%iout1);
    !
    do i=1,size(association_matrix,1)
    write(som_parameters(1)%imeas,'(1000f10.5)') (association_matrix(i,j)/dble(neval),j=1,size(association_matrix,2));
    enddo
    !
    call run_r_script(matrix_fl);
    !write(*,*) number_clusters_evaluations
    !write(*,*) association_matrix(1:5,1:5)/dble(neval)
    !write(*,*) allocated(total_results)
    !deallocate(total_results);
    call release_variables();
    !
    write(*,*)
    write(*,'(A,A,f10.5,2X,A)') name,' version: ',version,'Finished'
    write(*,*)
    !
    stop 
    !
    contains
    !
        subroutine print_help()
            write(*,'(a, /)') 'command-line options:'
            write(*,'(a)')    '  -v, --version     print version information and exit'
            write(*,'(a)') '  -h, --help        print usage information and exit'
            write(*,'(a, /)') '  two_level_som_estimate file.par  run the two_level_som_estimate program with the parameter file file.par'
    
        end subroutine print_help    
!    
    subroutine calculate_cluster_measures1(total_results,n,p,min_cluster,output_unit)
    !
    real(kind=8),dimension(:,:,:),intent(inout) :: total_results
    integer :: output_unit,n,p,min_cluster
    !
    integer :: neval,nseeds,nres,ieval,iseed,ierr,current_k,i
    real(kind=8),dimension(3) :: temp_results
    real(kind=8),allocatable :: W(:),B(:),sil(:) 
    logical :: testop
    real(kind=8),dimension(6) :: results
    real(kind=8) :: diff_k,diff_kp1
    !
    neval=size(total_results,1);
    nseeds=size(total_results,2);
    nres=size(total_results,3);
    !
    allocate(W(neval),stat=ierr);
    allocate(B(neval),stat=ierr);
    allocate(sil(neval),stat=ierr);
    ! 
    do ieval=1,neval
        temp_results=0.0d0;
        do iseed=1,nseeds
            temp_results(1:3)=temp_results(1:3)+total_results(ieval,iseed,1:3);
        enddo
        temp_results=temp_results/dble(nseeds);
        W(ieval)=temp_results(1);
        B(ieval)=temp_results(2);
        sil(ieval)=temp_results(3);
    enddo
    !
    inquire(unit=output_unit,opened=testop);
    if(.not. testop) then
        write(6,*) 'ERROR: the output unit is not active'
        stop
    endif
    !
    !  write(*,*) W
    !  write(*,*) B
    !
    do ieval=1,neval-1
        current_k=min_cluster+ieval-1;
        !H (First H<10)
        results(1)=((W(ieval)/W(ieval+1))-1.0d0)*dble(n-current_k-1);
        !KL (max KL)
        results(2)=0.0d0;
        if(ieval .le. neval-1 .and. current_k .ge. 2) then
        diff_k=((current_k-1)**(2/p))*W(ieval)-((current_k+1)**(2/p))*W(ieval+1);
        diff_kp1=((ieval+1)**(2/p))*W(ieval+1)-((ieval+2)**(2/p))*W(ieval+2);
        results(2)=dabs(diff_k/diff_kp1);
        endif
        !CH (max CH)
        results(3)=(dble(n-current_k)*B(ieval))/(W(ieval)*dble(current_k-1))
        !BALL index (Max value)
        results(4)=(W(ieval+1)-W(ieval))/dble(current_k);
        ! Silhouette (max sil)
        results(5)=sil(ieval);
        ! Friedman
        results(6)=(B(ieval+1)/W(ieval+1))-(B(ieval)/W(ieval))
        write(output_unit,'(I6,1X,6f12.6)') current_k,(results(i),i=1,6)
    enddo
    !   
    end subroutine calculate_cluster_measures1
    !
    subroutine update_association_matrix(cluster_sample,association_matrix)
    !
    integer,dimension(:),intent(inout) :: cluster_sample
    real(kind=8),dimension(:,:),intent(inout) :: association_matrix
    !
    integer :: i,j,nrow,ncol
    !
    nrow=size(association_matrix,1);
    ncol=size(association_matrix,2);
    do i=1,nrow
        do j=1,ncol
            if( (i .ne. j) .and. (cluster_sample(i) .eq. cluster_sample(j)) ) then
            association_matrix(i,j)=association_matrix(i,j)+1.0d0;
            endif
        enddo
    enddo
    !
    end subroutine update_association_matrix
    !========================================================================================
    subroutine run_r_script(matrix_file)
    !========================================================================================
    character(len=*) :: matrix_file
    !
    
    open(100,file='kohonen_cluster.R',status='unknown',action='write',access='sequential');
    write(100,'(A)') 'library(cluster)';
    write(100,'(A)') "library(stringi)"
    !  write(100,'(A)') "Stations <- read.table(file='files.out',header=F)";
    write(100,'(A)') "vote <- read.table(file='"//trim(matrix_file)//"',header=F)";
    write(100,'(A)') "vote.dist <- as.dist(vote)";
    write(100,'(A)') "fit <- hclust(vote.dist, method='average')";
    write(100,'(A)') "fit1 <- hclust(1.0-vote.dist, method='single')";
    write(100,'(A)') "pdf_file='Dendogram1-'%s+%'"//trim(matrix_file)//"'%s+%'.pdf';"
    write(100,'(A)') "pdf(file=pdf_file,height = 10,width = 10);";
    write(100,'(A)') "plot(fit)"
    write(100,'(A)') "dev.off()"
    write(100,'(A)') "pdf_file='Dendogram2-'%s+%'"//trim(matrix_file)//"'%s+%'.pdf';"
    write(100,'(A)') "pdf(file=pdf_file,height = 10,width = 10);";
    write(100,'(A)') "plot(fit1)"
    write(100,'(A)') "dev.off()"
    close(100);
    call system("R CMD BATCH kohonen_cluster.R kohonen_cluster.out");
    
    
    ! 
    close(100)
    end subroutine run_r_script
!
end program two_level_som_estimate