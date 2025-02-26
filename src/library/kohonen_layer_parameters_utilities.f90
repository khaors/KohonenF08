module kohonen_layer_parameters_utilities
!
use error_handling, only: error_t,error_stop;
use precision_utilities, only: wp;
use constants_utilities, only: NUMCHAR;
use tomlf, only: toml_table;
implicit none
!
private
!
type kohonen_layer_parameters
  integer :: train_option !
  integer :: number_nodes_nx,number_nodes_ny,number_nodes_nz,number_patterns
  integer :: number_variables1,number_variables2,number_variables
  integer,allocatable :: column_var(:)
  integer :: number_epochs,debug_level !number_clusters,
  integer,dimension(1) :: random_seed_
  real(kind=wp) :: learning_rate
  character(len=NUMCHAR) :: node_type !rectangular, hexagonal
  character(len=NUMCHAR) :: debug_file,pattern_file,output_file
  character(len=NUMCHAR) :: distance_type !euclidean, manhattan, correlation, correlation2
  character(len=NUMCHAR) :: neighborhood_type !gaussian,bubble
  character(len=NUMCHAR) :: som_type !normal_som,visom,robust_som
  character(len=NUMCHAR) :: m_estimator !name of the M-estimator to be used
  logical :: toroidal_grid
  ! UNITS FOR TRAINING OUTPUT
  integer :: idbg,iout,iindex,iprot,ihit,idist,iumat,ipar,isam,iclus,icen,iclus1,idisto
  ! UNITS FOR ESTIMATION OUTPUT
  integer :: iout1,imeas 
  logical :: view_flag
  ! FLAG FOR DEBUGGING REALIZATION
  integer :: ireal
!
  contains
    procedure,public :: print => print_parameters
    procedure,public :: read_parameters
    procedure,public :: read_parameters_toml
    !generic,public :: read => read_parameters, read_parameters_toml
end type kohonen_layer_parameters

public :: kohonen_layer_parameters
!
 contains
!============================================================================== 
 subroutine print_parameters(parameters,layer_ind,unit_)
!============================================================================== 
   class(kohonen_layer_parameters) :: parameters
   integer,intent(inout) :: layer_ind
   integer,intent(inout),optional :: unit_
!
   integer :: unit1,j,toroidal
   character(len=NUMCHAR) :: current_line
!
   if(.not. present(unit_)) then 
      unit1=6;
   else
      unit1=unit_;
   endif
   write(unit1,'(A)') adjustl('Kohonen Map Parameters');
   write(unit1,'(A)') adjustl('SOM_TRAIN_PARAMETERS')
   write(current_line,'(I5)') parameters%train_option
   write(unit1,'(A40,A)') adjustl(current_line),'!Train option';
   if(layer_ind == 1) then
     write(unit1,'(A40,A)') adjustl(parameters%pattern_file),'!Pattern file';
   endif
   write(current_line,'(I5)') parameters%number_patterns   
   write(unit1,'(A40,A)') adjustl(current_line),'!Number Patterns';
   write(current_line,'(2I5)') parameters%number_variables1,parameters%number_variables2
   write(unit1,'(A40,A)') adjustl(current_line),'!Number Variables1,Number Variables2';
   if(parameters%number_variables .le. 10) then
     write(current_line,'(10I4)') (parameters%column_var(j),j=1,parameters%number_variables)
   else
     write(current_line,'(4I4)') -1,-1,-1,-1
   endif
   write(unit1,'(A40,A)') adjustl(current_line),'!Columns'
   if(layer_ind == 1) then
     write(unit1,'(A40,A)') adjustl(parameters%som_type),'!SOM type'
   endif
   write(current_line,'(3I5)') parameters%number_nodes_nx,&
                              parameters%number_nodes_ny,&
                              parameters%number_nodes_nz
   write(unit1,'(A40,A)') adjustl(current_line),'!Number nodes x, Number nodes y, Number nodes z'
   write(current_line,'(I6)') parameters%number_epochs
   write(unit1,'(A40,A)') adjustl(current_line),'!Number epochs';
   write(current_line,'(f10.5)') parameters%learning_rate
   write(unit1,'(A40,A)') adjustl(current_line),'!Learning rate';
   write(current_line,'(I10)') parameters%random_seed_
   write(unit1,'(A40,A)') adjustl(current_line),'!Random seed';
   write(unit1,'(A40,A)') adjustl(parameters%distance_type),'!Distance type';
   write(unit1,'(A40,A)') adjustl(parameters%node_type),'!Node type';
   write(unit1,'(A40,A)') adjustl(parameters%neighborhood_type),'!Neigh type';
   write(current_line,'(I5)') parameters%debug_level;
   write(unit1,'(A40,A)') adjustl(current_line),'!Debug level';
   if(layer_ind == 1) then
     write(unit1,'(A40,A)') adjustl(parameters%debug_file),'!Debug file';
   endif
   if(layer_ind == 1) then
     write(unit1,'(A40,A)') adjustl(parameters%output_file),'!Output file';
   endif
   if(parameters%toroidal_grid) then
     toroidal=1;
   else 
     toroidal=0;
   endif
   write(current_line,'(I2)') toroidal
   write(unit1,'(A40,A)') adjustl(current_line),'!Toroidal grid'
!
   return
!
 end subroutine print_parameters
!============================================================================== 
 subroutine read_parameters(parameters,unit_)
!============================================================================== 
   class(kohonen_layer_parameters) :: parameters
   integer,intent(inout),optional :: unit_
!
   integer :: unit1
!
   if(.not. present(unit_)) then 
      unit1=6;
   else
      unit1=unit_;
   endif
   read(unit1,*) !'Kohonen Map Parameters'
   read(unit1,'(A40)') parameters%pattern_file !,'!Pattern file';
   read(unit1,'(I40)') parameters%number_patterns!,'!Number Patterns';
   read(unit1,'(I40)') parameters%number_variables1!,'!Number Variables1';
   read(unit1,'(I40)') parameters%number_variables2!,'!Number Variables2';
   parameters%number_variables=parameters%number_variables1*&
                               parameters%number_variables2
!    read(unit1,'(A40)') parameters%output_file!,'!Output file';
!    read(unit1,'(A40)') parameters%debug_file!,'!Debug file';
!    read(unit1,'(I40)') parameters%debug_level!,'!Debug level'; 
   read(unit1,'(I40)') parameters%number_nodes_nx!,'!Number nodes x';
   read(unit1,'(I40)') parameters%number_nodes_ny!,'!Number nodes y';
   read(unit1,'(I40)') parameters%number_nodes_nz!,'!Number nodes z';
   read(unit1,'(I40)') parameters%number_epochs!,'!Number epochs';
   read(unit1,*) parameters%learning_rate!,'!Learning rate';
   read(unit1,'(I40)') parameters%random_seed_!,'!Random seed';
   read(unit1,'(A40)') parameters%distance_type!,'!Distance type';
   read(unit1,'(A40)') parameters%node_type!,'!Node type';
   read(unit1,'(A40)') parameters%neighborhood_type!,'!Neigh type';
   read(unit1,'(I40)') parameters%debug_level!,'!Debug level';
   read(unit1,'(A40)') parameters%debug_file!,'!Debug file';
   read(unit1,'(A40)') parameters%output_file!,'!Output file';
!   
 end subroutine read_parameters
!========================================================================================
    subroutine read_parameters_toml(parameters,unit_)
!========================================================================================
      class(kohonen_layer_parameters) :: parameters
      integer,intent(inout),optional :: unit_
! Create a subroutine to read a toml file 
    end subroutine read_parameters_toml
!
end module kohonen_layer_parameters_utilities