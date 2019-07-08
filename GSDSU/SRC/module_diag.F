#include <define_CPP.h>
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 module module_diag
 use module_floating_point
 use module_const
 use module_simulator
 use module_mpi
 implicit none

!
! initialize (reading) gaseous profiles.
!

 private  ! all are encapsulate.

 public :: zonal_mean_2d  , &   ! initialize gaseous profiles of climatology
           spatial_mean_2d, &
           spatial_mean_3d, &
           spatial_mean_daily_2d, &
           spatial_mean_daily_3d, &
           spatial_mean_hourly_2d, &
           spatial_mean_hourly_3d, &
           diurnal_mean_2d, &
           diurnal_daily_2d,&
           diurnal_mean_3d  

 save     ! all parameters are saved.



!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 contains

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 subroutine spatial_mean_2d(proc,var2d)
 implicit none
!--------------------------------------------------------------------------------------------------
! Comments: Derive spatial mean map of 2D parameters.
!           
! History:
! 11/2016  Toshi Matsui@NASA GSFC ; Initial.
!
! References:
!-----------------------------------------------------------------------------------------------------

 character*(*) :: proc ! process
 type ( surface_parameter ), dimension(:,:), optional :: var2d ! input surface parameter assumed 2d array

 integer :: bnd(2)  !upper memory bound for 2D array

!
! statisticsl parameters
!
 type( surface_parameter ), allocatable, dimension(:,:),save :: spatial_sum, spatial_mean
 real(sdsu_fps), allocatable, dimension(:,:),save :: spatial_cnt
 real(sdsu_fps), allocatable, dimension(:),save :: spatial_lat, spatial_lon

 integer,save :: ibin_max
 integer :: i,j, ibin, ierr, irec
 integer,parameter :: io = 149

 integer,save :: imax, jmax
 integer :: ii, jj

 select case(trim(proc))
 case('init') !---------------------------------------------------------------------------------

   if(masterproc) print*, 'MSG spatial_mean_2d: init'

   imax = int( ( maxlon - minlon ) / res_latlon ) + 1
   jmax = int( ( maxlat - minlat ) / res_latlon ) + 1

   if( imax < 1 ) call stop_sdsu(myrank,'MSG spatial_mean_2d: maxlon is smaller than minlon')
   if( jmax < 1 ) call stop_sdsu(myrank,'MSG spatial_mean_2d: maxlat is smaller than minlat')

  
   if( allocated(spatial_sum) ) deallocate(spatial_sum, spatial_cnt, spatial_mean, spatial_lat, spatial_lon)
 
   allocate( spatial_sum(imax,jmax)  , &
             spatial_cnt(imax,jmax)  , &
             spatial_mean(imax,jmax) , &
             spatial_lat(jmax)  , &
             spatial_lon(imax)  , &
             stat=ierr )

   ! initialized
   do j = 1, jmax
      spatial_lat(j) = minlat + res_latlon * real(j-1) ![deg]
   enddo
   do i = 1, imax
      spatial_lon(i) = minlon + res_latlon * real(i-1) ![deg]
   enddo

  !
  ! zero out other parameters
  !
  spatial_cnt = 0.

  spatial_sum%rain_rate      = 0. ; spatial_sum%h2o_col       = 0.
  spatial_sum%path_cloud_liq = 0. ; spatial_sum%path_rain_liq = 0.
  spatial_sum%path_cloud_ice = 0. ; spatial_sum%path_rain_ice = 0.
  spatial_sum%toa_lw_up      = 0. ; spatial_sum%toa_lw_dn     = 0.
  spatial_sum%toa_sw_up      = 0. ; spatial_sum%toa_sw_dn     = 0.
  spatial_sum%boa_lw_up      = 0. ; spatial_sum%boa_lw_dn     = 0.
  spatial_sum%boa_sw_up      = 0. ; spatial_sum%boa_sw_dn     = 0.

  spatial_mean%rain_rate      = undefined ; spatial_mean%h2o_col       = undefined
  spatial_mean%path_cloud_liq = undefined ; spatial_mean%path_rain_liq = undefined
  spatial_mean%path_cloud_ice = undefined ; spatial_mean%path_rain_ice = undefined
  spatial_mean%toa_lw_up      = undefined ; spatial_mean%toa_lw_dn     = undefined
  spatial_mean%toa_sw_up      = undefined ; spatial_mean%toa_sw_dn     = undefined
  spatial_mean%boa_lw_up      = undefined ; spatial_mean%boa_lw_dn     = undefined
  spatial_mean%boa_sw_up      = undefined ; spatial_mean%boa_sw_dn     = undefined


 case('add') !-------------------------------------------------------------------------------------

  if(masterproc) print*, 'MSG spatial_mean_2d: add'

  if(present(var2d) ) then

   !
   ! find upper bound from assumped 3D array
   !
   bnd = UBOUND(var2d)

   do j = 1,bnd(2) ; do i = 1,bnd(1)

      !
      ! loop for latlon_grid domain. 
      !
      do jj = 1, jmax
        within_grid: if( var2d(i,j)%lat >  (spatial_lat(jj)-0.5*res_latlon) .and. &
                         var2d(i,j)%lat <= (spatial_lat(jj)+0.5*res_latlon) ) then

!        if(masterproc) print*, 'MSG spatial_mean_2d: found lat ', (spatial_lat(jj)-0.5*res_latlon), &
!         var2d(i,j)%lat, (spatial_lat(jj)+0.5*res_latlon)

        do ii = 1, imax
          if( var2d(i,j)%lon >  (spatial_lon(ii)-0.5*res_latlon) .and. &
              var2d(i,j)%lon <= (spatial_lon(ii)+0.5*res_latlon) ) then

!             if(masterproc) print*, 'MSG spatial_mean_2d: found lon ',(spatial_lon(ii)-0.5*res_latlon),&
!                var2d(i,j)%lon, (spatial_lon(ii)+0.5*res_latlon)
 
             spatial_cnt(ii,jj) = spatial_cnt(ii,jj) + 1.e0  !update total count

             spatial_sum(ii,jj)%rain_rate      = spatial_sum(ii,jj)%rain_rate + var2d(i,j)%rain_rate
             spatial_sum(ii,jj)%h2o_col        = spatial_sum(ii,jj)%h2o_col   + var2d(i,j)%h2o_col
             spatial_sum(ii,jj)%path_cloud_liq = spatial_sum(ii,jj)%path_cloud_liq + var2d(i,j)%path_cloud_liq
             spatial_sum(ii,jj)%path_rain_liq  = spatial_sum(ii,jj)%path_rain_liq  + var2d(i,j)%path_rain_liq
             spatial_sum(ii,jj)%path_cloud_ice = spatial_sum(ii,jj)%path_cloud_ice + var2d(i,j)%path_cloud_ice
             spatial_sum(ii,jj)%path_rain_ice  = spatial_sum(ii,jj)%path_rain_ice  + var2d(i,j)%path_rain_ice
             spatial_sum(ii,jj)%toa_lw_up      = spatial_sum(ii,jj)%toa_lw_up + var2d(i,j)%toa_lw_up
             spatial_sum(ii,jj)%toa_lw_dn      = spatial_sum(ii,jj)%toa_lw_dn + var2d(i,j)%toa_lw_dn
             spatial_sum(ii,jj)%toa_sw_up      = spatial_sum(ii,jj)%toa_sw_up + var2d(i,j)%toa_sw_up
             spatial_sum(ii,jj)%toa_sw_dn      = spatial_sum(ii,jj)%toa_sw_dn + var2d(i,j)%toa_sw_dn
             spatial_sum(ii,jj)%boa_lw_up      = spatial_sum(ii,jj)%boa_lw_up + var2d(i,j)%boa_lw_up
             spatial_sum(ii,jj)%boa_lw_dn      = spatial_sum(ii,jj)%boa_lw_dn + var2d(i,j)%boa_lw_dn
             spatial_sum(ii,jj)%boa_sw_up      = spatial_sum(ii,jj)%boa_sw_up + var2d(i,j)%boa_sw_up
             spatial_sum(ii,jj)%boa_sw_dn      = spatial_sum(ii,jj)%boa_sw_dn + var2d(i,j)%boa_sw_dn

          endif
        enddo
         
        endif within_grid
      enddo


   enddo ; enddo


  endif

 case('finish') !---------------------------------------------------------------------------------------

#if MPI > 0 
!
! MPI reduce to add all together
!
  call mpi_sdsu_reduce( 'SUM', spatial_cnt  )
  call mpi_sdsu_reduce( 'SUM', spatial_sum%rain_rate )
  call mpi_sdsu_reduce( 'SUM', spatial_sum%h2o_col   )
  call mpi_sdsu_reduce( 'SUM', spatial_sum%path_cloud_liq )
  call mpi_sdsu_reduce( 'SUM', spatial_sum%path_rain_liq )
  call mpi_sdsu_reduce( 'SUM', spatial_sum%path_cloud_ice )
  call mpi_sdsu_reduce( 'SUM', spatial_sum%path_rain_ice )
  call mpi_sdsu_reduce( 'SUM', spatial_sum%toa_lw_up )
  call mpi_sdsu_reduce( 'SUM', spatial_sum%toa_lw_dn )
  call mpi_sdsu_reduce( 'SUM', spatial_sum%toa_sw_up )
  call mpi_sdsu_reduce( 'SUM', spatial_sum%toa_sw_dn )
  call mpi_sdsu_reduce( 'SUM', spatial_sum%boa_lw_up )
  call mpi_sdsu_reduce( 'SUM', spatial_sum%boa_lw_dn )
  call mpi_sdsu_reduce( 'SUM', spatial_sum%boa_sw_up )
  call mpi_sdsu_reduce( 'SUM', spatial_sum%boa_sw_dn )

  MPI_MASTERPROC: if(masterproc) then
#endif

  do jj = 1, jmax ; do ii = 1, imax

    if( spatial_cnt(ii,jj) > 0. ) then

     spatial_mean(ii,jj)%rain_rate      = spatial_sum(ii,jj)%rain_rate / spatial_cnt(ii,jj)
     spatial_mean(ii,jj)%h2o_col        = spatial_sum(ii,jj)%h2o_col   / spatial_cnt(ii,jj)
     spatial_mean(ii,jj)%path_cloud_liq = spatial_sum(ii,jj)%path_cloud_liq / spatial_cnt(ii,jj)
     spatial_mean(ii,jj)%path_rain_liq  = spatial_sum(ii,jj)%path_rain_liq  / spatial_cnt(ii,jj)
     spatial_mean(ii,jj)%path_cloud_ice = spatial_sum(ii,jj)%path_cloud_ice / spatial_cnt(ii,jj)
     spatial_mean(ii,jj)%path_rain_ice  = spatial_sum(ii,jj)%path_rain_ice  / spatial_cnt(ii,jj)
     spatial_mean(ii,jj)%toa_lw_up      = spatial_sum(ii,jj)%toa_lw_up / spatial_cnt(ii,jj)
     spatial_mean(ii,jj)%toa_lw_dn      = spatial_sum(ii,jj)%toa_lw_dn / spatial_cnt(ii,jj)
     spatial_mean(ii,jj)%toa_sw_up      = spatial_sum(ii,jj)%toa_sw_up / spatial_cnt(ii,jj)
     spatial_mean(ii,jj)%toa_sw_dn      = spatial_sum(ii,jj)%toa_sw_dn / spatial_cnt(ii,jj)
     spatial_mean(ii,jj)%boa_lw_up      = spatial_sum(ii,jj)%boa_lw_up / spatial_cnt(ii,jj)
     spatial_mean(ii,jj)%boa_lw_dn      = spatial_sum(ii,jj)%boa_lw_dn / spatial_cnt(ii,jj)
     spatial_mean(ii,jj)%boa_sw_up      = spatial_sum(ii,jj)%boa_sw_up / spatial_cnt(ii,jj)
     spatial_mean(ii,jj)%boa_sw_dn      = spatial_sum(ii,jj)%boa_sw_dn / spatial_cnt(ii,jj)
    
    endif

  enddo ; enddo


!
! write out cloud frequency zonal profile
!
 sdsu_io_file = trim(sdsu_dir_output)//'spatial_mean_CRM2D'//trim(output_suffix)//'.grads'
 open(io,file=trim(sdsu_io_file),access='DIRECT',status='replace',recl=imax*jmax*4)
 print("(a)"),'MSG zonal_mean2d: see output ->',trim(sdsu_io_file)

 irec = 1
 write(io,rec=irec) ((spatial_mean(ii,jj)%rain_rate      , ii=1,imax ),jj=1,jmax) ; irec = irec + 1
 write(io,rec=irec) ((spatial_mean(ii,jj)%h2o_col        , ii=1,imax ),jj=1,jmax) ; irec = irec + 1
 write(io,rec=irec) ((spatial_mean(ii,jj)%path_cloud_liq , ii=1,imax ),jj=1,jmax) ; irec = irec + 1
 write(io,rec=irec) ((spatial_mean(ii,jj)%path_rain_liq  , ii=1,imax ),jj=1,jmax) ; irec = irec + 1
 write(io,rec=irec) ((spatial_mean(ii,jj)%path_cloud_ice , ii=1,imax ),jj=1,jmax) ; irec = irec + 1
 write(io,rec=irec) ((spatial_mean(ii,jj)%path_rain_ice  , ii=1,imax ),jj=1,jmax) ; irec = irec + 1
 write(io,rec=irec) ((spatial_mean(ii,jj)%toa_lw_up      , ii=1,imax ),jj=1,jmax) ; irec = irec + 1
 write(io,rec=irec) ((spatial_mean(ii,jj)%toa_lw_dn      , ii=1,imax ),jj=1,jmax) ; irec = irec + 1
 write(io,rec=irec) ((spatial_mean(ii,jj)%toa_sw_up      , ii=1,imax ),jj=1,jmax) ; irec = irec + 1
 write(io,rec=irec) ((spatial_mean(ii,jj)%toa_sw_dn      , ii=1,imax ),jj=1,jmax) ; irec = irec + 1
 write(io,rec=irec) ((spatial_mean(ii,jj)%boa_lw_up      , ii=1,imax ),jj=1,jmax) ; irec = irec + 1
 write(io,rec=irec) ((spatial_mean(ii,jj)%boa_lw_dn      , ii=1,imax ),jj=1,jmax) ; irec = irec + 1
 write(io,rec=irec) ((spatial_mean(ii,jj)%boa_sw_up      , ii=1,imax ),jj=1,jmax) ; irec = irec + 1
 write(io,rec=irec) ((spatial_mean(ii,jj)%boa_sw_dn      , ii=1,imax ),jj=1,jmax) ; irec = irec + 1

 close(io)


!
! write grads control file
!
 sdsu_io_file = trim(sdsu_dir_output)//'spatial_mean_CRM2D'//trim(output_suffix)//'.ctl'
 print("(a)"),'MSG spatial_mean2d: see output.ctl ->',trim(sdsu_io_file)
 open(io,file=trim(sdsu_io_file))

 sdsu_io_file = 'spatial_mean_CRM2D'//trim(output_suffix)//'.grads'
 write(io,*) 'dset  ',trim(sdsu_io_file)
 write(io,*) 'title   spatial mean statistics '
 write(io,*) 'undef   -999.             '
 write(io,*) 'xdef    ',imax,' linear ',minlon,' ',res_latlon
 write(io,*) 'ydef    ',jmax,' linear ',minlat,' ',res_latlon
 write(io,*) 'zdef    1 linear 0. 0.1'
 write(io,*) 'tdef    1 linear ',grads_time,' 1hr'
 write(io,*) 'vars ',irec-1
 write(io,*) 'rain    0  0  rain rate [mm/hr]'
 write(io,*) 'h2ocol  0  0  column water vapro [kg/m2]'
 write(io,*) 'clwp    0  0  cloud liquid water path [kg/m2]'
 write(io,*) 'lrwp    0  0  liquid rain water path [kg/m2]'
 write(io,*) 'ciwp    0  0  cloud ice water path [kg/m2]'
 write(io,*) 'irwp    0  0  ice rain water path [kg/m2]'
 write(io,*) 'lwut   0 0  TOA LW upwelling   flux [W/m2]'
 write(io,*) 'lwdt   0 0  TOA LW downwelling flux [W/m2]'
 write(io,*) 'swut   0 0  TOA SW upwelling flux   [W/m2]'
 write(io,*) 'swdt   0 0  TOA SW downwelling flux [W/m2]'
 write(io,*) 'lwus   0 0  Surface LW upwelling   flux [W/m2]'
 write(io,*) 'lwds   0 0  Surface LW downwelling flux [W/m2]'
 write(io,*) 'swus   0 0  Surface SW upwelling   flux [W/m2]'
 write(io,*) 'swds   0 0  Surface SW downwelling flux [W/m2]'
 write(io,*) 'endvars '
 close(io)


#if MPI > 0 
  endif MPI_MASTERPROC
#endif

 case default ; call stop_sdsu(myrank,'MSG spatial_mean_2d: There is no such proc')
 end select


 return
 end subroutine spatial_mean_2d

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 subroutine spatial_mean_daily_2d(proc,var2d)
 implicit none
!--------------------------------------------------------------------------------------------------
! Comments: Derive daily spatial mean map of 2D parameters.
!           
! History:
! 06/2017  Toshi Matsui@NASA GSFC ; Initial.
!
! References:
!-----------------------------------------------------------------------------------------------------

 character*(*) :: proc ! process
 type ( surface_parameter ), dimension(:,:), optional :: var2d ! input surface parameter assumed 2d array

 integer :: bnd(2)  !upper memory bound for 2D array

!
! statisticsl parameters
!
 type( surface_parameter ), allocatable, dimension(:,:),save :: spatial_sum, spatial_mean
 real(sdsu_fps), allocatable, dimension(:,:),save :: spatial_cnt
 real(sdsu_fps), allocatable, dimension(:),save :: spatial_lat, spatial_lon

 integer,save :: ibin_max
 integer :: i,j, ibin, ierr, irec
 integer,parameter :: io = 149

 integer,save :: imax, jmax
 integer :: ii, jj

 character :: yyyy*4, mm*2, dd*2

 select case(trim(proc))
 case('init') !---------------------------------------------------------------------------------

   if(masterproc) print*, 'MSG spatial_mean_daily_2d: init'

   imax = int( ( maxlon - minlon ) / res_latlon ) + 1
   jmax = int( ( maxlat - minlat ) / res_latlon ) + 1

   if( imax < 1 ) call stop_sdsu(myrank,'MSG spatial_mean_daily_2d: maxlon is smaller than minlon')
   if( jmax < 1 ) call stop_sdsu(myrank,'MSG spatial_mean_daily_2d: maxlat is smaller than minlat')

  
   if( allocated(spatial_sum) ) deallocate(spatial_sum, spatial_cnt, spatial_mean, spatial_lat, spatial_lon)
 
   allocate( spatial_sum(imax,jmax)  , &
             spatial_cnt(imax,jmax)  , &
             spatial_mean(imax,jmax) , &
             spatial_lat(jmax)  , &
             spatial_lon(imax)  , &
             stat=ierr )

   ! initialized
   do j = 1, jmax
      spatial_lat(j) = minlat + res_latlon * real(j-1) ![deg]
   enddo
   do i = 1, imax
      spatial_lon(i) = minlon + res_latlon * real(i-1) ![deg]
   enddo

  !
  ! zero out other parameters
  !
  spatial_cnt = 0.

  spatial_sum%rain_rate      = 0. ; spatial_sum%h2o_col       = 0.
  spatial_sum%path_cloud_liq = 0. ; spatial_sum%path_rain_liq = 0.
  spatial_sum%path_cloud_ice = 0. ; spatial_sum%path_rain_ice = 0.
  spatial_sum%toa_lw_up      = 0. ; spatial_sum%toa_lw_dn     = 0.
  spatial_sum%toa_sw_up      = 0. ; spatial_sum%toa_sw_dn     = 0.
  spatial_sum%boa_lw_up      = 0. ; spatial_sum%boa_lw_dn     = 0.
  spatial_sum%boa_sw_up      = 0. ; spatial_sum%boa_sw_dn     = 0.

  spatial_mean%rain_rate      = undefined ; spatial_mean%h2o_col       = undefined
  spatial_mean%path_cloud_liq = undefined ; spatial_mean%path_rain_liq = undefined
  spatial_mean%path_cloud_ice = undefined ; spatial_mean%path_rain_ice = undefined
  spatial_mean%toa_lw_up      = undefined ; spatial_mean%toa_lw_dn     = undefined
  spatial_mean%toa_sw_up      = undefined ; spatial_mean%toa_sw_dn     = undefined
  spatial_mean%boa_lw_up      = undefined ; spatial_mean%boa_lw_dn     = undefined
  spatial_mean%boa_sw_up      = undefined ; spatial_mean%boa_sw_dn     = undefined


 case('add') !-------------------------------------------------------------------------------------

  if(masterproc) print*, 'MSG spatial_mean_daily_2d: add'

  if(present(var2d) ) then

   !
   ! find upper bound from assumped 3D array
   !
   bnd = UBOUND(var2d)

   do j = 1,bnd(2) ; do i = 1,bnd(1)

      !
      ! loop for latlon_grid domain. 
      !
      do jj = 1, jmax
        within_grid: if( var2d(i,j)%lat >  (spatial_lat(jj)-0.5*res_latlon) .and. &
                         var2d(i,j)%lat <= (spatial_lat(jj)+0.5*res_latlon) ) then

!        if(masterproc) print*, 'MSG spatial_mean_daily_2d: found lat ', (spatial_lat(jj)-0.5*res_latlon), &
!         var2d(i,j)%lat, (spatial_lat(jj)+0.5*res_latlon)

        do ii = 1, imax
          if( var2d(i,j)%lon >  (spatial_lon(ii)-0.5*res_latlon) .and. &
              var2d(i,j)%lon <= (spatial_lon(ii)+0.5*res_latlon) ) then

!             if(masterproc) print*, 'MSG spatial_mean_daily_2d: found lon ',(spatial_lon(ii)-0.5*res_latlon),&
!                var2d(i,j)%lon, (spatial_lon(ii)+0.5*res_latlon)
 
             spatial_cnt(ii,jj) = spatial_cnt(ii,jj) + 1.e0  !update total count

             spatial_sum(ii,jj)%rain_rate      = spatial_sum(ii,jj)%rain_rate + var2d(i,j)%rain_rate
             spatial_sum(ii,jj)%h2o_col        = spatial_sum(ii,jj)%h2o_col   + var2d(i,j)%h2o_col
             spatial_sum(ii,jj)%path_cloud_liq = spatial_sum(ii,jj)%path_cloud_liq + var2d(i,j)%path_cloud_liq
             spatial_sum(ii,jj)%path_rain_liq  = spatial_sum(ii,jj)%path_rain_liq  + var2d(i,j)%path_rain_liq
             spatial_sum(ii,jj)%path_cloud_ice = spatial_sum(ii,jj)%path_cloud_ice + var2d(i,j)%path_cloud_ice
             spatial_sum(ii,jj)%path_rain_ice  = spatial_sum(ii,jj)%path_rain_ice  + var2d(i,j)%path_rain_ice
             spatial_sum(ii,jj)%toa_lw_up      = spatial_sum(ii,jj)%toa_lw_up + var2d(i,j)%toa_lw_up
             spatial_sum(ii,jj)%toa_lw_dn      = spatial_sum(ii,jj)%toa_lw_dn + var2d(i,j)%toa_lw_dn
             spatial_sum(ii,jj)%toa_sw_up      = spatial_sum(ii,jj)%toa_sw_up + var2d(i,j)%toa_sw_up
             spatial_sum(ii,jj)%toa_sw_dn      = spatial_sum(ii,jj)%toa_sw_dn + var2d(i,j)%toa_sw_dn
             spatial_sum(ii,jj)%boa_lw_up      = spatial_sum(ii,jj)%boa_lw_up + var2d(i,j)%boa_lw_up
             spatial_sum(ii,jj)%boa_lw_dn      = spatial_sum(ii,jj)%boa_lw_dn + var2d(i,j)%boa_lw_dn
             spatial_sum(ii,jj)%boa_sw_up      = spatial_sum(ii,jj)%boa_sw_up + var2d(i,j)%boa_sw_up
             spatial_sum(ii,jj)%boa_sw_dn      = spatial_sum(ii,jj)%boa_sw_dn + var2d(i,j)%boa_sw_dn

          endif
        enddo
         
        endif within_grid
      enddo


   enddo ; enddo


  endif

 case('finish') !---------------------------------------------------------------------------------------

#if MPI > 0 
!
! MPI reduce to add all together
!
  call mpi_sdsu_reduce( 'SUM', spatial_cnt  )
  call mpi_sdsu_reduce( 'SUM', spatial_sum%rain_rate )
  call mpi_sdsu_reduce( 'SUM', spatial_sum%h2o_col   )
  call mpi_sdsu_reduce( 'SUM', spatial_sum%path_cloud_liq )
  call mpi_sdsu_reduce( 'SUM', spatial_sum%path_rain_liq )
  call mpi_sdsu_reduce( 'SUM', spatial_sum%path_cloud_ice )
  call mpi_sdsu_reduce( 'SUM', spatial_sum%path_rain_ice )
  call mpi_sdsu_reduce( 'SUM', spatial_sum%toa_lw_up )
  call mpi_sdsu_reduce( 'SUM', spatial_sum%toa_lw_dn )
  call mpi_sdsu_reduce( 'SUM', spatial_sum%toa_sw_up )
  call mpi_sdsu_reduce( 'SUM', spatial_sum%toa_sw_dn )
  call mpi_sdsu_reduce( 'SUM', spatial_sum%boa_lw_up )
  call mpi_sdsu_reduce( 'SUM', spatial_sum%boa_lw_dn )
  call mpi_sdsu_reduce( 'SUM', spatial_sum%boa_sw_up )
  call mpi_sdsu_reduce( 'SUM', spatial_sum%boa_sw_dn )

  MPI_MASTERPROC: if(masterproc) then
#endif

  do jj = 1, jmax ; do ii = 1, imax

    if( spatial_cnt(ii,jj) > 0. ) then

     spatial_mean(ii,jj)%rain_rate      = spatial_sum(ii,jj)%rain_rate / spatial_cnt(ii,jj)
     spatial_mean(ii,jj)%h2o_col        = spatial_sum(ii,jj)%h2o_col   / spatial_cnt(ii,jj)
     spatial_mean(ii,jj)%path_cloud_liq = spatial_sum(ii,jj)%path_cloud_liq / spatial_cnt(ii,jj)
     spatial_mean(ii,jj)%path_rain_liq  = spatial_sum(ii,jj)%path_rain_liq  / spatial_cnt(ii,jj)
     spatial_mean(ii,jj)%path_cloud_ice = spatial_sum(ii,jj)%path_cloud_ice / spatial_cnt(ii,jj)
     spatial_mean(ii,jj)%path_rain_ice  = spatial_sum(ii,jj)%path_rain_ice  / spatial_cnt(ii,jj)
     spatial_mean(ii,jj)%toa_lw_up      = spatial_sum(ii,jj)%toa_lw_up / spatial_cnt(ii,jj)
     spatial_mean(ii,jj)%toa_lw_dn      = spatial_sum(ii,jj)%toa_lw_dn / spatial_cnt(ii,jj)
     spatial_mean(ii,jj)%toa_sw_up      = spatial_sum(ii,jj)%toa_sw_up / spatial_cnt(ii,jj)
     spatial_mean(ii,jj)%toa_sw_dn      = spatial_sum(ii,jj)%toa_sw_dn / spatial_cnt(ii,jj)
     spatial_mean(ii,jj)%boa_lw_up      = spatial_sum(ii,jj)%boa_lw_up / spatial_cnt(ii,jj)
     spatial_mean(ii,jj)%boa_lw_dn      = spatial_sum(ii,jj)%boa_lw_dn / spatial_cnt(ii,jj)
     spatial_mean(ii,jj)%boa_sw_up      = spatial_sum(ii,jj)%boa_sw_up / spatial_cnt(ii,jj)
     spatial_mean(ii,jj)%boa_sw_dn      = spatial_sum(ii,jj)%boa_sw_dn / spatial_cnt(ii,jj)
    
    endif

  enddo ; enddo


!
! write out cloud frequency diurnal profile
!
 write(yyyy,"(I4.4)") int( sdsu_yyyy )
 write(mm  ,"(I2.2)") int( sdsu_mm   )
 write(dd  ,"(I2.2)") int( sdsu_dd   )

!
! write out cloud frequency zonal profile
!
 sdsu_io_file = trim(sdsu_dir_output)//'spatial_mean_CRM2D_'//yyyy//'_'//mm//'_'//dd&
                 //trim(output_suffix)//'.grads'
 open(io,file=trim(sdsu_io_file),access='DIRECT',status='replace',recl=imax*jmax*4)
 print("(a)"),'MSG spatial_mean_daily_2d: see output ->',trim(sdsu_io_file)

 irec = 1
 write(io,rec=irec) ((spatial_mean(ii,jj)%rain_rate      , ii=1,imax ),jj=1,jmax) ; irec = irec + 1
 write(io,rec=irec) ((spatial_mean(ii,jj)%h2o_col        , ii=1,imax ),jj=1,jmax) ; irec = irec + 1
 write(io,rec=irec) ((spatial_mean(ii,jj)%path_cloud_liq , ii=1,imax ),jj=1,jmax) ; irec = irec + 1
 write(io,rec=irec) ((spatial_mean(ii,jj)%path_rain_liq  , ii=1,imax ),jj=1,jmax) ; irec = irec + 1
 write(io,rec=irec) ((spatial_mean(ii,jj)%path_cloud_ice , ii=1,imax ),jj=1,jmax) ; irec = irec + 1
 write(io,rec=irec) ((spatial_mean(ii,jj)%path_rain_ice  , ii=1,imax ),jj=1,jmax) ; irec = irec + 1
 write(io,rec=irec) ((spatial_mean(ii,jj)%toa_lw_up      , ii=1,imax ),jj=1,jmax) ; irec = irec + 1
 write(io,rec=irec) ((spatial_mean(ii,jj)%toa_lw_dn      , ii=1,imax ),jj=1,jmax) ; irec = irec + 1
 write(io,rec=irec) ((spatial_mean(ii,jj)%toa_sw_up      , ii=1,imax ),jj=1,jmax) ; irec = irec + 1
 write(io,rec=irec) ((spatial_mean(ii,jj)%toa_sw_dn      , ii=1,imax ),jj=1,jmax) ; irec = irec + 1
 write(io,rec=irec) ((spatial_mean(ii,jj)%boa_lw_up      , ii=1,imax ),jj=1,jmax) ; irec = irec + 1
 write(io,rec=irec) ((spatial_mean(ii,jj)%boa_lw_dn      , ii=1,imax ),jj=1,jmax) ; irec = irec + 1
 write(io,rec=irec) ((spatial_mean(ii,jj)%boa_sw_up      , ii=1,imax ),jj=1,jmax) ; irec = irec + 1
 write(io,rec=irec) ((spatial_mean(ii,jj)%boa_sw_dn      , ii=1,imax ),jj=1,jmax) ; irec = irec + 1

 close(io)


!
! write grads control file
!
 sdsu_io_file = trim(sdsu_dir_output)//'spatial_mean_CRM2D_'//yyyy//'_'//mm//'_'//dd&
                //trim(output_suffix)//'.ctl'
 print("(a)"),'MSG spatial_mean_daily_2d: see output.ctl ->',trim(sdsu_io_file)
 open(io,file=trim(sdsu_io_file))

 sdsu_io_file = 'spatial_mean_CRM2D_'//yyyy//'_'//mm//'_'//dd&
                 //trim(output_suffix)//'.grads'
 write(io,*) 'dset  ',trim(sdsu_io_file)
 write(io,*) 'title   spatial mean statistics '
 write(io,*) 'undef   -999.             '
 write(io,*) 'xdef    ',imax,' linear ',minlon,' ',res_latlon
 write(io,*) 'ydef    ',jmax,' linear ',minlat,' ',res_latlon
 write(io,*) 'zdef    1 linear 0. 0.1'
 write(io,*) 'tdef    1 linear ',grads_time,' 1hr'
 write(io,*) 'vars ',irec-1
 write(io,*) 'rain    0  0  rain rate [mm/hr]'
 write(io,*) 'h2ocol  0  0  column water vapro [kg/m2]'
 write(io,*) 'clwp    0  0  cloud liquid water path [kg/m2]'
 write(io,*) 'lrwp    0  0  liquid rain water path [kg/m2]'
 write(io,*) 'ciwp    0  0  cloud ice water path [kg/m2]'
 write(io,*) 'irwp    0  0  ice rain water path [kg/m2]'
 write(io,*) 'lwut   0 0  TOA LW upwelling   flux [W/m2]'
 write(io,*) 'lwdt   0 0  TOA LW downwelling flux [W/m2]'
 write(io,*) 'swut   0 0  TOA SW upwelling flux   [W/m2]'
 write(io,*) 'swdt   0 0  TOA SW downwelling flux [W/m2]'
 write(io,*) 'lwus   0 0  Surface LW upwelling   flux [W/m2]'
 write(io,*) 'lwds   0 0  Surface LW downwelling flux [W/m2]'
 write(io,*) 'swus   0 0  Surface SW upwelling   flux [W/m2]'
 write(io,*) 'swds   0 0  Surface SW downwelling flux [W/m2]'
 write(io,*) 'endvars '
 close(io)

#if MPI > 0 
  endif MPI_MASTERPROC
#endif

 case default ; call stop_sdsu(myrank,'MSG spatial_mean_daily_2d: There is no such proc')
 end select

 return
 end subroutine spatial_mean_daily_2d

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 subroutine spatial_mean_hourly_2d(proc,var2d)
 implicit none
!--------------------------------------------------------------------------------------------------
! Comments: Derive hourly spatial mean map of 2D parameters.
!           
! History:
! 06/2017  Toshi Matsui@NASA GSFC ; Initial.
!
! References:
!-----------------------------------------------------------------------------------------------------

 character*(*) :: proc ! process
 type ( surface_parameter ), dimension(:,:), optional :: var2d ! input surface parameter assumed 2d array

 integer :: bnd(2)  !upper memory bound for 2D array

!
! statisticsl parameters
!
 type( surface_parameter ), allocatable, dimension(:,:),save :: spatial_sum, spatial_mean
 real(sdsu_fps), allocatable, dimension(:,:),save :: spatial_cnt
 real(sdsu_fps), allocatable, dimension(:),save :: spatial_lat, spatial_lon

 integer,save :: ibin_max
 integer :: i,j, ibin, ierr, irec
 integer,parameter :: io = 149

 integer,save :: imax, jmax
 integer :: ii, jj

 character :: yyyy*4, mm*2, dd*2, hh*2

 select case(trim(proc))
 case('init') !---------------------------------------------------------------------------------

   if(masterproc) print*, 'MSG spatial_mean_hourly_2d: init'

   imax = int( ( maxlon - minlon ) / res_latlon ) + 1
   jmax = int( ( maxlat - minlat ) / res_latlon ) + 1

   if( imax < 1 ) call stop_sdsu(myrank,'MSG spatial_mean_hourly_2d: maxlon is smaller than minlon')
   if( jmax < 1 ) call stop_sdsu(myrank,'MSG spatial_mean_hourly_2d: maxlat is smaller than minlat')

  
   if( allocated(spatial_sum) ) deallocate(spatial_sum, spatial_cnt, spatial_mean, spatial_lat, spatial_lon)
 
   allocate( spatial_sum(imax,jmax)  , &
             spatial_cnt(imax,jmax)  , &
             spatial_mean(imax,jmax) , &
             spatial_lat(jmax)  , &
             spatial_lon(imax)  , &
             stat=ierr )

   ! initialized
   do j = 1, jmax
      spatial_lat(j) = minlat + res_latlon * real(j-1) ![deg]
   enddo
   do i = 1, imax
      spatial_lon(i) = minlon + res_latlon * real(i-1) ![deg]
   enddo

  !
  ! zero out other parameters
  !
  spatial_cnt = 0.

  spatial_sum%rain_rate      = 0. ; spatial_sum%h2o_col       = 0.
  spatial_sum%path_cloud_liq = 0. ; spatial_sum%path_rain_liq = 0.
  spatial_sum%path_cloud_ice = 0. ; spatial_sum%path_rain_ice = 0.
  spatial_sum%toa_lw_up      = 0. ; spatial_sum%toa_lw_dn     = 0.
  spatial_sum%toa_sw_up      = 0. ; spatial_sum%toa_sw_dn     = 0.
  spatial_sum%boa_lw_up      = 0. ; spatial_sum%boa_lw_dn     = 0.
  spatial_sum%boa_sw_up      = 0. ; spatial_sum%boa_sw_dn     = 0.

  spatial_mean%rain_rate      = undefined ; spatial_mean%h2o_col       = undefined
  spatial_mean%path_cloud_liq = undefined ; spatial_mean%path_rain_liq = undefined
  spatial_mean%path_cloud_ice = undefined ; spatial_mean%path_rain_ice = undefined
  spatial_mean%toa_lw_up      = undefined ; spatial_mean%toa_lw_dn     = undefined
  spatial_mean%toa_sw_up      = undefined ; spatial_mean%toa_sw_dn     = undefined
  spatial_mean%boa_lw_up      = undefined ; spatial_mean%boa_lw_dn     = undefined
  spatial_mean%boa_sw_up      = undefined ; spatial_mean%boa_sw_dn     = undefined


 case('add') !-------------------------------------------------------------------------------------

  if(masterproc) print*, 'MSG spatial_mean_hourly_2d: add'

  if(present(var2d) ) then

   !
   ! find upper bound from assumped 3D array
   !
   bnd = UBOUND(var2d)

   do j = 1,bnd(2) ; do i = 1,bnd(1)

      !
      ! loop for latlon_grid domain. 
      !
      do jj = 1, jmax
        within_grid: if( var2d(i,j)%lat >  (spatial_lat(jj)-0.5*res_latlon) .and. &
                         var2d(i,j)%lat <= (spatial_lat(jj)+0.5*res_latlon) ) then

!        if(masterproc) print*, 'MSG spatial_mean_hourly_2d: found lat ', (spatial_lat(jj)-0.5*res_latlon), &
!         var2d(i,j)%lat, (spatial_lat(jj)+0.5*res_latlon)

        do ii = 1, imax
          if( var2d(i,j)%lon >  (spatial_lon(ii)-0.5*res_latlon) .and. &
              var2d(i,j)%lon <= (spatial_lon(ii)+0.5*res_latlon) ) then

!             if(masterproc) print*, 'MSG spatial_mean_hourly_2d: found lon ',(spatial_lon(ii)-0.5*res_latlon),&
!                var2d(i,j)%lon, (spatial_lon(ii)+0.5*res_latlon)
 
             spatial_cnt(ii,jj) = spatial_cnt(ii,jj) + 1.e0  !update total count

             spatial_sum(ii,jj)%rain_rate      = spatial_sum(ii,jj)%rain_rate + var2d(i,j)%rain_rate
             spatial_sum(ii,jj)%h2o_col        = spatial_sum(ii,jj)%h2o_col   + var2d(i,j)%h2o_col
             spatial_sum(ii,jj)%path_cloud_liq = spatial_sum(ii,jj)%path_cloud_liq + var2d(i,j)%path_cloud_liq
             spatial_sum(ii,jj)%path_rain_liq  = spatial_sum(ii,jj)%path_rain_liq  + var2d(i,j)%path_rain_liq
             spatial_sum(ii,jj)%path_cloud_ice = spatial_sum(ii,jj)%path_cloud_ice + var2d(i,j)%path_cloud_ice
             spatial_sum(ii,jj)%path_rain_ice  = spatial_sum(ii,jj)%path_rain_ice  + var2d(i,j)%path_rain_ice
             spatial_sum(ii,jj)%toa_lw_up      = spatial_sum(ii,jj)%toa_lw_up + var2d(i,j)%toa_lw_up
             spatial_sum(ii,jj)%toa_lw_dn      = spatial_sum(ii,jj)%toa_lw_dn + var2d(i,j)%toa_lw_dn
             spatial_sum(ii,jj)%toa_sw_up      = spatial_sum(ii,jj)%toa_sw_up + var2d(i,j)%toa_sw_up
             spatial_sum(ii,jj)%toa_sw_dn      = spatial_sum(ii,jj)%toa_sw_dn + var2d(i,j)%toa_sw_dn
             spatial_sum(ii,jj)%boa_lw_up      = spatial_sum(ii,jj)%boa_lw_up + var2d(i,j)%boa_lw_up
             spatial_sum(ii,jj)%boa_lw_dn      = spatial_sum(ii,jj)%boa_lw_dn + var2d(i,j)%boa_lw_dn
             spatial_sum(ii,jj)%boa_sw_up      = spatial_sum(ii,jj)%boa_sw_up + var2d(i,j)%boa_sw_up
             spatial_sum(ii,jj)%boa_sw_dn      = spatial_sum(ii,jj)%boa_sw_dn + var2d(i,j)%boa_sw_dn

          endif
        enddo
         
        endif within_grid
      enddo


   enddo ; enddo


  endif

 case('finish') !---------------------------------------------------------------------------------------

#if MPI > 0 
!
! MPI reduce to add all together
!
  call mpi_sdsu_reduce( 'SUM', spatial_cnt  )
  call mpi_sdsu_reduce( 'SUM', spatial_sum%rain_rate )
  call mpi_sdsu_reduce( 'SUM', spatial_sum%h2o_col   )
  call mpi_sdsu_reduce( 'SUM', spatial_sum%path_cloud_liq )
  call mpi_sdsu_reduce( 'SUM', spatial_sum%path_rain_liq )
  call mpi_sdsu_reduce( 'SUM', spatial_sum%path_cloud_ice )
  call mpi_sdsu_reduce( 'SUM', spatial_sum%path_rain_ice )
  call mpi_sdsu_reduce( 'SUM', spatial_sum%toa_lw_up )
  call mpi_sdsu_reduce( 'SUM', spatial_sum%toa_lw_dn )
  call mpi_sdsu_reduce( 'SUM', spatial_sum%toa_sw_up )
  call mpi_sdsu_reduce( 'SUM', spatial_sum%toa_sw_dn )
  call mpi_sdsu_reduce( 'SUM', spatial_sum%boa_lw_up )
  call mpi_sdsu_reduce( 'SUM', spatial_sum%boa_lw_dn )
  call mpi_sdsu_reduce( 'SUM', spatial_sum%boa_sw_up )
  call mpi_sdsu_reduce( 'SUM', spatial_sum%boa_sw_dn )

  MPI_MASTERPROC: if(masterproc) then
#endif

  do jj = 1, jmax ; do ii = 1, imax

    if( spatial_cnt(ii,jj) > 0. ) then

     spatial_mean(ii,jj)%rain_rate      = spatial_sum(ii,jj)%rain_rate / spatial_cnt(ii,jj)
     spatial_mean(ii,jj)%h2o_col        = spatial_sum(ii,jj)%h2o_col   / spatial_cnt(ii,jj)
     spatial_mean(ii,jj)%path_cloud_liq = spatial_sum(ii,jj)%path_cloud_liq / spatial_cnt(ii,jj)
     spatial_mean(ii,jj)%path_rain_liq  = spatial_sum(ii,jj)%path_rain_liq  / spatial_cnt(ii,jj)
     spatial_mean(ii,jj)%path_cloud_ice = spatial_sum(ii,jj)%path_cloud_ice / spatial_cnt(ii,jj)
     spatial_mean(ii,jj)%path_rain_ice  = spatial_sum(ii,jj)%path_rain_ice  / spatial_cnt(ii,jj)
     spatial_mean(ii,jj)%toa_lw_up      = spatial_sum(ii,jj)%toa_lw_up / spatial_cnt(ii,jj)
     spatial_mean(ii,jj)%toa_lw_dn      = spatial_sum(ii,jj)%toa_lw_dn / spatial_cnt(ii,jj)
     spatial_mean(ii,jj)%toa_sw_up      = spatial_sum(ii,jj)%toa_sw_up / spatial_cnt(ii,jj)
     spatial_mean(ii,jj)%toa_sw_dn      = spatial_sum(ii,jj)%toa_sw_dn / spatial_cnt(ii,jj)
     spatial_mean(ii,jj)%boa_lw_up      = spatial_sum(ii,jj)%boa_lw_up / spatial_cnt(ii,jj)
     spatial_mean(ii,jj)%boa_lw_dn      = spatial_sum(ii,jj)%boa_lw_dn / spatial_cnt(ii,jj)
     spatial_mean(ii,jj)%boa_sw_up      = spatial_sum(ii,jj)%boa_sw_up / spatial_cnt(ii,jj)
     spatial_mean(ii,jj)%boa_sw_dn      = spatial_sum(ii,jj)%boa_sw_dn / spatial_cnt(ii,jj)
    
    endif

  enddo ; enddo


!
! write out cloud frequency diurnal profile
!
 write(yyyy,"(I4.4)") int( sdsu_yyyy )
 write(mm  ,"(I2.2)") int( sdsu_mm   )
 write(dd  ,"(I2.2)") int( sdsu_dd   )
 write(hh  ,"(I2.2)") int( sdsu_hh   )

!
! write out binary data
!
 sdsu_io_file = trim(sdsu_dir_output)//'spatial_mean_CRM2D_'//yyyy//'_'//mm//'_'//dd//'_'//hh&
                 //trim(output_suffix)//'.grads'
 open(io,file=trim(sdsu_io_file),access='DIRECT',status='replace',recl=imax*jmax*4)
 print("(a)"),'MSG spatial_mean_hourly_2d: see output ->',trim(sdsu_io_file)

 irec = 1
 write(io,rec=irec) ((spatial_mean(ii,jj)%rain_rate      , ii=1,imax ),jj=1,jmax) ; irec = irec + 1
 write(io,rec=irec) ((spatial_mean(ii,jj)%h2o_col        , ii=1,imax ),jj=1,jmax) ; irec = irec + 1
 write(io,rec=irec) ((spatial_mean(ii,jj)%path_cloud_liq , ii=1,imax ),jj=1,jmax) ; irec = irec + 1
 write(io,rec=irec) ((spatial_mean(ii,jj)%path_rain_liq  , ii=1,imax ),jj=1,jmax) ; irec = irec + 1
 write(io,rec=irec) ((spatial_mean(ii,jj)%path_cloud_ice , ii=1,imax ),jj=1,jmax) ; irec = irec + 1
 write(io,rec=irec) ((spatial_mean(ii,jj)%path_rain_ice  , ii=1,imax ),jj=1,jmax) ; irec = irec + 1
 write(io,rec=irec) ((spatial_mean(ii,jj)%toa_lw_up      , ii=1,imax ),jj=1,jmax) ; irec = irec + 1
 write(io,rec=irec) ((spatial_mean(ii,jj)%toa_lw_dn      , ii=1,imax ),jj=1,jmax) ; irec = irec + 1
 write(io,rec=irec) ((spatial_mean(ii,jj)%toa_sw_up      , ii=1,imax ),jj=1,jmax) ; irec = irec + 1
 write(io,rec=irec) ((spatial_mean(ii,jj)%toa_sw_dn      , ii=1,imax ),jj=1,jmax) ; irec = irec + 1
 write(io,rec=irec) ((spatial_mean(ii,jj)%boa_lw_up      , ii=1,imax ),jj=1,jmax) ; irec = irec + 1
 write(io,rec=irec) ((spatial_mean(ii,jj)%boa_lw_dn      , ii=1,imax ),jj=1,jmax) ; irec = irec + 1
 write(io,rec=irec) ((spatial_mean(ii,jj)%boa_sw_up      , ii=1,imax ),jj=1,jmax) ; irec = irec + 1
 write(io,rec=irec) ((spatial_mean(ii,jj)%boa_sw_dn      , ii=1,imax ),jj=1,jmax) ; irec = irec + 1

 close(io)


!
! write grads control file
!
 sdsu_io_file = trim(sdsu_dir_output)//'spatial_mean_CRM2D_'//yyyy//'_'//mm//'_'//dd//'_'//hh &
                //trim(output_suffix)//'.ctl'
 print("(a)"),'MSG spatial_mean_hourly_2d: see output.ctl ->',trim(sdsu_io_file)
 open(io,file=trim(sdsu_io_file))

 sdsu_io_file = 'spatial_mean_CRM2D_'//yyyy//'_'//mm//'_'//dd//'_'//hh&
                 //trim(output_suffix)//'.grads'
 write(io,*) 'dset  ',trim(sdsu_io_file)
 write(io,*) 'title   spatial mean statistics '
 write(io,*) 'undef   -999.             '
 write(io,*) 'xdef    ',imax,' linear ',minlon,' ',res_latlon
 write(io,*) 'ydef    ',jmax,' linear ',minlat,' ',res_latlon
 write(io,*) 'zdef    1 linear 0. 0.1'
 write(io,*) 'tdef    1 linear ',grads_time,' 1hr'
 write(io,*) 'vars ',irec-1
 write(io,*) 'rain    0  0  rain rate [mm/hr]'
 write(io,*) 'h2ocol  0  0  column water vapro [kg/m2]'
 write(io,*) 'clwp    0  0  cloud liquid water path [kg/m2]'
 write(io,*) 'lrwp    0  0  liquid rain water path [kg/m2]'
 write(io,*) 'ciwp    0  0  cloud ice water path [kg/m2]'
 write(io,*) 'irwp    0  0  ice rain water path [kg/m2]'
 write(io,*) 'lwut   0 0  TOA LW upwelling   flux [W/m2]'
 write(io,*) 'lwdt   0 0  TOA LW downwelling flux [W/m2]'
 write(io,*) 'swut   0 0  TOA SW upwelling flux   [W/m2]'
 write(io,*) 'swdt   0 0  TOA SW downwelling flux [W/m2]'
 write(io,*) 'lwus   0 0  Surface LW upwelling   flux [W/m2]'
 write(io,*) 'lwds   0 0  Surface LW downwelling flux [W/m2]'
 write(io,*) 'swus   0 0  Surface SW upwelling   flux [W/m2]'
 write(io,*) 'swds   0 0  Surface SW downwelling flux [W/m2]'
 write(io,*) 'endvars '
 close(io)

#if MPI > 0 
  endif MPI_MASTERPROC
#endif

 case default ; call stop_sdsu(myrank,'MSG spatial_mean_hourly_2d: There is no such proc')
 end select

 return
 end subroutine spatial_mean_hourly_2d

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 subroutine spatial_mean_3d(proc,var3d,var2d)
 implicit none
!--------------------------------------------------------------------------------------------------
! Comments: Derive spatial mean map of 2D parameters.
!           
! History:
! 11/2016  Toshi Matsui@NASA GSFC ; Initial.
!
! References:
!-----------------------------------------------------------------------------------------------------

 character*(*) :: proc ! process
 type ( atmos_parameter ), dimension(:,:,:), optional :: var3d ! input surface parameter assumed 3d array
 type ( surface_parameter ), dimension(:,:), optional :: var2d ! input surface parameter assumed 3d array

 integer :: bnd(3)  !upper memory bound for 2D array

!
! statisticsl parameters
!
 type( atmos_parameter ), allocatable, dimension(:,:,:),save :: spatial_sum, spatial_mean
 real(sdsu_fps), allocatable, dimension(:,:,:),save :: spatial_cnt
 real(sdsu_fps), allocatable, dimension(:),save :: spatial_lat, spatial_lon

 integer,save :: ibin_max
 integer :: i,j,k, ibin, ierr, irec
 integer,parameter :: io = 149


 integer,parameter :: play = 20, plev = 21
 real(sdsu_fps),parameter :: pressure_lay(play) =    (/1000.,950.,900.,850.,800.,750.,700.,650.,600.,550.,500.,&
                                                     450.,400.,350.,300.,250.,200.,150.,100.,50./)
 real(sdsu_fps),parameter :: pressure_lev(plev) = (/1025., 975.,925.,875.,825.,775.,725.,675.,625.,575.,525.,475.,&
                                                        425.,375.,325.,275.,225.,175.,125.,75.,25./)

 integer,save :: imax, jmax, kmax
 integer :: ii, jj, kk

 real(sdsu_fps) :: pr


 select case(trim(proc))
 case('init') !---------------------------------------------------------------------------------

   if(masterproc) print*, 'MSG spatial_mean_3d: init'

   imax = int( ( maxlon - minlon ) / res_latlon ) + 1
   jmax = int( ( maxlat - minlat ) / res_latlon ) + 1
   kmax = play

   if( imax < 1 ) call stop_sdsu(myrank,'MSG spatial_mean_3d: maxlon is smaller than minlon')
   if( jmax < 1 ) call stop_sdsu(myrank,'MSG spatial_mean_3d: maxlat is smaller than minlat')

   if( allocated(spatial_sum) ) deallocate(spatial_sum, spatial_cnt, spatial_mean, spatial_lat, spatial_lon)

   allocate( spatial_sum(imax,jmax,kmax)  , &
             spatial_cnt(imax,jmax,kmax)  , &
             spatial_mean(imax,jmax,kmax) , &
             spatial_lat(jmax)  , &
             spatial_lon(imax)  , &
             stat=ierr )

   ! initialized
   do j = 1, jmax
      spatial_lat(j) = minlat + res_latlon * real(j-1) ![deg]
   enddo
   do i = 1, imax
      spatial_lon(i) = minlon + res_latlon * real(i-1) ![deg]
   enddo

  !
  ! zero out other parameters
  !
  spatial_cnt = 0.

  spatial_sum%rain_rate      = 0. ; spatial_sum%t_air       = 0.
  spatial_sum%sh             = 0. ; spatial_sum%u           = 0.
  spatial_sum%v              = 0. ; spatial_sum%w           = 0.
  spatial_sum%lw_heat        = 0. ; spatial_sum%sw_heat     = 0.
  spatial_sum%tot_cloud      = 0.

  spatial_mean%rain_rate      = undefined ; spatial_mean%t_air       = undefined
  spatial_mean%sh             = undefined ; spatial_mean%u           = undefined
  spatial_mean%v              = undefined ; spatial_mean%w           = undefined
  spatial_mean%lw_heat        = undefined ; spatial_mean%sw_heat     = undefined
  spatial_mean%tot_cloud      = undefined 

 case('add') !-------------------------------------------------------------------------------------

  if(masterproc) print*, 'MSG spatial_mean_3d: add'

  if(present(var3d) ) then

   !
   ! find upper bound from assumped 3D array
   !
   bnd = UBOUND(var3d)

   do j = 1,bnd(2) ; do i = 1,bnd(1)

      !
      ! loop for latlon_grid domain. 
      !
      do jj = 1, jmax
        within_grid: if( var2d(i,j)%lat >  (spatial_lat(jj)-0.5*res_latlon) .and. &
                         var2d(i,j)%lat <= (spatial_lat(jj)+0.5*res_latlon) ) then

!        if(masterproc) print*, 'MSG spatial_mean_2d: found lat ', (spatial_lat(jj)-0.5*res_latlon), &
!         var2d(i,j)%lat, (spatial_lat(jj)+0.5*res_latlon)

        do ii = 1, imax
          if( var2d(i,j)%lon >  (spatial_lon(ii)-0.5*res_latlon) .and. &
              var2d(i,j)%lon <= (spatial_lon(ii)+0.5*res_latlon) ) then

!             if(masterproc) print*, 'MSG spatial_mean_2d: found lon ',(spatial_lon(ii)-0.5*res_latlon),&
!                var2d(i,j)%lon, (spatial_lon(ii)+0.5*res_latlon)

           do kk = 1, kmax

               do k = 1, bnd(3)
                  if( var3d(i,j,k)%press <= pressure_lev(kk) .and. &
                      var3d(i,j,k)%press >  pressure_lev(kk+1)  ) then

                    spatial_cnt(ii,jj,kk) = spatial_cnt(ii,jj,kk) + 1.e0  !update total count
                    spatial_sum(ii,jj,kk)%rain_rate  = spatial_sum(ii,jj,kk)%rain_rate + var3d(i,j,k)%rain_rate
                    spatial_sum(ii,jj,kk)%t_air      = spatial_sum(ii,jj,kk)%t_air     + var3d(i,j,k)%t_air
                    spatial_sum(ii,jj,kk)%sh         = spatial_sum(ii,jj,kk)%sh        + var3d(i,j,k)%sh
                    spatial_sum(ii,jj,kk)%u          = spatial_sum(ii,jj,kk)%u         + var3d(i,j,k)%u
                    spatial_sum(ii,jj,kk)%v          = spatial_sum(ii,jj,kk)%v         + var3d(i,j,k)%v
                    spatial_sum(ii,jj,kk)%w          = spatial_sum(ii,jj,kk)%w         + var3d(i,j,k)%w
                    spatial_sum(ii,jj,kk)%lw_heat    = spatial_sum(ii,jj,kk)%lw_heat   + var3d(i,j,k)%lw_heat
                    spatial_sum(ii,jj,kk)%sw_heat    = spatial_sum(ii,jj,kk)%sw_heat   + var3d(i,j,k)%sw_heat
                    spatial_sum(ii,jj,kk)%tot_cloud  = spatial_sum(ii,jj,kk)%tot_cloud + var3d(i,j,k)%tot_cloud

                  endif
               enddo

           enddo
           endif
        enddo

        endif within_grid
      enddo


   enddo ; enddo


  endif

 case('finish') !---------------------------------------------------------------------------------------

#if MPI > 0 
!
! MPI reduce to add all together
!
  call mpi_sdsu_reduce( 'SUM', spatial_cnt  )
  call mpi_sdsu_reduce( 'SUM', spatial_sum%rain_rate )
  call mpi_sdsu_reduce( 'SUM', spatial_sum%t_air )
  call mpi_sdsu_reduce( 'SUM', spatial_sum%sh   )
  call mpi_sdsu_reduce( 'SUM', spatial_sum%u )
  call mpi_sdsu_reduce( 'SUM', spatial_sum%v )
  call mpi_sdsu_reduce( 'SUM', spatial_sum%w )
  call mpi_sdsu_reduce( 'SUM', spatial_sum%lw_heat )
  call mpi_sdsu_reduce( 'SUM', spatial_sum%sw_heat )
  call mpi_sdsu_reduce( 'SUM', spatial_sum%tot_cloud )

  MPI_MASTERPROC: if(masterproc) then
#endif

  do kk = 1, kmax ; do jj = 1, jmax ; do ii = 1, imax 

    if( spatial_cnt(ii,jj,kk) > 0. ) then

     spatial_mean(ii,jj,kk)%rain_rate = spatial_sum(ii,jj,kk)%rain_rate / spatial_cnt(ii,jj,kk)
     spatial_mean(ii,jj,kk)%t_air     = spatial_sum(ii,jj,kk)%t_air / spatial_cnt(ii,jj,kk)
     spatial_mean(ii,jj,kk)%sh        = spatial_sum(ii,jj,kk)%sh   / spatial_cnt(ii,jj,kk)
     spatial_mean(ii,jj,kk)%u         = spatial_sum(ii,jj,kk)%u    / spatial_cnt(ii,jj,kk)
     spatial_mean(ii,jj,kk)%v         = spatial_sum(ii,jj,kk)%v    / spatial_cnt(ii,jj,kk)
     spatial_mean(ii,jj,kk)%w         = spatial_sum(ii,jj,kk)%w    / spatial_cnt(ii,jj,kk)
     spatial_mean(ii,jj,kk)%lw_heat   = spatial_sum(ii,jj,kk)%lw_heat / spatial_cnt(ii,jj,kk)
     spatial_mean(ii,jj,kk)%sw_heat   = spatial_sum(ii,jj,kk)%sw_heat / spatial_cnt(ii,jj,kk)
     spatial_mean(ii,jj,kk)%tot_cloud = spatial_sum(ii,jj,kk)%tot_cloud / spatial_cnt(ii,jj,kk)

    endif

  enddo ; enddo ; enddo


!
! write out cloud frequency zonal profile
!
 sdsu_io_file = trim(sdsu_dir_output)//'spatial_mean_CRM3D'//trim(output_suffix)//'.grads'
 open(io,file=trim(sdsu_io_file),access='DIRECT',status='replace',recl=imax*jmax*kmax*4)
 print("(a)"),'MSG spatial_mean3d: see output ->',trim(sdsu_io_file)

 irec = 1
 write(io,rec=irec) ((( spatial_mean(ii,jj,kk)%rain_rate , ii=1,imax ),jj=1,jmax),kk=1,kmax) ; irec = irec + 1
 write(io,rec=irec) ((( spatial_mean(ii,jj,kk)%t_air     , ii=1,imax ),jj=1,jmax),kk=1,kmax) ; irec = irec + 1
 write(io,rec=irec) ((( spatial_mean(ii,jj,kk)%sh        , ii=1,imax ),jj=1,jmax),kk=1,kmax) ; irec = irec + 1
 write(io,rec=irec) ((( spatial_mean(ii,jj,kk)%u         , ii=1,imax ),jj=1,jmax),kk=1,kmax) ; irec = irec + 1
 write(io,rec=irec) ((( spatial_mean(ii,jj,kk)%v         , ii=1,imax ),jj=1,jmax),kk=1,kmax) ; irec = irec + 1
 write(io,rec=irec) ((( spatial_mean(ii,jj,kk)%w         , ii=1,imax ),jj=1,jmax),kk=1,kmax) ; irec = irec + 1
 write(io,rec=irec) ((( spatial_mean(ii,jj,kk)%lw_heat   , ii=1,imax ),jj=1,jmax),kk=1,kmax) ; irec = irec + 1
 write(io,rec=irec) ((( spatial_mean(ii,jj,kk)%sw_heat   , ii=1,imax ),jj=1,jmax),kk=1,kmax) ; irec = irec + 1
 write(io,rec=irec) ((( spatial_mean(ii,jj,kk)%tot_cloud , ii=1,imax ),jj=1,jmax),kk=1,kmax) ; irec = irec + 1

 close(io)


!
! write grads control file
!
 sdsu_io_file = trim(sdsu_dir_output)//'spatial_mean_CRM3D'//trim(output_suffix)//'.ctl'
 print("(a)"),'MSG spatial_mean3d: see output.ctl ->',trim(sdsu_io_file)
 open(io,file=trim(sdsu_io_file))

 sdsu_io_file = 'spatial_mean_CRM3D'//trim(output_suffix)//'.grads'
 write(io,*) 'dset  ',trim(sdsu_io_file)
 write(io,*) 'title   spatial mean statistics '
 write(io,*) 'undef   -999.             '
 write(io,*) 'xdef    ',imax,' linear ',minlon,' ',res_latlon
 write(io,*) 'ydef    ',jmax,' linear ',minlat,' ',res_latlon
 write(io,*) 'zdef    ',play,' levels ', pressure_lay(1:play)
 write(io,*) 'tdef    1 linear ',grads_time,' 1hr'
 write(io,*) 'vars ',irec-1
 write(io,*) 'rain   ',play,'  0  rain rate [mm/hr]'
 write(io,*) 'tair   ',play,'  0  air temperature [degK]'
 write(io,*) 'sh   ',play ,'  0  specific humidity [g/g]'
 write(io,*) 'u    ',play,'  0  u wind [m/s]'
 write(io,*) 'v    ',play,'  0  v wind [m/s]'
 write(io,*) 'w    ',play,'  0  w wind [m/s]'
 write(io,*) 'lwheat',play,'  0  LW heating rate [K/day]'
 write(io,*) 'swheat',play,'  0  SW heating rate [K/day]'
 write(io,*) 'totcloud',play,'  0  total cloud + ice [g/m3]'

 write(io,*) 'endvars '
 close(io)


#if MPI > 0 
  endif MPI_MASTERPROC
#endif

 case default ; call stop_sdsu(myrank,'MSG spatial_mean_3d: There is no such proc')
 end select


 return
 end subroutine spatial_mean_3d

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 


 subroutine spatial_mean_daily_3d(proc,var3d,var2d)
 implicit none
!--------------------------------------------------------------------------------------------------
! Comments: Derive spatial mean daily map of 3D parameters.
!           
! History:
! 11/2016  Toshi Matsui@NASA GSFC ; Initial.
!
! References:
!-----------------------------------------------------------------------------------------------------

 character*(*) :: proc ! process
 type ( atmos_parameter ), dimension(:,:,:), optional :: var3d ! input surface parameter assumed 3d array
 type ( surface_parameter ), dimension(:,:), optional :: var2d ! input surface parameter assumed 3d array

 integer :: bnd(3)  !upper memory bound for 2D array

!
! statisticsl parameters
!
 type( atmos_parameter ), allocatable, dimension(:,:,:),save :: spatial_sum, spatial_mean
 real(sdsu_fps), allocatable, dimension(:,:,:),save :: spatial_cnt
 real(sdsu_fps), allocatable, dimension(:),save :: spatial_lat, spatial_lon

 integer,save :: ibin_max
 integer :: i,j,k, ibin, ierr, irec
 integer,parameter :: io = 149


 integer,parameter :: play = 20, plev = 21
 real(sdsu_fps),parameter :: pressure_lay(play) =    (/1000.,950.,900.,850.,800.,750.,700.,650.,600.,550.,500.,&
                                                     450.,400.,350.,300.,250.,200.,150.,100.,50./)
 real(sdsu_fps),parameter :: pressure_lev(plev) = (/1025., 975.,925.,875.,825.,775.,725.,675.,625.,575.,525.,475.,&
                                                        425.,375.,325.,275.,225.,175.,125.,75.,25./)

 integer,save :: imax, jmax, kmax
 integer :: ii, jj, kk

 real(sdsu_fps) :: pr

 character :: yyyy*4, mm*2, dd*2


 select case(trim(proc))
 case('init') !---------------------------------------------------------------------------------

   if(masterproc) print*, 'MSG spatial_mean_daily_3d: init'

   imax = int( ( maxlon - minlon ) / res_latlon ) + 1
   jmax = int( ( maxlat - minlat ) / res_latlon ) + 1
   kmax = play

   if( imax < 1 ) call stop_sdsu(myrank,'MSG spatial_mean_daily_3d: maxlon is smaller than minlon')
   if( jmax < 1 ) call stop_sdsu(myrank,'MSG spatial_mean_daily_3d: maxlat is smaller than minlat')

   if( allocated(spatial_sum) ) deallocate(spatial_sum, spatial_cnt, spatial_mean, spatial_lat, spatial_lon)

   allocate( spatial_sum(imax,jmax,kmax)  , &
             spatial_cnt(imax,jmax,kmax)  , &
             spatial_mean(imax,jmax,kmax) , &
             spatial_lat(jmax)  , &
             spatial_lon(imax)  , &
             stat=ierr )

   ! initialized
   do j = 1, jmax
      spatial_lat(j) = minlat + res_latlon * real(j-1) ![deg]
   enddo
   do i = 1, imax
      spatial_lon(i) = minlon + res_latlon * real(i-1) ![deg]
   enddo

  !
  ! zero out other parameters
  !
  spatial_cnt = 0.

  spatial_sum%rain_rate      = 0. ; spatial_sum%t_air       = 0.
  spatial_sum%sh             = 0. ; spatial_sum%u           = 0.
  spatial_sum%v              = 0. ; spatial_sum%w           = 0.
  spatial_sum%lw_heat        = 0. ; spatial_sum%sw_heat     = 0.
  spatial_sum%tot_cloud      = 0.

  spatial_mean%rain_rate      = undefined ; spatial_mean%t_air       = undefined
  spatial_mean%sh             = undefined ; spatial_mean%u           = undefined
  spatial_mean%v              = undefined ; spatial_mean%w           = undefined
  spatial_mean%lw_heat        = undefined ; spatial_mean%sw_heat     = undefined
  spatial_mean%tot_cloud      = undefined

 case('add') !-------------------------------------------------------------------------------------

  if(masterproc) print*, 'MSG spatial_mean_daily_3d: add'

  if(present(var3d) ) then

   !
   ! find upper bound from assumped 3D array
   !
   bnd = UBOUND(var3d)

   do j = 1,bnd(2) ; do i = 1,bnd(1)

      !
      ! loop for latlon_grid domain. 
      !
      do jj = 1, jmax
        within_grid: if( var2d(i,j)%lat >  (spatial_lat(jj)-0.5*res_latlon) .and. &
                         var2d(i,j)%lat <= (spatial_lat(jj)+0.5*res_latlon) ) then

!        if(masterproc) print*, 'MSG spatial_mean_2d: found lat ', (spatial_lat(jj)-0.5*res_latlon), &
!         var2d(i,j)%lat, (spatial_lat(jj)+0.5*res_latlon)

        do ii = 1, imax
          if( var2d(i,j)%lon >  (spatial_lon(ii)-0.5*res_latlon) .and. &
              var2d(i,j)%lon <= (spatial_lon(ii)+0.5*res_latlon) ) then

!             if(masterproc) print*, 'MSG spatial_mean_2d: found lon ',(spatial_lon(ii)-0.5*res_latlon),&
!                var2d(i,j)%lon, (spatial_lon(ii)+0.5*res_latlon)

           do kk = 1, kmax

               do k = 1, bnd(3)
                  if( var3d(i,j,k)%press <= pressure_lev(kk) .and. &
                      var3d(i,j,k)%press >  pressure_lev(kk+1)  ) then

                    spatial_cnt(ii,jj,kk) = spatial_cnt(ii,jj,kk) + 1.e0  !update total count
                    spatial_sum(ii,jj,kk)%rain_rate  = spatial_sum(ii,jj,kk)%rain_rate + var3d(i,j,k)%rain_rate
                    spatial_sum(ii,jj,kk)%t_air      = spatial_sum(ii,jj,kk)%t_air     + var3d(i,j,k)%t_air
                    spatial_sum(ii,jj,kk)%sh         = spatial_sum(ii,jj,kk)%sh        + var3d(i,j,k)%sh
                    spatial_sum(ii,jj,kk)%u          = spatial_sum(ii,jj,kk)%u         + var3d(i,j,k)%u
                    spatial_sum(ii,jj,kk)%v          = spatial_sum(ii,jj,kk)%v         + var3d(i,j,k)%v
                    spatial_sum(ii,jj,kk)%w          = spatial_sum(ii,jj,kk)%w         + var3d(i,j,k)%w
                    spatial_sum(ii,jj,kk)%lw_heat    = spatial_sum(ii,jj,kk)%lw_heat   + var3d(i,j,k)%lw_heat
                    spatial_sum(ii,jj,kk)%sw_heat    = spatial_sum(ii,jj,kk)%sw_heat   + var3d(i,j,k)%sw_heat
                    spatial_sum(ii,jj,kk)%tot_cloud  = spatial_sum(ii,jj,kk)%tot_cloud + var3d(i,j,k)%tot_cloud

                  endif
               enddo

           enddo
           endif
        enddo

        endif within_grid
      enddo


   enddo ; enddo


  endif

 case('finish') !---------------------------------------------------------------------------------------

#if MPI > 0 
!
! MPI reduce to add all together
!
  call mpi_sdsu_reduce( 'SUM', spatial_cnt  )
  call mpi_sdsu_reduce( 'SUM', spatial_sum%rain_rate )
  call mpi_sdsu_reduce( 'SUM', spatial_sum%t_air )
  call mpi_sdsu_reduce( 'SUM', spatial_sum%sh   )
  call mpi_sdsu_reduce( 'SUM', spatial_sum%u )
  call mpi_sdsu_reduce( 'SUM', spatial_sum%v )
  call mpi_sdsu_reduce( 'SUM', spatial_sum%w )
  call mpi_sdsu_reduce( 'SUM', spatial_sum%lw_heat )
  call mpi_sdsu_reduce( 'SUM', spatial_sum%sw_heat )
  call mpi_sdsu_reduce( 'SUM', spatial_sum%tot_cloud )

  MPI_MASTERPROC: if(masterproc) then
#endif

  do kk = 1, kmax ; do jj = 1, jmax ; do ii = 1, imax 

    if( spatial_cnt(ii,jj,kk) > 0. ) then

     spatial_mean(ii,jj,kk)%rain_rate = spatial_sum(ii,jj,kk)%rain_rate / spatial_cnt(ii,jj,kk)
     spatial_mean(ii,jj,kk)%t_air     = spatial_sum(ii,jj,kk)%t_air / spatial_cnt(ii,jj,kk)
     spatial_mean(ii,jj,kk)%sh        = spatial_sum(ii,jj,kk)%sh   / spatial_cnt(ii,jj,kk)
     spatial_mean(ii,jj,kk)%u         = spatial_sum(ii,jj,kk)%u    / spatial_cnt(ii,jj,kk)
     spatial_mean(ii,jj,kk)%v         = spatial_sum(ii,jj,kk)%v    / spatial_cnt(ii,jj,kk)
     spatial_mean(ii,jj,kk)%w         = spatial_sum(ii,jj,kk)%w    / spatial_cnt(ii,jj,kk)
     spatial_mean(ii,jj,kk)%lw_heat   = spatial_sum(ii,jj,kk)%lw_heat / spatial_cnt(ii,jj,kk)
     spatial_mean(ii,jj,kk)%sw_heat   = spatial_sum(ii,jj,kk)%sw_heat / spatial_cnt(ii,jj,kk)
     spatial_mean(ii,jj,kk)%tot_cloud = spatial_sum(ii,jj,kk)%tot_cloud / spatial_cnt(ii,jj,kk)

    endif

  enddo ; enddo ; enddo


!
! write out cloud frequency diurnal profile
!
 write(yyyy,"(I4.4)") int( sdsu_yyyy )
 write(mm  ,"(I2.2)") int( sdsu_mm   )
 write(dd  ,"(I2.2)") int( sdsu_dd   )


!
! write out cloud frequency zonal profile
!
 sdsu_io_file = trim(sdsu_dir_output)//'spatial_mean_CRM3D_'//yyyy//'_'//mm//'_'//dd&
                //trim(output_suffix)//'.grads'
 open(io,file=trim(sdsu_io_file),access='DIRECT',status='replace',recl=imax*jmax*kmax*4)
 print("(a)"),'MSG spatial_mean3d: see output ->',trim(sdsu_io_file)

 irec = 1
 write(io,rec=irec) ((( spatial_mean(ii,jj,kk)%rain_rate , ii=1,imax ),jj=1,jmax),kk=1,kmax) ; irec = irec + 1
 write(io,rec=irec) ((( spatial_mean(ii,jj,kk)%t_air     , ii=1,imax ),jj=1,jmax),kk=1,kmax) ; irec = irec + 1
 write(io,rec=irec) ((( spatial_mean(ii,jj,kk)%sh        , ii=1,imax ),jj=1,jmax),kk=1,kmax) ; irec = irec + 1
 write(io,rec=irec) ((( spatial_mean(ii,jj,kk)%u         , ii=1,imax ),jj=1,jmax),kk=1,kmax) ; irec = irec + 1
 write(io,rec=irec) ((( spatial_mean(ii,jj,kk)%v         , ii=1,imax ),jj=1,jmax),kk=1,kmax) ; irec = irec + 1
 write(io,rec=irec) ((( spatial_mean(ii,jj,kk)%w         , ii=1,imax ),jj=1,jmax),kk=1,kmax) ; irec = irec + 1
 write(io,rec=irec) ((( spatial_mean(ii,jj,kk)%lw_heat   , ii=1,imax ),jj=1,jmax),kk=1,kmax) ; irec = irec + 1
 write(io,rec=irec) ((( spatial_mean(ii,jj,kk)%sw_heat   , ii=1,imax ),jj=1,jmax),kk=1,kmax) ; irec = irec + 1
 write(io,rec=irec) ((( spatial_mean(ii,jj,kk)%tot_cloud , ii=1,imax ),jj=1,jmax),kk=1,kmax) ; irec = irec + 1

 close(io)


!
! write grads control file
!
 sdsu_io_file = trim(sdsu_dir_output)//'spatial_mean_CRM3D_'//yyyy//'_'//mm//'_'//dd&
                //trim(output_suffix)//'.ctl'
 print("(a)"),'MSG spatial_mean3d: see output.ctl ->',trim(sdsu_io_file)
 open(io,file=trim(sdsu_io_file))

 sdsu_io_file = 'spatial_mean_CRM3D_'//yyyy//'_'//mm//'_'//dd&
                //trim(output_suffix)//'.grads'
 write(io,*) 'dset  ',trim(sdsu_io_file)
 write(io,*) 'title   spatial mean statistics '
 write(io,*) 'undef   -999.             '
 write(io,*) 'xdef    ',imax,' linear ',minlon,' ',res_latlon
 write(io,*) 'ydef    ',jmax,' linear ',minlat,' ',res_latlon
 write(io,*) 'zdef    ',play,' levels ', pressure_lay(1:play)
 write(io,*) 'tdef    1 linear ',grads_time,' 1hr'
 write(io,*) 'vars ',irec-1
 write(io,*) 'rain   ',play,'  0  rain rate [mm/hr]'
 write(io,*) 'tair   ',play,'  0  air temperature [degK]'
 write(io,*) 'sh   ',play ,'  0  specific humidity [g/g]'
 write(io,*) 'u    ',play,'  0  u wind [m/s]'
 write(io,*) 'v    ',play,'  0  v wind [m/s]'
 write(io,*) 'w    ',play,'  0  w wind [m/s]'
 write(io,*) 'lwheat',play,'  0  LW heating rate [K/day]'
 write(io,*) 'swheat',play,'  0  SW heating rate [K/day]'
 write(io,*) 'totcloud',play,'  0  total cloud + ice [g/m3]'

 write(io,*) 'endvars '
 close(io)


#if MPI > 0 
  endif MPI_MASTERPROC
#endif

 case default ; call stop_sdsu(myrank,'MSG spatial_mean_daily_3d: There is no such proc')
 end select


 return
 end subroutine spatial_mean_daily_3d

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 subroutine spatial_mean_hourly_3d(proc,var3d,var2d)
 implicit none
!--------------------------------------------------------------------------------------------------
! Comments: Derive spatial mean hourly map of 3D parameters.
!           
! History:
! 06/2017  Toshi Matsui@NASA GSFC ; Initial.
!
! References:
!-----------------------------------------------------------------------------------------------------

 character*(*) :: proc ! process
 type ( atmos_parameter ), dimension(:,:,:), optional :: var3d ! input surface parameter assumed 3d array
 type ( surface_parameter ), dimension(:,:), optional :: var2d ! input surface parameter assumed 3d array

 integer :: bnd(3)  !upper memory bound for 2D array

!
! statisticsl parameters
!
 type( atmos_parameter ), allocatable, dimension(:,:,:),save :: spatial_sum, spatial_mean
 real(sdsu_fps), allocatable, dimension(:,:,:),save :: spatial_cnt
 real(sdsu_fps), allocatable, dimension(:),save :: spatial_lat, spatial_lon

 integer,save :: ibin_max
 integer :: i,j,k, ibin, ierr, irec
 integer,parameter :: io = 149


 integer,parameter :: play = 20, plev = 21
 real(sdsu_fps),parameter :: pressure_lay(play) =    (/1000.,950.,900.,850.,800.,750.,700.,650.,600.,550.,500.,&
                                                     450.,400.,350.,300.,250.,200.,150.,100.,50./)
 real(sdsu_fps),parameter :: pressure_lev(plev) = (/1025., 975.,925.,875.,825.,775.,725.,675.,625.,575.,525.,475.,&
                                                        425.,375.,325.,275.,225.,175.,125.,75.,25./)

 integer,save :: imax, jmax, kmax
 integer :: ii, jj, kk

 real(sdsu_fps) :: pr

 character :: yyyy*4, mm*2, dd*2, hh*2


 select case(trim(proc))
 case('init') !---------------------------------------------------------------------------------

   if(masterproc) print*, 'MSG spatial_mean_hourly_3d: init'

   imax = int( ( maxlon - minlon ) / res_latlon ) + 1
   jmax = int( ( maxlat - minlat ) / res_latlon ) + 1
   kmax = play

   if( imax < 1 ) call stop_sdsu(myrank,'MSG spatial_mean_hourly_3d: maxlon is smaller than minlon')
   if( jmax < 1 ) call stop_sdsu(myrank,'MSG spatial_mean_hourly_3d: maxlat is smaller than minlat')

   if( allocated(spatial_sum) ) deallocate(spatial_sum, spatial_cnt, spatial_mean, spatial_lat, spatial_lon)

   allocate( spatial_sum(imax,jmax,kmax)  , &
             spatial_cnt(imax,jmax,kmax)  , &
             spatial_mean(imax,jmax,kmax) , &
             spatial_lat(jmax)  , &
             spatial_lon(imax)  , &
             stat=ierr )

   ! initialized
   do j = 1, jmax
      spatial_lat(j) = minlat + res_latlon * real(j-1) ![deg]
   enddo
   do i = 1, imax
      spatial_lon(i) = minlon + res_latlon * real(i-1) ![deg]
   enddo

  !
  ! zero out other parameters
  !
  spatial_cnt = 0.

  spatial_sum%rain_rate      = 0. ; spatial_sum%t_air       = 0.
  spatial_sum%sh             = 0. ; spatial_sum%u           = 0.
  spatial_sum%v              = 0. ; spatial_sum%w           = 0.
  spatial_sum%lw_heat        = 0. ; spatial_sum%sw_heat     = 0.
  spatial_sum%tot_cloud      = 0.

  spatial_mean%rain_rate      = undefined ; spatial_mean%t_air       = undefined
  spatial_mean%sh             = undefined ; spatial_mean%u           = undefined
  spatial_mean%v              = undefined ; spatial_mean%w           = undefined
  spatial_mean%lw_heat        = undefined ; spatial_mean%sw_heat     = undefined
  spatial_mean%tot_cloud      = undefined

 case('add') !-------------------------------------------------------------------------------------

  if(masterproc) print*, 'MSG spatial_mean_hourly_3d: add'

  if(present(var3d) ) then

   !
   ! find upper bound from assumped 3D array
   !
   bnd = UBOUND(var3d)

   do j = 1,bnd(2) ; do i = 1,bnd(1)

      !
      ! loop for latlon_grid domain. 
      !
      do jj = 1, jmax
        within_grid: if( var2d(i,j)%lat >  (spatial_lat(jj)-0.5*res_latlon) .and. &
                         var2d(i,j)%lat <= (spatial_lat(jj)+0.5*res_latlon) ) then

!        if(masterproc) print*, 'MSG spatial_mean_2d: found lat ', (spatial_lat(jj)-0.5*res_latlon), &
!         var2d(i,j)%lat, (spatial_lat(jj)+0.5*res_latlon)

        do ii = 1, imax
          if( var2d(i,j)%lon >  (spatial_lon(ii)-0.5*res_latlon) .and. &
              var2d(i,j)%lon <= (spatial_lon(ii)+0.5*res_latlon) ) then

!             if(masterproc) print*, 'MSG spatial_mean_2d: found lon ',(spatial_lon(ii)-0.5*res_latlon),&
!                var2d(i,j)%lon, (spatial_lon(ii)+0.5*res_latlon)

           do kk = 1, kmax

               do k = 1, bnd(3)
                  if( var3d(i,j,k)%press <= pressure_lev(kk) .and. &
                      var3d(i,j,k)%press >  pressure_lev(kk+1)  ) then

                    spatial_cnt(ii,jj,kk) = spatial_cnt(ii,jj,kk) + 1.e0  !update total count
                    spatial_sum(ii,jj,kk)%rain_rate  = spatial_sum(ii,jj,kk)%rain_rate + var3d(i,j,k)%rain_rate
                    spatial_sum(ii,jj,kk)%t_air      = spatial_sum(ii,jj,kk)%t_air     + var3d(i,j,k)%t_air
                    spatial_sum(ii,jj,kk)%sh         = spatial_sum(ii,jj,kk)%sh        + var3d(i,j,k)%sh
                    spatial_sum(ii,jj,kk)%u          = spatial_sum(ii,jj,kk)%u         + var3d(i,j,k)%u
                    spatial_sum(ii,jj,kk)%v          = spatial_sum(ii,jj,kk)%v         + var3d(i,j,k)%v
                    spatial_sum(ii,jj,kk)%w          = spatial_sum(ii,jj,kk)%w         + var3d(i,j,k)%w
                    spatial_sum(ii,jj,kk)%lw_heat    = spatial_sum(ii,jj,kk)%lw_heat   + var3d(i,j,k)%lw_heat
                    spatial_sum(ii,jj,kk)%sw_heat    = spatial_sum(ii,jj,kk)%sw_heat   + var3d(i,j,k)%sw_heat
                    spatial_sum(ii,jj,kk)%tot_cloud  = spatial_sum(ii,jj,kk)%tot_cloud + var3d(i,j,k)%tot_cloud

                  endif
               enddo

           enddo
           endif
        enddo

        endif within_grid
      enddo


   enddo ; enddo


  endif

 case('finish') !---------------------------------------------------------------------------------------

#if MPI > 0 
!
! MPI reduce to add all together
!
  call mpi_sdsu_reduce( 'SUM', spatial_cnt  )
  call mpi_sdsu_reduce( 'SUM', spatial_sum%rain_rate )
  call mpi_sdsu_reduce( 'SUM', spatial_sum%t_air )
  call mpi_sdsu_reduce( 'SUM', spatial_sum%sh   )
  call mpi_sdsu_reduce( 'SUM', spatial_sum%u )
  call mpi_sdsu_reduce( 'SUM', spatial_sum%v )
  call mpi_sdsu_reduce( 'SUM', spatial_sum%w )
  call mpi_sdsu_reduce( 'SUM', spatial_sum%lw_heat )
  call mpi_sdsu_reduce( 'SUM', spatial_sum%sw_heat )
  call mpi_sdsu_reduce( 'SUM', spatial_sum%tot_cloud )

  MPI_MASTERPROC: if(masterproc) then
#endif

  do kk = 1, kmax ; do jj = 1, jmax ; do ii = 1, imax 

    if( spatial_cnt(ii,jj,kk) > 0. ) then

     spatial_mean(ii,jj,kk)%rain_rate = spatial_sum(ii,jj,kk)%rain_rate / spatial_cnt(ii,jj,kk)
     spatial_mean(ii,jj,kk)%t_air     = spatial_sum(ii,jj,kk)%t_air / spatial_cnt(ii,jj,kk)
     spatial_mean(ii,jj,kk)%sh        = spatial_sum(ii,jj,kk)%sh   / spatial_cnt(ii,jj,kk)
     spatial_mean(ii,jj,kk)%u         = spatial_sum(ii,jj,kk)%u    / spatial_cnt(ii,jj,kk)
     spatial_mean(ii,jj,kk)%v         = spatial_sum(ii,jj,kk)%v    / spatial_cnt(ii,jj,kk)
     spatial_mean(ii,jj,kk)%w         = spatial_sum(ii,jj,kk)%w    / spatial_cnt(ii,jj,kk)
     spatial_mean(ii,jj,kk)%lw_heat   = spatial_sum(ii,jj,kk)%lw_heat / spatial_cnt(ii,jj,kk)
     spatial_mean(ii,jj,kk)%sw_heat   = spatial_sum(ii,jj,kk)%sw_heat / spatial_cnt(ii,jj,kk)
     spatial_mean(ii,jj,kk)%tot_cloud = spatial_sum(ii,jj,kk)%tot_cloud / spatial_cnt(ii,jj,kk)

    endif

  enddo ; enddo ; enddo


!
! write out cloud frequency diurnal profile
!
 write(yyyy,"(I4.4)") int( sdsu_yyyy )
 write(mm  ,"(I2.2)") int( sdsu_mm   )
 write(dd  ,"(I2.2)") int( sdsu_dd   )
 write(hh  ,"(I2.2)") int( sdsu_hh   )

!
! write out cloud frequency zonal profile
!
 sdsu_io_file = trim(sdsu_dir_output)//'spatial_mean_CRM3D_'//yyyy//'_'//mm//'_'//dd//'_'//hh&
                //trim(output_suffix)//'.grads'
 open(io,file=trim(sdsu_io_file),access='DIRECT',status='replace',recl=imax*jmax*kmax*4)
 print("(a)"),'MSG spatial_mean3d: see output ->',trim(sdsu_io_file)

 irec = 1
 write(io,rec=irec) ((( spatial_mean(ii,jj,kk)%rain_rate , ii=1,imax ),jj=1,jmax),kk=1,kmax) ; irec = irec + 1
 write(io,rec=irec) ((( spatial_mean(ii,jj,kk)%t_air     , ii=1,imax ),jj=1,jmax),kk=1,kmax) ; irec = irec + 1
 write(io,rec=irec) ((( spatial_mean(ii,jj,kk)%sh        , ii=1,imax ),jj=1,jmax),kk=1,kmax) ; irec = irec + 1
 write(io,rec=irec) ((( spatial_mean(ii,jj,kk)%u         , ii=1,imax ),jj=1,jmax),kk=1,kmax) ; irec = irec + 1
 write(io,rec=irec) ((( spatial_mean(ii,jj,kk)%v         , ii=1,imax ),jj=1,jmax),kk=1,kmax) ; irec = irec + 1
 write(io,rec=irec) ((( spatial_mean(ii,jj,kk)%w         , ii=1,imax ),jj=1,jmax),kk=1,kmax) ; irec = irec + 1
 write(io,rec=irec) ((( spatial_mean(ii,jj,kk)%lw_heat   , ii=1,imax ),jj=1,jmax),kk=1,kmax) ; irec = irec + 1
 write(io,rec=irec) ((( spatial_mean(ii,jj,kk)%sw_heat   , ii=1,imax ),jj=1,jmax),kk=1,kmax) ; irec = irec + 1
 write(io,rec=irec) ((( spatial_mean(ii,jj,kk)%tot_cloud , ii=1,imax ),jj=1,jmax),kk=1,kmax) ; irec = irec + 1

 close(io)


!
! write grads control file
!
 sdsu_io_file = trim(sdsu_dir_output)//'spatial_mean_CRM3D_'//yyyy//'_'//mm//'_'//dd//'_'//hh&
                //trim(output_suffix)//'.ctl'
 print("(a)"),'MSG spatial_mean3d: see output.ctl ->',trim(sdsu_io_file)
 open(io,file=trim(sdsu_io_file))

 sdsu_io_file = 'spatial_mean_CRM3D_'//yyyy//'_'//mm//'_'//dd//'_'//hh&
                //trim(output_suffix)//'.grads'
 write(io,*) 'dset  ',trim(sdsu_io_file)
 write(io,*) 'title   spatial mean statistics '
 write(io,*) 'undef   -999.             '
 write(io,*) 'xdef    ',imax,' linear ',minlon,' ',res_latlon
 write(io,*) 'ydef    ',jmax,' linear ',minlat,' ',res_latlon
 write(io,*) 'zdef    ',play,' levels ', pressure_lay(1:play)
 write(io,*) 'tdef    1 linear ',grads_time,' 1hr'
 write(io,*) 'vars ',irec-1
 write(io,*) 'rain   ',play,'  0  rain rate [mm/hr]'
 write(io,*) 'tair   ',play,'  0  air temperature [degK]'
 write(io,*) 'sh   ',play ,'  0  specific humidity [g/g]'
 write(io,*) 'u    ',play,'  0  u wind [m/s]'
 write(io,*) 'v    ',play,'  0  v wind [m/s]'
 write(io,*) 'w    ',play,'  0  w wind [m/s]'
 write(io,*) 'lwheat',play,'  0  LW heating rate [K/day]'
 write(io,*) 'swheat',play,'  0  SW heating rate [K/day]'
 write(io,*) 'totcloud',play,'  0  total cloud + ice [g/m3]'

 write(io,*) 'endvars '
 close(io)


#if MPI > 0 
  endif MPI_MASTERPROC
#endif

 case default ; call stop_sdsu(myrank,'MSG spatial_mean_hourly_3d: There is no such proc')
 end select


 return
 end subroutine spatial_mean_hourly_3d

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 subroutine zonal_mean_2d(proc,var2d)
 implicit none
!--------------------------------------------------------------------------------------------------
! Comments: Derive zonal mean composite of 2D parameters.
!           
! History:
! 06/2016  Toshi Matsui@NASA GSFC ; Initial.
!
! References:
!-----------------------------------------------------------------------------------------------------

 character*(*) :: proc ! process
 type ( surface_parameter ), dimension(:,:), optional :: var2d ! input surface parameter assumed 2d array

 integer :: bnd(2)  !upper memory bound for 2D array

 integer,save :: ibin_max

!
! statisticsl parameters
!
 type( surface_parameter ), allocatable, dimension(:),save :: zonal_sum,zonal_mean
 real(sdsu_fps), allocatable, dimension(:),save :: zonal_lat, zonal_cnt
 integer,save :: jmax

 integer :: i,j, ibin, ierr, irec
 integer,parameter :: io = 151

 select case(trim(proc))
 case('init') !---------------------------------------------------------------------------------
!print*,'MSG tb_precip_sample: init'

 if(masterproc) print*, 'MSG zonal_mean_2d: init'

!
! maximum bin of zone
!
 jmax  =int( (maxlat - minlat) / res_latlon ) + 1

 if( jmax < 1 ) call stop_sdsu(myrank,'MSG spatial_mean_2d: maxlat is smaller than minlat')
 
!
! allocate
!
 if( .not. allocated(zonal_sum) ) then
     allocate( zonal_sum (jmax), &
               zonal_mean(jmax), &
               zonal_cnt (jmax), &
               zonal_lat (jmax), &
               stat=ierr )
      if (ierr /= 0) call stop_sdsu(myrank,'MSG zonal_mean_2d: allocation error -> Terminate program.')
 endif

!
! assign latitude bin
!
  do ibin = 1, ibin_max 
     zonal_lat(ibin) = minlat + res_latlon * real(ibin) - 0.5 * res_latlon
  enddo



!
! zero out other parameters
!
 zonal_cnt = 0.

 zonal_sum%rain_rate      = 0. ; zonal_sum%h2o_col       = 0.  
 zonal_sum%path_cloud_liq = 0. ; zonal_sum%path_rain_liq = 0.
 zonal_sum%path_cloud_ice = 0. ; zonal_sum%path_rain_ice = 0.
 zonal_sum%toa_lw_up      = 0. ; zonal_sum%toa_lw_dn     = 0.
 zonal_sum%toa_sw_up      = 0. ; zonal_sum%toa_sw_dn     = 0.
 zonal_sum%boa_lw_up      = 0. ; zonal_sum%boa_lw_dn     = 0.
 zonal_sum%boa_sw_up      = 0. ; zonal_sum%boa_sw_dn     = 0.

 zonal_mean%rain_rate      = undefined ; zonal_mean%h2o_col       = undefined
 zonal_mean%path_cloud_liq = undefined ; zonal_mean%path_rain_liq = undefined
 zonal_mean%path_cloud_ice = undefined ; zonal_mean%path_rain_ice = undefined
 zonal_mean%toa_lw_up      = undefined ; zonal_mean%toa_lw_dn     = undefined
 zonal_mean%toa_sw_up      = undefined ; zonal_mean%toa_sw_dn     = undefined
 zonal_mean%boa_lw_up      = undefined ; zonal_mean%boa_lw_dn     = undefined
 zonal_mean%boa_sw_up      = undefined ; zonal_mean%boa_sw_dn     = undefined


 case('add') !-------------------------------------------------------------------------------------


 if(present(var2d) ) then


   !
   ! find upper bound from assumped 3D array
   !
   bnd = UBOUND(var2d)


   do j = 1,bnd(2) ; do i = 1,bnd(1)

      do ibin = 1, ibin_max
         if( var2d(i,j)%lat >  zonal_lat(ibin) -  0.5 * res_latlon  .and. &
             var2d(i,j)%lat <= zonal_lat(ibin) +  0.5 * res_latlon  ) then

             zonal_cnt(ibin) = zonal_cnt(ibin) + 1.e0  !update total count

             zonal_sum(ibin)%rain_rate      = zonal_sum(ibin)%rain_rate + var2d(i,j)%rain_rate
             zonal_sum(ibin)%h2o_col        = zonal_sum(ibin)%h2o_col   + var2d(i,j)%h2o_col
             zonal_sum(ibin)%path_cloud_liq = zonal_sum(ibin)%path_cloud_liq + var2d(i,j)%path_cloud_liq
             zonal_sum(ibin)%path_rain_liq  = zonal_sum(ibin)%path_rain_liq  + var2d(i,j)%path_rain_liq
             zonal_sum(ibin)%path_cloud_ice = zonal_sum(ibin)%path_cloud_ice + var2d(i,j)%path_cloud_ice
             zonal_sum(ibin)%path_rain_ice  = zonal_sum(ibin)%path_rain_ice  + var2d(i,j)%path_rain_ice
             zonal_sum(ibin)%toa_lw_up      = zonal_sum(ibin)%toa_lw_up + var2d(i,j)%toa_lw_up
             zonal_sum(ibin)%toa_lw_dn      = zonal_sum(ibin)%toa_lw_dn + var2d(i,j)%toa_lw_dn
             zonal_sum(ibin)%toa_sw_up      = zonal_sum(ibin)%toa_sw_up + var2d(i,j)%toa_sw_up
             zonal_sum(ibin)%toa_sw_dn      = zonal_sum(ibin)%toa_sw_dn + var2d(i,j)%toa_sw_dn
             zonal_sum(ibin)%boa_lw_up      = zonal_sum(ibin)%boa_lw_up + var2d(i,j)%boa_lw_up
             zonal_sum(ibin)%boa_lw_dn      = zonal_sum(ibin)%boa_lw_dn + var2d(i,j)%boa_lw_dn
             zonal_sum(ibin)%boa_sw_up      = zonal_sum(ibin)%boa_sw_up + var2d(i,j)%boa_sw_up
             zonal_sum(ibin)%boa_sw_dn      = zonal_sum(ibin)%boa_sw_dn + var2d(i,j)%boa_sw_dn

         endif
      enddo

   enddo ; enddo



 endif

 case('finish') !---------------------------------------------------------------------------------------


#if MPI > 0 
!
! MPI reduce to add all together
!
  call mpi_sdsu_reduce( 'SUM', zonal_cnt  )
  call mpi_sdsu_reduce( 'SUM', zonal_sum%rain_rate )
  call mpi_sdsu_reduce( 'SUM', zonal_sum%h2o_col   )
  call mpi_sdsu_reduce( 'SUM', zonal_sum%path_cloud_liq )
  call mpi_sdsu_reduce( 'SUM', zonal_sum%path_rain_liq )
  call mpi_sdsu_reduce( 'SUM', zonal_sum%path_cloud_ice )
  call mpi_sdsu_reduce( 'SUM', zonal_sum%path_rain_ice )
  call mpi_sdsu_reduce( 'SUM', zonal_sum%toa_lw_up )
  call mpi_sdsu_reduce( 'SUM', zonal_sum%toa_lw_dn )
  call mpi_sdsu_reduce( 'SUM', zonal_sum%toa_sw_up )
  call mpi_sdsu_reduce( 'SUM', zonal_sum%toa_sw_dn )
  call mpi_sdsu_reduce( 'SUM', zonal_sum%boa_lw_up )
  call mpi_sdsu_reduce( 'SUM', zonal_sum%boa_lw_dn )
  call mpi_sdsu_reduce( 'SUM', zonal_sum%boa_sw_up )
  call mpi_sdsu_reduce( 'SUM', zonal_sum%boa_sw_dn )

  MPI_MASTERPROC: if(masterproc) then
#endif

!
! derive zonal mean values
! 
 do ibin = 1, ibin_max

    if( zonal_cnt(ibin) > 0. ) then

     zonal_mean(ibin)%rain_rate      = zonal_sum(ibin)%rain_rate / zonal_cnt(ibin)
     zonal_mean(ibin)%h2o_col        = zonal_sum(ibin)%h2o_col   / zonal_cnt(ibin)
     zonal_mean(ibin)%path_cloud_liq = zonal_sum(ibin)%path_cloud_liq / zonal_cnt(ibin)
     zonal_mean(ibin)%path_rain_liq  = zonal_sum(ibin)%path_rain_liq  / zonal_cnt(ibin)
     zonal_mean(ibin)%path_cloud_ice = zonal_sum(ibin)%path_cloud_ice / zonal_cnt(ibin)
     zonal_mean(ibin)%path_rain_ice  = zonal_sum(ibin)%path_rain_ice  / zonal_cnt(ibin)
     zonal_mean(ibin)%toa_lw_up      = zonal_sum(ibin)%toa_lw_up / zonal_cnt(ibin)
     zonal_mean(ibin)%toa_lw_dn      = zonal_sum(ibin)%toa_lw_dn / zonal_cnt(ibin)
     zonal_mean(ibin)%toa_sw_up      = zonal_sum(ibin)%toa_sw_up / zonal_cnt(ibin)
     zonal_mean(ibin)%toa_sw_dn      = zonal_sum(ibin)%toa_sw_dn / zonal_cnt(ibin)
     zonal_mean(ibin)%boa_lw_up      = zonal_sum(ibin)%boa_lw_up / zonal_cnt(ibin)
     zonal_mean(ibin)%boa_lw_dn      = zonal_sum(ibin)%boa_lw_dn / zonal_cnt(ibin)
     zonal_mean(ibin)%boa_sw_up      = zonal_sum(ibin)%boa_sw_up / zonal_cnt(ibin)
     zonal_mean(ibin)%boa_sw_dn      = zonal_sum(ibin)%boa_sw_dn / zonal_cnt(ibin)

    endif

 enddo


!
! write out cloud frequency zonal profile
!

 sdsu_io_file = trim(sdsu_dir_output)//'zonal_mean_CRM2D'//trim(output_suffix)//'.grads'
 open(io,file=trim(sdsu_io_file),access='DIRECT',status='replace',recl=ibin_max*4)
 print("(a)"),'MSG zonal_mean2d: see output ->',trim(sdsu_io_file)

 irec = 1
 write(io,rec=irec) ( zonal_mean(ibin)%rain_rate      , ibin = 1, ibin_max ) ; irec = irec + 1
 write(io,rec=irec) ( zonal_mean(ibin)%h2o_col        , ibin = 1, ibin_max ) ; irec = irec + 1
 write(io,rec=irec) ( zonal_mean(ibin)%path_cloud_liq , ibin = 1, ibin_max ) ; irec = irec + 1
 write(io,rec=irec) ( zonal_mean(ibin)%path_rain_liq  , ibin = 1, ibin_max ) ; irec = irec + 1
 write(io,rec=irec) ( zonal_mean(ibin)%path_cloud_ice , ibin = 1, ibin_max ) ; irec = irec + 1
 write(io,rec=irec) ( zonal_mean(ibin)%path_rain_ice  , ibin = 1, ibin_max ) ; irec = irec + 1
 write(io,rec=irec) ( zonal_mean(ibin)%toa_lw_up      , ibin = 1, ibin_max ) ; irec = irec + 1
 write(io,rec=irec) ( zonal_mean(ibin)%toa_lw_dn      , ibin = 1, ibin_max ) ; irec = irec + 1
 write(io,rec=irec) ( zonal_mean(ibin)%toa_sw_up      , ibin = 1, ibin_max ) ; irec = irec + 1
 write(io,rec=irec) ( zonal_mean(ibin)%toa_sw_dn      , ibin = 1, ibin_max ) ; irec = irec + 1
 write(io,rec=irec) ( zonal_mean(ibin)%boa_lw_up      , ibin = 1, ibin_max ) ; irec = irec + 1
 write(io,rec=irec) ( zonal_mean(ibin)%boa_lw_dn      , ibin = 1, ibin_max ) ; irec = irec + 1
 write(io,rec=irec) ( zonal_mean(ibin)%boa_sw_up      , ibin = 1, ibin_max ) ; irec = irec + 1
 write(io,rec=irec) ( zonal_mean(ibin)%boa_sw_dn      , ibin = 1, ibin_max ) ; irec = irec + 1

 close(io)


!
! write grads control file
!
 sdsu_io_file = trim(sdsu_dir_output)//'zonal_mean_CRM2D'//trim(output_suffix)//'.ctl'
 print("(a)"),'MSG zonal_mean2d: see output.ctl ->',trim(sdsu_io_file)
 open(io,file=trim(sdsu_io_file))

 sdsu_io_file = 'zonal_mean_CRM2D'//trim(output_suffix)//'.grads'
 write(io,*) 'dset  ',trim(sdsu_io_file)
 write(io,*) 'title   zonal mean statistics '
 write(io,*) 'undef   -999.             '
 write(io,*) 'xdef    1 linear 1. 1'
 write(io,*) 'ydef    ',ibin_max,' linear ',minlat,' ',res_latlon
 write(io,*) 'zdef    1 linear 0. 0.1'
 write(io,*) 'tdef    1 linear ',grads_time,' 1hr'
 write(io,*) 'vars ',irec-1
 write(io,*) 'rain    0  0  rain rate [mm/hr]'

 write(io,*) 'h2ocol  0  0  column water vapro [kg/m2]'
 write(io,*) 'clwp    0  0  cloud liquid water path [kg/m2]'
 write(io,*) 'lrwp    0  0  liquid rain water path [kg/m2]'
 write(io,*) 'ciwp    0  0  cloud ice water path [kg/m2]'
 write(io,*) 'irwp    0  0  ice rain water path [kg/m2]'

 write(io,*) 'lwut   0 0  TOA LW upwelling   flux [W/m2]'
 write(io,*) 'lwdt   0 0  TOA LW downwelling flux [W/m2]'
 write(io,*) 'swut   0 0  TOA SW upwelling flux   [W/m2]'
 write(io,*) 'swdt   0 0  TOA SW downwelling flux [W/m2]'
 write(io,*) 'lwus   0 0  Surface LW upwelling   flux [W/m2]'
 write(io,*) 'lwds   0 0  Surface LW downwelling flux [W/m2]'
 write(io,*) 'swus   0 0  Surface SW upwelling   flux [W/m2]'
 write(io,*) 'swds   0 0  Surface SW downwelling flux [W/m2]'

 write(io,*) 'endvars '
 close(io)


#if MPI > 0 
  endif MPI_MASTERPROC
#endif

 case default ; call stop_sdsu(myrank,'MSG zonal_mean_2d: There is no such proc')
 end select


 return
 end subroutine zonal_mean_2d

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 subroutine diurnal_mean_2d(proc,var2d)
 implicit none
!--------------------------------------------------------------------------------------------------
! Comments: Derive diurnal composite of 2D parameters.
!           
! History:
! 06/2016  Toshi Matsui@NASA GSFC ; Initial.
!
! References:
!-----------------------------------------------------------------------------------------------------
 character*(*) :: proc ! process
 type ( surface_parameter ), dimension(:,:), optional :: var2d ! input surface parameter assumed 2d array

 integer :: bnd(2)  !upper memory bound for 2D array


!
! statisticsl parameters
!
 type( surface_parameter ), allocatable, dimension(:),save :: diurnal_sum,diurnal_mean
 real(sdsu_fps), allocatable, dimension(:),save :: diurnal_lst, diurnal_cnt
 integer,save :: ibin_max
 real(sdsu_fps) :: hour_loc

 integer :: i,j, ibin, ierr, irec
 integer,parameter :: io = 154

 select case(trim(proc))
 case('init') !---------------------------------------------------------------------------------
 if(masterproc) print*,'MSG diurnal_mean_2d: init'

!
! maximum bin of zone
!
  ibin_max  = 24

!
! allocate
!
 if( .not. allocated(diurnal_sum) ) then
     allocate( diurnal_sum (ibin_max), &
               diurnal_mean(ibin_max), &
               diurnal_cnt (ibin_max), &
               diurnal_lst (ibin_max), &
               stat=ierr )
      if (ierr /= 0) call stop_sdsu(myrank,'MSG diurnal_mean_2d: allocation error -> Terminate program.')
 endif

!
! assign latitude bin
!
  do ibin = 1, ibin_max
     diurnal_lst(ibin) = real(ibin) -1.   !0~23LST (starting -0.5~0.5LST and ending 22.5~23.5LST
  enddo



!
! zero out other parameters
!
 diurnal_cnt = 0.

 diurnal_sum%rain_rate      = 0. ; diurnal_sum%h2o_col       = 0.
 diurnal_sum%path_cloud_liq = 0. ; diurnal_sum%path_rain_liq = 0.
 diurnal_sum%path_cloud_ice = 0. ; diurnal_sum%path_rain_ice = 0.
 diurnal_sum%toa_lw_up      = 0. ; diurnal_sum%toa_lw_dn     = 0.
 diurnal_sum%toa_sw_up      = 0. ; diurnal_sum%toa_sw_dn     = 0.
 diurnal_sum%boa_lw_up      = 0. ; diurnal_sum%boa_lw_dn     = 0.
 diurnal_sum%boa_sw_up      = 0. ; diurnal_sum%boa_sw_dn     = 0.

 diurnal_mean%rain_rate      = undefined ; diurnal_mean%h2o_col       = undefined
 diurnal_mean%path_cloud_liq = undefined ; diurnal_mean%path_rain_liq = undefined
 diurnal_mean%path_cloud_ice = undefined ; diurnal_mean%path_rain_ice = undefined
 diurnal_mean%toa_lw_up      = undefined ; diurnal_mean%toa_lw_dn     = undefined
 diurnal_mean%toa_sw_up      = undefined ; diurnal_mean%toa_sw_dn     = undefined
 diurnal_mean%boa_lw_up      = undefined ; diurnal_mean%boa_lw_dn     = undefined
 diurnal_mean%boa_sw_up      = undefined ; diurnal_mean%boa_sw_dn     = undefined


 case('add') !-------------------------------------------------------------------------------------

 if(masterproc) print*,'MSG diurnal_mean_2d: add'

 if(present(var2d) ) then



   !
   ! find upper bound from assumped 3D array
   !
   bnd = UBOUND(var2d)


   do j = 1,bnd(2) ; do i = 1,bnd(1)

    land_if: if( var2d(i,j)%iland == 1 ) then !over land

    latlon_if: if( var2d(i,j)%lat >= minlat .and. var2d(i,j)%lat <= maxlat .and.&
                   var2d(i,j)%lon >= minlon .and. var2d(i,j)%lon <= maxlon ) then


         hour_loc = sdsu_gmt + var2d(i,j)%lon/15.
         if( hour_loc < -0.5 ) hour_loc = hour_loc + 24.
         if( hour_loc > 23.5 ) hour_loc = hour_loc -24.


      do ibin = 1, ibin_max
         if( hour_loc >  diurnal_lst(ibin) -  0.5  .and. &
             hour_loc <= diurnal_lst(ibin) +  0.5  ) then

             diurnal_cnt(ibin) = diurnal_cnt(ibin) + 1.e0  !update total count

             diurnal_sum(ibin)%rain_rate      = diurnal_sum(ibin)%rain_rate + var2d(i,j)%rain_rate
             diurnal_sum(ibin)%h2o_col        = diurnal_sum(ibin)%h2o_col   + var2d(i,j)%h2o_col
             diurnal_sum(ibin)%path_cloud_liq = diurnal_sum(ibin)%path_cloud_liq + var2d(i,j)%path_cloud_liq
             diurnal_sum(ibin)%path_rain_liq  = diurnal_sum(ibin)%path_rain_liq  + var2d(i,j)%path_rain_liq
             diurnal_sum(ibin)%path_cloud_ice = diurnal_sum(ibin)%path_cloud_ice + var2d(i,j)%path_cloud_ice
             diurnal_sum(ibin)%path_rain_ice  = diurnal_sum(ibin)%path_rain_ice  + var2d(i,j)%path_rain_ice
             diurnal_sum(ibin)%toa_lw_up      = diurnal_sum(ibin)%toa_lw_up + var2d(i,j)%toa_lw_up
             diurnal_sum(ibin)%toa_lw_dn      = diurnal_sum(ibin)%toa_lw_dn + var2d(i,j)%toa_lw_dn
             diurnal_sum(ibin)%toa_sw_up      = diurnal_sum(ibin)%toa_sw_up + var2d(i,j)%toa_sw_up
             diurnal_sum(ibin)%toa_sw_dn      = diurnal_sum(ibin)%toa_sw_dn + var2d(i,j)%toa_sw_dn
             diurnal_sum(ibin)%boa_lw_up      = diurnal_sum(ibin)%boa_lw_up + var2d(i,j)%boa_lw_up
             diurnal_sum(ibin)%boa_lw_dn      = diurnal_sum(ibin)%boa_lw_dn + var2d(i,j)%boa_lw_dn
             diurnal_sum(ibin)%boa_sw_up      = diurnal_sum(ibin)%boa_sw_up + var2d(i,j)%boa_sw_up
             diurnal_sum(ibin)%boa_sw_dn      = diurnal_sum(ibin)%boa_sw_dn + var2d(i,j)%boa_sw_dn

         endif
      enddo

    endif latlon_if
    endif land_if

   enddo ; enddo

 endif


 case('finish') !---------------------------------------------------------------------------------------



#if MPI > 0 
!
! MPI reduce to add all together
!
  call mpi_sdsu_reduce( 'SUM', diurnal_cnt  )
  call mpi_sdsu_reduce( 'SUM', diurnal_sum%rain_rate )
  call mpi_sdsu_reduce( 'SUM', diurnal_sum%h2o_col   )
  call mpi_sdsu_reduce( 'SUM', diurnal_sum%path_cloud_liq )
  call mpi_sdsu_reduce( 'SUM', diurnal_sum%path_rain_liq )
  call mpi_sdsu_reduce( 'SUM', diurnal_sum%path_cloud_ice )
  call mpi_sdsu_reduce( 'SUM', diurnal_sum%path_rain_ice )
  call mpi_sdsu_reduce( 'SUM', diurnal_sum%toa_lw_up )
  call mpi_sdsu_reduce( 'SUM', diurnal_sum%toa_lw_dn )
  call mpi_sdsu_reduce( 'SUM', diurnal_sum%toa_sw_up )
  call mpi_sdsu_reduce( 'SUM', diurnal_sum%toa_sw_dn )
  call mpi_sdsu_reduce( 'SUM', diurnal_sum%boa_lw_up )
  call mpi_sdsu_reduce( 'SUM', diurnal_sum%boa_lw_dn )
  call mpi_sdsu_reduce( 'SUM', diurnal_sum%boa_sw_up )
  call mpi_sdsu_reduce( 'SUM', diurnal_sum%boa_sw_dn )

  MPI_MASTERPROC: if(masterproc) then
#endif

!
! derive diurnal mean values
! 
 do ibin = 1, ibin_max

    if( diurnal_cnt(ibin) > 0. ) then

     diurnal_mean(ibin)%rain_rate      = diurnal_sum(ibin)%rain_rate / diurnal_cnt(ibin)
     diurnal_mean(ibin)%h2o_col        = diurnal_sum(ibin)%h2o_col   / diurnal_cnt(ibin)
     diurnal_mean(ibin)%path_cloud_liq = diurnal_sum(ibin)%path_cloud_liq / diurnal_cnt(ibin)
     diurnal_mean(ibin)%path_rain_liq  = diurnal_sum(ibin)%path_rain_liq  / diurnal_cnt(ibin)
     diurnal_mean(ibin)%path_cloud_ice = diurnal_sum(ibin)%path_cloud_ice / diurnal_cnt(ibin)
     diurnal_mean(ibin)%path_rain_ice  = diurnal_sum(ibin)%path_rain_ice  / diurnal_cnt(ibin)
     diurnal_mean(ibin)%toa_lw_up      = diurnal_sum(ibin)%toa_lw_up / diurnal_cnt(ibin)
     diurnal_mean(ibin)%toa_lw_dn      = diurnal_sum(ibin)%toa_lw_dn / diurnal_cnt(ibin)
     diurnal_mean(ibin)%toa_sw_up      = diurnal_sum(ibin)%toa_sw_up / diurnal_cnt(ibin)
     diurnal_mean(ibin)%toa_sw_dn      = diurnal_sum(ibin)%toa_sw_dn / diurnal_cnt(ibin)
     diurnal_mean(ibin)%boa_lw_up      = diurnal_sum(ibin)%boa_lw_up / diurnal_cnt(ibin)
     diurnal_mean(ibin)%boa_lw_dn      = diurnal_sum(ibin)%boa_lw_dn / diurnal_cnt(ibin)
     diurnal_mean(ibin)%boa_sw_up      = diurnal_sum(ibin)%boa_sw_up / diurnal_cnt(ibin)
     diurnal_mean(ibin)%boa_sw_dn      = diurnal_sum(ibin)%boa_sw_dn / diurnal_cnt(ibin)

    endif

 enddo


!
! write out cloud frequency diurnal profile
!


 sdsu_io_file = trim(sdsu_dir_output)//'diurnal_mean_CRM2D'//trim(output_suffix)//'.grads'
 open(io,file=trim(sdsu_io_file),access='DIRECT',status='replace',recl=ibin_max*4)
 print("(a)"),'MSG diurnal_mean2d: see output ->',trim(sdsu_io_file)

 irec = 1
 write(io,rec=irec) ( diurnal_mean(ibin)%rain_rate      , ibin = 1, ibin_max ) ; irec = irec + 1
 write(io,rec=irec) ( diurnal_mean(ibin)%h2o_col        , ibin = 1, ibin_max ) ; irec = irec + 1
 write(io,rec=irec) ( diurnal_mean(ibin)%path_cloud_liq , ibin = 1, ibin_max ) ; irec = irec + 1
 write(io,rec=irec) ( diurnal_mean(ibin)%path_rain_liq  , ibin = 1, ibin_max ) ; irec = irec + 1
 write(io,rec=irec) ( diurnal_mean(ibin)%path_cloud_ice , ibin = 1, ibin_max ) ; irec = irec + 1
 write(io,rec=irec) ( diurnal_mean(ibin)%path_rain_ice  , ibin = 1, ibin_max ) ; irec = irec + 1
 write(io,rec=irec) ( diurnal_mean(ibin)%toa_lw_up      , ibin = 1, ibin_max ) ; irec = irec + 1
 write(io,rec=irec) ( diurnal_mean(ibin)%toa_lw_dn      , ibin = 1, ibin_max ) ; irec = irec + 1
 write(io,rec=irec) ( diurnal_mean(ibin)%toa_sw_up      , ibin = 1, ibin_max ) ; irec = irec + 1
 write(io,rec=irec) ( diurnal_mean(ibin)%toa_sw_dn      , ibin = 1, ibin_max ) ; irec = irec + 1
 write(io,rec=irec) ( diurnal_mean(ibin)%boa_lw_up      , ibin = 1, ibin_max ) ; irec = irec + 1
 write(io,rec=irec) ( diurnal_mean(ibin)%boa_lw_dn      , ibin = 1, ibin_max ) ; irec = irec + 1
 write(io,rec=irec) ( diurnal_mean(ibin)%boa_sw_up      , ibin = 1, ibin_max ) ; irec = irec + 1
 write(io,rec=irec) ( diurnal_mean(ibin)%boa_sw_dn      , ibin = 1, ibin_max ) ; irec = irec + 1

 close(io)


!
! write grads control file
!
 sdsu_io_file = trim(sdsu_dir_output)//'diurnal_mean_CRM2D'//trim(output_suffix)//'.ctl'
 print("(a)"),'MSG diurnal_mean2d: see output.ctl ->',trim(sdsu_io_file)
 open(io,file=trim(sdsu_io_file))

 sdsu_io_file = 'diurnal_mean_CRM2D'//trim(output_suffix)//'.grads'
 write(io,*) 'dset  ',trim(sdsu_io_file)
 write(io,*) 'title   diurnal mean statistics '
 write(io,*) 'undef   -999.             '
 write(io,*) 'xdef    1 linear 1. 1'
 write(io,*) 'ydef    ',ibin_max,' linear 0   1. '
 write(io,*) 'zdef    1 linear 0. 0.1'
 write(io,*) 'tdef    1 linear ',grads_time,' 1hr'
 write(io,*) 'vars ',irec-1
 write(io,*) 'rain    0  0  rain rate [mm/hr]'

 write(io,*) 'h2ocol  0  0  column water vaper [kg/m2]'
 write(io,*) 'clwp    0  0  cloud liquid water path [kg/m2]'
 write(io,*) 'lrwp    0  0  liquid rain water path [kg/m2]'
 write(io,*) 'ciwp    0  0  cloud ice water path [kg/m2]'
 write(io,*) 'irwp    0  0  ice rain water path [kg/m2]'

 write(io,*) 'lwut   0 0  TOA LW upwelling   flux [W/m2]'
 write(io,*) 'lwdt   0 0  TOA LW downwelling flux [W/m2]'
 write(io,*) 'swut   0 0  TOA SW upwelling flux   [W/m2]'
 write(io,*) 'swdt   0 0  TOA SW downwelling flux [W/m2]'
 write(io,*) 'lwus   0 0  Surface LW upwelling   flux [W/m2]'
 write(io,*) 'lwds   0 0  Surface LW downwelling flux [W/m2]'
 write(io,*) 'swus   0 0  Surface SW upwelling   flux [W/m2]'
 write(io,*) 'swds   0 0  Surface SW downwelling flux [W/m2]'

 write(io,*) 'endvars '
 close(io)


#if MPI > 0 
  endif MPI_MASTERPROC
#endif

 case default ; call stop_sdsu(myrank,'MSG diurnal_mean_2d: There is no such proc')
 end select


 return
 end subroutine diurnal_mean_2d


!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 subroutine diurnal_daily_2d(proc,var2d)
 implicit none
!--------------------------------------------------------------------------------------------------
! Comments: Derive diurnal composite of 2D parameters.
!           
! History:
! 06/2016  Toshi Matsui@NASA GSFC ; Initial.
!
! References:
!-----------------------------------------------------------------------------------------------------
 character*(*) :: proc ! process
 type ( surface_parameter ), dimension(:,:), optional :: var2d ! input surface parameter assumed 2d array

 integer :: bnd(2)  !upper memory bound for 2D array

 character :: yyyy*4, mm*2, dd*2

!
! statisticsl parameters
!
 type( surface_parameter ), allocatable, dimension(:),save :: diurnal_sum,diurnal_mean
 real(sdsu_fps), allocatable, dimension(:),save :: diurnal_lst, diurnal_cnt
 integer,save :: ibin_max
 real(sdsu_fps) :: hour_loc

 integer :: i,j, ibin, ierr, irec
 integer,parameter :: io = 153

 select case(trim(proc))
 case('init') !---------------------------------------------------------------------------------

 if( masterproc ) print*,'MSG diurnal_daily_2d: init'
!
! maximum bin of zone
!
  ibin_max  = 24

!
! allocate
!
 if( .not. allocated(diurnal_sum) ) then
     allocate( diurnal_sum (ibin_max), &
               diurnal_mean(ibin_max), &
               diurnal_cnt (ibin_max), &
               diurnal_lst (ibin_max), &
               stat=ierr )
      if (ierr /= 0) call stop_sdsu(myrank,'MSG diurnal_daily_2d: allocation error -> Terminate program.')
 endif

!
! assign latitude bin
!
  do ibin = 1, ibin_max
     diurnal_lst(ibin) = real(ibin) -1.   !0~23LST (starting -0.5~0.5LST and ending 22.5~23.5LST
  enddo


!
! zero out other parameters
!
 diurnal_cnt = 0.

 diurnal_sum%rain_rate      = 0. ; diurnal_sum%h2o_col       = 0.
 diurnal_sum%path_cloud_liq = 0. ; diurnal_sum%path_rain_liq = 0.
 diurnal_sum%path_cloud_ice = 0. ; diurnal_sum%path_rain_ice = 0.
 diurnal_sum%toa_lw_up      = 0. ; diurnal_sum%toa_lw_dn     = 0.
 diurnal_sum%toa_sw_up      = 0. ; diurnal_sum%toa_sw_dn     = 0.
 diurnal_sum%boa_lw_up      = 0. ; diurnal_sum%boa_lw_dn     = 0.
 diurnal_sum%boa_sw_up      = 0. ; diurnal_sum%boa_sw_dn     = 0.

 diurnal_mean%rain_rate      = undefined ; diurnal_mean%h2o_col       = undefined
 diurnal_mean%path_cloud_liq = undefined ; diurnal_mean%path_rain_liq = undefined
 diurnal_mean%path_cloud_ice = undefined ; diurnal_mean%path_rain_ice = undefined
 diurnal_mean%toa_lw_up      = undefined ; diurnal_mean%toa_lw_dn     = undefined
 diurnal_mean%toa_sw_up      = undefined ; diurnal_mean%toa_sw_dn     = undefined
 diurnal_mean%boa_lw_up      = undefined ; diurnal_mean%boa_lw_dn     = undefined
 diurnal_mean%boa_sw_up      = undefined ; diurnal_mean%boa_sw_dn     = undefined


 case('add') !-------------------------------------------------------------------------------------


 if(present(var2d) ) then

  if( masterproc ) print*,'MSG diurnal_daily_2d: add'


   !
   ! find upper bound from assumped 3D array
   !
   bnd = UBOUND(var2d)


   do j = 1,bnd(2) ; do i = 1,bnd(1)

    land_if: if( var2d(i,j)%iland == 1 ) then !over land

    latlon_if: if( var2d(i,j)%lat >= minlat .and. var2d(i,j)%lat <= maxlat .and.&
                   var2d(i,j)%lon >= minlon .and. var2d(i,j)%lon <= maxlon ) then

     !local solar time
     hour_loc = sdsu_gmt + var2d(i,j)%lon/15.
     if( hour_loc < -0.5 ) hour_loc = hour_loc + 24.
     if( hour_loc > 23.5 ) hour_loc = hour_loc -24.


      do ibin = 1, ibin_max
         if( hour_loc >  diurnal_lst(ibin) -  0.5  .and. &
             hour_loc <= diurnal_lst(ibin) +  0.5  ) then

             diurnal_cnt(ibin) = diurnal_cnt(ibin) + 1.e0  !update total count

             diurnal_sum(ibin)%rain_rate      = diurnal_sum(ibin)%rain_rate + var2d(i,j)%rain_rate
             diurnal_sum(ibin)%h2o_col        = diurnal_sum(ibin)%h2o_col   + var2d(i,j)%h2o_col
             diurnal_sum(ibin)%path_cloud_liq = diurnal_sum(ibin)%path_cloud_liq + var2d(i,j)%path_cloud_liq
             diurnal_sum(ibin)%path_rain_liq  = diurnal_sum(ibin)%path_rain_liq  + var2d(i,j)%path_rain_liq
             diurnal_sum(ibin)%path_cloud_ice = diurnal_sum(ibin)%path_cloud_ice + var2d(i,j)%path_cloud_ice
             diurnal_sum(ibin)%path_rain_ice  = diurnal_sum(ibin)%path_rain_ice  + var2d(i,j)%path_rain_ice
             diurnal_sum(ibin)%toa_lw_up      = diurnal_sum(ibin)%toa_lw_up + var2d(i,j)%toa_lw_up
             diurnal_sum(ibin)%toa_lw_dn      = diurnal_sum(ibin)%toa_lw_dn + var2d(i,j)%toa_lw_dn
             diurnal_sum(ibin)%toa_sw_up      = diurnal_sum(ibin)%toa_sw_up + var2d(i,j)%toa_sw_up
             diurnal_sum(ibin)%toa_sw_dn      = diurnal_sum(ibin)%toa_sw_dn + var2d(i,j)%toa_sw_dn
             diurnal_sum(ibin)%boa_lw_up      = diurnal_sum(ibin)%boa_lw_up + var2d(i,j)%boa_lw_up
             diurnal_sum(ibin)%boa_lw_dn      = diurnal_sum(ibin)%boa_lw_dn + var2d(i,j)%boa_lw_dn
             diurnal_sum(ibin)%boa_sw_up      = diurnal_sum(ibin)%boa_sw_up + var2d(i,j)%boa_sw_up
             diurnal_sum(ibin)%boa_sw_dn      = diurnal_sum(ibin)%boa_sw_dn + var2d(i,j)%boa_sw_dn

         endif
      enddo

    endif latlon_if
    endif land_if

   enddo ; enddo

 endif


 case('finish') !---------------------------------------------------------------------------------------

  if( masterproc ) print*,'MSG diurnal_daily_2d: finish'


#if MPI > 0 
!
! MPI reduce to add all together
!
  call mpi_sdsu_reduce( 'SUM', diurnal_cnt  )
  call mpi_sdsu_reduce( 'SUM', diurnal_sum%rain_rate )
  call mpi_sdsu_reduce( 'SUM', diurnal_sum%h2o_col   )
  call mpi_sdsu_reduce( 'SUM', diurnal_sum%path_cloud_liq )
  call mpi_sdsu_reduce( 'SUM', diurnal_sum%path_rain_liq )
  call mpi_sdsu_reduce( 'SUM', diurnal_sum%path_cloud_ice )
  call mpi_sdsu_reduce( 'SUM', diurnal_sum%path_rain_ice )
  call mpi_sdsu_reduce( 'SUM', diurnal_sum%toa_lw_up )
  call mpi_sdsu_reduce( 'SUM', diurnal_sum%toa_lw_dn )
  call mpi_sdsu_reduce( 'SUM', diurnal_sum%toa_sw_up )
  call mpi_sdsu_reduce( 'SUM', diurnal_sum%toa_sw_dn )
  call mpi_sdsu_reduce( 'SUM', diurnal_sum%boa_lw_up )
  call mpi_sdsu_reduce( 'SUM', diurnal_sum%boa_lw_dn )
  call mpi_sdsu_reduce( 'SUM', diurnal_sum%boa_sw_up )
  call mpi_sdsu_reduce( 'SUM', diurnal_sum%boa_sw_dn )

  MPI_MASTERPROC: if(masterproc) then
#endif

!
! derive diurnal mean values
! 
 do ibin = 1, ibin_max

    if( diurnal_cnt(ibin) > 0. ) then

     diurnal_mean(ibin)%rain_rate      = diurnal_sum(ibin)%rain_rate / diurnal_cnt(ibin)
     diurnal_mean(ibin)%h2o_col        = diurnal_sum(ibin)%h2o_col   / diurnal_cnt(ibin)
     diurnal_mean(ibin)%path_cloud_liq = diurnal_sum(ibin)%path_cloud_liq / diurnal_cnt(ibin)
     diurnal_mean(ibin)%path_rain_liq  = diurnal_sum(ibin)%path_rain_liq  / diurnal_cnt(ibin)
     diurnal_mean(ibin)%path_cloud_ice = diurnal_sum(ibin)%path_cloud_ice / diurnal_cnt(ibin)
     diurnal_mean(ibin)%path_rain_ice  = diurnal_sum(ibin)%path_rain_ice  / diurnal_cnt(ibin)
     diurnal_mean(ibin)%toa_lw_up      = diurnal_sum(ibin)%toa_lw_up / diurnal_cnt(ibin)
     diurnal_mean(ibin)%toa_lw_dn      = diurnal_sum(ibin)%toa_lw_dn / diurnal_cnt(ibin)
     diurnal_mean(ibin)%toa_sw_up      = diurnal_sum(ibin)%toa_sw_up / diurnal_cnt(ibin)
     diurnal_mean(ibin)%toa_sw_dn      = diurnal_sum(ibin)%toa_sw_dn / diurnal_cnt(ibin)
     diurnal_mean(ibin)%boa_lw_up      = diurnal_sum(ibin)%boa_lw_up / diurnal_cnt(ibin)
     diurnal_mean(ibin)%boa_lw_dn      = diurnal_sum(ibin)%boa_lw_dn / diurnal_cnt(ibin)
     diurnal_mean(ibin)%boa_sw_up      = diurnal_sum(ibin)%boa_sw_up / diurnal_cnt(ibin)
     diurnal_mean(ibin)%boa_sw_dn      = diurnal_sum(ibin)%boa_sw_dn / diurnal_cnt(ibin)

    endif

 enddo

!
! write out cloud frequency diurnal profile
!
 write(yyyy,"(I4.4)") int( sdsu_yyyy )
 write(mm  ,"(I2.2)") int( sdsu_mm   )
 write(dd  ,"(I2.2)") int( sdsu_dd   )

 sdsu_io_file = trim(sdsu_dir_output)//'diurnal_mean_CRM2D_'//yyyy//'_'//mm//'_'//dd&
                //trim(output_suffix)//'.grads'
 print("(a)"),'MSG diurnal_mean2d: see output ->',trim(sdsu_io_file)

 open(io,file=trim(sdsu_io_file),access='DIRECT',status='replace',recl=ibin_max*4)

 irec = 1
 write(io,rec=irec) ( diurnal_mean(ibin)%rain_rate      , ibin = 1, ibin_max ) ; irec = irec + 1
 write(io,rec=irec) ( diurnal_mean(ibin)%h2o_col        , ibin = 1, ibin_max ) ; irec = irec + 1
 write(io,rec=irec) ( diurnal_mean(ibin)%path_cloud_liq , ibin = 1, ibin_max ) ; irec = irec + 1
 write(io,rec=irec) ( diurnal_mean(ibin)%path_rain_liq  , ibin = 1, ibin_max ) ; irec = irec + 1
 write(io,rec=irec) ( diurnal_mean(ibin)%path_cloud_ice , ibin = 1, ibin_max ) ; irec = irec + 1
 write(io,rec=irec) ( diurnal_mean(ibin)%path_rain_ice  , ibin = 1, ibin_max ) ; irec = irec + 1
 write(io,rec=irec) ( diurnal_mean(ibin)%toa_lw_up      , ibin = 1, ibin_max ) ; irec = irec + 1
 write(io,rec=irec) ( diurnal_mean(ibin)%toa_lw_dn      , ibin = 1, ibin_max ) ; irec = irec + 1
 write(io,rec=irec) ( diurnal_mean(ibin)%toa_sw_up      , ibin = 1, ibin_max ) ; irec = irec + 1
 write(io,rec=irec) ( diurnal_mean(ibin)%toa_sw_dn      , ibin = 1, ibin_max ) ; irec = irec + 1
 write(io,rec=irec) ( diurnal_mean(ibin)%boa_lw_up      , ibin = 1, ibin_max ) ; irec = irec + 1
 write(io,rec=irec) ( diurnal_mean(ibin)%boa_lw_dn      , ibin = 1, ibin_max ) ; irec = irec + 1
 write(io,rec=irec) ( diurnal_mean(ibin)%boa_sw_up      , ibin = 1, ibin_max ) ; irec = irec + 1
 write(io,rec=irec) ( diurnal_mean(ibin)%boa_sw_dn      , ibin = 1, ibin_max ) ; irec = irec + 1

 close(io)


!
! write grads control file
!


 sdsu_io_file = trim(sdsu_dir_output)//'diurnal_mean_CRM2D_'//yyyy//'_'//mm//'_'//dd&
                //trim(output_suffix)//'.ctl'

 print("(a)"),'MSG diurnal_mean2d: see output.ctl ->',trim(sdsu_io_file)
 open(io,file=trim(sdsu_io_file))

 sdsu_io_file = 'diurnal_mean_CRM2D_'//yyyy//'_'//mm//'_'//dd&
                //trim(output_suffix)//'.grads'

 write(io,*) 'dset  ',trim(sdsu_io_file)
 write(io,*) 'title   diurnal mean statistics '
 write(io,*) 'undef   -999.             '
 write(io,*) 'xdef    1 linear 1. 1'
 write(io,*) 'ydef    ',ibin_max,' linear 0   1. '
 write(io,*) 'zdef    1 linear 0. 0.1'
 write(io,*) 'tdef    1 linear ',grads_time,' 1hr'
 write(io,*) 'vars ',irec-1
 write(io,*) 'rain    0  0  rain rate [mm/hr]'

 write(io,*) 'h2ocol  0  0  column water vaper [kg/m2]'
 write(io,*) 'clwp    0  0  cloud liquid water path [kg/m2]'
 write(io,*) 'lrwp    0  0  liquid rain water path [kg/m2]'
 write(io,*) 'ciwp    0  0  cloud ice water path [kg/m2]'
 write(io,*) 'irwp    0  0  ice rain water path [kg/m2]'

 write(io,*) 'lwut   0 0  TOA LW upwelling   flux [W/m2]'
 write(io,*) 'lwdt   0 0  TOA LW downwelling flux [W/m2]'
 write(io,*) 'swut   0 0  TOA SW upwelling flux   [W/m2]'
 write(io,*) 'swdt   0 0  TOA SW downwelling flux [W/m2]'
 write(io,*) 'lwus   0 0  Surface LW upwelling   flux [W/m2]'
 write(io,*) 'lwds   0 0  Surface LW downwelling flux [W/m2]'
 write(io,*) 'swus   0 0  Surface SW upwelling   flux [W/m2]'
 write(io,*) 'swds   0 0  Surface SW downwelling flux [W/m2]'

 write(io,*) 'endvars '
 close(io)


#if MPI > 0 
  endif MPI_MASTERPROC
#endif

 case default ; call stop_sdsu(myrank,'MSG diurnal_daily_2d: There is no such proc')
 end select


 return
 end subroutine diurnal_daily_2d

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 subroutine diurnal_mean_3d(proc,var3d,press_stag,var2d)
 implicit none
!--------------------------------------------------------------------------------------------------
! Comments: Derive diurnal composite of 3D parameters.
!           
! History:
! 06/2016  Toshi Matsui@NASA GSFC ; Initial.
!
! References:
!-----------------------------------------------------------------------------------------------------
 character*(*) :: proc ! process
 type ( atmos_parameter ), dimension(:,:,:), optional :: var3d ! input surface parameter assumed 3d array
 real, dimension(:,:,:), optional :: press_stag ! input pressure sraggered array
 type ( surface_parameter ), dimension(:,:), optional :: var2d ! input surface parameter assumed 3d array

 integer :: bnd(3)  !upper memory bound for 2D array

!
! statisticsl parameters
!
 type( atmos_parameter ), allocatable, dimension(:,:),save :: diurnal_sum,diurnal_mean
 real(sdsu_fps), allocatable, save :: diurnal_lst(:), diurnal_cnt(:,:)
 integer,save :: ibin_max
 real(sdsu_fps) :: hour_loc

 integer :: i,j,k,p, ibin, ierr, irec
 integer,parameter :: io = 154

 integer,parameter :: plev = 20
 real(sdsu_fps),parameter :: pressure_levels(20) = (/1000.,950.,900.,850.,800.,750.,700.,650.,600.,550.,500.,&
                                                     450.,400.,350.,300.,250.,200.,150.,100.,50./)

 real(sdsu_fps) :: pr

 select case(trim(proc))
 case('init') !---------------------------------------------------------------------------------
!print*,'MSG tb_precip_sample: init'
 if(masterproc) print*,'MSG diurnal_mean_3d: initialize parameters'

!
! maximum bin of zone
!
  ibin_max  = 24

!
! allocate
!
 if( .not. allocated(diurnal_sum) ) then
     allocate( diurnal_sum (ibin_max,plev), &
               diurnal_mean(ibin_max,plev), &
               diurnal_cnt (ibin_max,plev), &
               diurnal_lst (ibin_max), &
               stat=ierr )
      if (ierr /= 0) call stop_sdsu(myrank,'MSG diurnal_mean_3d: allocation error -> Terminate program.')
 endif

!
! assign latitude bin
!
  do ibin = 1, ibin_max
     diurnal_lst(ibin) = real(ibin) -1.   !0~23LST (starting -0.5~0.5LST and ending 22.5~23.5LST
  enddo


!
! zero out other parameters
!
 diurnal_cnt = 0.

 diurnal_sum%rain_rate      = 0. ; diurnal_sum%sh            = 0.
 diurnal_sum%w              = 0. ; diurnal_sum%latent_heat   = 0.
 diurnal_sum%lw_heat        = 0. ; diurnal_sum%sw_heat       = 0.

 diurnal_mean%rain_rate      = undefined ; diurnal_mean%sh            = undefined
 diurnal_mean%w              = undefined ; diurnal_mean%latent_heat   = undefined
 diurnal_mean%lw_heat        = undefined ; diurnal_mean%sw_heat       = undefined


 case('add') !-------------------------------------------------------------------------------------


 if(present(var3d) ) then

  if(masterproc) print*,'MSG diurnal_mean_3d: add parameters'

   !
   ! find upper bound from assumped 3D array
   !
   bnd = UBOUND(var3d)


   do j = 1,bnd(2) ; do i = 1,bnd(1)


    land_if: if( var2d(i,j)%iland == 1 ) then !over land

    latlon_if: if( var2d(i,j)%lat >= minlat .and. var2d(i,j)%lat <= maxlat .and.&
                   var2d(i,j)%lon >= minlon .and. var2d(i,j)%lon <= maxlon ) then

      !
      ! estimate local solar hour
      !
      hour_loc = sdsu_gmt + var2d(i,j)%lon/15.

      if( hour_loc < -0.5 ) hour_loc = hour_loc + 24.
      if( hour_loc > 23.5 ) hour_loc = hour_loc -24.

      do ibin = 1, ibin_max

         if( hour_loc >  diurnal_lst(ibin) -  0.5  .and. &
             hour_loc <= diurnal_lst(ibin) +  0.5  ) then

            !
            ! interpolation level
            !
            do p = 1, plev
               pr = pressure_levels(p)
               do k = 1, bnd(3)
                  if( pr <= press_stag(i,j,k) .and. pr > press_stag(i,j,k+1) ) then
                     diurnal_cnt(ibin,p)           = diurnal_cnt(ibin,p) + 1.
                     diurnal_sum(ibin,p)%rain_rate = diurnal_sum(ibin,p)%rain_rate + var3d(i,j,k)%rain_rate
                     diurnal_sum(ibin,p)%sh        = diurnal_sum(ibin,p)%sh        + var3d(i,j,k)%sh
                     diurnal_sum(ibin,p)%w         = diurnal_sum(ibin,p)%w         + var3d(i,j,k)%w
                     diurnal_sum(ibin,p)%latent_heat = diurnal_sum(ibin,p)%latent_heat + var3d(i,j,k)%latent_heat
                     diurnal_sum(ibin,p)%lw_heat   = diurnal_sum(ibin,p)%lw_heat   + var3d(i,j,k)%lw_heat
                     diurnal_sum(ibin,p)%sw_heat   = diurnal_sum(ibin,p)%sw_heat   + var3d(i,j,k)%sw_heat
                  endif
               enddo

            enddo

         endif
      enddo

    endif latlon_if
    endif land_if

   enddo ; enddo

 endif

!call mpi_wait_for_master !this process hold slave processors to wait for master proc. 
!stop

 case('finish') !---------------------------------------------------------------------------------------


#if MPI > 0 
!
! MPI reduce to add all together
!
  call mpi_sdsu_reduce( 'SUM', diurnal_cnt  )
  call mpi_sdsu_reduce( 'SUM', diurnal_sum%rain_rate )
  call mpi_sdsu_reduce( 'SUM', diurnal_sum%sh        )
  call mpi_sdsu_reduce( 'SUM', diurnal_sum%w         )
  call mpi_sdsu_reduce( 'SUM', diurnal_sum%latent_heat)
  call mpi_sdsu_reduce( 'SUM', diurnal_sum%lw_heat   )
  call mpi_sdsu_reduce( 'SUM', diurnal_sum%sw_heat   )

  MPI_MASTERPROC: if(masterproc) then
#endif

!
! derive diurnal mean values
! 
 do p = 1, plev ; do ibin = 1, ibin_max

    if( diurnal_cnt(ibin,p) > 0. ) then

     diurnal_mean(ibin,p)%rain_rate      = diurnal_sum(ibin,p)%rain_rate / diurnal_cnt(ibin,p)
     diurnal_mean(ibin,p)%sh             = diurnal_sum(ibin,p)%sh        / diurnal_cnt(ibin,p)
     diurnal_mean(ibin,p)%w              = diurnal_sum(ibin,p)%w         / diurnal_cnt(ibin,p)
     diurnal_mean(ibin,p)%latent_heat    = diurnal_sum(ibin,p)%latent_heat / diurnal_cnt(ibin,p)
     diurnal_mean(ibin,p)%lw_heat        = diurnal_sum(ibin,p)%lw_heat   / diurnal_cnt(ibin,p)
     diurnal_mean(ibin,p)%sw_heat        = diurnal_sum(ibin,p)%sw_heat   / diurnal_cnt(ibin,p)

    endif

 enddo ; enddo


!
! write out cloud frequency diurnal profile
!

 sdsu_io_file = trim(sdsu_dir_output)//'diurnal_mean_CRM3D.grads'
 open(io,file=trim(sdsu_io_file),access='DIRECT',status='replace',recl=ibin_max*plev*4)
 print("(a)"),'MSG diurnal_mean3d: see output ->',trim(sdsu_io_file)

 irec = 1
 write(io,rec=irec) ( ( diurnal_mean(ibin,p)%rain_rate   , ibin = 1, ibin_max ), p =1,plev) ; irec = irec + 1
 write(io,rec=irec) ( ( diurnal_mean(ibin,p)%sh          , ibin = 1, ibin_max ), p =1,plev) ; irec = irec + 1
 write(io,rec=irec) ( ( diurnal_mean(ibin,p)%w           , ibin = 1, ibin_max ), p =1,plev) ; irec = irec + 1
 write(io,rec=irec) ( ( diurnal_mean(ibin,p)%latent_heat , ibin = 1, ibin_max ), p =1,plev) ; irec = irec + 1
 write(io,rec=irec) ( ( diurnal_mean(ibin,p)%lw_heat     , ibin = 1, ibin_max ), p =1,plev) ; irec = irec + 1
 write(io,rec=irec) ( ( diurnal_mean(ibin,p)%sw_heat     , ibin = 1, ibin_max ), p =1,plev) ; irec = irec + 1

 close(io)


!
! write grads control file
!
 sdsu_io_file = trim(sdsu_dir_output)//'diurnal_mean_CRM3D'//trim(output_suffix)//'.ctl'
 print("(a)"),'MSG diurnal_mean2d: see output.ctl ->',trim(sdsu_io_file)
 open(io,file=trim(sdsu_io_file))

 sdsu_io_file = 'diurnal_mean_CRM3D'//trim(output_suffix)//'.grads'
 write(io,*) 'dset  ',trim(sdsu_io_file)
 write(io,*) 'title   diurnal mean statistics '
 write(io,*) 'undef   -999.             '
 write(io,*) 'xdef    1 linear 1. 1'
 write(io,*) 'ydef    ',ibin_max,' linear 0   1. '
 write(io,*) 'zdef    ',plev,' levels ', pressure_levels(1:plev)
 write(io,*) 'tdef    1 linear ',grads_time,' 1hr'
 write(io,*) 'vars   ',irec-1
 write(io,*) 'rain   ',plev,'  0  rain rate [mm/hr]'
 write(io,*) 'sh     ',plev,'  0  specific humidity [kg/kg]'
 write(io,*) 'w      ',plev,'  0  vertical velocity [m/s]'
 write(io,*) 'lhr    ',plev,'  0  latent heat release [K/day]'
 write(io,*) 'lwh    ',plev,'  0  long wave heating [K/day]'
 write(io,*) 'swh    ',plev,'  0  short wave heating [K/day]'
 write(io,*) 'endvars'
 close(io)


#if MPI > 0 
  endif MPI_MASTERPROC
#endif

 case default ; call stop_sdsu(myrank,'MSG diurnal_mean_3d: There is no such proc')
 end select


 return
 end subroutine diurnal_mean_3d

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 end module module_diag


