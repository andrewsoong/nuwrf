!-----------------------BEGIN NOTICE -- DO NOT EDIT-----------------------
! NASA Goddard Space Flight Center Land Information System (LIS) v7.2
!
! Copyright (c) 2015 United States Government as represented by the
! Administrator of the National Aeronautics and Space Administration.
! All Rights Reserved.
!-------------------------END NOTICE -- DO NOT EDIT-----------------------
!BOP
!
! !ROUTINE: noahmp36_setwrfexport.F90
!
! !DESCRIPTION:  
!  Defines the export states from Noah to WRF in coupled mode
!
! !REVISION HISTORY:
! 02 Dec 2003; Sujay Kumar, Initial Version
! 17 Nov 2008; Sujay Kumar, Modified for the ESMF coupled version
! 11 Dec 2015; Eric Kemp, updated for Noah 3.6.
! 08 Nov 2018; Carlos Cruz, Adapted for NoahMP 3.6.
! 
! !INTERFACE:
subroutine noahmp36_setwrfexport(n)
! !USES:
  use ESMF
  use LIS_coreMod
  use LIS_historyMod, only : LIS_patch2tile
  use LIS_logMod
  use LISWRFGridCompMod, only : LISWRF_export
  use noahmp36_lsmMod
  
  implicit none
! !ARGUMENTS:
  integer, intent(in) :: n 
!EOP
  integer               :: i,j,k,t
  real, allocatable     :: temp(:)

  allocate(temp(LIS_rc%npatch(n,LIS_rc%lsm_index)))

  call LIS_patch2tile(n,LIS_rc%lsm_index,LISWRF_export(n)%avgsurft_t,&
       noahmp36_struc(n)%noahmp36%t1)
  
  call LIS_patch2tile(n,LIS_rc%lsm_index,LISWRF_export(n)%qh_t,&
       noahmp36_struc(n)%noahmp36%fsh)

  call LIS_patch2tile(n,LIS_rc%lsm_index,LISWRF_export(n)%eta_kinematic_t,&
       noahmp36_struc(n)%noahmp36%eta_kinematic)

  call LIS_patch2tile(n,LIS_rc%lsm_index,LISWRF_export(n)%qle_t,&
       noahmp36_struc(n)%noahmp36%qle)
  
  call LIS_patch2tile(n,LIS_rc%lsm_index,LISWRF_export(n)%qg_t,&
       noahmp36_struc(n)%noahmp36%ssoil)
  
  call LIS_patch2tile(n,LIS_rc%lsm_index,LISWRF_export(n)%albedo_t,&
       noahmp36_struc(n)%noahmp36%albedo)

! roughness length
  call LIS_patch2tile(n,LIS_rc%lsm_index,LISWRF_export(n)%znt_t,&
       noahmp36_struc(n)%noahmp36%z0)

! mixing ratio
  call LIS_patch2tile(n,LIS_rc%lsm_index,LISWRF_export(n)%q1_t,&
       noahmp36_struc(n)%noahmp36%q1)

  do i=1,LIS_rc%npatch(n,LIS_rc%lsm_index)
     temp(i) = noahmp36_struc(n)%noahmp36(i)%smc(1)
  enddo
  call LIS_patch2tile(n,LIS_rc%lsm_index,LISWRF_export(n)%smc1_t,&
       temp)
  do i=1,LIS_rc%npatch(n,LIS_rc%lsm_index)
     temp(i) = noahmp36_struc(n)%noahmp36(i)%smc(2)
  enddo
  call LIS_patch2tile(n,LIS_rc%lsm_index,LISWRF_export(n)%smc2_t,&
       temp)
  do i=1,LIS_rc%npatch(n,LIS_rc%lsm_index)
     temp(i) = noahmp36_struc(n)%noahmp36(i)%smc(3)
  enddo
  call LIS_patch2tile(n,LIS_rc%lsm_index,LISWRF_export(n)%smc3_t,&
       temp)
  do i=1,LIS_rc%npatch(n,LIS_rc%lsm_index)
     temp(i) = noahmp36_struc(n)%noahmp36(i)%smc(4)
  enddo
  call LIS_patch2tile(n,LIS_rc%lsm_index,LISWRF_export(n)%smc4_t,&
       temp)
  
  do i=1,LIS_rc%npatch(n,LIS_rc%lsm_index)
     temp(i) = noahmp36_struc(n)%noahmp36(i)%sstc(1)
  enddo
  
  call LIS_patch2tile(n,LIS_rc%lsm_index,LISWRF_export(n)%stc1_t,temp)
  do i=1,LIS_rc%npatch(n,LIS_rc%lsm_index)
     temp(i) = noahmp36_struc(n)%noahmp36(i)%sstc(2)
  enddo
  
  call LIS_patch2tile(n,LIS_rc%lsm_index,LISWRF_export(n)%stc2_t,temp)
  do i=1,LIS_rc%npatch(n,LIS_rc%lsm_index)
     temp(i) = noahmp36_struc(n)%noahmp36(i)%sstc(3)
  enddo
  
  call LIS_patch2tile(n,LIS_rc%lsm_index,LISWRF_export(n)%stc3_t,temp)
  do i=1,LIS_rc%npatch(n,LIS_rc%lsm_index)
     temp(i) = noahmp36_struc(n)%noahmp36(i)%sstc(4)
  enddo
  call LIS_patch2tile(n,LIS_rc%lsm_index,LISWRF_export(n)%stc4_t,temp)

  call LIS_patch2tile(n,LIS_rc%lsm_index,LISWRF_export(n)%chs2_t,&
       noahmp36_struc(n)%noahmp36%chs2)
  
  call LIS_patch2tile(n,LIS_rc%lsm_index,LISWRF_export(n)%cqs2_t,&
       noahmp36_struc(n)%noahmp36%cqs2)
  
! snow cover fraction  
  call LIS_patch2tile(n,LIS_rc%lsm_index,LISWRF_export(n)%snocvr_t,&
       noahmp36_struc(n)%noahmp36%fsno)
  
  call LIS_patch2tile(n,LIS_rc%lsm_index,LISWRF_export(n)%snow_t,&
       noahmp36_struc(n)%noahmp36%sneqv*1000.0)
  
  ! NUWRF EMK
  call LIS_patch2tile(n,LIS_rc%lsm_index,LISWRF_export(n)%snowh_t,&
       noahmp36_struc(n)%noahmp36%snowh)
  
  call LIS_patch2tile(n,LIS_rc%lsm_index,LISWRF_export(n)%lispor_t,&
       noahmp36_struc(n)%noahmp36%smcmax)

  call LIS_patch2tile(n,LIS_rc%lsm_index,LISWRF_export(n)%rootmoist_t,&
       noahmp36_struc(n)%noahmp36%rootmoist) !units?
  
  call LIS_patch2tile(n,LIS_rc%lsm_index,LISWRF_export(n)%soilm_t,&
       noahmp36_struc(n)%noahmp36%soilm)
  call LIS_patch2tile(n,LIS_rc%lsm_index,LISWRF_export(n)%qs_t,&
       noahmp36_struc(n)%noahmp36%qs)
  call LIS_patch2tile(n,LIS_rc%lsm_index,LISWRF_export(n)%qsb_t,&
       noahmp36_struc(n)%noahmp36%qsb)
  call LIS_patch2tile(n,LIS_rc%lsm_index,LISWRF_export(n)%cmc_t,&
       noahmp36_struc(n)%noahmp36%canliq)
  call LIS_patch2tile(n,LIS_rc%lsm_index,LISWRF_export(n)%qsm_t,&
       noahmp36_struc(n)%noahmp36%qsnbot)
  call LIS_patch2tile(n,LIS_rc%lsm_index,LISWRF_export(n)%emiss_t,&
       noahmp36_struc(n)%noahmp36%emissi)
  call LIS_patch2tile(n,LIS_rc%lsm_index,LISWRF_export(n)%xice_t,&
       noahmp36_struc(n)%noahmp36%xice)

  do i=1,LIS_rc%npatch(n,LIS_rc%lsm_index)
     temp(i) = noahmp36_struc(n)%noahmp36(i)%sh2o(1)
  enddo
  call LIS_patch2tile(n,LIS_rc%lsm_index,LISWRF_export(n)%sh2o1_t,temp)
  do i=1,LIS_rc%npatch(n,LIS_rc%lsm_index)
     temp(i) = noahmp36_struc(n)%noahmp36(i)%sh2o(2)
  enddo
  call LIS_patch2tile(n,LIS_rc%lsm_index,LISWRF_export(n)%sh2o2_t,temp)
  do i=1,LIS_rc%npatch(n,LIS_rc%lsm_index)
     temp(i) = noahmp36_struc(n)%noahmp36(i)%sh2o(3)
  enddo
  call LIS_patch2tile(n,LIS_rc%lsm_index,LISWRF_export(n)%sh2o3_t,temp)
  do i=1,LIS_rc%npatch(n,LIS_rc%lsm_index) 
     temp(i) = noahmp36_struc(n)%noahmp36(i)%sh2o(4)
  enddo
  call LIS_patch2tile(n,LIS_rc%lsm_index,LISWRF_export(n)%sh2o4_t,temp)

  do i=1,LIS_rc%npatch(n,LIS_rc%lsm_index)
     temp(i) = noahmp36_struc(n)%noahmp36(i)%relsmc(1)
  enddo
  call LIS_patch2tile(n,LIS_rc%lsm_index,LISWRF_export(n)%relsmc1_t,temp)
  do i=1,LIS_rc%npatch(n,LIS_rc%lsm_index)
     temp(i) = noahmp36_struc(n)%noahmp36(i)%relsmc(2)
  enddo
  call LIS_patch2tile(n,LIS_rc%lsm_index,LISWRF_export(n)%relsmc2_t,temp)
  do i=1,LIS_rc%npatch(n,LIS_rc%lsm_index)
     temp(i) = noahmp36_struc(n)%noahmp36(i)%relsmc(3)
  enddo
  call LIS_patch2tile(n,LIS_rc%lsm_index,LISWRF_export(n)%relsmc3_t,temp)
  do i=1,LIS_rc%npatch(n,LIS_rc%lsm_index) 
     temp(i) = noahmp36_struc(n)%noahmp36(i)%relsmc(4)
  enddo
  call LIS_patch2tile(n,LIS_rc%lsm_index,LISWRF_export(n)%relsmc4_t,temp)

  do i=1,LIS_rc%npatch(n,LIS_rc%lsm_index) 
     temp(i) = real(noahmp36_struc(n)%noahmp36(i)%tg)
  enddo
  call LIS_patch2tile(n,LIS_rc%lsm_index,LISWRF_export(n)%xland_t,temp)

  deallocate(temp)
  !  do j=1,LIS_rc%lnr(n)
  !     do i=1,LIS_rc%lnc(n)
  !        k = LIS_domain(n)%gindex(i,j) 
  !        if(LIS_domain(n)%gindex(i,j).ne.-1) then 
  !           LISWRF_export(n)%xland(i,j) = noahmp36_struc(n)%noahmp36(k)%vegt
  !        endif
  !     enddo
  !  enddo

end subroutine noahmp36_setwrfexport
 
