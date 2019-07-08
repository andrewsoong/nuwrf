!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!------------------------------------------------------------------------------
!              = Goddard Satellite Data Simulation Unit =
!                        Configuration File
! Comments:
!   Users must configure each section of various options before running Goddard SDSU.
!   Configuration File is written in Fortran NameList-style text file, but 
!   you DO NOT need to re-compile codes after modification.
!   You must also configure MPI / HDF option in define_CPP.h.
!
! History:
! 09/2017  Toshi Matsui@NASA GSFC : Updated for V4.1
! 09/2016  Toshi Matsui@NASA GSFC : Updated for V4.0 
! 09/2015  Toshi Matsui@NASA GSFC : Updated for V3.5
! 03/2014  Toshi Matsui@NASA GSFC : Updated for V3.3.3
! 12/2009  Toshi Matsui@NASA GSFC : Initial.
!
! References:
!
! Matsui, T., J. Santanello, J. J. Shi, W.-K. Tao, D. Wu, C. Peters-Lidard, E. Kemp, M. Chin, 
!   D. Starr, M. Sekiguchi, and F. Aires, (2014): Introducing multisensor satellite radiance-based 
!   evaluation for regional Earth System modeling, Journal of Geophysical Research, 119, 8450-8475, 
!   doi:10.1002/2013JD021424.
!------------------------------------------------------------------------------

! #############################################################################
! #####################  Configure Simulator Switch  ##########################
! #############################################################################

$simulator_switch
  micro  = .false.     ! microwave simulator switch; on when .true. (logical)
  radar  = .false.     ! radar simulator switch; on  when .true.  (logical)
  visir  = .false.     ! visble/IR simulator switch; on when .true. (logical)
  lidar  = .false.     ! Lidar simulator switch; on when .true. (logical)
  broad  = .false.     ! Broad-band simulator switch; on when .true. (logical)
  sarta  = .false.      ! SARTA (AIRS) simulator switch; on when .true. (logical)
  GV     = .false.      ! GV simulator switch; on when .true. (logical)
  polarris = .false.    !POLLLARIS switch; on when .true. (logical)
$end


! #############################################################################
! ##################  Configure Input-Output Options ##########################
! #############################################################################

$io_options
  sdsu_dir_sslut= './../LUT/'         ! directory for the single-scattering LUTs (character)
  sdsu_dir_data = './../DATAFILES/'     ! directory for various datafiles needed for simulator (character)

  sdsu_io_name  = 'inpfile'

  verbose_SDSU  = .false.               ! if true, print out more comments during run. (logical)
  write_surface = .false.               ! if true, write out surface single scattering properties (logical)
  write_opt     = .false.               ! if true, write out single scattering properties (logical)
  write_CRM3D   = .false.                ! if true, write out CRM 3D input file in GrADS format (logical)
  write_CRM2D   = .false.                ! if true, write out CRM 2D input file in GrADS format (logical)
  write_grads_ctl = .true.              ! if true, write grads control files (logical)
  write_psdmmt  = .false.               ! if true, write out PSD moment value from HUCM SBM
  output_suffix = ''       ! output suffix

! for lat-lon output domain needs to be specified for orbital simulator and crm_diagnostic process
  minlat        =  -5.0                  ! min longitude of lat-lon output ( -90 ~  90) [deg]
  maxlat        =  25.0                  ! max latitude  of lat-lon output ( -90 ~  90) [deg]
  minlon        = -20.0                  ! min longitude of lat-lon output (-180 ~ 180) [deg]
  maxlon        =  3.0                   ! max longitude of lat-lon output (-180 ~ 180) [deg]
  res_latlon    =  0.5                   ! grid spacing  of lat-lon output [deg]

  diag_crm_spatial_mean = .false.   ! if true, CRM time-averaged gridded values are clacualted based on the grid specified below. (MPI=0 or 2)
  diag_crm_spatial_mean_daily = .false.   ! if true, CRM time-averaged gridded values are clacualted based on the grid specified below. (MPI=0 or 2)
  diag_crm_spatial_mean_hourly = .false.  ! if true, CRM time-averaged gridded values are clacualted based on the grid specified below. (MPI=0 or 2)
  diag_crm_diurnal_mean = .false.   ! if true, CRM time-averaged diurnal cycle are clacualted based on the grid specified below. (MPI=0 or 2)
  diag_crm_diurnal_daily= .false.   ! if true, CRM daily diurnal cycle are clacualted based on the grid specified below. (MPI=0 or 2)

$end


! #############################################################################
! #####################  Configure Input Model options ##########################
! #############################################################################

$crm_options

! AMMA 3KM case 
  sim_case  = 'WRF'  ! Weatether Research & Forecasting Model (character*10)

  sdsu_dir_input = './../INPUTS/'    ! input directory (character*) finish with /
  sdsu_dir_output = './../OUTPUTS/'  ! output directory (character* finish with /

  mxgridx   = 672  ! max grid # in horizontal x direction  (integer)  (can be smaller than west_east)
  mxgridy   = 594  ! max grid # in horizontal y direction (integer)   (can be smaller than south_north)
  mxlyr     = 60     ! max grid # in vertical direction  (integer) (must be exactly bottom_top )
  gridsize  = 1.e0   ! horizontal grid spacings [km] (real)

 cloud_microphysics = 'GMP4ICE'  ! Cloud Microphyiscs Type (character*20)
                             ! GOD: Goddard 3ICE scheme [Tao et al. 2003] 
                             ! GOD10: Goddard Improve 3ICE scheme [Lang et al. 2010]
                             ! GMP4ICE: Goddard 4ICE scheme [Lang et al. 2014]
                             ! LIN: LIN bulk 1-mmt scheme  [Lin et al. ]
                             ! WSM: WRF-Single-Moment 6-Class Scheme [Hong et al. 2004]
                             ! WDM: WRF-Double-Moment 6-Class Scheme [Hong et al. 2010]
                             ! MORR: Morrison two-moment scheme [Morrison et al. 2009]
                             ! THOM: Thompson hybrid moment scheme [Thompson et al. 2008]
                             ! NTUCLR: National Taiwan University semi-two moment microphysics [Chen and Liu 2004]
                             ! RAMS1: RAMS 1-mmt scheme [Walko et al., 1995]
                             ! RAMS2: RAMS 2-mmt scheme [Meyers et al., 1997] 
                             ! HUCM_SBM: HUCM spectra-bin microphysics scheme [Khain et al. 2007]  
                             ! HUCM_SBM43: HUCM spectra-bin microphysics 43 bin scheme [Khain et al. 2010] 

 gsfc_hail = .false.         ! GSFC hail options (graupel --> hail)


!------------- idealized scence option -----------------------

 clear_sky_scene = .false.   ! if .true., zero out all condensates (cloud-precip) to create clear sky. 
                                         
 uniform_surface = .false.             ! When it is true, this option assigns spatially uniform
                                       ! surface characters over the entire domain. 
                                       ! (When sim_case='GCE', this must be always .true., because GCE input 
                                       !  does not have these surface parameters.)

 idealized_surface%lat       =   2.62e0 ! latitude [deg] 
 idealized_surface%lon       =  76.97e0 ! lon      [deg] 
 idealized_surface%frac_veg  =   0.e0  ! vegetation fraction [%] (optional for WRF)
 idealized_surface%albedo    =   0.05  ! surface SW albedo [-]
 idealized_surface%h2o_snow  =   0.e0  ! snow water equivalent [kg m-2] 
 idealized_surface%h2o_soil  =   0.9e0 ! soil moisture fraction [0-1]
 idealized_surface%elev      =   0.e0  ! surface elevation [m]
 idealized_surface%dhgt_snow =   0.e0  ! snow depth [m]
 idealized_surface%iland     =   2     ! 1-land,  2-water
 idealized_surface%igbp_typ  =   0     ! IGBP land-cover type (dominant vegetation type ) 
                                       !--------------IGBP LULC type--------------
                                       ! water body =                    0
                                       ! evergreen needleleaf forest =   1
                                       ! evergreen broadleaf forest =    2
                                       ! deciduous needleleaf forest =   3
                                       ! deciduous broadleaf forest =    4
                                       ! mixed forests =                 5
                                       ! closed shrubland =              6
                                       ! open shrublands =               7
                                       ! woody savannas =                8 
                                       ! savannas =                      9
                                       ! grasslands =                   10 
                                       ! permanent wetlands =           11  
                                       ! croplands =                    12
                                       ! urban and built-up =           13
                                       ! cropland/natural vegetation mosaic =  14
                                       ! snow and ice =                 15
                                       ! barren or sparsely vegetated = 16
                                       !--------------------------------------------

!----------- aerosol options ----------------

 account_aerosol  = .false.   ! if true, account aerosol particles (logical)

 aerosol_microphysics = 'GOCART'  ! Aerosol Microphyiscs Type (character*20)
                                ! GOCART: GOCART 1moment bulk aerosol scheme [Chin et al. ] 
                                ! UCD: UC Davis dust scheme


 nudge_gocart_on  = .false.   ! if true, read GLOBAL GOCART aerosl (you must prepare input separately)
                              ! defalt is .false.

$end

! #############################################################################
! ###############  Configure Single-Scattering LUTs Options ###################
! #############################################################################

$single_scatter_options


  lut_micro    = .true.    ! Particle single-scattering LUT options for microwave /radar simulator (logical).
                           !.true.  : Use LUTs for microwave opt. Very Fast. (recommended)
                           !.false. : Full solution of Mie routine. Slow, but accurate. 
                           ! This will be false for HUCM_SBM or HUCM_SBM43 microphysics.

  lut_visir    = .true.     ! Particle single-scattering LUT option for Visible-IR /lidar simulator (logical)
                            !.true.  : Use LUTs for microwave opt. Very fast. (recommended)
                            !.false. : Full solution of Mie routine Slow, but accurate. 
                            ! This will  be .false. for HUCM_SBM or HUCM_SBM43 microphysics.

  lut_polarris = .true.     ! Polarimetric Radar Observable LUT option for POLARRIS (logical)
                            !.true.  : Use LUTs for polarris opt. Very fast. (recommended)
                            !.false. : Full solution of Mueller-T-matrix routine Slow, but accurate. (x8000 slow)
                            ! This will  be .false. for HUCM_SBM or HUCM_SBM43 microphysics.

  lut_replace = .false.    ! Replace existing single-scattering LUT, if you modify single-scattering routines (logical).
                          !.true.  : Replace single-scattering LUTs. 
                          !.false. : Use existing Mie LUTs data.  (default)

  ss_opt_micro = 4        ! Single scattering calculation options for Microwave/Radar simulator (integer)
                          ! If you change this option, you must set lut_replace = .true.
                          ! 1 - Mie      (flufy sphere)
                          ! 2 - SCATDB   (non-spherical ice crystals via DDSCAT: G. Liu @ FSU)
                          ! 3 - OpenSSP  (non-spherical ice crystals, snow aggregate via DDSCAT) , not available
                          ! 4 - Tmatrix  (oblate/spheroid from 10GHz - 190GHz: L. Liao) (recommended) 
                          ! Note POLARRIS automatically use Tmatrix calculation regardless of this choice, and no bounds in frequency.

  scatdb_ice_type = 8     ! SCATDB ice crystal shape index (if ss_opt_micro = 2, you must choose) for Microwave/Radar simulator.
                          ! 0-hexl, 1-hexs, 2-hexb, 3-hexf, 4-hexp, 5-ros3,
                          ! 6-ros4, 7-ros5, 8-ros6 (recommended), 9-sstr, 10-sden

  ice_refraction_func = 1 ! Effective refraction functions for Microwave/Radar/POLARRIS simulator (integer)
                          ! for Mie - maxwell-garnett combination (if ss_opt_micro == 1 or 2)
                          ! 1: Oblique Maxwell-Garnett function that assumes ice inclusion within air matrix. (recommneded)
                          ! 2: Oblique Maxwell-Garnett function that assumes air inclusion within ice matrix.
                          ! 3: Effective-Medium function that assumes homogeneous mixing.
                          ! 4: Debye theory. 

  melt_opt = 0   ! Effective refraction functions for melting particles for Microwave/Radar/POLARRIS simulator (integer) 
                 ! 0: Does not account melting particle
                 ! 1: Oblique Maxwell-Garnett function that assumes ice inclusion within water matrix.
                 ! 2: Oblique Maxwell-Garnett function that assumes water inclusion within ice matrix.
                 ! 3: Oblique Maxwell-Garnett function averaging option 1 and 2 (recommended)
                 ! 4: Effective-Medium function that assumes homogeneous mixing.

  ss_opt_visir = 1        ! Visible-IR / Lidar / broadband Ice cyrytals single scattering calculation options (integer)
                          ! , which affect visir simulator and lidar simulator
                          ! If you change this option, you must set lut_replace = .true.
                          ! 0 - Sphere    (Mie)
                          ! 1 - Ping Yang Non-spherical IceScattering database (recommended)

  land_emiss_micro =  2   ! Microwave land-surface emissivity scheme for Microwave simulator.
                          ! 0: simple scheme
                          ! 1: NESDIS land emissivity model (V1)
                          ! 2: TELSEM emissivity database  (recommended)

  input_bf_emiss  = 'global_emis_inf10_monthFilled_MYD11C3.A2006182.nc'  !IR land-surface emissvity database for Visible-IR simulator.
                    ! file name for IR spectrum emissivity for visir simulator (char)
                    ! you must have these files under DATAFILE/FILLED_IR_EMISS/DATABASE/
                    ! if not, download it from http://cimss.ssec.wisc.edu/iremis/

$end

! #############################################################################
! ######################  Configure Microwave Sensor  #########################
! #############################################################################

$micro_options
!  Note that if scan_micron = .true. , following settings are ignored.
  slant_path_micro = .true. ! if .true., it account two-way slantpath for microwave RT in east-west direction.

  micro_sensor = 'GMI'   !sensor name (GPM Microwave Imager)
  ground_micro = .false.    !=.true. for ground based; =.false. for satellite based
  ona_angle_micro = 48.5      ! off-nadir angle [deg]
  mxfreq_micro = 10                 ! The number of microwave-radiometer channels
  freq_micro = 10.65, 18.7, 23.8, 36.5, 89.0, 166., 176.31, 180.31, 186.31, 190.31  ! Channel frequencies [GHz]
  fov_ct_micro =  19.4, 11.2, 9.2, 8.6, 4.4, 4.4, 4.4, 4.4, 4.4, 4.4       ! cross-track FOV
  fov_dt_micro =  32.2, 18.3, 15.0, 14.4, 7.3, 7.3, 7.3, 7.3, 7.3, 7.3     ! down-track FOV
  beamwidth    =  1.75, 1.00, 0.90, 0.90, 0.40, 0.4, 0.4, 0.4, 0.4, 0.4
  sma_micro    = 6776.14    ! semi-major axix of GPM Core satellite [km]
  nf_L2        = 4          ! main frequency for L2 parameter 

!
! ##########################  Orbit / Scan Simulator set up.  ####################################
!                 (if scan_micron = .true. , G-SDSU ignores above "micro" setting )
!
  scan_micro = .false.  ! If true, scan / orbit simulator will run. Slow but more accurate geometry. (logical)
                       ! If true, specify satellite/instrument name below. 
                       ! if true, all of above sensor setups are ignored.

  satellite_micro = 'GPM'    ! Available satellite type (character*20)
                             ! GPM: GPM core satellite (2013~)
                             ! TRMM_PRE: TRMM satellite before orbit boost (1998~~2002) 
                             ! TRMM_POST: TRMM satelite after orbit boost (2002~2011)
                             ! AQUA: Aqua satellite (2002 May ~)
                             ! DMSP_F16: DMSP satellite F16 (2003 Oct ~)
                             ! DMSP_F17: DMSP satellite F17 (2006 Nov ~)
 
  scan_type_micro = 'GMI_HF'  ! Available sensor scan type
                              ! GMI_LF : GMI low-frequency channels  (10.65, 18.7, 23.8, 36.5, 89.0GHz)
                              ! GMI_HF : GMI high-Frequency channels (166, 183pm1, 183pm3, 183pm6GHz)
                              ! TMI_LF : TMI low-frequency channels  (10.65, 19.35, 21.3, 37.0GHz)
                              ! TMI_HF : TMI high-Frequency channels (85.5GHz)
                              ! AMSR_E_LF : AMSR-E low-freq channels  (6.925, 10.65, 18.7, 23.8, 36.5)
                              ! AMSR_E_HF : AMSR-E high-freq channels (89.0 GHz)
                              ! GMI_LF37 : just test purpose
                              ! SSMIS : SSMIS imager channels (19.35, 37, 91.655, 150, 183pm3, 183pm6.6GHz )

 inpfile_overpass_micro  = 'overpass_satellite'  ! name of model-input-list file  (character)
                              ! file must contains lat, lon, ascend/descend, orbit_TAG for each CRM input

$end

! #############################################################################
! ##########################  Configure Radar Sensor  #########################
! #############################################################################

$radar_options

                              !but output size could be large. 

!  Note that if scan_radar = .true. , following settings are ignored.

  radar_output_L2 = .F.     !if .true. it also output radar-range-sampled geophysical parameter. 

  radar_sensor = 'DPR'  !sensor name (Dual-frequency Precipitation Radar)
  ground_radar = .false.    !=.true. for ground-based sensor; =.false. for satellite-based sensor
  mxfreq_radar = 2              !The number of channels
  min_echo = 17.                     !minimal_detactable echo [dBZ]
  inc_angle_radar = 0.            !incident angle [deg]
  k2 = -999.,-999.  !dielectric constant |k^2| defaults (if not known -> -999.)
  freq_radar = 13.6, 35.5    !Channel frequencies [GHz]  
  fov_ct_radar =  5.0,5.0       ! Spatial resolution for cross-track FOV
  fov_dt_radar =  5.0,5.0       ! Spatial resolution for down-track FOV 
  mxhgt_radar  = 19.0       ! maximum height of measuremenit (above sea level) [km]    
  range_radar= 0.25    ! radar measurement range resolution [km]

!
! ###########################  Orbit / Scan Simulator set up. #################################
!                    (  if scan_radar = .true. , G-SDSU ignores above radar setting )
!

  scan_radar = .false.  ! If true, scan / orbit simulator will run. Slow but more accurate geometry. (logical)
                       ! If true, specify satellite/instrument name below. 
                       ! if true, all of above sensor setups are ignored.

  satellite_radar = 'GPM'    ! Available satellite type (character*20)
                             ! GPM: GPM core satellite (2013~)
                             ! TRMM_PRE: TRMM satellite before orbit boost (1998~~2002) 
                             ! TRMM_POST: TRMM satelite after orbit boost (2002~2011)
                             ! CLOUDSAT: CloudSat satellite

  scan_type_radar = 'DPR_Ku'  ! Available sensor scan type
                              ! DPR_Ku : GPM Dual-Frequency Radar Ku band (13.6GHz)
                              ! DPR_Ka : GPM Dual Frequency Radar Ka band (35.5GHz)
                              ! PR     : TRMM Precipitation Radar (13.6GHz)
                              ! CPR    : CloudSat CPR (94GHz)

 inpfile_overpass_radar  = 'overpass_satellite'  ! name of model-input-list file  (character)
                          ! file must contains lat, lon, ascend/descend, orbit_TAG for each CRM input

$end

! #############################################################################
! ######################  Configure VisibleIR Sensor  #########################
! #############################################################################

$visir_options

  visir_sensor = 'TbIR'   ! sensor name  (IR channel for T3EF) (character*20)
  znth_slr = 0.      ! solar zenith angle [deg] (if -999. coszen depends on model time.) (real)
  znth_obs = 12.13   ! viewing zenith angle [deg] (real)
  azmth    = 0.      ! azimuth angle between the sun and sensor [deg] (real)
  mxwavel = 1        ! The number of channels (real)
  wavel = 12.e0      ! Channel wavelengths  [micron] (real,dimension(mxwavel))
  fov_ct_visir =  5.0      ! Spatial resolution for cross-track FOV (adjusted for PR FOV) (real,dimension(mxwavel))
  fov_dt_visir =  5.0      ! Spatial resolution for down-track FOV  (adjusted for PR FOV) (real,dimension(mxwavel))

!
! ##########################  Orbit / Scan Simulator set up.  ####################################
!                 (if scan_visir = .true. , G-SDSU ignores above "visir" setting )
!
  scan_visir = .false.  ! If true, scan / orbit simulator will run to account more accurate scan geometry.(logical)
                       ! If true, specify satellite/instrument name below. 
                       ! if true, all of above sensor setups are ignored.

  satellite_visir = 'AQUA'    ! Available satellite type (character*20)
                              ! AQUA: Aqua satellite (2002 May ~)
                              ! TERRA: Terra satellite (2000 Jan ~ )
                              ! TRMM_PRE: TRMM satellite before orbit boost (1998~~2002) 
                              ! TRMM_POST: TRMM satelite after orbit boost (2002~2011)

  scan_type_visir = 'MODIS_IR'  ! Available sensor scan type
                                ! MODIS_IR : MODIS sensor IR channel (11um )

  inpfile_overpass_visir  = 'overpass_satellite'  ! name of model-input-list file  (character)
                                ! file must contains lat, lon, ascend/descend, orbit_TAG for each CRM input



$end

! #############################################################################
! #########################  Configure Lidar Sensor  ##########################
! #############################################################################

$lidar_options

  lidar_sensor = 'CALIOP'         ! sensor name in three character (character*20)
  ground_lidar = .false.          !=.true. for ground-based sensor; =.false. for satellite-based sensor (logical)
  MS_Correct = 1.                ! multiple scttering correction factor (real)
  mxwavel_lidar = 2               ! The number of channels (integer)
  wavel_lidar = 0.532, 1.064             ! Channel wavelengths  [micron] (real,dimension(mxwavel_lidar))
  inst_profile_lidar = .true.     ! = .true. for instrument-defined profile (must define mxhgt_lidar,range_lidar)
  mxhgt_lidar  =  20.0            ! maximum height of measuremenit (above sea level) [km]  
  range_lidar  =  0.48             ! lidar measurement range resolution [km]

!
! ##########################  Orbit / Scan Simulator set up.  ####################################
!                 (if scan_lidar = .true. , G-SDSU ignores above "lidar" setting )
!
  scan_lidar = .false.  ! If true, scan / orbit simulator will run to account more accurate scan geometry.(logical)
                       ! If true, specify satellite/instrument name below. 
                       ! if true, all of above sensor setups are ignored.

  satellite_lidar = 'CALIPSO'    ! Available satellite type (character*20)
                              ! CALIPSO: CALIPSO satellite

  scan_type_lidar = 'CALIOP'  ! Available sensor scan type
                              ! CALIOP : CALIOP (532nm, 1064nm)

  inpfile_overpass_lidar  = 'overpass_satellite'  ! name of model-input-list file  (character)
                                    ! file must contains lat, lon, ascend/descend, orbit_TAG for each CRM input


$end

! #############################################################################
! ######################  Configure BroadBand Sensor  #########################
! #############################################################################

$broad_options
 
  broad_scheme = 'goddard'   ! goddard - Goddard Radition (CliRad) scheme

  heating_rate = .false. ! write out 3D braodband SW/LW heating rate [K/day] (logical)
                         ! in addition to the default energy budget output.

  fov_ct_broad =  1.e0    ! Spatial resolution for cross-track FOV (CERES) (real) [km]
  fov_dt_broad =  1.e0    ! Spatial resolution for down-track FOV  (CERES) (real) [km]

!
! ##########################  Orbit / Scan Simulator set up.  ####################################
!                 (if scan_broad = .true. , G-SDSU ignores above "broad" setting )
! --> NOT READY YET DO NOT USE BELOW OPTIONS
  scan_broad = .false.  ! If true, scan / orbit simulator will run to account more accurate scan geometry.(logical)
                        ! If true, specify satellite/instrument name below. 
                        ! if true, all of above sensor setups are ignored.

  satellite_broad = 'AQUA'    ! Available satellite type (character*20)
                              ! AQUA: Aqua satellite (2002 May ~)
                              ! TERRA: Terra satellite (2000 Jan ~ )
                              ! TRMM_PRE: TRMM satellite before orbit boost (1998~~2002) 
                              ! TRMM_POST: TRMM satelite after orbit boost (2002~2011)

  scan_type_broad = 'CERES'  ! Available sensor scan type
                             ! CERES : CERES sensor (SW, LW TOA flux)

  inpfile_overpass_broad  = 'overpass_satellite'  ! name of model-input-list file  (character)
                             ! file must contains lat, lon, ascend/descend, orbit_TAG for each CRM input

$end

! #############################################################################
! ######################  Configure GV options        #########################
! #############################################################################
!  DESIGNED FOR WRF-SBM only.
$GV_options

  particle_shape = 0  ! 0 - sphere
                      ! 1 - irregular (assumption from SnowFake and 2DVD measurements)

  aircraft_on = .true.  ! true - simulate aircraft 2D-probe measurables parameters,
                        ! then dump output file (***.GV3D.bin), including following parameters.
                        ! bulk water content [g/m3]
                        ! bulk effective radius [micron]
                        ! bulk particle volume [cm3/m3]
                        ! bulk density [g/cm3]
                        ! liquid water fraction [-]

  aircraft_ice = .true. ! if true, it only sample ice particle
                        ! if .false. it only sample liquid particle

  parsivel_on = .false.  ! true - simulate ground-based Persivel measurable parameters
                        ! , then dump output (***.GV2D.bin), including following parameters
                        ! Parsivel 5min rainfall accumulation [mm]
                        ! Geonor 5min rainfall accumulation [mm]
                        ! bulk effective density [g/cm3]
                        ! bulk effective radius [micron]

  parsivel_liq_cutoff = .true. !if True, parisvel parameters are only accounted for diameter less than 8mm. 

  dump_psd = .false.   ! true - output full PSD (large output: ***.GV3D_PSD.bin or ***.GV2D_PSD.bin).

  zonal_sampling_on = .true. ! true - sample Aircraft- or PARSIVEL-simulator parameters in specific zone
                             ! and dump zonal statistical output (mean normalized PSDs)

                            ! if zonal_sampling_on is true, define sampling zone below 
  minlat_gv =  35.0         ! minimum latitude [deg] (Aircraf and Parsivel)
  maxlat_gv =  37.5         ! maximum latitudfe [deg] (Aircraf and Parsivel)
  minlon_gv = -99.0         ! minimum longitude [deg] (Aircraf and Parsivel)
  maxlon_gv = -95.5         ! maximum longitude [deg] (Aircraf and Parsivel) 
  minalt_gv =   7.e0        ! minimum altitude [km]
  maxalt_gv = 9.e0         ! maximum altitude [km]

$end

! #############################################################################
! #####################  Configure SARTA options        #######################
! #############################################################################
!                      NOTE SARTA IS CLEAR-SKY ONLY 
$sarta_options

  all_channel_airs = .true.    ! if true, it simulates default all AIRS (2784) channels (ichan will be ignored).
                               ! if false, you must specify specific channels. 

  nchan_airs  = 6         ! number of airs channels
  ichan_airs  = 1, 101, 501, 1001, 1501, 2001 ! specific airs channel ID numbers

  sol_zen_airs  = 0.e0   ! solar zenith angle [deg] (if -999. coszen depends on model time.)
  view_ang_airs = 0.0   ! veiwing zenith angle [deg] (should be between -49.6 and49.6)
  airs_unit = 'tb'       !'tb' gives output in brightness temperature.
                         !'rad' gives output in radiance. 

$end

! #############################################################################
! #####################  Configure POLARRIS options        ####################
! #############################################################################

$polarris_options

  generate_lut_polarris  = .true. ! if true, it generate new scattering LUTs, if you change the fundamental 
                                   ! assumption of dielectric constants or canting angles. 
                                   ! This process takes time, and usually requires 1140 CPUs in MPI task. 
                                   ! Also, if true, it just generate LUT, and won't drive POLARRIS.

 polarris_axiscant_typ = 3         !types of axis ratio and canting angle assumptions.
                                   ! 1:  Ryzhkov et al. 2011 JAMC
                                   ! 2:  Putnam et al. 2017 MWR
                                   ! 3:  Matsui 2018

  polarris_crmout   = .true.  ! if true, dump output of CRM interpolated geophysical parameter (logical)

! Specify information of polarimetric radar instrument.

!TWPICE (Darwin)
  mxinst_polarris   =        2             ! maximum instrument number (varies 1~2)
  name_polarris     =    'CPOL', 'BERRIMA' ! name of polarimetric radar (chracter)
  freq_polarris     =       5.6,     5.6   ! polarimetric radar frequencies (mxinst_polarris)
  lat_polarris      =   -12.249,  -12.457  ! polarimetric radar location (lat) [deg] (mxinst_polarris)
  lon_polarris      =   131.044,  130.925  ! polarimetric radar location (lat) [deg] (mxinst_polarris)
  elev_polradar     =     0.050,    0.040  ! radar elevation [km] ( if -999.0 ---> CRM domain elevation. )
  coverage_polarris =     143.7,    201.0  ! polarimetric radar coverage [km] (mxinst_polarris)

!MC3E (DOE ARM Oklahoma)
!  mxinst_polarris   =   3         ! maximum instrument number (varies 1 ~ 5)
!  name_polarris     =  'CSAPR',  'XSE',   'XSW',  'NPOL',   'KVNX' ! name of polarimetric radar (chracter)
!  freq_polarris     =    6.25,     9.4,     9.4,     2.8,     3.0  ! polarimetric radar frequencies (mxinst_polarris)
!  lat_polarris      =  36.796,  36.578,  36.491,  36.544,  36.741  ! polarimetric radar location (lat) [deg] (mxinst_polarris)
!  lon_polarris      = -97.451, -97.364, -97.594, -97.176, -98.128  ! polarimetric radar location (lat) [deg] (mxinst_polarris)
!  elev_polradar     =   0.327,   0.340,   0.214,   0.300,   0.378  ! radar elevation [km] ( if -999.0 ---> CRM domain elevation. )
!  coverage_polarris =   118.0,    40.0,    40.0,   150.0,   150.0  ! polarimetric radar coverage [km] (mxinst_polarris)

  use_crm_level_polarris = .false. ! if true, use original CRM height levels, otherwise define constant vertical level.
  mxhgt_polarris  = 20.0           ! maximum height of measuremenit (above sea level) [km] (real)   
  range_polarris  = 0.5            ! radar measurement range resolution [km] (real)

  output_grads_polarris = .false.  ! GrADS output for quick sanity check of output. 
  output_netcdf_polarris = .true.  ! NetCDF output (recommended for using iPOLARRIS)

!
! Detailed polariemtric radar parameters, such as particle caxis ratio and canting angle distributions, 
! can be modified through polarris_axiscant.txt in DATAFILE. 
! If you changed such parameter, you must re-generate POLARRIS LUTs by
! activating generate_lut_polarris = .true.
!

$end

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
