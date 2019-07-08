import logging
import os
import sys

logger = logging.getLogger(__name__)


def require_mpi(my_build):
    logger.debug("Require MPI.")
    # These conditions should NEVER fail unless user has messed with config file
    if my_build.env_vars["WRF_USE_MPI"] is None:
        logger.error("Please set WRF_USE_MPI = 1.")
        logger.error("NU-WRF currently requires WRF to be built with MPI.")
        sys.exit(1)
    if my_build.env_vars["WPS_USE_MPI"] is None:
        logger.error("Please set WPS_USE_MPI = 1.")
        logger.error("NU-WRF currently requires WPS to be built with MPI.")
        sys.exit(1)
    if my_build.env_vars["UPP_USE_MPI"] is None:
        logger.error("Please set UPP_USE_MPI = 1.")
        logger.error("NU-WRF currently requires UPP to be built with MPI.")
        sys.exit(1)


def files_required_by_wps(my_build):
    logger.debug("Check for WRF files required by WPS.")
    adir = os.environ.get("NUWRFDIR") + "/WRF/external/"
    files = []
    if "wps" in my_build.targets and "wrf" not in my_build.targets:
        files.append(adir + "io_grib1/libio_grib1.a")
        files.append(adir + "io_grib_share/libio_grib_share.a")
        files.append(adir + "io_int/libwrfio_int.a")
        files.append(adir + "io_netcdf/libwrfio_nf.a")
        for f in files:
            if not os.path.isfile(f):
                logger.error("Cannot find file {}, will rebuild WRF".format(f))
                my_build.targets.append("wrf")
                my_build.set_targets_options(my_build.targets)
                # The following _can_ be useful but it is generaly disruptive
                # my_build.options.append('cleanfirst')
                break


def files_required_by_upp(my_build):
    logger.debug("Check for WRF files required by UPP.")
    adir = os.environ.get("NUWRFDIR") + "/WRF/"
    files = []
    if "upp" in my_build.targets and "wrf" not in my_build.targets:
        files.append(adir + "main/libwrflib.a")
        files.append(adir + "external/io_grib_share/libio_grib_share.a")
        files.append(adir + "external/io_int/libwrfio_int.a")
        files.append(adir + "external/io_netcdf/libwrfio_nf.a")
        files.append(adir + "external/esmf_time_f90/libesmf_time.a")
        files.append(adir + "external/RSL_LITE/librsl_lite.a")
        files.append(adir + "external/fftpack/fftpack5/libfftpack.a")
        files.append(adir + "frame/module_internal_header_util.o")
        files.append(adir + "frame/pack_utils.o")
        files.append(adir + "frame/module_machine.o")
        for f in files:
            if not os.path.isfile(f):
                logger.error("Cannot find file {}, will rebuild WRF".format(f))
                my_build.targets.append("wrf")
                my_build.set_targets_options(my_build.targets)
                # The following _can_ be useful but it is generaly disruptive
                # my_build.options.append('cleanfirst')
                break


def set_envs(envs):
    logger.debug("Set internal WRF ENV variables")
    envs["BUILD_WRF_LIS"] = "1"
    envs["WRF_CHEM"] = "0"
    envs["WRF_LIS"] = "0"
    envs["WRF_KPP"] = "0"
    envs["WRF_EM_CORE"] = "1"
    envs["WRF_NMM_CORE"] = "0"
    envs["WRF_NMM_NEST"] = "0"
    envs["HWRF"] = "0"
    envs["WRF_DA_CORE"] = "0"
    envs["WRF_COAMPS_CORE"] = "0"
    envs["WRF_EXP_CORE"] = "0"
    envs["WRF_TITAN"] = "0"
    envs["WRF_MARS"] = "0"
    envs["WRF_VENUS"] = "0"
    envs["OMP_NUM_THREADS"] = "1"
    envs["WRF_DFI_RADAR"] = "0"
    envs["WRF_CONVERT"] = "0"
    envs["MADIS"] = "0"
    envs["BUFR"] = "0"
    envs["RTTOV"] = "0"
    envs["CRTM"] = "0"
    envs["WRF_ESMF_LIBS"] = envs["WRF_ESMF_LIBS_MPI"]


def additional_envs(envs, options):
    logger.debug("Reset WRF ENV variables based on config file settings.")
    # Modify WRF env variables as needed:
    if "BUILD_WRF_LIS" in envs:
        envs["WRF_LIS"] = envs["BUILD_WRF_LIS"]
    if "BUILD_WRF_CHEM" in envs:
        envs["WRF_CHEM"] = envs["BUILD_WRF_CHEM"]
    if "BUILD_WRF_KPP" in envs:
        envs["WRF_CHEM"] = envs["BUILD_WRF_CHEM"]
        envs["WRF_KPP"] = envs["BUILD_WRF_KPP"]
    if "nompi" in options:
        envs["WRF_ESMF_LIBS"] = envs["WRF_ESMF_LIBS_NOMPI"]

    envs["WRF_CONFIGURE_OPT"] = envs["WRF_CONFIGURE_MPI_OPT"]
    envs["WRF_CONFIGURE_LIS"] = envs["WRF_CONFIGURE_LIS_MPI"]
    if "nompi" in options:
        envs["WRF_CONFIGURE_OPT"] = envs["WRF_CONFIGURE_NOMPI_OPT"]
        envs["WRF_CONFIGURE_LIS"] = envs["WRF_CONFIGURE_LIS_NOMPI"]


def check_previous_build(my_build):
    ''' 
    Check previous build settings and determine if we need to distclean 
    '''
    options = my_build.options
    envs = my_build.env_vars
    if "cleanfirst" in options:
        return
    if not os.path.isfile(os.environ.get("NUWRFDIR") + "/.build_settings"):
        logger.info("There is no evidence of a previous WRF-LIS build.")
        return
    
    logger.info("Check previous build settings.")

    my_build.get_build_config()
    build_debug = my_build.build_state["build_debug"]
    build_nompi = my_build.build_state["build_nompi"]
    build_ideal_case = my_build.build_state["build_ideal_case"]
    nests = my_build.build_state["nests"]
    build_wrf_lis = my_build.build_state["build_wrf_lis"]
    build_chem = my_build.build_state["build_chem"]
    build_kpp = my_build.build_state["build_kpp"]
    wrf_configure_opt = my_build.build_state["wrf_configure_opt"]
    wps_configure_opt = my_build.build_state["wps_configure_opt"]
    configure_lis_file = my_build.build_state["wrf_configure_lis_file"]

    if (
        (build_debug == "0"
        and "debug" in options)
        or (build_debug == "1"
        and "debug" not in options)
    ):
        logger.warning("Force clean WRF when DEBUG options differ.")
        envs["force_clean_wrf"] = "yes"
        return

    if (
        (build_nompi == "0"
        and "nompi" in options)
        or (build_nompi == "1"
        and "nompi" not in options)
    ):
        logger.warning("Force clean WRF when MPI options differ.")
        envs["force_clean_wrf"] = "yes"
        return
    
    ideal_case = "ideal_case" in my_build.options
    if (
        (ideal_case
        and build_ideal_case == "0")
        or (not ideal_case
        and build_ideal_case == "1")
    ):
        logger.warning("Force clean: idealized and current WRF builds differ.")
        envs["force_clean_wrf"] = "yes"
        return

    nestx = [s for s in my_build.options if "nest" in s]
    if len(nestx) == 1:
        opt_nests = nestx[0]
    else:
        opt_nests = "1"
    if nests is not None:
        if opt_nests != nests:
            logger.warning("Previous build NEST option differs.")
            envs["force_clean_wrf"] = "yes"
            return

    if build_chem is not None:
        if build_chem != envs["WRF_CHEM"]:
            logger.warning("Force clean WRF when WRF-CHEM build options differ.")
            envs["force_clean_wrf"] = "yes"
            return

    if build_kpp is not None:
        if build_kpp != envs["WRF_KPP"]:
            logger.warning("Force clean WRF when WRF-KPP build options differ.")
            envs["force_clean_wrf"] = "yes"
            return

    # Always clean if wrf/wps//lis configurations are different
    if wrf_configure_opt is not None:
        if wrf_configure_opt != envs["WRF_CONFIGURE_OPT"]:
            logger.warning("Force clean when WRF_CONFIGURE_OPT options differ")
            logger.warning(
                "Current  WRF_CONFIGURE_OPT: <"+envs["WRF_CONFIGURE_OPT"]+">"
            )
            logger.warning("Previous WRF_CONFIGURE_OPT: <"+wrf_configure_opt+">"
            )
            envs["force_clean_wrf"] = "yes"
            my_build.options.append('cleanfirst')
            return

    if configure_lis_file is not None:
        if configure_lis_file != envs["WRF_CONFIGURE_LIS"]:
            logger.warning("Force clean when WRF_CONFIGURE_LIS options differ")
            logger.warning(
                "Current  WRF_CONFIGURE_LIS: <"+envs["WRF_CONFIGURE_LIS"]+">"
            )
            logger.warning("Previous WRF_CONFIGURE_LIS: <"+configure_lis_file+">"
            )
            envs["force_clean_wrf"] = "yes"
            return
        
    if wps_configure_opt is not None:
        if wps_configure_opt != envs["WPS_CONFIGURE_MPI_OPT"]:
            logger.warning("Force clean when WPS_CONFIGURE_MPI_OPT options differ")
            logger.warning(
                "Current  WRF_CONFIGURE_MPI_OPT: <"+envs["WRF_CONFIGURE_MPI_OPT"]+">"
            )
            logger.warning("Previous WRF_CONFIGURE_MPI_OPT: <"+wps_configure_opt+">"
            )
            envs["force_clean_wrf"] = "yes"
            my_build.options.append('cleanfirst')
            return


def main(my_build):
    build_options = my_build.options
    envs = my_build.env_vars
    set_envs(envs)
    additional_envs(envs, build_options)
    check_previous_build(my_build)
