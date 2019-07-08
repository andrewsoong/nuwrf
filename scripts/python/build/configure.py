from shutil import copyfile, move
from filecmp import cmp
import logging
import os
import shared.utils as u

logger = logging.getLogger(__name__)

wrf_dir = os.environ.get("NUWRFDIR") + "/WRF/"


def configure_wrf(my_build):
    envs = my_build.env_vars
    options = my_build.options
    # Set nest option for WRF. Default is nest=1:
    nest = "1"
    if "nest=2" in options:
        nest = "2"
    elif "nest=3" in options:
        nest = "3"

    logger.debug("Generate configure.wrf")
    heredoc = "<<EOF\n" + envs["WRF_CONFIGURE_OPT"] + "\n" + nest + "\nEOF"
    if "debug" in options:
        rc = u.run_configure("./configure -D ", heredoc, envs)
    else:
        rc = u.run_configure("./configure -s ", heredoc, envs)
    logger.debug("rc=" + str(rc))

    if os.environ.get("DEBUG_BUILD") is not None:
        return 0
    if not os.path.isfile("configure.wrf"):
        return 1

    logger.debug("Edit configure.wrf")
    # Optionally disable compilation of CLM4 in WRF
    if "WRF_SKIP_CLM4" in envs:
        u.sed_inplace("configure.wrf", r"-DWRF_USE_CLM", "")

    # Remove NETCDF4 preprocessor flag if appropriate
    if "NETCDF4" not in envs:
        u.sed_inplace("configure.wrf", r"-DUSE_NETCDF4_FEATURES", "")
    # WRF electrification scheme hacks
    wrf_elec_opt = "-DWRF_ELEC"  # This is the default
    if envs["WRF_ELEC"] == "0":
        u.replace_infile("configure.wrf", wrf_elec_opt, "")

    wrf_elec_opt = "WRF_ELEC\t=\t0"
    if envs["WRF_ELEC"] == "1":
        u.replace_infile("configure.wrf", wrf_elec_opt, "WRF_ELEC\t=\t1")

    if "debug" in options:
        u.sed_inplace(
            "configure.wrf",
            r"^CFLAGS_LOCAL.*",
            "CFLAGS_LOCAL = " + envs["WRF_DEBUG_CFLAGS_LOCAL"],
        )
        u.sed_inplace(
            "configure.wrf", r"^FCOPTIM.*", "FCOPTIM = " + envs["WRF_DEBUG_FCOPTIM"]
        )
        u.sed_inplace(
            "configure.wrf", r"^FCNOOPT.*", "FCNOOPT = " + envs["WRF_DEBUG_FCNOOPT"]
        )
        u.sed_inplace(
            "configure.wrf",
            r"^FCREDUCEOPT.*",
            "FCREDUCEOPT = " + envs["WRF_DEBUG_FCNOOPT"],
        )


def configure(my_build, target):
    logger.info("Configure [" + target + "]")
    envs = my_build.env_vars
    options = my_build.options

    if target == "arw":
        heredoc = "<<EOF\n " + envs["ARWPOST_CONFIGURE_OPT"] + " \n EOF"
        rc = u.run_configure("./configure ", heredoc, envs)

    elif target == "lvt":
        opt = "2"
        if any("debug" in x for x in options):
            opt = "-1"

        # Description of options:
        # Optimization level (-2=strict checks, -1=debug, 0,1,2,3, default=2):
        # Assume little/big_endian data format (1-little, 2-big, default=2):
        # Use GRIBAPI/ECCODES? (1-gribapi, 2-eccodes, default=1):
        # Use NETCDF? (1-yes, 0-no, default=1):
        # NETCDF version (3 or 4, default=4):
        # NETCDF use shuffle filter? (1-yes, 0-no, default = 1):
        # NETCDF use deflate filter? (1-yes, 0-no, default = 1):
        # NETCDF use deflate level? (1 to 9-yes, 0-no, default = 9):
        # Use HDF4? (1-yes, 0-no, default=1):
        # Use HDF5? (1-yes, 0-no, default=1):
        # Use HDFEOS? (1-yes, 0-no, default=1):
        # Enable AFWA-specific grib configuration settings? (1-yes, 0-no, default=0):
        # Enable GeoTIFF support? (1-yes, 0-no, default=1):
        # Use MATLAB support? (1-yes, 0-no, default=0):
        # --- Note that in NU-WRF GeoTIFF support is NOT enabled (yet)
        heredoc = (
            "<< EOF\n"
            + opt
            + "\n"
            + "\n"
            + "\n"
            + "\n"
            + "\n"
            + "\n"
            + "\n"
            + "\n"
            + "\n"
            + "\n"
            + "\n"
            + "\n"
            + "0\n"
            + "\n"
            + "EOF"
        )
        rc = u.run_configure("./configure ", heredoc, envs)

    elif target == "ldt":
        include_history = "1"
        if envs["LDT_SKIP_HISTORY"] == 1:
            include_history = "0"
        opt_level = "2"
        if any("debug" in x for x in options):
            opt_level = "-1"

        # Description of options:
        # Parallelism (0-serial, 1-dmpar, default0):
        # Optimization level (-2=strict checks, -1=debug, 0,1,2,3, default=2):
        # Assume little/big_endian data format (1-little, 2-big, default=2):
        # Use GRIBAPI/ECCODES? (0-neither, 1-gribapi, 2-eccodes, default=1):
        # NETCDF version (3 or 4, default=4)?:
        # NETCDF use shuffle filter? (1-yes, 0-no, default = 1):
        # NETCDF use deflate filter? (1-yes, 0-no, default = 1):
        # NETCDF use deflate level? (1 to 9-yes, 0-no, default = 9):
        # Use HDF4? (1-yes, 0-no, default=1):
        # Use HDF5? (1-yes, 0-no, default=1):
        # Use HDFEOS? (1-yes, 0-no, default=1):
        # Enable GeoTIFF support? (1-yes, 0-no, default=1):
        # Enable LIBGEOTIFF support? (1-yes, 0-no, default=1):
        # Include date/time stamp history? (1-yes, 0-no, default=1):
        # --- Note that in NU-WRF GeoTIFF/LIBGEOTIFF support is NOT enabled (yet)
        heredoc = (
            "<< EOF\n"
            + "\n "
            + opt_level
            + "\n"
            + "\n"
            + "\n"
            + "\n"
            + "\n"
            + "\n"
            + "\n"
            + "\n"
            + "\n"
            + "\n"
            + "0\n"
            + "0\n"
            + include_history
            + "\n"
            + "EOF"
        )
        rc = u.run_configure("./configure ", heredoc, envs)

    elif target == "met":
        u.sp_call("touch config.h.in")
        u.sp_call("touch aclocal.m4")
        fs = u.find_files(".", "Makefile.am")
        for f in fs:
            u.sp_call("touch " + f)
        fs = u.find_files(".", "Makefile.in")
        for f in fs:
            u.sp_call("touch " + f)
        opts = (
            "--prefix="
            + os.getcwd()
            + " --enable-grib2 --enable-modis --enable-mode_graphics --enable-lidar2nc"
        )
        rc = u.run_shell_command("./configure " + opts, envs)

    elif target == "rip":
        heredoc = "<< EOF \n" + envs["RIP_CONFIGURE_OPT"] + "\n EOF"
        rc = u.run_configure("./configure ", heredoc, envs)

    elif target == "upp":
        envs["WRF_ESMF_LIBS"] = envs["WRF_ESMF_LIBS_MPI"]
        envs["UPP_CONFIGURE_OPT"] = envs["UPP_CONFIGURE_MPI_OPT"]
        if any("nompi" in x for x in options):
            envs["WRF_ESMF_LIBS"] = envs["WRF_ESMF_LIBS_NOMPI"]
            envs["UPP_CONFIGURE_OPT"] = envs["UPP_CONFIGURE_NOMPI_OPT"]
        envs["JASPERLIB"] = envs["UPP_GRIB2_LIBS"]
        envs["JASPERINC"] = envs["UPP_GRIB2_INC"]
        heredoc = "<< EOF\n" + envs["UPP_CONFIGURE_OPT"] + "\nEOF"
        rc = u.run_configure("./configure ", heredoc, envs)

    elif target == "wps":
        envs["JASPERLIB"] = envs["WPS_GRIB2_LIBS"]
        envs["JASPERINC"] = envs["WPS_GRIB2_INC"]

        heredoc = "<<EOF\n" + envs["WPS_CONFIGURE_MPI_OPT"] + "\nEOF"
        rc = u.run_configure("./configure ", heredoc, envs)
        if os.environ.get("DEBUG_BUILD") is not None:
            return 0
        if not os.path.isfile("configure.wps"):
            return 1

        if any("debug" in x for x in options):
            u.sed_inplace(
                "configure.wps", r"^CFLAGS.*", "CFLAGS = " + envs["WPS_DEBUG_CFLAGS"]
            )
            u.sed_inplace(
                "configure.wps", r"^FFLAGS.*", "FFLAGS = " + envs["WPS_DEBUG_FFLAGS"]
            )
            u.sed_inplace(
                "configure.wps",
                r"^F77FLAGS.*",
                "F77FLAGS = " + envs["WPS_DEBUG_F77FLAGS"],
            )

    elif target == "lis" or target == "wrf":
        logger.debug("Generate configure.lis")
        if os.environ.get("DEBUG_BUILD") is not None:
            return 0
        opt_para = "1"
        if "nompi" in options:
            opt_para = "0"
        opt_optim = "2"
        if "debug" in options:
            opt_optim = "-1"
        root = os.environ.get("NUWRFDIR")
        if not os.path.islink(root + "/WRF/lis"):
            os.symlink(root + "/LISF/lis", wrf_dir + "/lis")
        os.chdir(wrf_dir + "lis")

        # Description of options:
        # Parallelism (0-serial, 1-dmpar, default=1):
        # Optimization level (-3=strict checks with warnings, -2=strict checks, -1=debug, 0,1,2,3, default=2):
        # Assume little/big_endian data format (1-little, 2-big, default=2):
        # Use GRIBAPI/ECCODES? (0-neither, 1-gribapi, 2-eccodes, default=1):
        # Enable AFWA-specific grib configuration settings? (1-yes, 0-no, default=0):
        # Use NETCDF? (1-yes, 0-no, default=1):
        # NETCDF version (3 or 4, default=4):
        # NETCDF use shuffle filter? (1-yes, 0-no, default = 1):
        # NETCDF use deflate filter? (1-yes, 0-no, default = 1):
        # NETCDF use deflate level? (1 to 9-yes, 0-no, default = 9):
        # Use HDF4? (1-yes, 0-no, default=1):
        # Use HDF5? (1-yes, 0-no, default=1):
        # Use HDFEOS? (1-yes, 0-no, default=1):
        # Use MINPACK? (1-yes, 0-no, default=0):
        # Use LIS-CRTM? (1-yes, 0-no, default=0):
        # Use LIS-CMEM? (1-yes, 0-no, default=0):
        # Use LIS-LAPACK? (1-yes, 0-no, default=0):
        heredoc = (
            "<<EOF "
            + opt_para
            + "\n"
            + opt_optim
            + "\n"
            + "\n"
            + "\n"
            + "\n"
            + "\n"
            + "\n"
            + "\n"
            + "\n"
            + "\n"
            + "\n"
            + "\n"
            + "\n"
            + "\n"
            + "\n"
            + "\n"
            + "\n"
            + "EOF"
        )
        rc = u.run_configure("./configure ", heredoc, envs)
        os.chdir(wrf_dir)
        if target == "lis":
            return rc
        configure_wrf(my_build)
        return 0

    elif target == "wrf":
        configure_wrf(my_build)
        # TODO: Need error checking
        return 0

    elif target == "utils":
        if "debug" in options:
            envs["UTILS_DEBUG"] = "1"
        rc = 0

    else:
        return 0

    if os.environ.get("DEBUG_BUILD") is not None:
        return 0
    else:
        if rc != 0:
            logger.error(target + " configure failed!")
        return rc
