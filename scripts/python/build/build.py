import clean
import configure
import logging
import os
import time
import shared.utils as u
import NuwrfBuild as nuwrf

logger = logging.getLogger(__name__)
# For WRF ideal builds
ideal_case_names = {
    "ideal_b_wave": "em_b_wave",
    "ideal_convrad": "em_convrad",
    "ideal_heldsuarez": "em_heldsuarez",
    "ideal_les": "em_les",
    "ideal_quarter_ss": "em_quarter_ss",
    "ideal_scm_xy": "em_scm_xy",
    "ideal_scm_lis_xy": "em_scm_lis_xy",
    "ideal_tropical_cyclone": "em_tropical_cyclone",
}


def get_expected_output(my_build, target):
    envs = my_build.env_vars
    options = my_build.options
    expected_output = list()

    if target == "arw":
        expected_output.append("ARWpost.exe")

    elif target == "doc":
        expected_output.append("tutorial/tutorial.pdf")
        expected_output.append("userguide/nuwrf_userguide.pdf")

    elif target == "gsdsu":
        expected_output.append("../QRUN/GSDSU.x")
        expected_output.append("../QRUN/HOW_MANY_CPU_GSDSU")

    elif target == "ldt":
        expected_output.append("LDT")

    elif target == "lvt":
        expected_output.append("LVT")

    elif target == "met":
        expected_output.append("bin/ascii2nc")
        expected_output.append("bin/ensemble_stat")
        expected_output.append("bin/gen_vx_mask")
        expected_output.append("bin/grid_stat")
        expected_output.append("bin/gsid2mpr")
        expected_output.append("bin/gsidens2orank")
        # expected_output.append('bin/lidar2nc') #only in MET 6.x
        expected_output.append("bin/madis2nc")
        expected_output.append("bin/mode")
        expected_output.append("bin/mode_analysis")
        expected_output.append("bin/modis_regrid")
        expected_output.append("bin/mtd")
        expected_output.append("bin/pb2nc")
        expected_output.append("bin/pcp_combine")
        expected_output.append("bin/plot_data_plane")
        expected_output.append("bin/plot_point_obs")
        # expected_output.append('bin/plot_mode_field') #only in MET 6.x
        expected_output.append("bin/point_stat")
        expected_output.append("bin/regrid_data_plane")
        expected_output.append("bin/series_analysis")
        expected_output.append("bin/shift_data_plane")
        expected_output.append("bin/stat_analysis")
        expected_output.append("bin/tc_dland")
        expected_output.append("bin/tc_pairs")
        expected_output.append("bin/tc_stat")
        expected_output.append("bin/wavelet_stat")
        expected_output.append("bin/wwmca_plot")
        expected_output.append("bin/wwmca_regrid")

    elif target == "rip":
        expected_output.append("rip")
        expected_output.append("ripcomp")
        expected_output.append("ripcut")
        expected_output.append("ripdp_mm5")
        expected_output.append("ripdp_wrfarw")
        expected_output.append("ripdp_wrfnmm")
        expected_output.append("ripinterp")
        expected_output.append("ripshow")
        expected_output.append("showtraj")
        expected_output.append("tabdiag")
        expected_output.append("upscale")

    elif target == "upp":
        expected_output.append("bin/unipost.exe")
        expected_output.append("bin/ndate.exe")
        expected_output.append("bin/copygb.exe")

    elif target == "utils":
        for i in nuwrf.NuwrfBuild().utils_exe:
            expected_output.append("bin/" + i + ".x")

    elif target in nuwrf.NuwrfBuild().utils_exe:
        expected_output.append("bin/" + target + ".x")

    elif target == "wps":
        # Core WPS
        expected_output.append("geogrid.exe")
        expected_output.append("metgrid.exe")
        expected_output.append("ungrib.exe")
        # WPS Util
        expected_output.append("util/avg_tsfc.exe")
        expected_output.append("util/calc_ecmwf_p.exe")
        expected_output.append("ungrib/g1print.exe")
        expected_output.append("ungrib/g2print.exe")
        expected_output.append("util/height_ukmo.exe")
        expected_output.append("util/int2nc.exe")
        expected_output.append("util/mod_levs.exe")
        expected_output.append("util/plotfmt.exe")
        expected_output.append("util/plotgrids.exe")
        expected_output.append("util/rd_intermediate.exe")

    elif target == "lis":
        expected_output.append("lis/make/LIS")

    elif target == "wrf":
        # Specify directories for each ideal case target
        if "ideal_case" in options:
            expected_output.append("main/ideal.exe")
            expected_output.append("main/wrf.exe")
        else:
            expected_output.append("main/ndown.exe")
            expected_output.append("main/real.exe")
            expected_output.append("main/tc.exe")
            expected_output.append("main/wrf.exe")
            if envs["WRF_CHEM"] == "1":
                expected_output.append("chem/convert_emiss.exe")

    return expected_output


def is_built(target, expected_output):
    all_built = len(expected_output)
    i = 0
    if i < all_built:
        for executable in expected_output:
            if not os.path.isfile(executable):
                break
            else:
                i = i + 1
                continue
        if all_built == i:
            logger.info(target + " is already built.")
            return True
    return False


def build_lis(my_build, t):
    # If necessary create empty file to allow compilation of LIS
    if not os.path.isfile("configure.wrf"):
        u.sp_call("touch configure.wrf")
    logger.debug("Run make " + t)
    u.sp_check_call_make_log("make " + t, my_build.env_vars)


def build_wrf(my_build):
    if "ideal_case" in my_build.options:
        idn = my_build.env_vars["ideal_case_name"]
        # Remove all symbolic links to ideal.exe
        fs = u.find_files("test", "\*.exe")
        for f in fs:
            if os.environ.get("DEBUG_BUILD") is None:
                os.remove(f)
        logger.info(" - building ideal case [" + idn + "] -")
        logger.debug("Run compile " + ideal_case_names[idn])
        u.sp_check_call_make_log(
            "./compile " + ideal_case_names[idn], my_build.env_vars
        )
    else:
        logger.debug("Run compile em_real")
        if my_build.env_vars["BUILD_WRF_LIS"] == "0":
            u.sp_check_call_make_log("./compile em_real", my_build.env_vars)
        else:
            logger.info(" - building WRF with LIS coupling -")
            u.sp_check_call_make_log("./compile em_real", my_build.env_vars)
            # WRF-Chem includes LIS coupling but will be decoupled soon
            if my_build.env_vars["WRF_CHEM"] == "1":
                logger.info(" - building WRF with chem support -")
                logger.debug("Run compile emi_conv")
                u.sp_check_call_make_log("./compile emi_conv", my_build.env_vars)


def compile_it(my_build, target):
    logger.info("Compile [" + target + "]")

    expected_output = get_expected_output(my_build, target)
    os.chdir(os.environ.get("NUWRFDIR") + "/" + my_build.target_dir[target])
    
    if "rebuild" in my_build.options:  # and 'wrf' in target:
        for executable in expected_output:
            if os.environ.get("DEBUG_BUILD") is None:
                try:
                    os.remove(executable)
                except OSError:
                    pass
    # Else, check if we are all done and return to calling routine
    else:
        if "cleanfirst" in my_build.options:
            for f in expected_output:
                if os.environ.get("DEBUG_BUILD") is None:
                    try:
                        os.remove(f)
                    except OSError:
                        pass
        else:
            if is_built(target, expected_output):
                return
    envs = my_build.env_vars

    if target in ["arw", "doc", "ldt", "lvt", "rip", "upp"]:
        u.sp_check_call_make_log("./compile", envs)

    elif target == "gsdsu":
        make_args = (
            "make -f "
            + envs["GSDSU_MAKEFILE"]
            + " INC_NETCDF="
            + envs["GSDSU_NETCDF_INCDIR"]
            + " LD_NETCDF="
            + envs["GSDSU_NETCDF_LIBDIR"]
            + " NETCDF4_DEP_LIB="
            + "'"
            + envs["GSDSU_NETCDF4_DEP_LIB"]
            + "'"
            + " all"
        )
        u.sp_check_call_make_log(make_args, envs)

    elif target == "met":
        u.sp_check_call_make_log("make install", envs)

    elif target == "wps":
        u.sp_check_call_make_log("./compile wps", envs)
        u.sp_check_call_make_log("./compile util", envs)

    elif target == "lis":
        if "rebuild" in my_build.options or  "cleanfirst" in my_build.options: 
            envs["BUILD_LIS"] = "yes"
        build_lis(my_build, "LIS")

    elif target == "wrf":
        envs["BUILD_LIS"] = "no"

        logger.info("Build library for WRF-LIS")
        build_lis(my_build, "explis")

        build_wrf(my_build)

    elif target == "wrfonly":
        envs["BUILD_LIS"] = "no"
        build_wrf(my_build)

    if os.environ.get("DEBUG_BUILD") is not None:
        logger.info("[" + target + "] build was successful.")
        return

    # Assume nothing has built
    all_built = False
    num_built = len(expected_output)
    count = 0
    for f in expected_output:
        if not os.path.isfile(f):
            logger.warning("--- Target: {} was not built!".format(f))
        else:
            count = count + 1
        if count == num_built:
            all_built = True
    if not all_built:
        open(os.environ.get("NUWRFDIR") + "/.build_failed", "a").close()
        logger.error("[" + target + "] build FAILED")
        return

    logger.info("[" + target + "] build was successful.")


def compile_utils(my_build, target):
    logger.info("Compile [" + target + "]")
    envs = my_build.env_vars
    options = my_build.options

    make_target = target
    if "utils" in target:
        make_target = "all"

    J = envs["J"]
    if "prep_chem_sources" in make_target or "all" in make_target:
        J = ""

    # Required files:
    expected_output = []
    if "all" in make_target:
        for f in nuwrf.NuwrfBuild().utils_exe:
            expected_output.append("bin/" + f + ".x")
    else:
        expected_output.append("bin/" + make_target + ".x")

    if "rebuild" in my_build.options:  # and 'wrf' in target:
        for executable in expected_output:
            if os.environ.get("DEBUG_BUILD") is None:
                try:
                    os.remove(executable)
                except OSError:
                    pass
    # Else, check if we are all done and return to calling routine
    else:
        if "cleanfirst" in my_build.options:
            for f in expected_output:
                if os.environ.get("DEBUG_BUILD") is None:
                    try:
                        os.remove(f)
                    except OSError:
                        pass
        else:
            if is_built(target, expected_output):
                return

    if "debug" in options:
        envs["UTILS_DEBUG"] = "yes"
    make_args = (
        "make " + J + " " + make_target + " CONFIG_DIR=" + os.getcwd() + "/config"
    )
    rc = u.sp_check_call_make_log(make_args, envs)

    if os.environ.get("DEBUG_BUILD") is not None:
        logger.info("[" + target + "] build was successful.")
        return

    for f in expected_output:
        if not os.path.isfile(f):
            logger.error("[" + target + "] build FAILED: {} was not built!".format(f))
    else:
        if rc is not None:
            open(os.environ.get("NUWRFDIR") + "/.build_failed", "a").close()
            logger.error("utils build failed!")

    logger.info("[" + target + "] build was successful.")


def main(my_build, target):
    start_time = time.time()
    nuwrf_dir = os.environ.get("NUWRFDIR")

    if target in my_build.utils_exe:
        build_dir = nuwrf_dir + "/utils"
    else:
        build_dir = nuwrf_dir + "/" + my_build.target_dir[target]

    os.chdir(build_dir)
    logger.debug("Entering " + os.getcwd())

    build_options = my_build.options
    #is_wrflis = "wrf" in target or "lis" in target
    clean_wrf = "force_clean_wrf" in my_build.env_vars # and is_wrflis
    if clean_wrf or "cleanfirst" in build_options:
        rc = clean.clean_it(my_build, target)
        if os.environ.get("DEBUG_BUILD") is None:
            if rc != 0:
                logger.error("Clean failed")
                return

    rc = configure.configure(my_build, target)
    if os.environ.get("DEBUG_BUILD") is None:
        if rc != 0:
            logger.error("Configure failed")
            return

    if target == "utils" or target in my_build.utils_exe:
        compile_utils(my_build, target)
    else:
        compile_it(my_build, target)

    if clean_wrf:
        del my_build.env_vars["force_clean_wrf"]

    # Done
    end_time = time.time() - start_time
    logger.info("[" + target + "] build time taken = %f" % end_time)
