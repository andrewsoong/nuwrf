#!/usr/bin/env python
import sys
import os
import time
import platform
import argparse
import logging
import shared.utils as u
import NuwrfBuild as nuwrf
import build
import clean
import wrf_build as wrf
import install
import tarfile

logger = logging.getLogger("main")


def parse_args():
    """Parse command line arguments.
    @returns: populated namespace containing parsed arguments.
    """
    parser = argparse.ArgumentParser(
        formatter_class=argparse.RawTextHelpFormatter,
        usage="%(prog)s [-h] [-p prefix] [-c configfile] [-o target_option] [target]",
        prog="./build.sh",
    )
    if sys.version_info[0] < 3:
        default_cfg = "nu-wrf_python2.cfg"
    else:
        default_cfg = "nu-wrf.cfg"
    parser.add_argument("target", help="Name of NU-WRF component to build.")
    parser.add_argument(
        "-e",
        action="store_true",
        default=False,
        help="Dump NU-WRF build environment variables.",
    )
    parser.add_argument(
        "-p",
        "--prefix",
        metavar="prefix",
        type=str,
        default="",
        help="Specify installation prefix.",
    )
    parser.add_argument(
        "-c",
        "--config",
        metavar="configfile",
        type=str,
        default=default_cfg,
        help="Name of optional user-specified config file.",
    )
    parser.add_argument(
        "-o", "--options", type=str, default="", help="Options for target."
    )

    # Check for valid targets
    args_list = parser.parse_args().target.split(",")
    args_ok = set(args_list).issubset(nuwrf.NuwrfBuild().valid_targets)
    if not args_ok:
        print("Invalid targets in {}".format(args_list))
        print("Valid targets are {}".format(nuwrf.NuwrfBuild().valid_targets))
        sys.exit(1)

    return parser.parse_args()


def set_wrf_targets(key, bld, lst):
    bld.env_vars["BUILD_WRF_LIS"] = "1"
    if key == "allchem" or key == "chem":
        # Used to set WRF chemistry build
        bld.env_vars["BUILD_WRF_CHEM"] = "1"
    elif key == "allkpp" or key == "kpp":
        # Used to set WRF KPP build
        bld.env_vars["BUILD_WRF_CHEM"] = "1"
        bld.env_vars["BUILD_WRF_KPP"] = "1"
    lst.append("wrf")
    return lst


def set_all_targets(key, bld, lst):
    # Set WRF chem targets environment variables
    set_wrf_targets(key, bld, lst)
    if key in lst:
        lst.remove(key)
    # Replace "all" target with corresponding list of targets
    for k in list(bld.target_dir.keys()):
        # MET must be built separately
        if "met" not in k:
            lst.append(k)
    return lst


def process_args(args):
    # Create a NU-WRF build
    my_build = nuwrf.NuwrfBuild()

    if args.prefix:
        my_build.prefix = args.prefix
    if args.config:
        my_build.config = args.config
    if args.options:
        my_build.options = list(args.options.split(","))
    if args.e:
        my_build.dump_envs = True

    # Initial target list (may be modified below as forced by dependencies)
    target_list = args.target.split(",")

    # Set various flags to allow checking of invalid user selections
    all_build_case = (
        any("all" in x for x in target_list) and "allclean" not in target_list
    )
    ideal_case = any("ideal" in x for x in target_list)

    # Abort if an idealized case and and "all" case was selected
    if ideal_case and all_build_case:
        logger.error("Cannot compile all targets if also building ideal case")
        sys.exit()

    # Make sure no more than one idealized option was selected.
    num_ideal_targets = sum("ideal" in x for x in target_list)
    if num_ideal_targets > 1:
        logger.error("Only pick a single ideal case to build.")
        sys.exit()

    # If ideal case is selected make sure that no real WRF build option
    if ideal_case and "wrf" in target_list:
        logger.error("Cannot compile WRF in both real and idealized mode.")
        sys.exit()
    # After verification add wrf to target_list for an idealized build.
    if ideal_case:
        # 'ideal' is not really a WRF target so we remove it from
        # target_list and instead add ideal_case option.
        amatch = "".join([s for s in target_list if "ideal" in s])
        target_list.remove(amatch)
        my_build.env_vars["ideal_case_name"] = amatch
        my_build.env_vars["EXP_LIS"] = "1"
        my_build.options.append("ideal_case")
        target_list.append("wrf")

    # OK, done withs "target_list error checking". Now modify options/targets
    if "allclean" in target_list:
        my_build.options.append("cleanfirst")

    # Set ENV variables associated with "all" targets
    new_target_list = list(target_list)
    if all_build_case:
        for k in target_list:
            new_target_list = set_all_targets(k, my_build, new_target_list)
    target_list = new_target_list

    # Set ENV variables associated with "wrf" targets
    wrf_target_list = list()
    for k in ["kpp", "chem", "wrf"]:
        if k in target_list:
            wrf_target_list = set_wrf_targets(k, my_build, wrf_target_list)
    target_list.extend(wrf_target_list)
    # Remove duplicate targets
    s = set(target_list)
    target_list = list(s)

    my_build.targets = target_list
    my_build.set_targets_options(target_list)

    return my_build


def set_libdir_environment(config, my_build):
    # Use COMPILER_VENDOR, MPI_VENDOR and LIBDIR_TAG values set in  environment
    # If not set then grab from the config file. If not set, abort.
    lib_config = u.config_section_map(config, "libconfig")
    if os.environ.get("LIBDIR_TAG") is None:
        try:
            libdir = lib_config["libdir_tag"]
        except KeyError as err:
            print("Error: {} is not defined.".format(err))
    else:
        libdir = os.environ.get("LIBDIR_TAG")
        
    if os.environ.get("COMPILER_VENDOR") is None:
        try:
            compiler_vendor = lib_config["compiler_vendor"]
        except KeyError as err:
            print("Error: {} is not defined.".format(err))
    else:
        compiler_vendor = os.environ.get("COMPILER_VENDOR")

    if os.environ.get("MPI_VENDOR") is None:
        try:
            mpi_vendor = lib_config["mpi_vendor"]
        except KeyError as err:
            print("Error: {} is not defined.".format(err))
    else:
        mpi_vendor = os.environ.get("MPI_VENDOR")

    # Set these so that values can by used in other config sections
    config.set("libconfig", "libdir_tag", libdir)
    config.set("libconfig", "compiler_vendor", compiler_vendor)
    config.set("libconfig", "mpi_vendor", mpi_vendor)

    logger.debug("LIBDIR_TAG: " + libdir)
    logger.debug("COMPILER_VENDOR: " + compiler_vendor)
    logger.debug("MPI_VENDOR: " + mpi_vendor)
    my_build.env_vars["BASELIBS"] = libdir
    my_build.env_vars["LIBDIR_TAG"] = libdir
    my_build.env_vars["COMPILER_VENDOR"] = compiler_vendor
    my_build.env_vars["MPI_VENDOR"] = mpi_vendor

    return compiler_vendor, mpi_vendor


def get_key_val(envs, cfg, sec, comp):
    cv = comp+'compiler'
    for (key, val) in cfg.items(sec):
        if cv in key:
            key_split = key.split("_")
            key_opt = "_".join(key_split[:-1])
            if "debug" in key:
                envs[key_opt.upper()] = val
            else:
                envs[key_opt.upper()] = val
        else:
            if comp not in key and 'compiler' in key:
                continue
            envs[key.upper()] = val
    return envs


def config_section_old(envs, config, section, compiler_vendor):
    # Special treatment for some keys
    for (each_key, each_val) in config.items(section):
        if "met_" in each_key:
            for (each_key, each_val) in config.items(section):
                envs[each_key.upper()] = each_val
        else:
            envs = get_key_val(envs, config, section, compiler_vendor)
    return envs


def config_section(envs, config, section, compiler_vendor):
    # Special treatment for some sections
    if section == "wrfconfig" or section == "wpsconfig":
        for (each_key, each_val) in config.items(section):
            if compiler_vendor in each_key or "debug" in each_key:
                each_key_split = each_key.split("_")
                if "debug" in each_key and "intel" in compiler_vendor:
                    each_val = each_val + " -traceback"
                elif "debug" in each_key and "gnu" in compiler_vendor:
                    each_val = each_val + " -fbacktrace"
                each_key_opt = "_".join(each_key_split[:-1])
                envs[each_key_opt.upper()] = each_val
            else:
                envs[each_key.upper()] = each_val
    elif section == "uppconfig":
        for (each_key, each_val) in config.items(section):
            if compiler_vendor in each_key or "debug" in each_key:
                each_key_split = each_key.split("_")
                each_key_opt = "_".join(each_key_split[:-1])
                envs[each_key_opt.upper()] = each_val
            else:
                envs[each_key.upper()] = each_val
    elif section == "metconfig":
        for (each_key, each_val) in config.items(section):
            envs[each_key.upper()] = each_val
    else:
        for (each_key, each_val) in config.items(section):
            if compiler_vendor in each_key:
                each_key_split = each_key.split("_")
                each_key_opt = "_".join(each_key_split[:-1])
                envs[each_key_opt.upper()] = each_val
            else:
                envs[each_key.upper()] = each_val
    return envs


def process_config(my_build):
    logger.debug("Reading config file " + str(my_build.config))
    config = u.get_config(my_build.config)

    logger.info("Setting up build environment:")
    logger.debug("Using Python version: " + str(sys.version.split()[0]))
    logger.debug("Computing platform is " + platform.uname()[0])

    comp_vendor, mpi_vendor = set_libdir_environment(config, my_build)

    logger.debug("Set libpaths")
    if sys.version_info[0] < 3:
        my_build.env_vars = config_section_old(
            my_build.env_vars, config, "libconfig", comp_vendor
        )
    else:
        for (each_key, each_val) in config.items("libpaths"):
            my_build.env_vars[each_key.upper()] = each_val

        sections = [
            "wrfconfig",
            "wpsconfig",
            "arwpostconfig",
            "ripconfig",
            "gsdsuconfig",
            "ldtconfig",
            "lvtconfig",
            "uppconfig",
            "metconfig",
        ]
        for s in sections:
            my_build.env_vars = config_section(
                my_build.env_vars, config, s, comp_vendor
            )

    if my_build.dump_envs:
        my_build.dump_env_vars()
        logger.info("Dumped env vars.")
        sys.exit()

    my_build.set_env_vars()
    my_build.get_build_config()

    logger.debug("Done with setup.")


def clean_packages(my_build):
    for target, directory in my_build.target_dir.items():
        logger.info("Clean [" + target + "]")
        clean.clean_sub(my_build, target, directory)
    logger.info("Done cleaning NU-WRF targets")


def check_options(my_build, target):
    valid_options = my_build.target_options[target]
    options = my_build.options
    if not options:
        return
    options_ok = set(options).issubset(set(valid_options))
    if not options_ok:
        invalid_options = set(options) - set(valid_options)
        logger.error(
            "Target ["
            + target
            + "] invalid option(s): {0}".format(list(invalid_options))
        )
        logger.error("Valid options are {}".format(valid_options))
        sys.exit(1)


def build_from_tarball(my_build, target):
    apps_dir = ""
    nuwrf_dir = os.environ.get("NUWRFDIR")
    if "discover" in platform.node() or "borg" in platform.node():
        apps_dir = "/discover/nobackup/projects/nu-wrf/external_apps/"
    if os.path.isfile(apps_dir + target + ".tgz"):
        os.chdir(nuwrf_dir)
        tf = tarfile.open(apps_dir + target + ".tgz")
        tf.extractall()
        build.main(my_build, target)
    else:
        logger.error(
            apps_dir + target + ".tgz not found!\n"
            "To build, place tarball named "
            + target
            + ".tgz in {} and re-run command".format(nuwrf_dir)
        )


def build_packages(my_build):
    target_order = [
        "lis",
        "wrf",
        "wps",
        "upp",
        "arw",
        "doc",
        "gsdsu",
        "ldt",
        "lvt",
        "rip",
        "utils",
    ]
    utils_built = False
    nuwrf_dir = os.environ.get("NUWRFDIR")

    # Build 'met' separately
    if "met" in my_build.targets:
        check_options(my_build, "met")
        logger.info("Build [met] separate from other targets")
        build.main(my_build, "met")
        return

    for target in target_order:
        if target in my_build.targets or target in my_build.utils_exe:
            check_options(my_build, target)
            logger.info("Build [" + target + "]")

            if not os.path.isdir(nuwrf_dir + "/" + my_build.target_dir[target]):
                logger.warning(
                    "Target directory ["
                    + my_build.target_dir[target]
                    + "] does not exist."
                )
                logger.warning("Will try tarball...")
                if target in ["arw", "rip", "upp"]:
                    build_from_tarball(my_build, target)
                    continue

            if target == 'wrf':
                wrf.main(my_build)
                build.main(my_build, target)
            else:
                build.main(my_build, target)
                if target == 'utils':
                    utils_built = True
                continue

    # If 'utils' target was not chosen but rather one of the 'utils', then:
    if not utils_built:
        for target in my_build.utils_exe:
            if target in my_build.targets:
                check_options(my_build, target)
                logger.info("Build [" + target + "]")
                build.main(my_build, target)

    if my_build.prefix:
        print("\n")
        for target in target_order:
            if target in my_build.targets:
                install.install(my_build, target)
        if not utils_built:
            for target in my_build.utils_exe:
                if target in my_build.targets:
                    install.install(my_build, target)


def main():
    start_time = time.time()
    args = parse_args()

    # Set up a logger
    u.logger_setup(os.environ.get("NUWRFDIR") + "/nu-wrf")
    print("\nBuilding NU-WRF\n---------------\n")

    my_build = process_args(args)

    process_config(my_build)

    if "allclean" in my_build.targets:
        clean_packages(my_build)
    else:
        wrf.require_mpi(my_build)
        if "cleanfirst" not in my_build.options:
            if "wps" in my_build.targets:
                wrf.files_required_by_wps(my_build)
            if "upp" in my_build.targets:
                wrf.files_required_by_upp(my_build)
        build_packages(my_build)

    if "allclean" not in my_build.targets:
        my_build.set_build_config()
    
    # Done
    end_time = time.time() - start_time

    logger.info("NU-WRF build time taken = %f" % end_time)


if __name__ == "__main__":
    main()
