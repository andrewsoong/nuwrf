#!/usr/bin/env python
from __future__ import print_function
from __future__ import division
import sys
import numpy as np
import netCDF4 as nc4

mach_eps = np.finfo(float).eps


def rel_error(a, b):
    return np.max(np.abs(a - b) / (np.maximum(mach_eps, np.abs(a) + np.abs(b))))


file1 = sys.argv[1]
file2 = sys.argv[2]

fp1 = nc4.Dataset(file1, format="NETCDF4")
fp2 = nc4.Dataset(file2, format="NETCDF4")

allvars = list(fp1.variables.keys())
dims = list(fp1.dimensions.keys())

# Exclude dims from vars list
nc_vars = [x for x in allvars if x not in dims]
for var in nc_vars:
    v1 = fp1.variables[var][:]
    v2 = fp2.variables[var][:]
    # Treat string arrays (e.g. dates) differently
    is_S = np.issubdtype(v1.dtype, "S1")
    if is_S:
        diff = np.setdiff1d(v1, v2)
        if diff:
            print("%s differs: %r" % (var, diff))
    else:
        max_rel_err = rel_error(v1, v2)
        if max_rel_err > mach_eps:
            print("%20s max rel error: %30.18f" % (var, max_rel_err))

fp1.close()
fp2.close()
