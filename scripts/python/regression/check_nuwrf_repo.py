#!/usr/bin/env python
from __future__ import print_function
import os
import subprocess


def syscmd(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE):
    p = subprocess.Popen(cmd, shell=True, stdout=stdout, stderr=stderr)
    (out, err) = p.communicate()
    return out.decode("utf-8")


def main():
    branches = {"master": "NO"}
    root = "/gpfsm/dnb02/ccruz/devel/nu-wrf/code/"
    python_cmd = "/usr/local/other/SSSO_Ana-PyD/2.4.0_py2.7/bin/python"
    git_path = "/gpfsm/dulocal/sles11/other/SLES11.3/git/2.18.0/libexec/git-core"
    old_path = os.environ["PATH"]
    os.environ["PATH"] = git_path + os.pathsep + old_path

    print("Check for repository updates...")
    for branch, dotest in branches.items():
        os.chdir(root + branch)
        before = syscmd("git rev-parse HEAD")
        rc = syscmd("git pull")  # update repository for "next" time
        after = syscmd("git rev-parse HEAD")
        if str(before) != str(after):
            print(" -- Changes detected in branch " + branch)
            branches[branch] = "YES"
        else:
            print(" -- NO changes detected in branch " + branch)

    for branch, dotest in branches.items():
        if "YES" in dotest:
            print(" -- Running tests for branch " + branch)
            os.chdir(root + branch + "/scripts/python/regression/")
            with open(branch + ".cron", "w") as log:
                out = syscmd(python_cmd + " reg " + branch, stdout=log, stderr=log)
        else:
            print(" -- Nothing to do for branch " + branch)


if __name__ == "__main__":
    main()
