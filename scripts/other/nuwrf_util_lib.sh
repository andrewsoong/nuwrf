#!/bin/sh
#------------------------------------------------------------------------------
# NASA/GSFC, Software Integration & Visualization Office, Code 610.3           
#------------------------------------------------------------------------------
#                                                                              
# SCRIPT:  nuwrf_util_lib.sh
#                                                                              
# AUTHOR:                                                                      
# Eric Kemp, NGIS/NASA SIVO                                                    
#                                                                              
# DESCRIPTION:                                                                 
# Contains utility functions for NU-WRF build system.
#                                                                              
# REVISION HISTORY:                                                            
# 2 Apr 2011 - Initial version
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------

function nuwrf_check_exit {
    # Checks the return code from the previous shell execution.  If return
    # code is non-zero (indicating error), print error message and exit.
    # @param $1 is a string specifying what the return code is from.
    if [ ! "$?" -eq 0 ]
    then
        echo "ERROR returned from $1!"
        exit 1
    fi
    return 0
}

#------------------------------------------------------------------------------
