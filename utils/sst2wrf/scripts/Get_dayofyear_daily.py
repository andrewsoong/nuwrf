#!/usr/bin/env python
#
# Get_dayofyear_daily.py: To get the list of date  times from a Start Date To endDate with a specific interval 
# in days. 
# Jossy P. Jacob (Advanced Software Technology Group) Oct 30, 2014 
###################################################################
import datetime
import sys

if len(sys.argv) < 4:
    sys.exit('ERROR, invalid number of command line arguments!')
sd = sys.argv[1]
ed = sys.argv[2]
Interval = sys.argv[3]

dt = int(Interval)
year1 = int(sd[0:4])
month1 = int(sd[4:6])
day1 = int(sd[6:8])
sdate = datetime.date(year1,month1,day1)
year2 = int(ed[0:4])
month2 = int(ed[4:6])
day2 = int(ed[6:8])
edate = datetime.date(year2,month2,day2)
 
cdate = sdate
with open('Datetime.dat', 'w') as output:
    while cdate <= edate:
        tt=cdate.timetuple()
        cday = tt.tm_yday
        cdate2 = "%3.3d%3.3d\n" % (tt.tm_year,tt.tm_yday) # EMK
        output.write(cdate2)
        incr = datetime.timedelta(days=dt)
        cdate = cdate + incr

