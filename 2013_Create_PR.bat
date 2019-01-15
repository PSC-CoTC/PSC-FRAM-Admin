REM echo off

call ./bin/setup

set ANNUAL_REPORT_LOG=".\log\2013_PerRep.log"
set ANNUAL_REPORT_CONFIG=".\config\2013_PR_config.r"

%R_HOME% --vanilla -f .\lib\PeriodicReport.r --args %ANNUAL_REPORT_CONFIG% > %ANNUAL_REPORT_LOG% 2>&1
.\bin\tail %ANNUAL_REPORT_LOG%

start .\report\2013_PeriodicReport.html

pause
