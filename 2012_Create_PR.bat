REM echo off

call ./bin/setup

set ANNUAL_REPORT_LOG=".\log\2012_PerRep.log"
set ANNUAL_REPORT_CONFIG=".\config\2012_PR_config.r"

%R_HOME% --vanilla -f .\lib\PeriodicReport.r --args %ANNUAL_REPORT_CONFIG% > %ANNUAL_REPORT_LOG% 2>&1
.\bin\tail %ANNUAL_REPORT_LOG%

start .\report\2012_PeriodicReport.html

pause
