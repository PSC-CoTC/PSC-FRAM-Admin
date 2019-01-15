REM echo off

call ./bin/setup

set ANNUAL_REPORT_LOG=".\log\2017_Annual_Report.log"
set ANNUAL_REPORT_CONFIG=".\config\2017_report_config.r"

%R_HOME% --vanilla -f .\lib\AnnualReport.r --args %ANNUAL_REPORT_CONFIG% > %ANNUAL_REPORT_LOG% 2>&1
.\bin\tail %ANNUAL_REPORT_LOG%

start .\report\2017_AnnualReport.html


pause
