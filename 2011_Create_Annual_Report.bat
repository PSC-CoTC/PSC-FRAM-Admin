REM echo off

call ./bin/setup

set ANNUAL_REPORT_LOG="2011_Annual_Report.log"
set ANNUAL_REPORT_CONFIG="2011_report_config.r"

%R_HOME% .\lib\AnnualReport.r %ANNUAL_REPORT_CONFIG% > %ANNUAL_REPORT_LOG% 2>&1
.\bin\tail %ANNUAL_REPORT_LOG%

pause
