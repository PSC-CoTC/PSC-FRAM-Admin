REM echo off

call ./bin/setup

set PERIODIC_REPORT_LOG=".\log\PeriodicReportLog.txt"

%R_HOME% --vanilla -f .\lib\PeriodicReport.r --args > %PERIODIC_REPORT_LOG% 2>&1
.\bin\tail %PERIODIC_REPORT_LOG%

for %%G in (.\report\20*.html) do (
  start "" "%%G"
  )

pause
