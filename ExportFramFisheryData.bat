echo off

call ./bin/setup

set EXPORT_FILE_LOG=".\log\export_fram_fishery_data.log"
set EXPORT_FILE_CONFIG=".\config\export_fram_fishery_config.r"

%R_HOME% --vanilla -f .\lib\ExportFramFisheryData.r --args %EXPORT_FILE_CONFIG% > %EXPORT_FILE_LOG% 2>&1
.\bin\tail -n 30 %EXPORT_FILE_LOG%

pause
