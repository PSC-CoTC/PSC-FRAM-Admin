REM echo off

call ./bin/setup

set CREATE_IMPORT_LOG=".\log\create_import_files.log"
set CREATE_IMPORT_CONFIG=".\config\2014_report_config.r"

%R_HOME% .\lib\CreateImportFiles.r %CREATE_IMPORT_CONFIG% > %CREATE_IMPORT_LOG% 2>&1
.\bin\tail %CREATE_IMPORT_LOG%

pause
