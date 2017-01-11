REM echo off

call ./bin/setup

if not exist ".\log" mkdir .\log

set CREATE_IMPORT_LOG=".\log\create_import_files.log"
set CREATE_IMPORT_CONFIG=".\config\create_import_config.r"

%R_HOME% .\lib\CreateImportFiles.r %CREATE_IMPORT_CONFIG% > %CREATE_IMPORT_LOG% 2>&1
.\bin\tail %CREATE_IMPORT_LOG%

pause
