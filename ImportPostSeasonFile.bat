echo off

call ./bin/setup

set IMPORT_FILE_LOG=".\log\import_post_season_file.log"

%R_HOME% .\lib\ImportPostSeasonFile.r > %IMPORT_FILE_LOG% 2>&1
.\bin\tail %IMPORT_FILE_LOG%

pause
