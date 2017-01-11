#Import File Readme

This document describes how to create import file templates and import data into a FRAM model run.  The main purpose of this function is to clearly split up responsibilities around updating catch 

## Create Import Files

Import files can initially be created from an existing FRAM model run, typically the post-season run.

To create the import files, you must first configure the `"CreateImportFiles"` by editing the `"create_import_config.r"` in the `"config"` folder.  In that file you must set the following variables using R language syntax:

* `fram.db.name` should be set to the FRAM database file name, include path if needed.
* `fram.run.name` is the name of the FRAM run that you would like to create the import files for.

For example, if you wanted to setup an annual report for 1974, the year of the first "-gate" scandal ([Watergate](https://en.wikipedia.org/wiki/Watergate_scandal)).  You would need to setup these three files:

1. A `"1974_report_config.r"` in the `"config"` folder
2. A run script called `"1974_Create_Annual_Report.bat"` in the main `"PSC-FRAM-Admin"` folder
3. Put the database(s) with your pre-season and post-season model runs in the `"fram db"` folder

## Modify Import Files


## Update FRAM Post-Season Model Run with Import File

With the configuration file finished, you need to configure a windows batch file to generate the annual report.  Modify the following lines by replacing the year (`1974`) with the year you want to run.

```
set ANNUAL_REPORT_LOG=".\log\1974_Annual_Report.log"
set ANNUAL_REPORT_CONFIG=".\config\1974_report_config.r"
```


## Running

Just double-click the `1974_Create_Annual_Report.bat` to generate the 1974 annual report.

Once the script runs, you will have two output files:

The log of R script, showing possible errors and warnings is provided at:

`log\1974_Annual_Report.log`

The actual 1974 report is provided in the following file

`report\2014_AnnualReport.html`

Once the report is generated, you can convert the report from HTML to PDF using Chrome.

Voila! You should now (hopefully) have an annual report and not a -gate scandal :-)