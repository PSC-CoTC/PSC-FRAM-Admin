#Annual Report Readme

This document describes how to configure and run the Coho Techincal Committee Annual Reporting Tool.

## Setup

To run the annual report, you need to setup 3 files and have the databases with the FRAM model runs available to the script.

For example, if you wanted to setup an annual report for 1974, the year of the first "-gate" scandal ([Watergate](https://en.wikipedia.org/wiki/Watergate_scandal)).  You would need to setup these three files:

1. A `"1974_report_config.r"` in the `"config"` folder
2. A run script called `"1974_Create_Annual_Report.bat"` in the main `"PSC-FRAM-Admin"` folder
3. Put the database(s) with your pre-season and post-season model runs in the `"fram db"` folder

### Configuring `"1974_report_config.r"`

To configure the "1974_report_config.r" file you only need to modify following lines:
```
run.year <- 1974
```

```
pre.season.fram.db <- "./fram db/1974_Fram_postseason.mdb"
```
This line provides the file name of where the database containing the post-season FRAM model run is saved.  The same database can hold both the pre and post season model runs, just use the same file name for both the pre and post season database file name.

```
pre.season.run.name <- "1974 pre-season run name"
```
This line provides the name of the run in the post-season FRAM database file.  Just replace `"1974 pre-season run name"` with your model run name.


```
post.season.fram.db <- "./fram db/1974_Fram_postseason.mdb"
```
This line provides the file name of where the database containing the post-season FRAM model run is saved.  The same database can hold both the pre and post season model runs, just use the same file name for both the pre and post season database file name.

```
post.season.run.name <- "1974 post-season run name"
```
This line provides the name of the run in the post-season FRAM database file.  Just replace `"1974 post-season run name"` with your model run name.

### Configuring "1974_Create_Annual_Report.bat"

## Running
