#Import File Readme

This document describes how to create import file templates and import data into a FRAM model run.  The main purpose of this function is to clearly split up responsibilities around updating catch 

## Create Import Files

Import files can initially be created from an existing FRAM model run, typically the post-season run.

To create the import files, you must first configure the `"CreateImportFiles"` by editing the `"create_import_config.r"` in the `"config"` folder.  In that file you must set the following variables using R language syntax:

* `fram.db.name` should be set to the FRAM database file name, include path if needed.
* `fram.run.name` is the name of the FRAM run that you would like to create the import files for.

With the `create_import_config.r` file configured, you can then run the `CreateImportFiles.bat` command line script by double clicking it.  This will create separate import files for each person identified as a catch source in the `data\PersonCatch.csv` file.  All the import files are written to the `report` folder.

## Modify Import Files

Once the import files are create they can be distributed to individual people.  Generally speaking, the fisheries and time steps identified in the individual import files should not be modified because of two possible out comes:

*Catch values may override values provided by other import files
*Catch values may not be used by FRAM because they don't align with the base period


###Fishery Flag Values
First Header | Second Header
------------ | -------------

Fishery Flag | Description 
------------ | -------------
 1 | Non-Selective fishery based on base period scalar (not for use with Post Season catch) 
 2 | Non-Selective fishery based on quota (**use this to set final fishery catch**) 
 7 | Mark-Selective fishery based on base period scalar (not for use with Post Season catch) 
 8 | Mark-Selective fishery based on quota (**use this to set final fishery catch**) 
 17 | Non-Selective and Mark-Selective fishery based on base period scalar (not for use with Post Season catch) 
 18 | Non-Selective base period scalar and Mark-Selective quota fishery (not for use with Post Season catch) 
 27 | Non-Selective quota and Mark-Selective base period scalar fishery (not for use with Post Season catch) 
 28 | Non-Selective and Mark-Selective fishery based on quota (**use this to set final fishery catch**) 

## Update FRAM Post-Season Model Run with Import File

Once the import files have been updated, they can be imported back into the model run through `ImportPostSeasonFile.bat` command.  This command validates the file and updates the 

Note: The import file is validated for use as a post season catch data set for backward FRAM.  This mainly consists of checking fishery flags against non-selective and mark selective catch.


:+1: