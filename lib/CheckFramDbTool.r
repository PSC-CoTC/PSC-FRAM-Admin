################
#
# Code to perform common checks on FRAM Model runs for the Coho Technical Committee.
#
# Nicholas Komick
# nicholas.komick@dfo-mpo.gc.ca
# February 3, 2014
# Using: http://google-styleguide.googlecode.com/svn/trunk/google-r-style.html
#
#
################

source.lib.dir <- "./lib/"
if (exists("lib.dir")) {
  source.lib.dir <- lib.dir
} 

source(file.path(source.lib.dir, "Util.r"))


required.packages <- c("RODBC")
InstallRequiredPackages(required.packages)



msaccess.filter <- rbind(Filters[c("All"),], cbind("MS Access database (*.mdb)", "*.mdb"))


fram.db.name <- choose.files(default = "", caption = "Select FRAM Database",
                       multi = FALSE, filters = msaccess.filter,
                       index = nrow(Filters))


fram.db.conn <- odbcConnectAccess(fram.db.name)



