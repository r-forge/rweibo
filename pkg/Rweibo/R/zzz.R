# TODO: Add comment
# 
# Author: jli
###############################################################################

.onAttach <- function(libname, pkgname ){
	packageStartupMessage( paste("# Version:", packageDescription("Rweibo", fields = "Version")) )
}