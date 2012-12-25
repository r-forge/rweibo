# TODO: Add comment
# 
# Author: jli
###############################################################################

.onAttach <- function(libname, pkgname ){
	packageStartupMessage( paste("# Version:", utils:::packageDescription("Rwordseg", fields = "Version")) )
	.jpackage(pkgname, lib.loc=libname)
	dictpath <- system.file("config", "userdic", package = "Rwordseg")
	dictpath <- chartr("\\", "/", dictpath)
	if (!exists(".RwordsegEnv", envir = .GlobalEnv)) {
		assign(".RwordsegEnv", new.env(), envir = .GlobalEnv)
	}
	if (exists("Analyzer", envir = .RwordsegEnv)) {
		rm("Analyzer", envir = .RwordsegEnv)
	}
	Analyzer <- .jnew("org/jianl/wordseg/r/Analyzer")
	.jcall(Analyzer, "V", "setDicPath", dictpath)
	.jcall(Analyzer, "V", "initialAnalyzer")
	assign("Analyzer", Analyzer, envir = .RwordsegEnv)
	options(dic.dir = system.file("dict", package = "Rwordseg"))
	tryload <- try(loadUserDict(), silent = TRUE)
	if (inherits(tryload, "try-error")) warning(paste("Fail to load the user defined dictionary:\n", as.character(tryload)))
}

.onUnload <- function(libpath) {
	rm(.RwordsegEnv, envir = .GlobalEnv)
	library.dynam.unload("Rwordseg", libpath)
}
