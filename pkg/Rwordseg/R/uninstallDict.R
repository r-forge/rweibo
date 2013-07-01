

##' Uninstall the user defined dictionary.
##' 
##' @title Uninstall the user defined dictionary.
##' @param removedict Names of the dictionary to be uninstalled.
##' @return No results.
##' @author Jian Li <\email{rweibo@@sina.com}>
uninstallDict <- function(removedict = listDict()$Name) {
	if (length(removedict) > 0) {
		ori.dic <- readLines(file.path(getOption("app.dir"), "userdic"))
		Encoding(ori.dic) <- "UTF-8"
		keeprows <- which(!sapply(strsplit(ori.dic, "\t"), FUN = function(X) X[2]) %in% removedict)
		Encoding(ori.dic) <- "GBK"
		writeLines(ori.dic[keeprows], file.path(getOption("app.dir"), "userdic"))
		.removeDictMeta(removedict)
		cat("OK!\nThe user defined dictionary was uninstalled, please restart R.\n")
	}
}



