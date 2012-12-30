

##' Uninstall the user defined dictionary.
##' 
##' @title Uninstall the user defined dictionary.
##' @param removedict Names of the dictionary to be uninstalled.
##' @return No results.
##' @author Jian Li <\email{rweibo@@sina.com}>
uninstallDict <- function(removedict = listDict()) {
	ori.dic <- readLines(system.file("config", "userdic", package = "Rwordseg"))
	Encoding(ori.dic) <- "UTF-8"
	keeprows <- which(!sapply(strsplit(ori.dic, "\t"), FUN = function(X) X[2]) %in% removedict)
	Encoding(ori.dic) <- "GBK"
	writeLines(ori.dic[keeprows], system.file("config", "userdic", package = "Rwordseg"))
	cat("OK!\nThe user defined dictionary was uninstalled, please restart R.\n")
}



