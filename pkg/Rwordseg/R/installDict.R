


##' Install new dictionary.
##' 
##' @title Install new dictionary.
##' @param dictpath Path of dictionary.
##' @param transGBK Whether to transform GBK t0 UTF-8.
##' @return No results.
##' @author Jian Li <\email{rweibo@@sina.com}>

installDict <- function(dictpath, transGBK = FALSE) {
	tmp.dic <- readLines(dictpath)
	if (transGBK) {
		tmp.dic <- iconv(tmp.dic, "GBK", "UTF-8")
	} else {
		Encoding(tmp.dic) <- "UTF-8"
	}
	ori.dic <- readLines(system.file("config", "userdic", package = "Rwordseg"))
	Encoding(ori.dic) <- "UTF-8"
	
	oriwords <- sapply(strsplit(ori.dic, "\t"), FUN = function(X) X[1])
	oriwords <- tolower(unique(oriwords[!is.na(oriwords)]))
	allwords <- sapply(strsplit(tmp.dic, "\t| "), FUN = function(X) X[1])
	allwords <- unique(allwords[!is.na(allwords)])
	allwords <- tolower(gsub(" ", "", allwords))
	allwords <- c(oriwords, allwords[! allwords %in% oriwords])
	outwords <- paste(allwords, "userDefine", 1000, sep = "\t")
	Encoding(outwords) <- "GBK"

	writeLines(outwords, system.file("config", "userdic", package = "Rwordseg"))
	cat("New dictionary was installed, please restart R to use it.\n")
}


##' Uninstall the use defined dictionary.
##' 
##' @title Uninstall the use defined dictionary.
##' @return No results.
##' @author Jian Li <\email{rweibo@@sina.com}>
uninstallDict <- function() {
	outwords <- paste(c("r\u8BED\u8A00", "\u7EDF\u8BA1\u4E4B\u90FD"), "userDefine", 1000, sep = "\t")
	Encoding(outwords) <- "GBK"
	writeLines(outwords, system.file("config", "userdic", package = "Rwordseg"))
	cat("The use defined dictionary was uninstalled, please restart R.\n")
}



