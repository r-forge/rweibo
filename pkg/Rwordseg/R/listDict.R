

##' List the installed dictionary.
##' 
##' @title List the installed dictionary.
##' @return No results.
##' @author Jian Li <\email{rweibo@@sina.com}>
listDict <- function() {
	ori.dic <- readLines(system.file("config", "userdic", package = "Rwordseg"))
	Encoding(ori.dic) <- "UTF-8"
	OUT <- unique(sapply(strsplit(ori.dic, "\t"), FUN = function(X) X[2]))
	OUT <- setdiff(OUT, "n")
	return(OUT)
}



