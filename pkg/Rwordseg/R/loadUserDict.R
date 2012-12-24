


##' Load user defined dictionary.
##' 
##' @title Load user defined dictionary.
##' @return No results.
##' @author Jian Li <\email{rweibo@@sina.com}>

loadUserDict <- function() {
	dictfiles <- list.files(getOption("dic.dir"), pattern = ".*\\.dic", full.names = TRUE, ignore.case = TRUE)
	if (length(dictfiles) > 0) {
		for (i in seq_along(dictfiles)) {
			tmp <- readLines(dictfiles[i])
			if (getOption("encoding") != "UTF-8") Encoding(tmp) <- "UTF-8"
			tmp <- tmp[nzchar(tmp)]
			if (length(tmp) > 0) insertWords(tmp)
		}
	}
}





