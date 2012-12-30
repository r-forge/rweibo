


##' Load user defined dictionary.
##' 
##' @title Load user defined dictionary.
##' @return No results.
##' @author Jian Li <\email{rweibo@@sina.com}>

loadUserDict <- function() {
	dictfiles <- list.files(getOption("dic.dir"), pattern = ".*\\.dic", full.names = TRUE, ignore.case = TRUE)
	if (length(dictfiles) > 0) {
		for (i in seq_along(dictfiles)) {
			tmp.enc <- .detectEncoding(dictfiles[i])
			if (tmp.enc != "UTF-8") tmp.enc <- "GBK"
			tmp <- readLines(dictfiles[i])
			if (getOption("encoding") != "UTF-8") tmp <- iconv(tmp, tmp.enc, "UTF-8")
			tmp <- tmp[nzchar(tmp)]
			if (length(tmp) > 0) insertWords(tmp)
		}
	}
}





