
##' Read a corpus vector and generate the dictionary data frame.
##' 
##' @title Create a dictionary file from corpus.
##' @param trainvec A character vector of corpus.
##' @param dicfile The path of output file. Defult is NULL.
##' @param wordsplit Character containing regular expression to use for splitting words.
##' @param natruesplit Character containing regular expression to use for splitting nature.
##' @return 
##'  A data frame of: 
##'  \item{word}{Word.}
##'  \item{freq}{Frequency.}
##'  \item{nature}{Nature.}
##' @author Jian Li <\email{rweibo@@sina.com}>
##' @examples
##' data(PD980105)
##' d1 <- createDict(PD980105[1:10])
##' head(d1)
##' 
createDict <- function(trainvec, dicfile = NULL, wordsplit = "\\s+", natruesplit = "/") {
	l1 <- strsplit(trainvec, split = wordsplit)
	d2 <- as.data.frame(do.call("rbind", strsplit(unlist(l1), split = natruesplit)), stringsAsFactors = FALSE)
	d2$V1 <- gsub("[^\u4e00-\u9fa5]", "", d2$V1)
	d3 <- d2[nzchar(d2$V1), ]
	l4 <- split(d3, f = d3$V1)
	l5 <- lapply(l4, FUN = function(X) data.frame(word = X$V1[1], freq = nrow(X), nature = names(sort(table(X$V2), decreasing = TRUE)[1]), stringsAsFactors = FALSE))
	df6 <- do.call("rbind", l5)
	df6$nature <- gsub("\\[.*$", "", gsub("^.*\\]", "", df6$nature))
	rownames(df6) <- NULL
	if (!is.null(dicfile)) {
		write.table(df6, file = dicfile, sep = " ", row.names = FALSE, col.names = FALSE, quote = FALSE, fileEncoding = "UTF-8")
	}
	invisible(df6)
}


