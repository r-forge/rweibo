
##' A function segment Chinese sentence into words.
##' 
##' @title Sengment a sentence.
##' @param strwords A Chinese sentence in UTF-8.
##' @param analyzer A JAVA object of analyzer.
##' @param nature Whether to recognise the nature of the words.
##' @param nosymbol Whether to keep symbols in the sentence.
##' @return a vector of words (list if input is vecter) which have been segmented.
##' @author Jian Li <\email{rweibo@@sina.com}>
##' @examples \dontrun{
##' segmentCN("hello world!")
##' }

segmentCN <- function(strwords, analyzer = get("Analyzer", envir = .RwordsegEnv), nature = FALSE, nosymbol = TRUE) {
	if (!is.character(strwords)) stop("Please input character!")
	if (length(strwords) == 1) {
		if (nature) {
			OUT <- .jcall(analyzer, "S", "segWordNature", strwords)
			Encoding(OUT) <- "UTF-8"
			OUT <- gsub(" +", " ", OUT)
			if (nzchar(OUT)) {
				OUT <- strsplit(OUT, split = " ")[[1]]
				OUT <- gsub(":.*$", "", OUT)
				splitlist <- strsplit(OUT[nzchar(OUT)], split = "\\|")
				OUT <- sapply(splitlist, FUN = function(X) X[[1]])
				names(OUT) <- sapply(splitlist, FUN = function(X) X[[2]])
			}
		} else {
			OUT <- .jcall(analyzer, "S", "segWord", strwords)
			Encoding(OUT) <- "UTF-8"
			OUT <- gsub(" +", " ", OUT)
			if (nzchar(OUT)) OUT <- strsplit(OUT, split = " ")[[1]]
		}
		
		if (nosymbol && any(nzchar(OUT))) {
			OUT <- OUT[grep("[\u4e00-\u9fa5]|[a-z]|[0-9]", OUT)]
			OUT <- gsub("\\[|\\]", "", OUT)
			OUT <- OUT[nzchar(OUT)]
		}
		if (length(OUT) == 0) OUT <- ""
		return(OUT)
	} else {
		return(lapply(strwords, segmentCN, analyzer, nature, nosymbol))
	}
}


