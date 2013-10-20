
##' A function segment Chinese sentence into words.
##' 
##' @title Sengment a sentence.
##' @param strwords A Chinese sentence in UTF-8.
##' @param analyzer A JAVA object of analyzer.
##' @param nature Whether to recognise the nature of the words.
##' @param nosymbol Whether to keep symbols in the sentence.
##' @param recognition Whether to recognise the person names automatically.
##' @param returnType Default is a string vector but we also can choose 'tm' 
##' to output a single string separated by space so that it can be used by \code{\link[tm]{Corpus}} directly. 
##' @return a vector of words (list if input is vecter) which have been segmented.
##' @author Jian Li <\email{rweibo@@sina.com}>
##' @examples \dontrun{
##' segmentCN("hello world!")
##' }

segmentCN <- function(strwords, analyzer = get("Analyzer", envir = .RwordsegEnv), 
		nature = FALSE, nosymbol = TRUE, recognition = TRUE, returnType = c("vector", "tm")) {
	if (!is.character(strwords)) stop("Please input character!")
	returnType <- match.arg(returnType)
	if (length(strwords) == 1) {
		if (nature) {
			strfunc <- ifelse(recognition, "segWordNature", "segWordNatureNoRecog")
			OUT <- .jcall(analyzer, "S", strfunc, strwords)
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
			strfunc <- ifelse(recognition, "segWord", "segWordNoRecog")
			OUT <- .jcall(analyzer, "S", strfunc, strwords)
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
		if (returnType == "tm") OUT <- paste(OUT, collapse = " ")
		return(OUT)
	} else {
		OUT <- sapply(strwords, segmentCN, analyzer, nature, nosymbol, recognition, returnType)
		names(OUT) <- NULL
		return(OUT)
	}
}


