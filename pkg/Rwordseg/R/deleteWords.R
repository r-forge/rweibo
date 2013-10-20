

##' Remove words into dictionary.
##' 
##' @title Remove words into dictionary.
##' @param strwords Vector of words.
##' @param analyzer A JAVA object of analyzer.
##' @return No results.
##' @author Jian Li <\email{rweibo@@sina.com}>
deleteWords <- function(strwords, analyzer = get("Analyzer", envir = .RwordsegEnv)) {
	if (!is.character(strwords)) stop("Please input character!")
	strwords <- tolower(strwords)
	for (strword in strwords) {
		.jcall(analyzer, "V", "removeWord", strword)
	}
}






