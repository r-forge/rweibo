##' The default analyzer is 'hmm', which is implemented by native R codes and still in development. 
##' You can use 'jiebaR' instead. Or choose 'fmm' to try the forward maximum matching algorithm.
##' 
##' @title Set the default analyzer.
##' @param analyzer One of 'jiebaR', 'hmm' and 'fmm'.
##' @return No results.
##' @examples
##' setAnalyzer("hmm")
##' 
setAnalyzer <- function(analyzer = c("hmm", "jiebaR", "fmm")) {
	analyzer <- match.arg(analyzer)
	if (analyzer == "hmm") {
		.loadModels("hmm")
		options(RwordsegAnalyzer = "hmm")
	}
	if (analyzer == "jiebaR") {
		.loadModels("jiebaR")
		options(RwordsegAnalyzer = "jiebaR")
	}
	if (analyzer == "fmm") {
		.loadModels("fmm")
		options(RwordsegAnalyzer = "fmm")
	}
}


