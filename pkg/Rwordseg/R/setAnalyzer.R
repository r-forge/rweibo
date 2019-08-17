##' The default analyzer is 'hmm', which is implemented by native R codes and still in development. 
##' You can use 'jiebaR' instead. Or 'coreNLP' to invoke Stanford CoreNLP. Or choose 'fmm' to try the forward maximum matching algorithm.
##' 
##' @title Set the default analyzer.
##' @param analyzer One of 'jiebaR', 'hmm', 'fmm' and 'coreNLP'.
##' @param coreNLPdir, Set the coreNLP file path, only use for 'coreNLP'.
##' @return No results.
##' @examples
##' setAnalyzer("hmm")
##' 
setAnalyzer <- function(analyzer = c("hmm", "jiebaR", "fmm", "coreNLP"), coreNLPdir = "") {
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
	if (analyzer == "coreNLP") {
		if (file.exists(coreNLPdir)) {
			.setdboption("coreNLP.dir", coreNLPdir)
		}
		.loadModels("coreNLP")
		options(RwordsegAnalyzer = "coreNLP")
	}
}


