##' When you restart R, all of the wordes will be removed. If you want to keep them please try \code{\link{installDict}}.
##' 
##' @title Insert new words into analyzer.
##' @param strwords Vector of words.
##' @param analyzer Which analyzer.
##' @return No results.
##' @author Jian Li <\email{rweibo@@sina.com}>
##' 
insertWords <- function(strwords, analyzer = c("jiebaR", "fmm")) 
{
	analyzer <- match.arg(package)
	if (analyzer == "jiebaR") {
		if (suppressWarnings(requireNamespace("jiebaR", quietly = TRUE))) {
			.loadModels("jiebaR")
			jiebaAnalyzer <- get("jiebaAnalyzer", envir = .RwordsegEnv)
			jiebaR::new_user_word(jiebaAnalyzer, strwords, tags = rep("x", length(strwords)))
			assign("jiebaAnalyzer", jiebaAnalyzer, envir = .RwordsegEnv)
		}
	} 
}








