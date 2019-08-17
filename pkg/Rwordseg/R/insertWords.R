##' When you restart R, all of the wordes will be removed. If you want to keep them please try \code{\link{installDict}}.
##' 
##' @title Insert new words into analyzer.
##' @param strwords Vector of words.
##' @return No results.
##' @author Jian Li <\email{rweibo@@sina.com}>
##' 
insertWords <- function(strwords) 
{
	.RwordsegEnv <- .verifyRwordsegEnv()
	if (exists("jiebaAnalyzer", envir = .RwordsegEnv)) {
		jiebaAnalyzer <- get("jiebaAnalyzer", envir = .RwordsegEnv)
		jiebaR::new_user_word(jiebaAnalyzer, strwords, tags = rep("x", length(strwords)))
		assign("jiebaAnalyzer", jiebaAnalyzer, envir = .RwordsegEnv)
	} 
}








