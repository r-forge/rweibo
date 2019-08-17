##' Runs the CoreNLP annotators to parse a string of text.
##' 
##' @title Parse a string of text.
##' @param text A vector of strings for parsing.
##' @return 
##'  A list of: 
##'  \item{parse}{A data frame of the results of syntactic parsing tree.}
##'  \item{token}{A data frame of the results of word segmentation.}
##'  \item{depenencies}{A data frame of the results of dependency parsing.}
##' @author Jian Li <\email{rweibo@@sina.com}>
##' 
parseSentence <- function(text) {
	text <- tmcn:::.verifyChar(text)
	.loadModels("coreNLP")
	outlist <- coreNLP::annotateString(text)
	out1 <- outlist$token
	out2 <- outlist$parse
	out3 <- outlist$basicDep
	out1$token <- toUTF8(out1$token)
	out1$lemma <- toUTF8(out1$lemma)
	out2 <- toUTF8(out2)
	out3$governor <- toUTF8(out3$governor)
	out3$dependent <- toUTF8(out3$dependent)
	OUT <- list(parse = out2, token = out1, depenencies = out3)
	return(OUT)
}


	
