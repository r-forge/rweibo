##' A function to segment Chinese sentence into words.
##' 
##' @title Sengment a sentence.
##' @param strwords A charactor vector of Chinese sentence.
##' @param analyzer One of 'default', 'jiebaR', 'hmm' and 'fmm'. Default is 'hmm'.
##' @param nature Whether to recognise the nature of the words.
##' @param nosymbol Whether to keep symbols in the sentence. Default is TRUE, means no symbols kept.
##' @param returnType Default is a string vector but we also can choose 'tm' 
##' to output a single string separated by space so that it can be used by \code{\link[tm]{Corpus}} directly. 
##' @param ... Other arguments.
##' @return a vector of words (list if input is vecter) which have been segmented.
##' @author Jian Li <\email{rweibo@@sina.com}>
##' @examples
##' segmentCN("hello world!")
##' 
segmentCN <- function(strwords, analyzer = c("default", "hmm", "jiebaR", "fmm"), 
		nature = FALSE, nosymbol = TRUE, returnType = c("vector", "tm"), ...) 
{
	if (!is.character(strwords)) stop("Please input character!")
	analyzer <- match.arg(analyzer)
	returnType <- match.arg(returnType)	
	if (analyzer == "default") {
		analyzer <- getOption("RwordsegAnalyzer")
	}
	
	if (nosymbol) strwords <- gsub("[^\u4e00-\u9fa5a-zA-Z0-9]", " ", strwords)
	strwords <- gsub("^\\s+|\\s+$", "", gsub("\\s+", " ", strwords))
	
	OUT <- do.call(paste0(".seg", analyzer), list(strwords = strwords, nature = nature))
	
	if (returnType == "tm") OUT <- sapply(OUT, paste, collapse = " ")
	if (length(OUT) == 1) OUT <- OUT[[1]]
	
	return(OUT)
}


.segjiebaR <- function(strwords, nature = FALSE) {
	if (!suppressWarnings(requireNamespace("jiebaR", quietly = TRUE))) {
		stop("Package \"jiebaR\" is required!")
	}
	.loadModels("jiebaR")
	.RwordsegEnv <- .verifyRwordsegEnv()
	jiebaAnalyzer <- get("jiebaAnalyzer", envir = .RwordsegEnv)
		
	OUT <- jiebaR::segment(strwords, jiebaAnalyzer)
	if (nature) OUT <- lapply(OUT, jiebaR::vector_tag, jiebaAnalyzer)
	return(OUT)
}

.segfmm <- function(strwords, nature = FALSE) {
	
	.loadModels("fmm")
	.RwordsegEnv <- .verifyRwordsegEnv()
	nmax <- min(nchar(strwords), 8)
	
	if (nmax == 1) {
		n1 <- try(get(strwords, envir = get("fmmAnalyzer", envir = .RwordsegEnv)), silent = TRUE)
		if (nature) {
			if (inherits(n1, "try-error")) n1 <- "x"
			names(strwords) <- n1
		}
		return(strwords)
	} else {
		for (i in nmax:1) {
			s1 <- substr(strwords, 1, i)
			n1 <- try(get(s1, envir = get("fmmAnalyzer", envir = .RwordsegEnv)), silent = TRUE)
			if (nchar(s1) == 1 || inherits(n1, "character")) {
				s2 <- substr(strwords, i + 1, nchar(strwords))
				if (nchar(s2) == 0) {
					if (nature) {
						if (inherits(n1, "try-error")) n1 <- "x"
						names(s1) <- n1
					}
					return(s1)
				}
				if (s1 == " ") {
					return(.segfmm(s2, nature))
				} else {
					if (nature) {
						if (inherits(n1, "try-error")) n1 <- "x"
						names(s1) <- n1
					}
					return(c(s1, .segfmm(s2, nature)))
				}
			}
		}
	}
}

.seghmm <- function(strwords, nature = FALSE) {
	.loadModels("hmm")
	.RwordsegEnv <- .verifyRwordsegEnv()
	hmmAnalyzer <- get("hmmAnalyzer", envir = .RwordsegEnv)
	
	s1 <- strsplit(strwords, split = "")
	sr <- lapply(s1, FUN = function(X) gsub("[^\u4e00-\u9fa5a-zA-Z0-9]", " ", X))
	s2 <- lapply(sr, FUN = function(X) viterbi(hmm = hmmAnalyzer, X))
	OUT <- lapply(1:length(s1), FUN = function(X) .decodechar(s1[[X]], s2[[X]]))
	if (nature) {
		.loadModels("fmm")
		OUT <- lapply(OUT, FUN = function(X) {names(X) <- .getNature(X);X})
	}
	return(OUT)
}
	
	
	
	
