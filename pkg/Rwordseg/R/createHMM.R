##' Read a corpus vector and generate a HMM model file.
##' 
##' @title Create a HMM model from corpus.
##' @param trainvec A character vector of corpus.
##' @param outputfolder The folder of output file. Defult is NULL.
##' @param sensplit Character containing regular expression to use for splitting sentence.
##' @param wordsplit Character containing regular expression to use for splitting words.
##' @param natruesplit Character containing regular expression to use for splitting nature.
##' @param removestr Character containing regular expression to use for removing string.
##' @return a list from \code{\link[HMM]{initHMM}}.
##' @examples
##' data(PD980105)
##' m1 <- createHMM(PD980105)
##' names(m1)
##' 
createHMM <- function(trainvec, outputfolder = NULL, sensplit = "/w", wordsplit = "\\s+", natruesplit = "/", removestr = "^.*?/m") {
	if (!suppressWarnings(require("dplyr", quietly = TRUE))) {
		stop("Package \"dplyr\" is required!")
	}
	trainvec <- gsub(removestr, "", trainvec)
	l1 <- strsplit(trainvec, split = sensplit)
	v1 <- gsub("\\s+", " ", gsub("[^\u4e00-\u9fa5]", " ", unlist(l1)))
	v1 <- gsub("^\\s+|\\s+$", "", v1)
	v1 <- v1[nzchar(v1)]
	
	sen1 <- strsplit(v1, split = " ")
	sen2 <- lapply(sen1, FUN = function(X) sapply(X, .transchar))
	outlist <- lapply(sen2, FUN = function(X) data.frame(word = unlist(strsplit(names(X), split = "")), status = unlist(strsplit(X, split = "")), stringsAsFactors = FALSE))
	reldf <- do.call("rbind", lapply(outlist, FUN = function(X) if(nrow(X) ==1) data.frame() else data.frame(src = X$status[1:(nrow(X) - 1)], tar = X$status[2:nrow(X)], stringsAsFactors = FALSE)))
	outdf <- do.call("rbind", outlist)
	
	data(GBK, package = "tmcn", envir = environment())
	outdf2 <- data.frame(word = c(setdiff(GBK$GBK, outdf$word), letters, LETTERS, 0:9, " "), status = "S", stringsAsFactors = FALSE)
	outdf <- rbind(outdf, outdf2)
	rownames(outdf) <- NULL
	
	adf <- summarise(group_by(reldf, src, tar), num = length(src))
	adf$prob <- adf$num / sum(adf$num)
	bdf <- summarise(group_by(outdf, word, status), num = length(word))
	bdf.B <- bdf[bdf$status == "B", ]
	bdf.B$prob <- bdf.B$num / sum(bdf.B$num)
	bdf.E <- bdf[bdf$status == "E", ]
	bdf.E$prob <- bdf.E$num / sum(bdf.E$num)
	bdf.M <- bdf[bdf$status == "M", ]
	bdf.M$prob <- bdf.M$num / sum(bdf.M$num)
	bdf.S <- bdf[bdf$status == "S", ]
	bdf.S$prob <- bdf.S$num / sum(bdf.S$num)
	
	S <- c("B", "E", "M", "S")
	O <- unique(outdf$word)
	
	A <- matrix(0 ,4, 4, dimnames = list(S, S))
	for (i in 1:nrow(adf)) {
		A[adf$src[i], adf$tar[i]] <- adf$prob[i]
	}
	
	B <- matrix(0, 4, length(O))
	rownames(B) <- S
	colnames(B) <- O
	for (i in 1:nrow(bdf.B)) {
		B["B", bdf.B$word[i]] <- bdf.B$prob[i]
	}
	for (i in 1:nrow(bdf.E)) {
		B["E", bdf.E$word[i]] <- bdf.E$prob[i]
	}
	for (i in 1:nrow(bdf.M)) {
		B["M", bdf.M$word[i]] <- bdf.M$prob[i]
	}
	for (i in 1:nrow(bdf.S)) {
		B["S", bdf.S$word[i]] <- bdf.S$prob[i]
	}
	
	hmmmodel = initHMM(S, O, transProbs = A, emissionProbs = B)
	if (!is.null(outputfolder)) {
		saveRDS(hmmmodel, file.path(outputfolder, "hmmmodel.rds"))
	}
	return(hmmmodel)
}

.transchar <- function(s) {
	if(nchar(s) <= 1) {
		OUT <- "S"
	} else {
		OUT <- paste0(c("B", rep("M", nchar(s) - 2), "E"), collapse = "")
	}
	return(OUT)
}

.decodechar <- function(sv, cv) {
	outv <- rep(0, length(cv))
	if (length(cv) == 1) return(sv)
	
	cv[grepl("[a-zA-Z0-9]", sv)] <- "N"
	for (i in 1:length(cv)) {
		if (cv[i] %in% c("B", "S", "N")) {
			if (i > 1 && cv[i] == "N" && cv[i - 1] == "N") next
			outv[i:length(outv)] <- outv[i:length(outv)] + 1
		}
	}
	OUT <- sapply(split(sv, f = outv), paste0, collapse = "")
	names(OUT) <- NULL
	OUT <- OUT[OUT != " "]
	return(OUT)
}

