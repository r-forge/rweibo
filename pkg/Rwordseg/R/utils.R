
.detectEncoding <- function(strpaths) {
	pathverify <- try(file.exists(strpaths), silent = TRUE)
	if (inherits(pathverify, "try-error")) stop("Please input the path string of the dic file!")
	if (!any(pathverify)) stop ("Wrong path of the dic file!")
	strpath <- strpaths[pathverify][1]
	analyzer = get("Analyzer", envir = .RwordsegEnv)
	tmp <- try(.jcall(analyzer, "S", "detectEncoding", strpath), silent = TRUE)
	if (inherits(tmp, "try-error")) {
		stop(paste("Fail to detect encoding:\n", as.character(tmp), "\n"))
	} else {
		tmp <- gsub(" *", "", tmp)
	}
	iconv.list <- iconvlist()
	OUT <- iconv.list[grep(toupper(tmp), toupper(iconv.list))]
	return(OUT)
}

.addDictMeta <- function(Name, Type = "", Des = "") {
	Metafile <- file.path(getOption("app.dir"), "dicmeta")
	if (file.exists(Metafile)) {
		oriDf <- readRDS(Metafile)
	} else {
		oriDf <- data.frame(Name = character(0), Type = character(0), Des = character(0),  stringsAsFactors = FALSE)
	}	
	newDf <- data.frame(Name = Name, Type = Type, Des = Des,  stringsAsFactors = FALSE)
	if (Name %in% oriDf$Name) {
		warning(paste("'", Name, "' was installed!"))
	} else {
		outDf <- rbind(oriDf, newDf)
		saveRDS(outDf, Metafile)
	}
}

.removeDictMeta <- function(Names) {
	Metafile <- file.path(getOption("app.dir"), "dicmeta")
	oriDf <- readRDS(Metafile)
	if (!any(Names %in% oriDf$Name)) {
		warning(paste("There is no '", Names, "' installed!"))
	} else {
		outDf <- oriDf[-which(oriDf$Name %in% Names), ]
		saveRDS(outDf, Metafile)
	}
}

.setNameReco <- function(isReco = TRUE) {
	analyzer = get("Analyzer", envir = .RwordsegEnv)
	tmp <- try(.jcall(analyzer, "V", "setNameRecognition", isReco), silent = TRUE)
}

.setNumReco <- function(isReco = TRUE) {
	analyzer = get("Analyzer", envir = .RwordsegEnv)
	tmp <- try(.jcall(analyzer, "V", "setNumRecognition", isReco), silent = TRUE)
}

.setQuantifierReco <- function(isReco = TRUE) {
	analyzer = get("Analyzer", envir = .RwordsegEnv)
	tmp <- try(.jcall(analyzer, "V", "setQuantifierRecognition", isReco), silent = TRUE)
}

.toTrad <- function(string)
{
	transDf <- get("data.trad", envir = .RwordsegEnv)
	OUT <- chartr(transDf$Tra, transDf$Sim, string)
	return(OUT)
}

.segWord <- function(strwords, analyzer = get("Analyzer", envir = .RwordsegEnv)) {
	OUT <- .jcall(analyzer, "S", "segWord", strwords)
	Encoding(OUT) <- "UTF-8"
	return(OUT)
}

.segWordInd <- function(strwords, analyzer = get("Analyzer", envir = .RwordsegEnv)) {
	OUT <- .jcall(analyzer, "S", "segWordInd", strwords)
	return(OUT)
}

.segWord <- function(strwords, analyzer = get("Analyzer", envir = .RwordsegEnv)) {
	OUT <- .jcall(analyzer, "S", "segWord", strwords)
	Encoding(OUT) <- "UTF-8"
	return(OUT)
}



