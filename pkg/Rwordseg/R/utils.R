
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



