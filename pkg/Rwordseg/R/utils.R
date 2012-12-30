
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

