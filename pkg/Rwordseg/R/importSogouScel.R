


##' Import Sogou .scel dictionary.
##' 
##' @title Import Sogou .scel dictionary.
##' @param strpaths Path of dictionary.
##' @return A string vector of words with attributes "Type" and "Description".
##' @author Jian Li <\email{rweibo@@sina.com}>

importSogouScel <- function(strpaths) {
	pathverify <- try(file.exists(strpaths), silent = TRUE)
	if (inherits(pathverify, "try-error")) stop("Please input the path string of the Scel file!")
	if (!any(pathverify)) stop ("Wrong path of the Scel file!")
	strpath <- strpaths[pathverify][1]
	analyzer = get("Analyzer", envir = .RwordsegEnv)
	tmp <- try(.jcall(analyzer, "S", "importSogou", strpath), silent = TRUE)
	if (inherits(tmp, "try-error")) {
		stop(paste("Fail to import", basename(strpath), ":\n", as.character(tmp), "\n"))
	} else {
		Encoding(tmp) <- "UTF-8"
		out.type <- sub("Type: *", "", sub("Des:.*$", "", tmp))
		out.des <- sub("^.*?Des: +", "", sub("Dict:.*$", "", tmp))
		out.dict <- sub(paste("^.*?Des: *", out.des, "Dict: +", sep = ""), "", tmp)
	}
	OUT <- strsplit(out.dict, split = " ")[[1]]
	attr(OUT, "Type") <- out.type
	attr(OUT, "Description") <- out.des
	return(OUT)
	
}






