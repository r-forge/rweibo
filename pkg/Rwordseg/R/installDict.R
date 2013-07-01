

##' Install new dictionary.
##' 
##' @title Install new dictionary.
##' @param dictpath Path of dictionary.
##' @param dictname English name of the dictionary.
##' @param dicttype Type of the dictionary.
##' @return No results.
##' @author Jian Li <\email{rweibo@@sina.com}>

installDict <- function(dictpath, dictname = "userDefine", dicttype = c("text", "scel")) {
	pathverify <- try(file.exists(dictpath), silent = TRUE)
	if (inherits(pathverify, "try-error")) stop("Please input the path string of the dic file!")
	if (!any(pathverify)) stop ("Wrong path of the dic file!")
	dictpath <- dictpath[pathverify][1]
	
	dic.type <- match.arg(dicttype)
	dic.enc <- .detectEncoding(dictpath)
	dic.suffix <- tolower(gsub("^.*\\.", "", dictpath))
	if (dic.type == "text" && dic.suffix %in% dicttype) dic.type <- dic.suffix
	if (dictname == "n") dictname <- "userDefine"
	
	ori.dic <- readLines(file.path(getOption("app.dir"), "userdic"))
	Encoding(ori.dic) <- "UTF-8"
	ori.dic <- ori.dic[nzchar(ori.dic)]
	oriwords <- sapply(strsplit(ori.dic, "\t"), FUN = function(X) X[1])
	oriwords <- tolower(unique(oriwords[!is.na(oriwords)]))
	
	switch(dic.type, 
			text = {
				tmp.dic <- readLines(dictpath)
				if (dic.enc != "UTF-8") dic.enc <- "GBK"
				if (getOption("encoding") != "UTF-8") tmp.dic <- iconv(tmp.dic, dic.enc, "UTF-8")
				newwords <- sapply(strsplit(tmp.dic, "\t| "), FUN = function(X) X[1])
				newwords <- unique(newwords[!is.na(newwords)])
				newwords <- tolower(gsub(" ", "", newwords))
				.addDictMeta(dictname, "", basename(dictpath))
			},
			scel = {
				sogouV <- importSogouScel(dictpath)
				newwords <- tolower(as.vector(sogouV))
				.addDictMeta(dictname, attributes(sogouV)$Type, attributes(sogouV)$Description)
			}
	)
	
	addwords <- newwords[! newwords %in% oriwords]
	outwords <- c(ori.dic, paste(addwords, dictname, 1000, sep = "\t"))
	Encoding(outwords) <- "GBK"
	writeLines(outwords, file.path(getOption("app.dir"), "userdic"))
	cat("OK!\nNew dictionary was installed, please restart R to use it.\n")
}




