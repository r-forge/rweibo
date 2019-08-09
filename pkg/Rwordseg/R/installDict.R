##' Install a new dictionary from a Sogou scel file or text file. Make sure the file encoding is in UTF-8.
##' 
##' @title Install a new dictionary.
##' @param dictpath Path of dictionary.
##' @param dictname Name of the dictionary. Sogou scel file don't need this input.
##' @param dictdesc Description of the dictionary. Default is empty string.
##' @return No results.
##' @author Jian Li <\email{rweibo@@sina.com}>
##' 
installDict <- function(dictpath, dictname = "", dictdesc = "") {
	pathverify <- try(file.exists(dictpath), silent = TRUE)
	if (inherits(pathverify, "try-error")) stop("Please input the path string of the dic file!")
	if (!any(pathverify)) stop ("Wrong path of the dic file!")
	dictpath <- dictpath[pathverify][1]
	
	if (grepl("\\.scel$", dictpath, ignore.case = TRUE)) {
		diclist <- importSogouScel(dictpath)
		metadf0 <- readRDS(file.path(getOption("app.dir"), "dicmeta"))
		dicdf0 <- read.table(file.path(getOption("app.dir"), "user.dic"), sep = " ", fileEncoding = "UTF-8", stringsAsFactors = FALSE)
		dicdf1 <- data.frame(V1 = diclist$dict$word, V2 = diclist$dict$freq, V3 = "x", stringsAsFactors = FALSE)
		dicdf1 <- dicdf1[!dicdf1$V1 %in% dicdf0$V1, ]
		if (nrow(dicdf1) > 0) {
			dicdf <- rbind(dicdf0, dicdf1)
			metadf <- rbind(metadf0, data.frame(id = strpad(max(as.numeric(metadf0$id)) + 1, width = 5, pad = "0"), 
							dict = diclist$desc$dict, time = as.character(Sys.time()), size = nrow(dicdf1), 
							example = diclist$desc$example, desc = diclist$desc$desc, 
							start = nrow(dicdf0) + 1, end = nrow(dicdf0) + nrow(dicdf1), 
							stringsAsFactors = FALSE))
			rownames(dicdf) <- NULL
			rownames(metadf) <- NULL
			write.table(dicdf, file = file.path(getOption("app.dir"), "user.dic"), 				
					append = FALSE, sep = " ", row.names = FALSE, col.names = FALSE, quote = FALSE, fileEncoding = "UTF-8")
			saveRDS(metadf, file.path(getOption("app.dir"), "dicmeta"))
			cat(nrow(dicdf1))
			cat(" words were loaded ... \n")
			cat("New dictionary \"")
			cat(diclist$desc$dict)
			cat("\" was installed!\n")
			.loadModels(getOption("RwordsegAnalyzer"), renew = TRUE)
		}
	} else {
		diclist <- read.table(dictpath, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
		dicdf1 <- data.frame(V1 = diclist[[1]], stringsAsFactors = FALSE)
		tmp.freq <- NA
		tmp.nature <- NA
		if (ncol(diclist) >= 2) {
			if (is.numeric(diclist[[2]])) {
				tmp.freq <- diclist[[2]]
			} else {
				tmp.nature <- diclist[[2]]
			}
		}
		if (ncol(diclist) >= 3) {
			if (is.na(tmp.freq[1])) {
				tmp.freq <- diclist[[3]]
			} else {
				tmp.nature <- diclist[[3]]
			}
		}
		dicdf1$V2 <- tmp.freq
		dicdf1$V3 <- tmp.nature
		dicdf1$V2[is.na(dicdf1$V2)] <- 1
		dicdf1$V3[is.na(dicdf1$V3)] <- "x"
		
		metadf0 <- readRDS(file.path(getOption("app.dir"), "dicmeta"))
		dicdf0 <- read.table(file.path(getOption("app.dir"), "user.dic"), sep = " ", fileEncoding = "UTF-8", stringsAsFactors = FALSE)
		dicdf1 <- dicdf1[!dicdf1$V1 %in% dicdf0$V1, ]
		if (nrow(dicdf1) > 0) {
			dicdf <- rbind(dicdf0, dicdf1)
			if (!nzchar(dictname)) dictname <- format(Sys.time(), format = "%Y%m%d%H%M%S") 
			metadf <- rbind(metadf0, data.frame(id = strpad(max(as.numeric(metadf0$id)) + 1, width = 5, pad = "0"), 
							dict = dictname, time = as.character(Sys.time()), size = nrow(dicdf1), 
							example = dicdf1$V1[1], desc = dictdesc, 
							start = nrow(dicdf0) + 1, end = nrow(dicdf0) + nrow(dicdf1), 
							stringsAsFactors = FALSE))
			rownames(dicdf) <- NULL
			rownames(metadf) <- NULL
			write.table(dicdf, file = file.path(getOption("app.dir"), "user.dic"), 				
					append = FALSE, sep = " ", row.names = FALSE, col.names = FALSE, quote = FALSE, fileEncoding = "UTF-8")
			saveRDS(metadf, file.path(getOption("app.dir"), "dicmeta"))
			cat(nrow(dicdf1))
			cat(" words were loaded ... \n")
			cat("New dictionary \"")
			cat(dictname)
			cat("\" was installed!\n")
			.loadModels(getOption("RwordsegAnalyzer"), renew = TRUE)
		}
	}
	
}




