##' Uninstall a user-defined dictionary.
##' 
##' @title Uninstall a dictionary.
##' @param dictid The ID of the dictionary, which is shown in the result of \code{\link{listDict}}.
##' @return No results.
##' @author Jian Li <\email{rweibo@@sina.com}>
##' 
uninstallDict <- function(dictid) {
	metadf0 <- readRDS(file.path(getOption("app.dir"), "dicmeta"))
	dicdf0 <- read.table(file.path(getOption("app.dir"), "user.dic"), sep = " ", fileEncoding = "UTF-8", stringsAsFactors = FALSE)
	metadf <- metadf0[metadf0$id != dictid, ]
	dicdf <- dicdf0[-(metadf0[metadf0$id == dictid, "start"]:metadf0[metadf0$id == dictid, "end"]), ]
	rownames(dicdf) <- NULL
	rownames(metadf) <- NULL
	write.table(dicdf, file = file.path(getOption("app.dir"), "user.dic"), 				
			append = FALSE, sep = " ", row.names = FALSE, col.names = FALSE, quote = FALSE, fileEncoding = "UTF-8")
	saveRDS(metadf, file.path(getOption("app.dir"), "dicmeta"))
	cat("The dictionary \"")
	cat(metadf0[metadf0$id == dictid, "dict"])
	cat("\" was uninstalled!\n")
	.loadModels(getOption("RwordsegAnalyzer"), renew = TRUE)
}



