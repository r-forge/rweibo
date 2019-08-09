
.onAttach <- function(libname, pkgname ){
	options(dic.dir = system.file("dict", package = "Rwordseg"))
	options(app.dir = .verifyFolder(file.path(Sys.getenv("APPDATA"), "Rwordseg")))
	options(RwordsegAnalyzer = "hmm")
	
	assign(".RwordsegEnv", new.env(), envir = .GlobalEnv)
	.loadModels(getOption("RwordsegAnalyzer"))
	
	if (!file.exists(file.path(getOption("app.dir"), "user.dic"))) {
		write.table(data.frame(v1 = "R\u8BED\u8A00", v2 = 1, v3 = "n"), 
				file = file.path(getOption("app.dir"), "user.dic"), 
				sep = " ", row.names = FALSE, col.names = FALSE, quote = FALSE, fileEncoding = "UTF-8")
	}
	if (!file.exists(file.path(getOption("app.dir"), "dicmeta"))) {
		dicmeta <- data.frame(id = "00000", dict = "builtin", time = as.character(Sys.time()),
				size = 1, example = "", desc = "", start = 1, end = 1, stringsAsFactors = FALSE)
		saveRDS(dicmeta, file.path(getOption("app.dir"), "dicmeta"))
	}
	
	cat("# \nThe defalut analyzer is 'hmm' implemented by native R codes, which is still in development.\n")
	cat("If you want to improve the performance you can choose: \n")
	cat("  - \"jiebaR\", a popular segmentation module, by running \"setAnalyzer('jiebaR')\".\n")
	cat("  - \"fmm\", the easiest way of using forward maximum matching algorithm, by running \"setAnalyzer('fmm')\".\n")
}

.onUnload <- function(libpath) {
	rm(.RwordsegEnv, envir = .GlobalEnv)
}
