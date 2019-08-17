
.onAttach <- function(libname, pkgname ){
	options(model.dir = system.file("models", package = "Rwordseg"))
	options(app.dir = .verifyFolder(file.path(Sys.getenv("APPDATA"), "Rwordseg")))
	options(RwordsegAnalyzer = "hmm")
	if (is.null(.getdboption("coreNLP.dir"))) {
		.setdboption("coreNLP.dir", paste0(system.file("extdata", package = "coreNLP"), "/stanford-corenlp-full-2015-12-09"))
	}
	if (!exists(".RwordsegEnv", envir = .GlobalEnv)) {
		envir0 = as.environment(1)
		assign(".RwordsegEnv", new.env(), envir = envir0)
	}
	.loadModels(getOption("RwordsegAnalyzer"))
	
	if (!file.exists(file.path(getOption("app.dir"), "user.dic"))) {
		try(write.table(data.frame(v1 = "R\u8BED\u8A00", v2 = 1, v3 = "n"), 
				file = file.path(getOption("app.dir"), "user.dic"), 
				sep = " ", row.names = FALSE, col.names = FALSE, quote = FALSE, fileEncoding = "UTF-8"), silent = TRUE)
	}
	if (!file.exists(file.path(getOption("app.dir"), "dicmeta"))) {
		dicmeta <- data.frame(id = "00000", dict = "builtin", time = as.character(Sys.time()),
				size = 1, example = "", desc = "", start = 1, end = 1, stringsAsFactors = FALSE)
		try(saveRDS(dicmeta, file.path(getOption("app.dir"), "dicmeta")), silent = TRUE)
	}
	
	packageStartupMessage("# \nThe defalut analyzer is 'hmm' implemented by native R codes, which is still in development.")
	packageStartupMessage("If you want to improve the performance you can choose: ")
	packageStartupMessage("  - \"jiebaR\", a popular segmentation module, by running \"setAnalyzer('jiebaR')\".")
	packageStartupMessage("  - \"coreNLP\", a R wrappers around Stanford CoreNLP, by running \"setAnalyzer('coreNLP')\".")
	packageStartupMessage("  - \"fmm\", the easiest way of using forward maximum matching algorithm, by running \"setAnalyzer('fmm')\".")
}

.onUnload <- function(libpath) {
	.RwordsegEnv <- .verifyRwordsegEnv()
	rm(.RwordsegEnv, envir = .GlobalEnv)
}
