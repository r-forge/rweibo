
.vectrim <- function(v, side = c("both", "left", "right"), trim = NA) {
	side <- match.arg(side)
	r1 <- rle(v)
	N <- length(r1$values)
	OUT <- v
	if (side == "both" || side == "left") {
		if (identical(r1$values[1], trim)) {
			OUT <- OUT[-(1:r1$lengths[1])]
		} 
	}
	if (side == "both" || side == "right") {
		if (identical(r1$values[N], trim)) {
			OUT <- OUT[1:(length(OUT) - r1$lengths[N])]
		} 
	}
	return(OUT)
}

.raw2int <- function(rawv) {
	if (length(rawv) %% 2 != 0) stop("length error!")
	l1 <- split(rawv, f = rep(1:(length(rawv)/2), each = 2))
	OUT <- sapply(l1, FUN = function(X) as.integer(X[2]) * 256 + as.integer(X[1]))
	return(as.numeric(OUT))
}

.toSim <- function(string)
{
	transDf <- get("data.trad", envir = .RwordsegEnv)
	OUT <- chartr(transDf$Tra, transDf$Sim, string)
	return(OUT)
}

.verifyFolder <- function(...) {
	folderpath <- file.path(...)
	if (!file.exists(folderpath)) {
		iscreated <- try(dir.create( folderpath, recursive = TRUE), silent = TRUE)
		if (inherits(iscreated, "try-error")) stop("Can't create new folder!")
	}
	return(folderpath)
}

.loadModels <- function(analyzer = c("all", "jiebaR", "hmm", "fmm"), renew = FALSE) {
	analyzer <- match.arg(analyzer)
	if (!exists(".RwordsegEnv", envir = .GlobalEnv)) {
		assign(".RwordsegEnv", new.env(), envir = .GlobalEnv)
	}
	if (analyzer == "all" || analyzer == "hmm") {
		if (renew || !exists("hmmAnalyzer", envir = .RwordsegEnv)) {
			hmmAnalyzer <- readRDS(system.file("dict", "hmmmodel.rds", package = "Rwordseg"))
			assign("hmmAnalyzer", hmmAnalyzer, envir = .RwordsegEnv)
			cat("HMM model has been loaded.\n")
		}
	}
	if (analyzer == "all" || analyzer == "jiebaR") {
		if (suppressWarnings(requireNamespace("jiebaR", quietly = TRUE))) {
			if (renew || !exists("jiebaAnalyzer", envir = .RwordsegEnv)) {
				jiebaAnalyzer <- jiebaR::worker(bylines = TRUE, user = file.path(getOption("app.dir"), "user.dic"))
				assign("jiebaAnalyzer", jiebaAnalyzer, envir = .RwordsegEnv)
				cat("\"jiebaR\" has been loaded.\n")
			} 
		} else {
			cat("\"jiebaR\" has not been installed.\n")
		}
	}
	if (analyzer == "all" || analyzer == "fmm") {
		if (renew || !exists("fmmAnalyzer", envir = .RwordsegEnv)) {
			dic1 <- read.table(system.file("dict", "default.dic", package = "Rwordseg"), sep = " ", fileEncoding = "UTF-8", stringsAsFactors = FALSE)
			dic2 <- read.table(file.path(getOption("app.dir"), "user.dic"), sep = " ", fileEncoding = "UTF-8", stringsAsFactors = FALSE)
			dic2 <- dic2[!dic2$V1 %in% dic1$V1, ]
			dic0 <- rbind(dic1, dic2)
			dic0 <- dic0[nchar(dic0$V1) <= 8, ]
			assign("fmmAnalyzer", tmcn:::.createHashmapEnv(dic0$V1, dic0$V3), envir = .RwordsegEnv)
			cat("FMM model has been loaded.\n")
		}
	}
}

.getNature <- function(vec) {
	OUT <- rep("x", length(vec))
	OUT[grepl("[\u4e00-\u9fa5]", vec)] <- sapply(vec[grepl("[\u4e00-\u9fa5]", vec)], FUN = function(X) as.character(try(get(X, envir = get("fmmAnalyzer", envir = .RwordsegEnv)), silent = TRUE)))
	OUT[grepl("^Error", OUT)] <- "x"
	return(OUT)
}

