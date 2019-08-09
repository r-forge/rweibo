##' List all of the installed user-defined dictionaries.
##' 
##' @title List the installed dictionaries.
##' @return 
##'  A data frame of: 
##'  \item{id}{ID of the dictionary.}
##'  \item{dict}{Name of the dictionary.}
##'  \item{time}{Created time.}
##'  \item{size}{Word counts of the dictionary.}
##'  \item{example}{Example words.}
##'  \item{desc}{Description of the dictionary.}
##' @author Jian Li <\email{rweibo@@sina.com}>
##' @examples
##' listDict()
##'
listDict <- function() {
	Metafile <- file.path(getOption("app.dir"), "dicmeta")
	if (file.exists(Metafile)) {
		OUT <- readRDS(Metafile)
		OUT <- OUT[OUT$id != "00000", c("id", "dict", "time", "size", "example", "desc")]
		rownames(OUT) <- NULL
	} else {
		OUT <- data.frame(id = character(0), dict = character(0), time = character(0), size = numeric(0), example = character(0), desc = character(0), stringsAsFactors = FALSE)
	}
	return(OUT)
}



