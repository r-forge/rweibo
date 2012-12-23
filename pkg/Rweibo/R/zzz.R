# TODO: Add comment
# 
# Author: jli
###############################################################################

.onAttach <- function(libname, pkgname ){
	packageStartupMessage( paste("# Version:", packageDescription("Rweibo", fields = "Version")) )
	config <- readLines(file.path(system.file(package = "Rweibo"), "config", "Rweibo.txt"))
	port <- try(as.numeric(gsub("port:", "", config[grep("port", config)])), silent = TRUE)
	if (inherits(port, "try-error") || is.na(port)) {
		port <- 80
		packageStartupMessage(paste("Format of", 
						file.path(system.file(package = "Rweibo"), "config", "Rweibo.txt"),
						"is wrong!"))
	}
	options(help.ports = port)
	if (getOption("help.ports") == 80) {
		struri <- "http://127.0.0.1/library/Rweibo/doc/callback.html"
	} else {
		struri <- paste("http://127.0.0.1:", getOption("help.ports"), "/library/Rweibo/doc/callback.html", sep = "")
	}
	options(redirect_uri = struri)
	packageStartupMessage(paste("# The port of help server was set to", getOption("help.ports")))
	
	helpport <- try(tools:::startDynamicHelp(), silent = TRUE)
	if (inherits(helpport, "try-error")) {
		tools:::startDynamicHelp(start = FALSE)
		tools:::startDynamicHelp()
	}
	if (!file.exists(file.path(system.file(package = "Rweibo"), "doc", "callback.html"))) {
		file.copy(file.path(system.file(package = "Rweibo"), "config", "callback.html"), 
				file.path(system.file(package = "Rweibo"), "doc"))
	}
		
}