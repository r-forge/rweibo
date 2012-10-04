# TODO: Add comment
# 
# Author: jli
###############################################################################

.onAttach <- function(libname, pkgname ){
	packageStartupMessage( paste("# Version:", packageDescription("Rweibo", fields = "Version")) )
	config <- readLines(file.path(system.file(package = "Rweibo"), "config", "Rweibo.txt"))
	port <- try(as.numeric(gsub("port:", "", config[grep("port", config)])), silent = TRUE)
	if (inherits(port, "try-error") || is.na(port)) {
		port <- 12345
		packageStartupMessage(paste("Format of", 
						file.path(system.file(package = "Rweibo"), "config", "Rweibo.txt"),
						"is wrong!"))
	}
	options(help.ports = port)
	options(redirect_uri = paste("http://127.0.0.1:", getOption("help.ports"), 
					"/library/Rweibo/demo/callback.html", sep = ""))
	packageStartupMessage(paste("# The port of help server was set to", getOption("help.ports")))
	
	helpport <- try(tools:::startDynamicHelp(), silent = TRUE)
	if (inherits(helpport, "try-error")) {
		tools:::startDynamicHelp(start = FALSE)
		tools:::startDynamicHelp()
	}
	if (!file.exists(file.path(system.file(package = "Rweibo"), "demo", "callback.html"))) {
		file.copy(file.path(system.file(package = "Rweibo"), "config", "callback.html"), 
				file.path(system.file(package = "Rweibo"), "demo"))
	}
		
}