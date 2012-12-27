

.get <- function(strurl, oauthKey, params = character()) 
{
	params <- lapply(params, FUN = function(X) if (inherits(X, "name") || inherits(X, "call")) eval(X) else X)
	params["access_token"] <- oauthKey
	params <- params[order(names(params))]
	if (!is.null(params[["status"]])) params[["status"]] <- .cntoUTF8(curlPercentEncode(params[["status"]]))
	if (!is.null(params[["comment"]])) params[["comment"]] <- .cntoUTF8(curlPercentEncode(params[["comment"]]))
	if (!is.null(params[["screen_name"]])) params[["screen_name"]] <- .cntoUTF8(curlPercentEncode(params[["screen_name"]]))
	
	fields <- paste(names(params), sapply(params, curlPercentEncode), sep = "=", collapse = "&") 
	strurl <- paste(strurl, fields, sep ="?")

	OUT <- getURL(strurl, ssl.verifypeer = FALSE)
	OUT <- fromJSON(OUT)
	
	return(OUT)
}

.post <- function(strurl, oauthKey, params = character(), curl = getCurlHandle()) 
{
	params <- lapply(params, FUN = function(X) if (inherits(X, "name") || inherits(X, "call")) eval(X) else X)
	
	if(is.null(curl)) curl <- getCurlHandle()
	
	reader <- dynCurlReader(curl, baseURL = strurl, verbose = FALSE)
	
	params <- lapply(params, as.character)
	params[["access_token"]] <- oauthKey
	params <- params[order(names(params))]
	if (!is.null(params[["status"]])) params[["status"]] <- .cntoUTF8(curlPercentEncode(params[["status"]]))
	if (!is.null(params[["comment"]])) params[["comment"]] <- .cntoUTF8(curlPercentEncode(params[["comment"]]))
	if (!is.null(params[["screen_name"]])) params[["screen_name"]] <- .cntoUTF8(curlPercentEncode(params[["screen_name"]]))
	
	fields <- paste(names(params), unlist(params), sep = "=", collapse = "&")  
	
	curlPerform(curl = curl, URL = strurl, postfields = fields, writefunction = reader$update, ssl.verifypeer = FALSE)
	OUT <- fromJSON(reader$value())
	
	return(OUT)
}

.cntoUTF8 <- function(strcn) {
	OUT <- paste(strcn, "Rweibo", sep = "")
	OUT <- gsub("Rweibo$", "", OUT)
	return(OUT)
}

.setCallback <- function() {
	config <- readLines(file.path(system.file(package = "Rweibo"), "config", "Rweibo.txt"))
	port <- try(as.numeric(gsub("port:", "", config[grep("port", config)])), silent = TRUE)
	if (inherits(port, "try-error") || is.na(port)) {
		port <- 80
		cat(paste("Format of", 
						file.path(system.file(package = "Rweibo"), "config", "Rweibo.txt"),
						"is wrong!\n"))
	}
	if (port == 80) {
		struri <- "http://127.0.0.1/library/Rweibo/doc/callback.html"
	} else {
		struri <- paste("http://127.0.0.1:", port, "/library/Rweibo/doc/callback.html", sep = "")
	}
	options(redirect_uri = struri)
	cat(paste("# The port of help server was set to", port, "\n"))
	
	.setHttpPort(port)
		
	if (!file.exists(file.path(system.file(package = "Rweibo"), "doc", "callback.html"))) {
		file.copy(file.path(system.file(package = "Rweibo"), "config", "callback.html"), 
				file.path(system.file(package = "Rweibo"), "doc"))
	}

}

.setHttpPort <- function(port) {
	port <- try(as.integer(port), silent = TRUE)
	if (is.na(port)) stop("Not integer!")
	options(help.ports = port)
	try(startDynamicHelp(start = FALSE), silent = TRUE)
	try(startDynamicHelp(), silent = TRUE)
}


