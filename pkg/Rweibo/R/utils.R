

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






