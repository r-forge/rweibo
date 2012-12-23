setClass("OAuth2", representation(appKey = "character", appSecret = "character", 
				oauthToken = "character", oauthUser = "character", 
				oauthTime = "character", oauthLife = "character"))


##' Create an authorized OAuth object
##' 
##' @title create an OAuth object
##' @param app_name name of the application
##' @param access_name a string of your access name
##' @param forcelogin whether re-login the weibo
##' @return An S4 object of \code{\link{OAuth2}}.
##' @note There is only one OAuth object needed.
##' @author Jian Li <\email{rweibo@@sina.com}>
##' @seealso \code{\link{registerApp}}
##' @references \url{http://open.weibo.com/wiki/OAuth/en}
##' @keywords authorization
##' @examples \dontrun{
##' 
##' roauth <- createOAuth("sinademo", "user1")
##' }
createOAuth <- function(app_name, access_name, forcelogin = FALSE) {
	apppath <- system.file(package = "Rweibo", "oauth")
	if (app_name %in% list.files(apppath)) {
		applist <- fromJSON(file=file.path(apppath, app_name))
		
		isExpires <- TRUE
		try(
			isExpires <- as.numeric(difftime(Sys.time(), as.POSIXlt(applist$app_token[[access_name]]$token_time, 
								format = "%Y-%m-%d %H:%M:%S"), units = "secs")) > 
								as.numeric(applist$app_token[[access_name]]$expires_in), silent = TRUE
			)
		
		if (! access_name %in% names(applist$app_token) || forcelogin || isExpires) {
			.authorization(app_name, access_name, forcelogin = forcelogin)
		}
		applist <- fromJSON(file=file.path(apppath, app_name))
		oauthobj <- new("OAuth2", 
					appKey = applist$app_key, 
					appSecret = applist$app_secret, 
					oauthToken = applist$app_token[[access_name]]$token_key, 
					oauthUser = applist$app_token[[access_name]]$token_user,
					oauthTime = applist$app_token[[access_name]]$token_time,
					oauthLife = applist$app_token[[access_name]]$expires_in
			)
	} else {
		stop(paste(app_name, "doesn't exist, please use 'registerApp' to create"))
	}
	return(oauthobj)
}




.authorization <- function(app_name, access_name, authURL = "https://api.weibo.com/oauth2/authorize", 
		accessURL = "https://api.weibo.com/oauth2/access_token", forcelogin = FALSE) {
	apppath <- system.file(package = "Rweibo", "oauth")
	if (app_name %in% list.files(apppath)) {
		applist <- fromJSON(file=file.path(apppath, app_name))
		app_key <- applist$app_key
		app_secret <- applist$app_secret
		
		verifyURL <- paste(authURL, "?client_id=", app_key, "&response_type=code&redirect_uri=", 
				getOption("redirect_uri"), sep= "")
		if (forcelogin) verifyURL <- paste(verifyURL, "&forcelogin=true", sep = "")
		
		browseURL(verifyURL)
		msg <- paste("Please input the codes here\n",
				"CODE: ", sep='')
		verifierCode <- readline(prompt=msg)
		
		curl <- getCurlHandle()
		reader <- dynCurlReader(curl, baseURL = accessURL, verbose = FALSE)
		fields <- paste("client_id=", app_key, "&client_secret=", app_secret, 
				"&grant_type=authorization_code&redirect_uri=", getOption("redirect_uri"), 
				"&code=", verifierCode, sep = "")
		curlPerform(curl = curl, URL = accessURL, postfields = fields, writefunction = reader$update, ssl.verifypeer = FALSE)
		tokenList <- fromJSON(reader$value())
		
		oauthToken <- tokenList$access_token
		oauthUserID <- as.character(tokenList$uid)
		oauthExpires <- as.character(tokenList$expires_in)
		oauthTime <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
		
		
		if (access_name %in% names(applist$app_token)) {
			.modifyAccess(app_name, access_name, oauthToken, oauthUserID, oauthTime, oauthExpires)
		} else {
			.addAccess(app_name, access_name, oauthToken, oauthUserID, oauthTime, oauthExpires)
		}
	} else {
		stop(paste(app_name, "doesn't exist, please use '.registerApp' to create"))
	}
	return (TRUE)
}
