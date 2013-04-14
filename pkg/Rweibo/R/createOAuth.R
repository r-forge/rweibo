

##' Create an authorized OAuth object
##' 
##' @title create an OAuth object
##' @param app_name name of the application.
##' @param access_name a string of your access name.
##' @param authorize whether to authorize the oauth to use sina API.
##' @param login whether to login to impersonate the login.
##' @param username user name of the account to impersonate the login.
##' @param password password of the account to impersonate the login.
##' @return An reference object of \code{\link{weibo2.0}}.
##' @note There is only one OAuth object needed.
##' @author Jian Li <\email{rweibo@@sina.com}>
##' @seealso \code{\link{registerApp}}
##' @references \url{http://open.weibo.com/wiki/OAuth/en}
##' @keywords authorization
##' @examples \dontrun{
##' 
##' roauth <- createOAuth("sinademo", "user1")
##' }
createOAuth <- function(app_name, access_name, authorize = TRUE, 
		login = FALSE, username = "", password = "") {
	oauthobj <- new("weibo2.0", appName = app_name, oauthName = access_name)
	if (authorize) {
		if (oauthobj$expiresIn() < 0) {
			oauthobj$authorize(forcelogin = FALSE)
			oauthobj$save()
		}
	}
	if(login) {
		oauthobj$login(username, password) 
	}
	
	limitDf <- try(oauthobj$getLimits(TRUE), silent = TRUE)
	if (is.data.frame(limitDf)) {
		oauthobj$oauthMsg <- "oauth was authorized! (expires in HOURS hours)"
		oauthobj$oauthLimits <- limitDf
		oauthobj$oauthResetTime <- .hourtime(1)
	} else {
		warning("oauth test failed, please check the connection or your application.", call. = FALSE)
		oauthobj$oauthLife <- "-1"
	}
	
	testweburl <- "http://weibo.com"
	testwebcon <- getURL(testweburl, curl = oauthobj$webCurl, .encoding = "UTF-8")
	#testwebcon <- iconv(testwebcon, "GBK", "UTF-8")
	loginRetcode <- sapply(strsplit(.strextract(testwebcon, "retcode=[0-9]+")[[1]], split = "="), 
			FUN = function(X) as.numeric(X[2]))
	if (length(loginRetcode) == 0 || identical(loginRetcode[1], 0)) {
		configlist <- strsplit(.strextract(testwebcon, "\\$CONFIG\\[[^;]*;")[[1]], split = "=")
		configname <- .strtrim(gsub("[\\$CONFIG\\[']|['\\]]", "", sapply(configlist, FUN = function(X) X[1])))
		configvalue <- .strtrim(gsub("[.*']|['.*]|[;]", "", sapply(configlist, FUN = function(X) X[2])))
		if (configvalue[which(configname == "islogin")] == "1") {
			oauthobj$webName = configvalue[which(configname == "nick")]
			oauthobj$webUser = configvalue[which(configname == "uid")]
			oauthobj$webMsg <- "cookies were saved! (COOKIE.cookies)"
		} else {
			warning("cookies test failed (not login), please check the connection or your setting.", call. = FALSE)
		}
	} else {
		warning(paste("cookies test failed (", loginRetcode[1], 
						"), please check the connection or your setting.", sep = ""), call. = FALSE)
	}
	
	
	return(oauthobj)
}



