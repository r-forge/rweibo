


##' Return the latest N weibos metioned the authenticating user.
##' 
##' 
##' @title Return the authenticating user's mentions
##' @param roauth a OAuth object created by \code{\link{ROAuth}}
##' @param count the the returned request count
##' @param requestURL the request URL
##' @return a result list
##' @author lijian <\email{lijian.pku@@gmail.com}>
##' @export
##' @references \url{http://open.weibo.com/wiki/index.php/Statuses/mentions/en}
##' @keywords ROAuth
##' @examples \dontrun{
##' 
##' getMentions(roauth, 5)
##' }
getMentions <- function(joauth, params=list(), requestURL = "http://api.t.sina.com.cn/statuses/mentions.json") {
	#requestURL <- paste(requestURL, "?count=", count, sep="")
	#returnthis <- .jcall(joauth, "S", "getRequest", requestURL)
	returnthis <- roauth$OAuthRequest(requestURL, params = params, method="GET")
	#Encoding(returnthis) <- "UTF-8"
	return(fromJSON(returnthis))
}
