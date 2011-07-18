


##' Return 20 latest public weibo. The Respons is not strictly realtime, and it is cached for 60 seconds.
##' 
##' 
##' @title Return the latest public weibos
##' @param joauth a java object created by "ROAuth"
##' @param count the the returned request count
##' @param requestURL the request URL
##' @return a result list
##' @author lijian <\email{lijian.pku@@gmail.com}>
##' @export
##' @references \url{http://open.weibo.com/wiki/index.php/Statuses/public_timeline/en}
##' @keywords ROAuth
##' @examples \dontrun{
##' 
##' getPublicTimeline(roauth, 5)
##' }
getPublicTimeline <- function(joauth, params=list(), requestURL = "http://api.t.sina.com.cn/statuses/public_timeline.json") {
	#requestURL <- paste(requestURL, "?count=", count, sep="")
	#returnthis <- .jcall(joauth, "S", "getRequest", requestURL)
	returnthis <- roauth$OAuthRequest(requestURL, params = params, method="GET")
	#Encoding(returnthis) <- "UTF-8"
	return(fromJSON(returnthis))
}