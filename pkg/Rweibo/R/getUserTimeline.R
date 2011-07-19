


##' Return the authenticating user's latest weibos.
##' 
##' 
##' @title Return the user's latest timeline
##' @param joauth a java object created by "ROAuth"
##' @param count the the returned request count
##' @param requestURL the request URL
##' @return a result list
##' @author lijian <\email{lijian.pku@@gmail.com}>
##' @export
##' @references \url{http://open.weibo.com/wiki/index.php/Statuses/user_timeline/en}
##' @keywords ROAuth
##' @examples \dontrun{
##' 
##' getUserTimeline(roauth, 5)
##' }
getUserTimeline <- function(joauth, count = 20, requestURL = "http://api.t.sina.com.cn/statuses/user_timeline.json") {
	requestURL <- paste(requestURL, "?count=", count, sep="")
	returnthis <- .jcall(joauth, "S", "getRequest", requestURL)
	Encoding(returnthis) <- "UTF-8"
	return(fromJSON(returnthis))
}