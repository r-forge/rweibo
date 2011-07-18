


##' Return latest weibo of the authenticating user and his followings. It's the same with the contents of the "Home" page when user authenticated.
##' 
##' 
##' @title Return the authenticating user's and his friends' latest weibos
##' @param roauth a OAuth object created by "ROAuth"
##' @param count the the returned request count
##' @param requestURL the request URL
##' @return a result list
##' @author lijian <\email{lijian.pku@@gmail.com}>
##' @export
##' @references \url{http://open.weibo.com/wiki/index.php/Statuses/friends_timeline/en}
##' @keywords ROAuth
##' @examples \dontrun{
##' 
##' getFriendsTimeline(roauth, 5)
##' }
getFriendsTimeline <- function(roauth, params=list(), requestURL = "http://api.t.sina.com.cn/statuses/friends_timeline.json") {
	#requestURL <- paste(requestURL, "?count=", count, sep="")
	#returnthis <- .jcall(joauth, "S", "getRequest", requestURL)
	returnthis <- roauth$OAuthRequest(requestURL, params = params, method="GET")
	#Encoding(returnthis) <- "UTF-8"
	return(fromJSON(returnthis))
}