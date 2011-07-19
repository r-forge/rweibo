


##' Post a weibo as well as repost a weibo.
##' 
##' 
##' @title Post a new weibo
##' @param roauth a OAuth object created by \code{\link{ROAuth}}
##' @param TEXT the content you want to post
##' @param requestURL the request URL
##' @return a feedback list
##' @author lijian <\email{rweibo@@sina.com}>
##' @export
##' @references \url{http://open.weibo.com/wiki/index.php/Statuses/update/en}
##' @keywords ROAuth
##' @examples \dontrun{
##' 
##' statuses.update(roauth, "hello world!")
##' }
statuses.update <- function(joauth, params=list(), requestURL = "http://api.t.sina.com.cn/statuses/update.json") {
	#returnthis <- .jcall(joauth, "S", "postUpdate", requestURL, TEXT)
	returnthis <- roauth$OAuthRequest(requestURL, params = params, method="POST")
	#Encoding(returnthis) <- "UTF-8"
	return(fromJSON(returnthis))
}