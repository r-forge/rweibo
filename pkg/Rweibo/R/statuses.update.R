


##' Post a weibo as well as repost a weibo.
##' 
##' 
##' @title Post a new weibo
##' @param joauth a java object created by "ROAuth"
##' @param TEXT the content you want to post
##' @param requestURL the request URL
##' @return a feedback list
##' @author lijian <\email{lijian.pku@@gmail.com}>
##' @export
##' @references \url{http://open.weibo.com/wiki/index.php/Statuses/update/en}
##' @keywords ROAuth
##' @examples \dontrun{
##' 
##' statuses.update(roauth, "hello world!")
##' }
statuses.update <- function(joauth, TEXT, requestURL = "http://api.t.sina.com.cn/statuses/update.json") {
	returnthis <- .jcall(joauth, "S", "postUpdate", requestURL, TEXT)
	Encoding(returnthis) <- "UTF-8"
	return(fromJSON(returnthis))
}