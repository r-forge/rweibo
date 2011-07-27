


##' Reset a kind of unread messageof the authenticating user to 0. Kinds can be reset: 1. comment count, 2 metions count, 3 direct messages count, 4 followings count.
##' 
##' The argument "params" should be a list which cantains:
##' 
##' \tabular{ll}{ 
##' type \tab The kinds that is going to be reset\cr 
##' } 
##' 
##' @title Reset unread weibo count
##' @param roauth a OAuth object created by \code{\link{ROAuth}}
##' @param params parameters list
##' @param requestURL the request URL
##' @return list of result
##'  
##' @author lijian <\email{rweibo@@sina.com}>
##' @export
##' @references \url{http://open.weibo.com/wiki/index.php/Statuses/reset_count/en}
##' @keywords ROAuth
##' @examples \dontrun{
##' 
##' timeline.resetCount(roauth)
##' }
timeline.resetCount <- function(roauth, params=list(), requestURL = "http://api.t.sina.com.cn/statuses/reset_count.json") {
	returnthis <- .post(requestURL, roauth@appKey, roauth@appSecret,
			roauth@oauthKey, roauth@oauthSecret, params=params)
	return(fromJSON(returnthis))
}