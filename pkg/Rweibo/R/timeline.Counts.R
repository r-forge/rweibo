


##' Return the comment count and repost count of a batch of weibos. It has a limitation of 100 weibos for each call.
##' 
##' The argument "params" should be a list which cantains:
##' 
##' \tabular{ll}{ 
##' ids \tab ID list of weibos seprareted by comma\cr 
##' } 
##' 
##' @title Return the comment counts and repost counts of a list of weibos
##' @param roauth a OAuth object created by \code{\link{ROAuth}}
##' @param params parameters list
##' @param requestURL the request URL
##' @return 
##'  A list of weibos, each weibo contains: 
##'  \item{id}{weibo ID} 
##'  \item{comments}{count of comments}
##'  \item{rt}{count of repost}
##'  
##' @author lijian <\email{rweibo@@sina.com}>
##' @export
##' @references \url{http://open.weibo.com/wiki/index.php/Statuses/counts/en}
##' @keywords ROAuth
##' @examples \dontrun{
##' 
##' timeline.Counts(roauth, list(ids = "14762313082,14762313083"))
##' }
timeline.Counts <- function(roauth, params=list(), requestURL = "http://api.t.sina.com.cn/statuses/counts.json") {
	returnthis <- .get(requestURL, roauth@appKey, roauth@appSecret,
			roauth@oauthKey, roauth@oauthSecret, params=params)
	return(fromJSON(returnthis))
}