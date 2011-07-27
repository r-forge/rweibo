


##' Retrieve the rate limitation of API calls. Return how many times API can be called in this hour. This limitation is based on the user type.
##' 
##' @title Retrieve the rate limitation of API calls
##' @param roauth a OAuth object created by \code{\link{ROAuth}}
##' @param requestURL the request URL
##' @return 
##'  A list of weibos, each weibo contains: 
##'  \item{remaining_hits}{remaining hits}
##'  \item{hourly_limit}{hourly limit} 
##'  \item{reset_time_in_seconds}{reset time in seconds}
##'  \item{reset_time}{reset time}
##'  
##' @author lijian <\email{rweibo@@sina.com}>
##' @export
##' @references \url{http://open.weibo.com/wiki/index.php/Account/rate_limit_status/en}
##' @keywords ROAuth
##' @examples \dontrun{
##' 
##' account.RateLimit(roauth)
##' }
account.RateLimit <- function(roauth, requestURL = "http://api.t.sina.com.cn/account/rate_limit_status.json") {
	returnthis <- .get(requestURL, roauth@appKey, roauth@appSecret, roauth@oauthKey, roauth@oauthSecret)
	return(fromJSON(returnthis))
}