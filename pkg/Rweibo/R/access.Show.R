


##' Return a single weibo and author information by ID.
##' 
##' The argument "params" should be a list which cantains:
##' 
##' \tabular{ll}{ 
##' id \tab The ID of weibo\cr 
##' } 
##' 
##' @title Return the single weibo's content by its ID
##' @param roauth a OAuth object created by \code{\link{ROAuth}}
##' @param params parameters list
##' @param requestURL the request URL
##' @return 
##'  A list of this weibos: 
##'  \item{created_at}{created time}
##'  \item{id}{weibo ID} 
##'  \item{text}{weibo text}
##'  \item{source}{weibo source}
##'  \item{favorited}{whether favorited}
##'  \item{truncated}{whether truncated}
##'  \item{in_reply_to_status_id}{weibo ID that reply to}
##'  \item{in_reply_to_user_id}{Replyer ID}
##'  \item{in_reply_to_screen_name}{Replayer nickname}
##'  \item{user}{list of user information}
##'  \item{retweeted_status}{list of reposted information, only available for a reposted weibo}
##'  \item{annotations}{annotations}
##'  
##' @author lijian <\email{rweibo@@sina.com}>
##' @export
##' @references \url{http://open.weibo.com/wiki/index.php/Statuses/show/en}
##' @keywords ROAuth
##' @examples \dontrun{
##' 
##' access.Show(roauth, list(id = 14762313082))
##' }
access.Show <- function(roauth, params=list(), requestURL = "http://api.t.sina.com.cn/statuses/show/:id.json") {
	returnthis <- roauth$OAuthRequest(requestURL, params = params, method="GET")
	return(fromJSON(returnthis))
}