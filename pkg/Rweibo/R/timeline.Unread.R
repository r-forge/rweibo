


##' Return the total number of all kinds of unread message of the authenticating user.
##' 
##' The argument "params" should be a list which cantains:
##' 
##' \tabular{ll}{ 
##' with_new_status \tab Whether there's new status, 1 mean there is and 0 means there is not\cr 
##' since_id \tab Returns the weibos with an ID greater than the specified ID\cr 
##' } 
##' 
##' @title Return the authenticating user's unread weibo count
##' @param roauth a OAuth object created by \code{\link{ROAuth}}
##' @param params parameters list
##' @param requestURL the request URL
##' @return 
##'  A list of weibos, each weibo contains: 
##'  \item{followers}{number of followers} 
##'  \item{dm}{number of direct messages}
##'  \item{mentions}{number of the latest weibo metioned me}
##'  \item{comments}{new comments count }
##'  
##' @author lijian <\email{rweibo@@sina.com}>
##' @export
##' @references \url{http://open.weibo.com/wiki/index.php/Statuses/unread/en}
##' @keywords ROAuth
##' @examples \dontrun{
##' 
##' timeline.Unread(roauth)
##' }
timeline.Unread <- function(roauth, params=list(), requestURL = "http://api.t.sina.com.cn/statuses/unread.json") {
	returnthis <- roauth$OAuthRequest(requestURL, params = params, method="GET")
	return(fromJSON(returnthis))
}