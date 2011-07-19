


##' Return the recent N weibos sent or comment.
##' 
##' The argument "params" should be a list which cantains:
##' 
##' \tabular{ll}{ 
##' since_id \tab Returns the weibos with an ID greater than the specified ID\cr 
##' max_id \tab Return the weibos with an ID smaller than the specified ID\cr
##' count \tab Specify the number of weibos return\cr 
##' page \tab Specify results page\cr 
##' } 
##' 
##' @title Return comments of the authenticating user made and received
##' @param roauth a OAuth object created by \code{\link{ROAuth}}
##' @param params parameters list
##' @param requestURL the request URL
##' @return 
##'  A list of weibos, each weibo contains: 
##'  \item{created_at}{created time}
##'  \item{id}{weibo ID} 
##'  \item{text}{weibo text}
##'  \item{source}{weibo source}
##'  \item{user}{list of user information}
##'  \item{status}{list of status information}
##'  
##' @author lijian <\email{rweibo@@sina.com}>
##' @export
##' @references \url{http://open.weibo.com/wiki/index.php/Statuses/comments/en}
##' @keywords ROAuth
##' @examples \dontrun{
##' 
##' timeline.Comments(roauth, list(count = 5))
##' }
timeline.Comments <- function(roauth, params=list(), requestURL = "http://api.t.sina.com.cn/statuses/comments_timeline.json") {
	returnthis <- roauth$OAuthRequest(requestURL, params = params, method="GET")
	return(fromJSON(returnthis))
}