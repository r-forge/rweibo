


##' Return the comment list of a weibo by its ID.
##' 
##' The argument "params" should be a list which cantains:
##' 
##' \tabular{ll}{ 
##' id \tab Specify the weibo ID to get its comment list\cr
##' count \tab Specify the number of weibos return\cr 
##' page \tab Specify results page\cr 
##' } 
##' 
##' @title Return the weibo's comment list by the weibo ID
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
##'  \item{reply_comment}{list of reply comment information}
##'  
##' @author lijian <\email{rweibo@@sina.com}>
##' @export
##' @references \url{http://open.weibo.com/wiki/index.php/Statuses/comments/en}
##' @keywords ROAuth
##' @examples \dontrun{
##' 
##' timeline.CommentsList(roauth, list(id = 14762313082))
##' }
timeline.CommentsList <- function(roauth, params=list(), requestURL = "http://api.t.sina.com.cn/statuses/comments.json") {
	returnthis <- .get(requestURL, roauth@appKey, roauth@appSecret,
			roauth@oauthKey, roauth@oauthSecret, params=params)
	return(fromJSON(returnthis))
}