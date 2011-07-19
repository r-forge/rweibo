


##' Return 20 latest public weibo. The Respons is not strictly realtime, and it is cached for 60 seconds.
##' 
##' The argument "params" should be a list which cantains:
##' 
##' \tabular{ll}{ 
##' count \tab Specify the number of weibos return\cr 
##' base_app \tab whether get the data based on current application. 1 is only current application; 0 is no limitation\cr 
##' } 
##' 
##' @title Return the latest public weibos
##' @param roauth a OAuth object created by "ROAuth"
##' @param params parameters list
##' @param requestURL the request URL
##' @return 
##'  A list of weibos, each weibo contains: 
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
##' @references \url{http://open.weibo.com/wiki/index.php/Statuses/public_timeline/en}
##' @keywords ROAuth
##' @examples \dontrun{
##' 
##' timeline.Public(roauth, list(count = 5, base_app = 1))
##' }
timeline.Public <- function(roauth, params=list(), requestURL = "http://api.t.sina.com.cn/statuses/public_timeline.json") {
	returnthis <- roauth$OAuthRequest(requestURL, params = params, method="GET")
	return(fromJSON(returnthis))
}