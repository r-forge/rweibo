


##' Return latest weibo of the authenticating user and his followings. It's the same with the contents of the "Home" page when user authenticated.
##' 
##' The argument "params" should be a list which cantains:
##' 
##' \tabular{ll}{ 
##' since_id \tab Returns the weibos with an ID greater than the specified ID\cr 
##' max_id \tab Return the weibos with an ID smaller than the specified ID\cr
##' count \tab Specify the number of weibos return\cr 
##' page \tab Specify results page\cr 
##' base_app \tab whether get the data based on current application. 1 is only current application; 0 is no limitation\cr 
##' feature \tab Return the weibos by weibo type. 0 is all type. 1 is original. 2 is picture. 3 is video. 4 is music\cr 
##' } 
##' 
##' @title Return the authenticating user's and his friends' latest weibos
##' @param roauth a OAuth object created by \code{\link{ROAuth}}
##' @param params parameters list
##' @param requestURL the request URL
##' @return 
##'  A list of weibos, each weibo contains: 
##'  \item{created_at}{create time}
##'  \item{id}{id} 
##'  \item{text}{content of this weibo}
##'  \item{source}{client of weibo}
##'  \item{favorited}{whether favorited}
##'  \item{truncated}{whether truncated}
##'  \item{in_reply_to_status_id}{}
##'  \item{in_reply_to_user_id}{}
##'  \item{in_reply_to_screen_name}{}
##'  \item{geo}{}
##'  \item{mid}{}
##'  \item{user}{list of user information}
##'  \item{retweeted_status}{list of reposted information}
##'  \item{annotations}{}
##'  
##' @author lijian <\email{lijian.pku@@gmail.com}>
##' @export
##' @references \url{http://open.weibo.com/wiki/index.php/Statuses/friends_timeline/en}
##' @keywords ROAuth
##' @examples \dontrun{
##' 
##' getFriendsTimeline(roauth, list(count = 5))
##' }
getFriendsTimeline <- function(roauth, params=list(), requestURL = "http://api.t.sina.com.cn/statuses/friends_timeline.json") {
	#requestURL <- paste(requestURL, "?count=", count, sep="")
	#returnthis <- .jcall(joauth, "S", "getRequest", requestURL)
	returnthis <- roauth$OAuthRequest(requestURL, params = params, method="GET")
	#Encoding(returnthis) <- "UTF-8"
	return(fromJSON(returnthis))
}