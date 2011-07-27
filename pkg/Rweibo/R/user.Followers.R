


##' Return follower list and the latest weibo of each follower. The responses if sort by time, and the newest follower is at the top 20 records are returned in one call, you can use cursor for more than 20 followers, but it still has a 5000 limitation.
##' 
##' The argument "params" should be a list which cantains:
##' 
##' \tabular{ll}{ 
##' user_id \tab Return the weibos of specified ID of a user\cr 
##' screen_name \tab Weibo nickname\cr
##' cursor \tab Used for page request\cr
##' count \tab Max record count return in one page, 200 is the max, 20 by default\cr
##' } 
##' 
##' @title Return the user's follower list and the latest weibo of each follower
##' @param roauth a OAuth object created by \code{\link{ROAuth}}
##' @param params parameters list
##' @param requestURL the request URL
##' @return 
##'  A list of users, each user contains: 
##'  \item{id}{User ID}
##'  \item{screen_name}{User's nickname displayed on the home page} 
##'  \item{name}{Friendly displayed name, the same as screen_name}
##'  \item{province}{Province code}
##'  \item{city}{City code}
##'  \item{location}{Address}
##'  \item{description}{Personal description}
##'  \item{url}{Url of the user's blog}
##'  \item{profile_image_url}{Profile image}
##'  \item{domain}{The user's personalized weibo url}
##'  \item{gender}{Gender, m:male, f:female, n:unknown}
##'  \item{followers_count}{Followers count}
##'  \item{friends_count}{Following count}
##'  \item{statuses_count}{Weibo count}
##'  \item{favourites_count}{Favorites count}
##'  \item{created_at}{Created time}
##'  \item{following}{Whether the current user is following the user that posts the weibo}
##'  \item{verified}{Whether the user is verified by his real identity}
##'  
##' @author lijian <\email{rweibo@@sina.com}>
##' @export
##' @references \url{http://open.weibo.com/wiki/index.php/Statuses/followers/en}
##' @keywords ROAuth
##' @examples \dontrun{
##' 
##' user.Followers(roauth, c(screen_name = 'rweibo', count = 30))
##' }
user.Followers <- function(roauth, params=list(), requestURL = "http://api.t.sina.com.cn/statuses/followers.json") {
	returnthis <- .get(requestURL, roauth@appKey, roauth@appSecret,
			roauth@oauthKey, roauth@oauthSecret, params=params)
	return(fromJSON(returnthis))
}
