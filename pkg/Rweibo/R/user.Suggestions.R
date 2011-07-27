


##' Returns some user that authentication user may be interesting in.
##' 
##' The argument "params" should be a list which cantains:
##' 
##' \tabular{ll}{ 
##' with_reason \tab whether has reason, can be 1 or 0, 1 means response includes the reason\cr 
##' } 
##' 
##' @title Return the user that authenticating user may be interesting in
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
##' @references \url{http://open.weibo.com/wiki/index.php/Users/suggestions/en}
##' @keywords ROAuth
##' @examples \dontrun{
##' 
##' user.Suggestions(roauth)
##' }
user.Suggestions <- function(roauth, params=list(), requestURL = "http://api.t.sina.com.cn/users/suggestions.json") {
	returnthis <- .get(requestURL, roauth@appKey, roauth@appSecret,
			roauth@oauthKey, roauth@oauthSecret, params=params)
	return(fromJSON(returnthis))
}