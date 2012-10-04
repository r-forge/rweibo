


##' Delete a comment of authenticating user's.
##' 
##' @title Delete a comment of authenticating user's.
##' @param roauth A OAuth object created by \code{\link{createOAuth}}.
##' @param cid The comment ID to be deleted.
##' @param ... Other request parameters for this API.
##' @return 
##'  A list of: 
##'  \item{created_at}{Created time}
##'  \item{id}{Weibo ID}
##'  \item{text}{Weibo content}
##'  \item{source}{Weibo source}
##'  \item{mid}{Weibo MID}
##'  \item{user}{User profile that posted the weibo}
##'  \item{status}{The weibo that is commented}
##'  \item{reply_comment}{Replied comment information}
##'  
##' @author Jian Li <\email{rweibo@@sina.com}>
##' @references \url{http://open.weibo.com/wiki/2/comments/destroy}
##' @keywords Comments
##' @examples \dontrun{
##' 
##' comments.destroy(roauth, cid = 54321)
##' }

comments.destroy <- function(roauth, cid, ...) {
	requestURL <- "https://api.weibo.com/2/comments/destroy.json"
	funCall <- match.call()
	params <- as.list(funCall)
	params[[1]] <- NULL
	params[["roauth"]] <- NULL

	returnthis <- .post(requestURL, roauth@oauthToken, params=params)
	return(returnthis)
}
