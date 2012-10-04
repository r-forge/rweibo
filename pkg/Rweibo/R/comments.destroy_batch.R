


##' Delete a comment of authenticating user's.
##' 
##' @title Delete a comment of authenticating user's.
##' @param roauth A OAuth object created by \code{\link{createOAuth}}.
##' @param cids The ID of the comment to be deleted.These should be separated by simeangle comma, and are limited by 20 ID per call.
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
##' @references \url{http://open.weibo.com/wiki/2/comments/destroy_batch}
##' @keywords Comments
##' @examples \dontrun{
##' 
##' comments.destroy_batch(roauth, cids = 54321)
##' }

comments.destroy_batch <- function(roauth, cids, ...) {
	requestURL <- "https://api.weibo.com/2/comments/destroy_batch.json"
	funCall <- match.call()
	params <- as.list(funCall)
	params[[1]] <- NULL
	params[["roauth"]] <- NULL

	returnthis <- .post(requestURL, roauth@oauthToken, params=params)
	return(returnthis)
}
