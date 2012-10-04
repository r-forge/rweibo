


##' Reply a comment.
##' 
##' @title Reply a comment.
##' @param roauth A OAuth object created by \code{\link{createOAuth}}.
##' @param cid The ID of the comment to be replied.
##' @param id The ID of the weibo to be commented.
##' @param comment The comment content.It must be encoded by URLEncode within 140 Chinese characters.
##' @param without_mention Whether add a mention to the comment content automatically. 0: yes; 1: no; Default is 0.
##' @param comment_ori Whether comment the original weibo when comment a reposted weibo. 0: no; 1: yes; Default is 0.
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
##' @references \url{http://open.weibo.com/wiki/2/comments/reply}
##' @keywords Comments
##' @examples \dontrun{
##' 
##' comments.reply(roauth, cid = 54321, id = 12345, comment = "hello world")
##' }

comments.reply <- function(roauth, cid, id, comment, without_mention = 0, comment_ori = 0, ...) {
	requestURL <- "https://api.weibo.com/2/comments/reply.json"
	funCall <- match.call()
	params <- as.list(funCall)
	params[[1]] <- NULL
	params[["roauth"]] <- NULL

	returnthis <- .post(requestURL, roauth@oauthToken, params=params)
	return(returnthis)
}
