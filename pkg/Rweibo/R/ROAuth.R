


##' Create an authorized OAuth object
##' 
##' 
##' @title create an OAuth object
##' @param app_name name of the application
##' @param access_name a string of your access name
##' @return a java object
##' @note There is only one OAuth needed.
##' @author lijian <\email{lijian.pku@@gmail.com}>
##' @seealso See Also as \code{\link{authorization}}
##' @export
##' @references \url{http://open.weibo.com/wiki/index.php/Oauth}
##' @keywords ROAuth
##' @examples \dontrun{
##' 
##' roauth <- ROAuth("sinademo", "user1")
##' }
ROAuth <- function(app_name, access_name) {
	apppath <- system.file(package = "Rweibo", "oauth")
	libpath <- system.file(package = "Rweibo", "java")
	if (app_name %in% list.files(apppath)) {
		applist <- fromJSON(file = file.path(apppath, app_name))
		if (access_name %in% names(applist$app_token)) {
			.jinit(file.path(libpath, "Scribe.jar"))
			jobj <- .jnew("org/rweibo/RweiboOAuth", applist$app_key, applist$app_secret, applist$app_token[[access_name]]$token_key, applist$app_token[[access_name]]$token_secret)
		} else {
			stop(paste(access_name, "doesn't exist, please use '.addAccess' to create"))
		}
	} else {
		stop(paste(app_name, "doesn't exist, please use '.registerApp' to create"))
	}
	return(jobj)
}