


##' Implement an OAuth authorization process.
##' 
##' Before using this API to connect weibo, you should authorize one weibo account at least.
##' 
##' @title Authorization
##' @param app_name name of the application
##' @param access_name a string of your access name (the accout name or nick name of your weibo is suggested) 
##' @return a logical value 
##' @note When an on-screen prompts appear please follow it.
##' @author lijian <\email{lijian.pku@@gmail.com}>
##' @seealso See Also as \code{\link{registerApp}} \code{\link{ROAuth}}
##' @export
##' @references \url{http://open.weibo.com/wiki/index.php/Oauth}
##' @keywords ROAuth
##' @examples \dontrun{
##' 
##' authorization("sinademo", "user1")
##' # Then direct to the authorization URL with your browser manually
##' }
authorization <- function(app_name, access_name) {
	apppath <- system.file(package = "Rweibo", "oauth")
	libpath <- system.file(package = "Rweibo", "java")
	if (app_name %in% list.files(apppath)) {
		applist <- fromJSON(file = file.path(apppath, app_name))
		app_key <- applist$app_key
		app_secret <- applist$app_secret
		.jinit(file.path(libpath, "Scribe.jar"))
		jtoken <- .jnew("org/rweibo/RweiboToken", app_key, app_secret)
		str_request <- .jcall(jtoken,"S","requestToken")
		v_request <- strsplit(str_request,",")[[1]]
		msg <- paste("Please direct your web browser to: \n", v_request[3], "\nWhen complete, record the PIN given to you and provide it here: ", sep='')
        verifier <- readline(prompt=msg)
		str_access <- .jcall(jtoken, "S", "accessToken", v_request[1], v_request[2], verifier)
		v_access <- strsplit(str_access,",")[[1]]
		if (access_name %in% names(applist$app_token)) {
			.modifyAccess(app_name, access_name, v_access[1], v_access[2])
		} else {
			.addAccess(app_name, access_name, v_access[1], v_access[2])
		}
	} else {
		stop(paste(app_name, "doesn't exist, please use '.registerApp' to create"))
	}
	return (TRUE)
}