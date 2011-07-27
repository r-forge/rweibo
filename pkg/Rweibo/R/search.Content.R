


##' Search for weibos with the keyword.
##' 
##' The argument "params" should be a list which cantains:
##' 
##' \tabular{ll}{ 
##' q \tab Keyword, encoded by URL Encode\cr 
##' page \tab Page.default is 1\cr
##' rpp \tab Weibos count per page\cr 
##' callback \tab Only support JSON. For JSONP cross site data access\cr 
##' geocode \tab Returns weibos around the place with the geo data\cr  
##' } 
##' 
##' @title Search for weibos with the keyword
##' @param roauth a OAuth object created by \code{\link{ROAuth}}
##' @param requestURL the request URL
##' @return 
##'  A list: 
##'  \item{results}{search results}
##'  \item{since_id}{since id} 
##'  \item{max_id}{max id}
##'  \item{results_per_page}{results per page}
##'  \item{next_page}{next page}
##'  \item{refresh_url}{refresh url}
##'  \item{completed_in}{completed in}
##'  \item{page}{page}
##'  \item{query}{query}
##'  
##' @author lijian <\email{rweibo@@sina.com}>
##' @export
##' @references \url{http://open.weibo.com/wiki/index.php/Search/en}
##' @keywords ROAuth
##' @examples \dontrun{
##' 
##' search.Content(roauth, list(q = "rcurl"))
##' }
search.Content <- function(roauth, params, requestURL = "http://api.t.sina.com.cn/search.json") {
	returnthis <- .get(requestURL, roauth@appKey, roauth@appSecret,
			roauth@oauthKey, roauth@oauthSecret, params=params)
	return(fromJSON(returnthis))
}