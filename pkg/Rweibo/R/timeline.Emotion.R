


##' Return all the relative information of Sina Weibo official emotions and magic emotions, including phrases, emotion kind, emtion category, whether it is hot etc.
##' 
##' The argument "params" should be a list which cantains:
##' 
##' \tabular{ll}{ 
##' type \tab Emotion type. face: common emotion. ani: magic emotion. cartoon: cartoon emotion\cr 
##' language \tab Language type, cnname: simplified Chinese, twname: traditional Chinese\cr 
##' } 
##' 
##' @title Emotion Iinterface, return the emotion list
##' @param roauth a OAuth object created by \code{\link{ROAuth}}
##' @param params parameters list
##' @param requestURL the request URL
##' @return 
##'  A list of weibos, each weibo contains: 
##'  \item{phrase}{phrase of the emotion} 
##'  \item{type}{type}
##'  \item{url}{url of th gif}
##'  \item{is_hot}{whether is hot}
##'  \item{is_common}{whether is common}
##'  \item{order_number}{order number}
##'  \item{category}{category}
##'  
##' @author lijian <\email{rweibo@@sina.com}>
##' @export
##' @references \url{http://open.weibo.com/wiki/index.php/Emotions/en}
##' @keywords ROAuth
##' @examples \dontrun{
##' 
##' timeline.Emotion(roauth, list(type = "face", language = "cnname"))
##' }
timeline.Emotion <- function(roauth, params=list(), requestURL = "http://api.t.sina.com.cn/emotions.json") {
	returnthis <- roauth$OAuthRequest(requestURL, params = params, method="POST")
	return(fromJSON(returnthis))
}