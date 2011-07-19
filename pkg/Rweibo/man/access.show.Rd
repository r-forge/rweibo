\name{access.show}
\alias{access.show}
\title{Return the single weibo's content by its ID}
\usage{access.show(roauth, params=list(),
    requestURL="http://api.t.sina.com.cn/statuses/show/:id.json")
}
\description{Return a single weibo and author information by ID.}
\details{The argument "params" should be a list which cantains:

\tabular{ll}{ 
id \tab The ID of weibo\cr 
}}
\value{A list of this weibos: 
\item{created_at}{created time}
\item{id}{weibo ID} 
\item{text}{weibo text}
\item{source}{weibo source}
\item{favorited}{whether favorited}
\item{truncated}{whether truncated}
\item{in_reply_to_status_id}{weibo ID that reply to}
\item{in_reply_to_user_id}{Replyer ID}
\item{in_reply_to_screen_name}{Replayer nickname}
\item{user}{list of user information}
\item{retweeted_status}{list of reposted information, only available for a reposted weibo}
\item{annotations}{annotations}}
\author{lijian <\email{rweibo@sina.com}>}
\references{\url{http://open.weibo.com/wiki/index.php/Statuses/show/en}}
\keyword{ROAuth}
\arguments{\item{roauth}{a OAuth object created by \code{\link{ROAuth}}}
\item{params}{parameters list}
\item{requestURL}{the request URL}
}
\examples{\dontrun{

access.show(roauth, list(id = 14762313082))
}}

