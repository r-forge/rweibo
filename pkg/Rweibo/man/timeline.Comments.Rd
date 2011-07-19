\name{timeline.Comments}
\alias{timeline.Comments}
\title{Return comments of the authenticating user made and received}
\usage{timeline.Comments(roauth, params=list(),
    requestURL="http://api.t.sina.com.cn/statuses/comments_timeline.json")
}
\description{Return the recent N weibos sent or comment.}
\details{The argument "params" should be a list which cantains:

\tabular{ll}{ 
since_id \tab Returns the weibos with an ID greater than the specified ID\cr 
max_id \tab Return the weibos with an ID smaller than the specified ID\cr
count \tab Specify the number of weibos return\cr 
page \tab Specify results page\cr 
}}
\value{A list of weibos, each weibo contains: 
\item{created_at}{created time}
\item{id}{weibo ID} 
\item{text}{weibo text}
\item{source}{weibo source}
\item{user}{list of user information}
\item{status}{list of status information}}
\author{lijian <\email{rweibo@sina.com}>}
\references{\url{http://open.weibo.com/wiki/index.php/Statuses/comments/en}}
\keyword{ROAuth}
\arguments{\item{roauth}{a OAuth object created by \code{\link{ROAuth}}}
\item{params}{parameters list}
\item{requestURL}{the request URL}
}
\examples{\dontrun{

timeline.Comments(roauth, list(count = 5))
}}

