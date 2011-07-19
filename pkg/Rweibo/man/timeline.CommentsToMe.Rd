\name{timeline.CommentsToMe}
\alias{timeline.CommentsToMe}
\title{Return comments the authenticating user received}
\usage{timeline.CommentsToMe(roauth, params=list(),
    requestURL="http://api.t.sina.com.cn/statuses/comments_to_me.json")
}
\description{Return the comments retrieved by the authenticating user.}
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
\item{status}{list of status information}
\item{reply_comment}{list of reply comment information}}
\author{lijian <\email{rweibo@sina.com}>}
\references{\url{http://open.weibo.com/wiki/index.php/Statuses/comments_to_me/en}}
\keyword{ROAuth}
\arguments{\item{roauth}{a OAuth object created by \code{\link{ROAuth}}}
\item{params}{parameters list}
\item{requestURL}{the request URL}
}
\examples{\dontrun{

timeline.CommentsToMe(roauth, list(count = 5))
}}

