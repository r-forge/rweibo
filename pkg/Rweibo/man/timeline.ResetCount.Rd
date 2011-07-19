\name{timeline.ResetCount}
\alias{timeline.ResetCount}
\title{Reset unread weibo count}
\usage{timeline.ResetCount(roauth, params=list(),
    requestURL="http://api.t.sina.com.cn/statuses/reset_count.json")
}
\description{Reset a kind of unread messageof the authenticating user to 0. Kinds can be reset: 1. comment count, 2 metions count, 3 direct messages count, 4 followings count.}
\details{The argument "params" should be a list which cantains:

\tabular{ll}{ 
type \tab The kinds that is going to be reset\cr 
}}
\value{list of result}
\author{lijian <\email{rweibo@sina.com}>}
\references{\url{http://open.weibo.com/wiki/index.php/Statuses/reset_count/en}}
\keyword{ROAuth}
\arguments{\item{roauth}{a OAuth object created by \code{\link{ROAuth}}}
\item{params}{parameters list}
\item{requestURL}{the request URL}
}
\examples{\dontrun{

timeline.ResetCount(roauth)
}}

