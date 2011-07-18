\name{getUserTimeline}
\alias{getUserTimeline}
\title{Return the user's latest timeline}
\usage{getUserTimeline(joauth, params=list(),
    requestURL="http://api.t.sina.com.cn/statuses/user_timeline.json")
}
\description{Return the authenticating user's latest weibos.}
\value{a result list}
\author{lijian <\email{lijian.pku@gmail.com}>}
\references{\url{http://open.weibo.com/wiki/index.php/Statuses/user_timeline/en}}
\keyword{ROAuth}
\arguments{\item{joauth}{a java object created by "ROAuth"}
\item{count}{the the returned request count}
\item{requestURL}{the request URL}
}
\examples{\dontrun{

getUserTimeline(roauth, 5)
}}

