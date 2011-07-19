\name{getPublicTimeline}
\alias{getPublicTimeline}
\title{Return the latest public weibos}
\usage{getPublicTimeline(joauth, params=list(),
    requestURL="http://api.t.sina.com.cn/statuses/public_timeline.json")
}
\description{Return 20 latest public weibo. The Respons is not strictly realtime, and it is cached for 60 seconds.}
\value{a result list}
\author{lijian <\email{lijian.pku@gmail.com}>}
\references{\url{http://open.weibo.com/wiki/index.php/Statuses/public_timeline/en}}
\keyword{ROAuth}
\arguments{\item{roauth}{a OAuth object created by "ROAuth"}
\item{count}{the the returned request count}
\item{requestURL}{the request URL}
}
\examples{\dontrun{

getPublicTimeline(roauth, 5)
}}

