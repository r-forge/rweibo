\name{getMentions}
\alias{getMentions}
\title{Return the authenticating user's mentions}
\usage{getMentions(joauth, count=20,
    requestURL="http://api.t.sina.com.cn/statuses/mentions.json")
}
\description{Return the latest N weibos metioned the authenticating user.}
\value{a result list}
\author{lijian <\email{lijian.pku@gmail.com}>}
\references{\url{http://open.weibo.com/wiki/index.php/Statuses/mentions/en}}
\keyword{ROAuth}
\arguments{\item{joauth}{a java object created by "ROAuth"}
\item{count}{the the returned request count}
\item{requestURL}{the request URL}
}
\examples{\dontrun{

getMentions(roauth, 5)
}}

