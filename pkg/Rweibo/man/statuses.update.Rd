\name{statuses.update}
\alias{statuses.update}
\title{Post a new weibo}
\usage{statuses.update(joauth, TEXT,
    requestURL="http://api.t.sina.com.cn/statuses/update.json")
}
\description{Post a weibo as well as repost a weibo.}
\value{a feedback list}
\author{lijian <\email{lijian.pku@gmail.com}>}
\references{\url{http://open.weibo.com/wiki/index.php/Statuses/update/en}}
\keyword{ROAuth}
\arguments{\item{joauth}{a java object created by "ROAuth"}
\item{TEXT}{the content you want to post}
\item{requestURL}{the request URL}
}
\examples{\dontrun{

statuses.update(roauth, "hello world!")
}}

