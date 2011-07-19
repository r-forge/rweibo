\name{timeline.Counts}
\alias{timeline.Counts}
\title{Return the comment counts and repost counts of a list of weibos}
\usage{timeline.Counts(roauth, params=list(),
    requestURL="http://api.t.sina.com.cn/statuses/counts.json")
}
\description{Return the comment count and repost count of a batch of weibos. It has a limitation of 100 weibos for each call.}
\details{The argument "params" should be a list which cantains:

\tabular{ll}{ 
ids \tab ID list of weibos seprareted by comma\cr 
}}
\value{A list of weibos, each weibo contains: 
\item{id}{weibo ID} 
\item{comments}{count of comments}
\item{rt}{count of repost}}
\author{lijian <\email{rweibo@sina.com}>}
\references{\url{http://open.weibo.com/wiki/index.php/Statuses/counts/en}}
\keyword{ROAuth}
\arguments{\item{roauth}{a OAuth object created by \code{\link{ROAuth}}}
\item{params}{parameters list}
\item{requestURL}{the request URL}
}
\examples{\dontrun{

timeline.Counts(roauth, list(ids = "14762313082,14762313083"))
}}

