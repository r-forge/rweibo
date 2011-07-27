\name{account.RateLimit}
\alias{account.RateLimit}
\title{Retrieve the rate limitation of API calls}
\usage{account.RateLimit(roauth,
    requestURL="http://api.t.sina.com.cn/account/rate_limit_status.json")
}
\description{Retrieve the rate limitation of API calls. Return how many times API can be called in this hour. This limitation is based on the user type.}
\value{A list of weibos, each weibo contains: 
\item{remaining_hits}{remaining hits}
\item{hourly_limit}{hourly limit} 
\item{reset_time_in_seconds}{reset time in seconds}
\item{reset_time}{reset time}}
\author{lijian <\email{rweibo@sina.com}>}
\references{\url{http://open.weibo.com/wiki/index.php/Account/rate_limit_status/en}}
\keyword{ROAuth}
\arguments{\item{roauth}{a OAuth object created by \code{\link{ROAuth}}}
\item{requestURL}{the request URL}
}
\examples{\dontrun{

account.RateLimit(roauth)
}}

