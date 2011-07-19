\name{timeline.Mentions}
\alias{timeline.Mentions}
\title{Return the authenticating user's mentions}
\usage{timeline.Mentions(roauth, params=list(),
    requestURL="http://api.t.sina.com.cn/statuses/mentions.json")
}
\description{Return the latest N weibos metioned the authenticating user.}
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
\item{favorited}{whether favorited}
\item{truncated}{whether truncated}
\item{in_reply_to_status_id}{weibo ID that reply to}
\item{in_reply_to_user_id}{Replyer ID}
\item{in_reply_to_screen_name}{Replayer nickname}
\item{user}{list of user information}
\item{retweeted_status}{list of reposted information, only available for a reposted weibo}
\item{annotations}{annotations}}
\author{lijian <\email{rweibo@sina.com}>}
\references{\url{http://open.weibo.com/wiki/index.php/Statuses/mentions/en}}
\keyword{ROAuth}
\arguments{\item{roauth}{a OAuth object created by \code{\link{ROAuth}}}
\item{params}{parameters list}
\item{requestURL}{the request URL}
}
\examples{\dontrun{

timeline.Mentions(roauth, list(count = 5))
}}

