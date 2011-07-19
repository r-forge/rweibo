\name{access.update}
\alias{access.update}
\title{Post a weibo}
\usage{access.update(roauth, params=list(),
    requestURL="http://api.t.sina.com.cn/statuses/update.json")
}
\description{Post a weibo as well as repost a weibo. Request must be handed in POST method.}
\details{The argument "params" should be a list which cantains:

\tabular{ll}{ 
status \tab Weibo content\cr 
in_reply_to_status_id \tab Reposted weibo ID\cr
lat \tab Latitude. Range: -90.0 to + 90.0. + stands for Norht latitude\cr 
long \tab Longitude. Range: -180.0 to + 180.0. + stands for East Longitude\cr 
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
\references{\url{http://open.weibo.com/wiki/index.php/Statuses/update/en}}
\keyword{ROAuth}
\arguments{\item{roauth}{a OAuth object created by \code{\link{ROAuth}}}
\item{params}{parameters list}
\item{requestURL}{the request URL}
}
\examples{\dontrun{

access.update(roauth, list(status = "hello world"))
}}

