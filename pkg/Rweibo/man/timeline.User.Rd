\name{timeline.User}
\alias{timeline.User}
\title{Return the user's latest timeline}
\usage{timeline.User(roauth, params=list(),
    requestURL="http://api.t.sina.com.cn/statuses/user_timeline.json")
}
\description{Return the authenticating user's latest weibos.}
\details{The argument "params" should be a list which cantains:

\tabular{ll}{ 
user_id \tab Return the weibos of specified ID of a user \cr 
screen_name \tab Weibo nickname\cr 
since_id \tab Returns the weibos with an ID greater than the specified ID\cr 
max_id \tab Return the weibos with an ID smaller than the specified ID\cr
count \tab Specify the number of weibos return\cr 
page \tab Specify results page\cr 
base_app \tab whether get the data based on current application. 1 is only current application; 0 is no limitation\cr 
feature \tab Return the weibos by weibo type. 0 is all type. 1 is original. 2 is picture. 3 is video. 4 is music\cr 
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
\references{\url{http://open.weibo.com/wiki/index.php/Statuses/user_timeline/en}}
\keyword{ROAuth}
\arguments{\item{roauth}{a OAuth object created by \code{\link{ROAuth}}}
\item{params}{parameters list}
\item{requestURL}{the request URL}
}
\examples{\dontrun{

timeline.User(roauth, list(screen_name = "rweibo", count = 5))
}}

