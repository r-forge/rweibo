\name{getFriendsTimeline}
\alias{getFriendsTimeline}
\title{Return the authenticating user's and his friends' latest weibos}
\usage{getFriendsTimeline(joauth, count=20,
    requestURL="http://api.t.sina.com.cn/statuses/friends_timeline.json")
}
\description{Return latest weibo of the authenticating user and his followings. It's the same with the contents of the "Home" page when user authenticated.}
\value{a result list}
\author{lijian <\email{lijian.pku@gmail.com}>}
\references{\url{http://open.weibo.com/wiki/index.php/Statuses/friends_timeline/en}}
\keyword{ROAuth}
\arguments{\item{joauth}{a java object created by "ROAuth"}
\item{count}{the the returned request count}
\item{requestURL}{the request URL}
}
\examples{\dontrun{

getFriendsTimeline(roauth, 5)
}}

