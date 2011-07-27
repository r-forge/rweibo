\name{user.Hot}
\alias{user.Hot}
\title{Return system recommended users}
\usage{user.Hot(roauth, params=list(),
    requestURL="http://api.t.sina.com.cn/users/hot.json")
}
\description{Return recommend user list created by the system.}
\details{The argument "params" should be a list which cantains:

\tabular{ll}{ 
category \tab Category, return a certain kind recommend user, default is by default.\cr 
}}
\value{A list of users, each user contains: 
\item{id}{User ID}
\item{screen_name}{User's nickname displayed on the home page} 
\item{name}{Friendly displayed name, the same as screen_name}
\item{province}{Province code}
\item{city}{City code}
\item{location}{Address}
\item{description}{Personal description}
\item{url}{Url of the user's blog}
\item{profile_image_url}{Profile image}
\item{domain}{The user's personalized weibo url}
\item{gender}{Gender, m:male, f:female, n:unknown}
\item{followers_count}{Followers count}
\item{friends_count}{Following count}
\item{statuses_count}{Weibo count}
\item{favourites_count}{Favorites count}
\item{created_at}{Created time}
\item{following}{Whether the current user is following the user that posts the weibo}
\item{verified}{Whether the user is verified by his real identity}}
\author{lijian <\email{rweibo@sina.com}>}
\references{\url{http://open.weibo.com/wiki/index.php/Users/hot/en}}
\keyword{ROAuth}
\arguments{\item{roauth}{a OAuth object created by \code{\link{ROAuth}}}
\item{params}{parameters list}
\item{requestURL}{the request URL}
}
\examples{\dontrun{

user.Hot(roauth)
}}

