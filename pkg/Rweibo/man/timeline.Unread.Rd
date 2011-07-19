\name{timeline.Unread}
\alias{timeline.Unread}
\title{Return the authenticating user's unread weibo count}
\usage{timeline.Unread(roauth, params=list(),
    requestURL="http://api.t.sina.com.cn/statuses/unread.json")
}
\description{Return the total number of all kinds of unread message of the authenticating user.}
\details{The argument "params" should be a list which cantains:

\tabular{ll}{ 
with_new_status \tab Whether there's new status, 1 mean there is and 0 means there is not\cr 
since_id \tab Returns the weibos with an ID greater than the specified ID\cr 
}}
\value{A list of weibos, each weibo contains: 
\item{followers}{number of followers} 
\item{dm}{number of direct messages}
\item{mentions}{number of the latest weibo metioned me}
\item{comments}{new comments count }}
\author{lijian <\email{rweibo@sina.com}>}
\references{\url{http://open.weibo.com/wiki/index.php/Statuses/unread/en}}
\keyword{ROAuth}
\arguments{\item{roauth}{a OAuth object created by \code{\link{ROAuth}}}
\item{params}{parameters list}
\item{requestURL}{the request URL}
}
\examples{\dontrun{

timeline.Unread(roauth)
}}

