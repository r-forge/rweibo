\name{timeline.CommentsList}
\alias{timeline.CommentsList}
\title{Return the weibo's comment list by the weibo ID}
\usage{timeline.CommentsList(roauth, params=list(),
    requestURL="http://api.t.sina.com.cn/statuses/comments.json")
}
\description{Return the comment list of a weibo by its ID.}
\details{The argument "params" should be a list which cantains:

\tabular{ll}{ 
id \tab Specify the weibo ID to get its comment list\cr
count \tab Specify the number of weibos return\cr 
page \tab Specify results page\cr 
}}
\value{A list of weibos, each weibo contains: 
\item{created_at}{created time}
\item{id}{weibo ID} 
\item{text}{weibo text}
\item{source}{weibo source}
\item{user}{list of user information}
\item{status}{list of status information}
\item{reply_comment}{list of reply comment information}}
\author{lijian <\email{rweibo@sina.com}>}
\references{\url{http://open.weibo.com/wiki/index.php/Statuses/comments/en}}
\keyword{ROAuth}
\arguments{\item{roauth}{a OAuth object created by \code{\link{ROAuth}}}
\item{params}{parameters list}
\item{requestURL}{the request URL}
}
\examples{\dontrun{

timeline.CommentsList(roauth, list(id = 14762313082))
}}

