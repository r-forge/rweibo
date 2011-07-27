\name{createOAuth}
\alias{createOAuth}
\title{create an OAuth object}
\usage{createOAuth(app_name, access_name)
}
\description{Create an authorized OAuth object}
\value{an OAuth object}
\note{There is only one OAuth needed.}
\author{lijian <\email{rweibo@sina.com}>}
\seealso{See Also as \code{\link{authorization}}}
\references{\url{http://open.weibo.com/wiki/index.php/Oauth}}
\keyword{OAuth}
\arguments{\item{app_name}{name of the application}
\item{access_name}{a string of your access name}
}
\examples{\dontrun{

roauth <- createOAuth("sinademo", "user1")
}}

