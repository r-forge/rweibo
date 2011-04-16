\name{ROAuth}
\alias{ROAuth}
\title{create an OAuth object}
\usage{ROAuth(app_name, access_name)
}
\description{Create an authorized OAuth object}
\value{a java object}
\note{There is only one OAuth needed.}
\author{lijian <\email{lijian.pku@gmail.com}>}
\seealso{See Also as \code{\link{authorization}}}
\references{\url{http://open.weibo.com/wiki/index.php/Oauth}}
\keyword{ROAuth}
\arguments{\item{app_name}{name of the application}
\item{access_name}{a string of your access name}
}
\examples{\dontrun{

roauth <- ROAuth("sinademo", "user1")
}}

