\name{createOAuth}
\alias{createOAuth}
\title{create an OAuth object}
\usage{
  createOAuth(app_name, access_name, forcelogin = FALSE)
}
\arguments{
  \item{app_name}{name of the application}

  \item{access_name}{a string of your access name}

  \item{forcelogin}{whether re-login the weibo}
}
\value{
  An S4 object of \code{\link{OAuth2}}.
}
\description{
  Create an authorized OAuth object
}
\note{
  There is only one OAuth object needed.
}
\examples{
\dontrun{

roauth <- createOAuth("sinademo", "user1")
}
}
\author{
  Jian Li <\email{rweibo@sina.com}>
}
\references{
  \url{http://open.weibo.com/wiki/OAuth/en}
}
\seealso{
  \code{\link{registerApp}}
}
\keyword{authorization}

