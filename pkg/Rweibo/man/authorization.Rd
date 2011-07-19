\name{authorization}
\alias{authorization}
\title{Authorization}
\usage{authorization(app_name, access_name)
}
\description{Implement an OAuth authorization process.}
\details{Before using this API to connect weibo, you should authorize one weibo account at least.}
\value{a logical value}
\note{When an on-screen prompts appear please follow it.}
\author{lijian <\email{rweibo@sina.com}>}
\seealso{See Also as \code{\link{registerApp}} \code{\link{ROAuth}}}
\references{\url{http://open.weibo.com/wiki/index.php/Oauth}}
\keyword{ROAuth}
\arguments{\item{app_name}{name of the application}
\item{access_name}{a string of your access name (the accout name or nick name of your weibo is suggested)}
}
\examples{\dontrun{

authorization("sinademo", "user1")
# Then direct to the authorization URL with your browser manually
}}

