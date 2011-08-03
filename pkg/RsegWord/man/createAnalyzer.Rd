\name{createAnalyzer}
\alias{createAnalyzer}
\title{Create the analyzer object}
\usage{createAnalyzer(jarpath=system.file(package = "RsegWord", "java", "rcn.jar"))
}
\description{Create a Java object to segment words.}
\value{a Java object}
\note{You should install the library \code{\link{rJava}} firstly.}
\author{lijian <\email{rweibo@sina.com}>}
\seealso{See Also as \code{\link{.jinit}}}
\references{\url{http://jliblog.com/app/RsegWord}}
\arguments{\item{jarpath}{path of jar file "imdict-chinese-analyzer"}
}
\examples{\dontrun{
ws <- createAnalyzer()
class(ws)
}}

