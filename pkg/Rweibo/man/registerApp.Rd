\name{registerApp}
\alias{registerApp}
\title{Manage application}
\usage{registerApp(app_key, app_secret, app_name=app_key)
modifyApp(app_name, app_key, app_secret)
deleteApp(app_name)
listApp(app_name)
}
\description{Reigster, modify and delete application.}
\details{\code{registerApp}: Use the key and secret you get from "open.weibo.com"

}
\value{\code{registerApp}: a logical value

\code{modifyApp}: a logical value


\code{deleteApp}: a logical value


\code{listApp}: a list with components as below
\item{app_key }{key of the application} 
\item{app_secret }{secret of the application} 
\item{app_token }{authorization information of OAuth}


}
\note{You should register an application on sina firstly.}
\author{lijian <\email{rweibo@sina.com}>}
\seealso{See Also as \code{\link{authorization}}}
\references{\url{http://open.weibo.com/}}
\keyword{application}
\arguments{\item{app_name}{name of an application}
\item{app_key}{key of an application}
\item{app_secret}{secret of an application}
}
\examples{\dontrun{
registerApp("GDdmIQH6jhtmLUypg82g", "MCD8BKwGdgPHvAuvgvz4EQpqDAtx89grbuNMRd7Eh98", app_name = "sinademo")
listApp("sinademo")
modifyApp("sinademo", "t1", "t2")
deleteApp("sinademo")
}}
\alias{modifyApp}
\alias{deleteApp}
\alias{listApp}

