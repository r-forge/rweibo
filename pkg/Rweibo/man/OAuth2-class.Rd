\name{OAuth2-class}
\docType{class}
\alias{OAuth2-class}
\alias{OAuth2}

\title{Class "OAuth2" }
\description{To use the APIs of Sina Weibo open platform, you have to register a application first. 
The open platform will provide a unique App Key and App Secret for every application. You can use these
key and secret to invoke the authorize interface of OAuth2. All the authorization information will be 
stored in this object of OAuth2.}
\section{Objects from the Class}{
Objects can be created by calls of the function \code{\link{createOAuth}}. 
}
\section{Slots}{
	\describe{
    \item{\code{appKey}:}{Object of class \code{"character"}. Key of the application.}
    \item{\code{appSecret}:}{Object of class \code{"character"}. Secret of the application. }
    \item{\code{oauthToken}:}{Object of class \code{"character"}. Token of the oauth.}
    \item{\code{oauthUser}:}{Object of class \code{"character"}. User ID.}
    \item{\code{oauthTime}:}{Object of class \code{"character"}. Created time.}
    \item{\code{oauthLife}:}{Object of class \code{"character"}. Expected life (in sec) of the oauth.}
    }
}

\author{Jian li <rweibo@sina.com>}

\examples{
	showClass("OAuth2")
}
\keyword{classes}
