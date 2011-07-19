\name{timeline.Emotion}
\alias{timeline.Emotion}
\title{Emotion Iinterface, return the emotion list}
\usage{timeline.Emotion(roauth, params=list(),
    requestURL="http://api.t.sina.com.cn/emotions.json")
}
\description{Return all the relative information of Sina Weibo official emotions and magic emotions, including phrases, emotion kind, emtion category, whether it is hot etc.}
\details{The argument "params" should be a list which cantains:

\tabular{ll}{ 
type \tab Emotion type. face: common emotion. ani: magic emotion. cartoon: cartoon emotion\cr 
language \tab Language type, cnname: simplified Chinese, twname: traditional Chinese\cr 
}}
\value{A list of weibos, each weibo contains: 
\item{phrase}{phrase of the emotion} 
\item{type}{type}
\item{url}{url of th gif}
\item{is_hot}{whether is hot}
\item{is_common}{whether is common}
\item{order_number}{order number}
\item{category}{category}}
\author{lijian <\email{rweibo@sina.com}>}
\references{\url{http://open.weibo.com/wiki/index.php/Emotions/en}}
\keyword{ROAuth}
\arguments{\item{roauth}{a OAuth object created by \code{\link{ROAuth}}}
\item{params}{parameters list}
\item{requestURL}{the request URL}
}
\examples{\dontrun{

timeline.Emotion(roauth, list(type = "face", language = "cnname"))
}}

