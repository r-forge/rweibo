\name{search.Content}
\alias{search.Content}
\title{Search for weibos with the keyword}
\usage{search.Content(roauth, params, requestURL="http://api.t.sina.com.cn/search.json")
}
\description{Search for weibos with the keyword.}
\details{The argument "params" should be a list which cantains:

\tabular{ll}{ 
q \tab Keyword, encoded by URL Encode\cr 
page \tab Page.default is 1\cr
rpp \tab Weibos count per page\cr 
callback \tab Only support JSON. For JSONP cross site data access\cr 
geocode \tab Returns weibos around the place with the geo data\cr  
}}
\value{A list: 
\item{results}{search results}
\item{since_id}{since id} 
\item{max_id}{max id}
\item{results_per_page}{results per page}
\item{next_page}{next page}
\item{refresh_url}{refresh url}
\item{completed_in}{completed in}
\item{page}{page}
\item{query}{query}}
\author{lijian <\email{rweibo@sina.com}>}
\references{\url{http://open.weibo.com/wiki/index.php/Search/en}}
\keyword{ROAuth}
\arguments{\item{roauth}{a OAuth object created by \code{\link{ROAuth}}}
\item{requestURL}{the request URL}
}
\examples{\dontrun{

search.Content(roauth, list(q = "rcurl"))
}}

