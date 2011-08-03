\name{segWord}
\alias{segWord}
\title{Sengment a sentence}
\usage{segWord(strword, ws=.jnew("org/rAnalysisCN/imdict/wordsegmenter/WordSegment"))
}
\description{A function segment Chinese sentence into words.}
\value{a vector of words which have been segmented}
\note{One "analyzer" object is enough.}
\author{lijian <\email{rweibo@sina.com}>}
\seealso{See Also as \code{\link{createAnalyzer}}}
\references{\url{http://jliblog.com/app/RsegWord}}
\arguments{\item{strword}{a Chinese sentence}
\item{ws}{the analyzer object}
}
\examples{\dontrun{
# ws <- createAnalyzer()
segWord("please input Chinese character here", ws)
segWord("please input Chinese character here", ws)
}}

