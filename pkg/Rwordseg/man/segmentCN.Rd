\name{segmentCN}
\alias{segmentCN}
\title{Sengment a sentence.}
\usage{
  segmentCN(strwords,
    analyzer = get("Analyzer", envir = .RwordsegEnv),
    nature = FALSE, nosymbol = TRUE, recognition = TRUE)
}
\arguments{
  \item{strwords}{A Chinese sentence in UTF-8.}

  \item{analyzer}{A JAVA object of analyzer.}

  \item{nature}{Whether to recognise the nature of the
  words.}

  \item{nosymbol}{Whether to keep symbols in the sentence.}

  \item{recognition}{Whether to recognise the person names
  automatically.}
}
\value{
  a vector of words (list if input is vecter) which have
  been segmented.
}
\description{
  A function segment Chinese sentence into words.
}
\examples{
\dontrun{
segmentCN("hello world!")
}
}
\author{
  Jian Li <\email{rweibo@sina.com}>
}

