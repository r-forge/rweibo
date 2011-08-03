\name{newWS}
\alias{newWS}
\title{New a JAVA Object to Segment Words %% ~~function to do .}
\usage{newWS(jarpath)}
\description{New a JAVA Object to Segment Words %% ~~function to do ... ~~
%% ~~ A concise (1-5 lines) description of what the function does. ~~}
\details{%% ~~ If necessary, more details than the description above ~~}
\value{%% ~Describe the value returned %% If it is a LIST, use %%
\item{comp1 }{Description of 'comp1'} %% \item{comp2 }{Description of
'comp2'} %% ...}
\note{%% ~~further notes~~}
\author{%% ~~who you are~~}
\seealso{%% ~~objects to See Also as \code{\link{help}}, ~~~}
\references{%% ~put references to the literature/web site here ~}
\keyword{~kwd1}
\keyword{~kwd2}
\arguments{\item{jarpath}{%% ~~Describe \code{jarpath} here~~}}
\examples{##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(jarpath)
{
require("rJava")
.jinit(jarpath)
return(.jnew("org/rAnalysisCN/imdict/wordsegmenter/WordSegment"))
}}
