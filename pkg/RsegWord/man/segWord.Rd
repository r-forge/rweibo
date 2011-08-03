\name{segWord}
\alias{segWord}
\title{Segment Words %% ~~function to do .}
\usage{segWord(strword, ws =
.jnew("org/rAnalysisCN/imdict/wordsegmenter/WordSegment"), isSim = T)}
\description{Segment Words %% ~~function to do ... ~~
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
\arguments{\item{strword}{%% ~~Describe \code{strword} here~~}
\item{ws}{%% ~~Describe \code{ws} here~~}
\item{isSim}{%% ~~Describe \code{isSim} here~~}}
\examples{##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(strword, ws = .jnew("org/rAnalysisCN/imdict/wordsegmenter/WordSegment"), isSim=T)
{
require("rJava")
if(!isSim) strword <- tra2Sim(strword)
tst <- .jcall(ws,"S","segWord",strword)
Encoding(tst) <- "UTF-8"
tst <- ifelse(isSim,tst,sim2Tra(tst))  
return(strsplit(tst,",")[[1]])
}}
