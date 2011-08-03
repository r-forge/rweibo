


##' A function segment Chinese sentence into words.
##' 
##' 
##' @title Sengment a sentence
##' @rdname segWord
##' @param strword a Chinese sentence
##' @param ws the analyzer object
##' @return a vector of words which have been segmented 
##' @note One "analyzer" object is enough.
##' @author lijian <\email{rweibo@@sina.com}>
##' @seealso See Also as \code{\link{createAnalyzer}}
##' @export
##' @references \url{http://jliblog.com/app/RsegWord}
##' @examples \dontrun{
##' # ws <- createAnalyzer()
##' segWord("please input Chinese character here", ws)
##' segWord("please input Chinese character here", ws)
##' }
segWord <-
function(strword, ws = .jnew("org/rAnalysisCN/imdict/wordsegmenter/WordSegment"))
{
    tst <- .jcall(ws,"S","segWord",strword)
    Encoding(tst) <- "UTF-8"
    return(strsplit(tst,",")[[1]])
}

