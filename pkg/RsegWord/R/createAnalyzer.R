

##' Create a Java object to segment words.
##' 
##' @title Create the analyzer object
##' @rdname createAnalyzer
##' @param jarpath path of jar file "imdict-chinese-analyzer"
##' @return a Java object
##' @note You should install the library \code{\link{rJava}} firstly.
##' @author lijian <\email{rweibo@@sina.com}>
##' @seealso See Also as \code{\link{.jinit}}
##' @export
##' @references \url{http://jliblog.com/app/RsegWord}
##' @examples \dontrun{
##' ws <- createAnalyzer()
##' class(ws)
##' }
createAnalyzer <-
function(jarpath = system.file(package = "RsegWord", "java", "rcn.jar"))
{
    .jinit(jarpath)
    return(.jnew("org/rAnalysisCN/imdict/wordsegmenter/WordSegment"))
}
