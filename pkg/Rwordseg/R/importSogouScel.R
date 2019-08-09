##' Import a scel file of Sogou dictionary.
##' 
##' @title Import a Sogou dictionary.
##' @param strpaths The path of .scel file.
##' @return 
##'  A list of: 
##'  \item{desc}{A data frame of the description.}
##'  \item{dict}{A data frame of the dictionary.}
##' @author Jian Li <\email{rweibo@@sina.com}>
##' @references \url{https://pinyin.sogou.com/dict/}
importSogouScel <- function(strpaths) {
	pathverify <- try(file.exists(strpaths), silent = TRUE)
	if (inherits(pathverify, "try-error")) stop("Please input the path string of the Scel file!")
	if (!any(pathverify)) stop ("Wrong path of the Scel file!")
	strpath <- strpaths[pathverify][1]
	
	MAXN <- file.info(strpaths)$size * 2
	zz <- file(strpaths, "rb")
	try(rawvec <- readBin(zz, "raw", n = MAXN))
	close(zz)
	
	raw1 <- rawvec[305:824]
	raw2 <- rawvec[825:1344]
	raw3 <- rawvec[1345:3392]
	raw4 <- rawvec[3393:5440]
	raw5 <- rawvec[5445:9768]
	raw6 <- rawvec[9769:length(rawvec)]
	
	res1 <- iconv(list(.vectrim(raw1, "right", as.raw(0))), "UTF-16LE", "UTF-8")
	res2 <- iconv(list(.vectrim(raw2, "right", as.raw(0))), "UTF-16LE", "UTF-8")
	res3 <- iconv(list(.vectrim(raw3, "right", as.raw(0))), "UTF-16LE", "UTF-8")
	
	raw4 <- .vectrim(raw4, "right", as.raw(0))
	res4 <- iconv(split(raw4, f = c(0, cumsum(raw4 == as.raw(0))[-length(raw4)])), "UTF-16LE", "UTF-8")
	res4 <- paste0(res4[!is.na(res4)], collapse = "")
	
	res5 <- data.frame(id = rep(NA, ceiling(length(raw5) / 6)), py = NA, stringsAsFactors = FALSE)
	pos <- 1
	n <- 1
	while (pos < length(raw5)) {
		i.id <- .raw2int(raw5[pos:(pos + 1)])
		i.len <- .raw2int(raw5[(pos + 2):(pos + 3)])
		i.py <- paste0(rawToChar(raw5[(pos + 4):(pos + 3 + i.len)], multiple = TRUE), collapse = "")
		res5$id[n] <- i.id
		res5$py[n] <- i.py
		pos <- pos + 4 + i.len
		n <- n + 1
	}
	res5 <- res5[apply(res5, 1, FUN = function(X) !all(is.na(X))), ]
	
	res6 <- data.frame(py = rep(NA, ceiling(length(raw6) / 14)), word = NA, freq = NA, stringsAsFactors = FALSE)
	pos <- 1
	n <- 1
	while (pos < length(raw6)) {
		i.same <- .raw2int(raw6[pos:(pos + 1)])
		i.pylen <- .raw2int(raw6[(pos + 2):(pos + 3)])			
		i.py <- paste0(sapply(.raw2int(raw6[(pos + 4):(pos + 3 + i.pylen)]), FUN = function(X) res5$py[res5$id == X]), collapse = "")
		pos <- pos + 4 + i.pylen
		for (j in 1:i.same) {
			j.wlen <- .raw2int(raw6[pos:(pos + 1)])
			j.word <- iconv(list(raw6[(pos + 2):(pos + 1 + j.wlen)]), "UTF-16LE", "UTF-8")
			pos <- pos + 2 + j.wlen
			j.extlen <- .raw2int(raw6[pos:(pos + 1)])
			j.freq <- .raw2int(raw6[(pos + 2):(pos + 3)])
			pos <- pos + 2 + j.extlen
			res6$py[n] <- i.py
			res6$word[n] <- j.word
			res6$freq[n] <- j.freq
			n <- n + 1
		}
	}
	res6 <- res6[apply(res6, 1, FUN = function(X) !all(is.na(X))), ]
	out1 <- data.frame(dict = res1, type = res2, desc = res3, example = res4, size = nrow(res6), stringsAsFactors = FALSE)
	cat(paste0("Dictionary: ", res1, "\n"))
	cat(paste0("Type: ", res2, "\n"))
	cat(paste0("Description: ", res3, "\n"))
	cat(paste0("Example: ", res4, "\n"))
	cat(paste0("Size: ", nrow(res6), "\n"))
	return(list(desc = out1, dict = res6))
}


