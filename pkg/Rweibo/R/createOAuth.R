setClass("Weibo", representation(appKey = "character", appSecret = "character", oauthKey = "character", oauthSecret = "character", oauthUser = "numeric", signMethod = "character"))


##' Create an authorized OAuth object
##' 
##' 
##' @title create an OAuth object
##' @param app_name name of the application
##' @param access_name a string of your access name
##' @return an OAuth object
##' @note There is only one OAuth needed.
##' @author lijian <\email{rweibo@@sina.com}>
##' @seealso See Also as \code{\link{authorization}}
##' @export
##' @references \url{http://open.weibo.com/wiki/index.php/Oauth}
##' @keywords OAuth
##' @examples \dontrun{
##' 
##' roauth <- createOAuth("sinademo", "user1")
##' }
createOAuth <- function(app_name, access_name) {
	apppath <- system.file(package = "Rweibo", "oauth")
	if (app_name %in% list.files(apppath)) {
		applist <- fromJSON(file=file.path(apppath, app_name))
		if (! access_name %in% names(applist$app_token)) {
			.authorization(app_name, access_name)
		}
		applist <- fromJSON(file=file.path(apppath, app_name))
		oauthobj <- new("Weibo", 
					appKey = applist$app_key, 
					appSecret = applist$app_secret, 
					oauthKey = applist$app_token[[access_name]]$token_key, 
					oauthSecret = applist$app_token[[access_name]]$token_secret, 
					oauthUser = applist$app_token[[access_name]]$token_user,
					signMethod = "HMAC")
	} else {
		stop(paste(app_name, "doesn't exist, please use 'registerApp' to create"))
	}
	return(oauthobj)
}


.get <- function(url, consumerKey, consumerSecret,
		oauthKey, oauthSecret, params=character(), 
		curl = getCurlHandle(), signMethod='HMAC') {
	if(is.null(curl))
		curl <- getCurlHandle()
	
	auth <- .signRequest(url, params, consumerKey, consumerSecret,
			oauthKey=oauthKey, oauthSecret=oauthSecret,
			httpMethod="GET", signMethod=signMethod)
	
	params <- c(params, as.list(auth))
	getForm(url, .params = params, curl = curl)
}

.post <- function(url, consumerKey, consumerSecret,
		oauthKey, oauthSecret, params=character(),
		curl = getCurlHandle(), signMethod='HMAC', postform=FALSE) {
	if(is.null(curl))
		curl <- getCurlHandle()
	
	auth <- .signRequest(url, params, consumerKey, consumerSecret,
			oauthKey=oauthKey, oauthSecret=oauthSecret,
			httpMethod="POST", signMethod=signMethod)
	
	if(postform) {
		postForm(url, .params = c(params, lapply(auth, I)), curl = curl,
				 style = "POST", .contentEncodeFun = curlEscape)
	} else {
		reader <- dynCurlReader(curl, baseURL = url, verbose = FALSE)
		fields <- paste(names(auth), sapply(auth, curlPercentEncode),
				sep = "=", collapse = "&")
		curlPerform(curl = curl, URL = url, postfields = fields,
				writefunction = reader$update)
		reader$value()
	}
}

.parseResponse <- function(response) {
	## Will return a named vector, so a response field of the
	## form foo=blah&qwerty=asdf will have vals c(blah,asdf) and
	## names will be c(foo, qwerty).  If the response is borked,
	## the output of this function will be as well, so caveat
	## emptor, GIGO, etc
	pairs <- sapply(strsplit(response, '&')[[1]], strsplit, '=')
	out <- sapply(pairs, function(x) x[2])
	names(out) <- sapply(pairs, function(x) x[1])
	out
}

.signRequest  <- function(url, params, consumerKey, consumerSecret,
		oauthKey = "", oauthSecret = "", httpMethod = "GET",
		signMethod = "HMAC", nonce = .genNonce(),
		timestamp = Sys.time(),
		escapeFun = curlPercentEncode) {
	## Sign an request made up of the URL, the parameters as a named character
	## vector the consumer key and secret and the token and token secret.
	httpMethod <- toupper(httpMethod)
	signMethod <- toupper(signMethod)
	
	params["oauth_nonce"] <- nonce
	params["oauth_timestamp"] <- as.integer(timestamp)
	
	token <- oauthKey
	if(!is.null(token) && !is.na(token) && token != "")
		params["oauth_token"] <- token
	
	params["oauth_consumer_key"] <- consumerKey
	params["oauth_signature_method"] <- switch(signMethod,
			HMAC = 'HMAC-SHA1',
			RSA  = 'RSA-SHA1',
			text = 'PLAINTEXT',
			stop("Unsupported signature method: ", signMethod)
	)
	params["oauth_version"] <- '1.0'
	
	## we escape the values of the parameters in a special way that escapes
	## the resulting % prefix in the escaped characters, e.g. %20 becomes
	## %2520 as %25 is the escape for %
	params <- params[order(names(params))]
	args <- paste(names(params), sapply(params, escapeFun, post.amp = TRUE),
			sep = "%3D", collapse = "%26")  
	
	if(is.null(oauthSecret))
		oauthSecret <- ""
	
	okey <- paste(sapply(c(consumerSecret, oauthSecret), escapeFun),
			collapse = "&")
	## note that we don't escape the args string again.
	odat <- paste(c(sapply(c(httpMethod, url), escapeFun), args),
			collapse = "&")
	
	sig <- .signString(odat, okey, signMethod)
	
	params["oauth_signature"] <- sig  # curlPercentEncode(sig)
	params[grepl("^oauth_", names(params))]
}

.signString <- function(str, key, method) {
	## Perform the actual computation to get the signature of the data
	sigFunc <- switch(toupper(method),
			HMAC = .signWithHMAC,
			RSA = .signWithRSA,
			text = .signWithPlaintext,
			stop("No signature method for ", method)
	)
	sigFunc(key, str)
}

.genNonce <- function(len = 15L + sample(1:16, 1L)) {
	## Get a random sequence of characters.
	## Nonce - number used only once.
	els <- c(letters, LETTERS, 0:9, "_")
	paste(sample(els, len, replace = TRUE), collapse = "")
}

.signWithHMAC <- function(key, data) {
	## From Ozaki Toru's code at https://gist.github.com/586468
	blockSize <- 64
	hashlength <- 20
	innerpad   <- rawToBits(as.raw(rep(0x36, blockSize)))
	outerpad   <- rawToBits(as.raw(rep(0x5C, blockSize)))
	zero       <- rep(0 ,64)
	
	HexdigestToDigest <- function(digest) {
		as.raw(strtoi(substring(digest, (1:hashlength)*2-1,
								(1:hashlength)*2), base=16))
	}
	
	mac <- function(pad, text) {
		HexdigestToDigest(digest(append(packBits(xor(key, pad)), text),
						algo='sha1', serialize=FALSE))
	}
	
	if(nchar(key) >= 64) {
		keyDigested <- digest(key, algo="sha1", serialize=FALSE)
		key <- intToUtf8(strtoi(HexdigestToDigest(keyDigested), base=16))
	}
	key <- rawToBits(as.raw(append(utf8ToInt(key), zero)[1:blockSize]))
	
	base64(mac(outerpad, mac(innerpad, charToRaw(data))))[1]
}

.signWithRSA <- function(key, data) {
	stop("RSA signature not implemented")
}

.signWithPlaintext <- function(key, data) {
	key
}

.authorization <- function(app_name, access_name, requestURL = "http://api.t.sina.com.cn/oauth/request_token", authURL = "http://api.t.sina.com.cn/oauth/authorize", accessURL = "http://api.t.sina.com.cn/oauth/access_token") {
	apppath <- system.file(package = "Rweibo", "oauth")
	if (app_name %in% list.files(apppath)) {
		applist <- fromJSON(file=file.path(apppath, app_name))
		app_key <- applist$app_key
		app_secret <- applist$app_secret
		
		resp <- .post(requestURL, app_key, app_secret, NULL, NULL)
		vals <- .parseResponse(resp)
		if (!all(c('oauth_token', 'oauth_token_secret') %in%
						names(vals))) {
			stop("Invalid response from site, please ",
					"check your consumerKey and consumerSecret",
					" and try again.")
		}
		oauthKey <- vals["oauth_token"]
		oauthSecret <- vals["oauth_token_secret"]
		verifyURL <- paste(authURL, "?oauth_token=", oauthKey, sep="")
		msg <- paste("To enable the connection, please direct",
				" your web browser to: \n",
				verifyURL,
				"\nWhen complete, record the PIN given ",
				"to you and provide it here: ", sep='')
		verifier <- readline(prompt=msg)
		params <- list(oauth_verifier = verifier)
		resp <- .post(accessURL, app_key, app_secret, oauthKey, oauthSecret, params=params)
		vals <- .parseResponse(resp)
		if (!all(c('oauth_token', 'oauth_token_secret') %in%
						names(vals))) {
			stop("Invalid response after authorization.  ",
					"You likely misentered your PIN, try rerunning",
					" this handshake & browser authorization to get",
					" a new PIN.")
		}
		oauthKey <- vals["oauth_token"][[1]]
		oauthSecret <- vals["oauth_token_secret"][[1]]
		oauthUserID <- as.numeric(vals["user_id"][[1]])
		
		if (access_name %in% names(applist$app_token)) {
			.modifyAccess(app_name, access_name, oauthKey, oauthSecret, oauthUserID)
		} else {
			.addAccess(app_name, access_name, oauthKey, oauthSecret, oauthUserID)
		}
	} else {
		stop(paste(app_name, "doesn't exist, please use '.registerApp' to create"))
	}
	return (TRUE)
}