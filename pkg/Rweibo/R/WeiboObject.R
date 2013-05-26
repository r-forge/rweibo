
setRefClass("weibo2.0", 
		fields = list(
				appPath = "character", 
				appName = "character", 
				appKey = "character", 
				appSecret = "character", 
				oauthName = "character", 
				oauthToken = "character", 
				oauthUser = "character", 
				oauthTime = "character", 
				oauthResetTime = "character", 
				oauthLife = "character",
				authURL = "character",
				accessURL = "character",
				oauthLimits = "ANY",
				oauthMsg = "character",
				webMsg = "character",
				webCurl = "ANY",
				webName = "character", 
				webUser = "character"
		),
		
		methods = list(
			initialize = function(appName, oauthName) {
				.self$appPath <- system.file(package = "Rweibo", "oauth")
				.self$appName <- appName
				.self$oauthName <- oauthName
				applist <- listApp(appName)
				.self$appKey <- applist$app_key
				.self$appSecret <- applist$app_secret
				.self$authURL <- "https://api.weibo.com/oauth2/authorize"
				.self$accessURL <- "https://api.weibo.com/oauth2/access_token"

				.self$oauthToken <- ""
				.self$oauthUser <- ""
				.self$oauthTime <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
				.self$oauthLife <- "-1"
				if (oauthName %in% names(applist$app_token) ) {
					try(.self$oauthToken <- applist$app_token[[oauthName]][["token_key"]], silent = TRUE)
					try(.self$oauthUser <- applist$app_token[[oauthName]][["token_user"]], silent = TRUE)
					try(.self$oauthTime <- applist$app_token[[oauthName]][["token_time"]], silent = TRUE)
					try(.self$oauthLife <- applist$app_token[[oauthName]][["expires_in"]], silent = TRUE)
				} 
				.self$oauthResetTime <- ""
				.self$oauthLimits <- list()
				.self$oauthMsg <- "oauth was not authorized, please use '$authorize()' to authorize."
				.self$oauthMsg <- "not logged in, please use '$login(username, password)' to login."
				.self$webCurl <- getCurlHandle()
				cookieFile <- file.path(.self$appPath, paste(.self$oauthName, ".cookie", sep = ""))
				if (file.exists(cookieFile)) {
					.self$webCurl <- getCurlHandle(followlocation = TRUE, verbose = TRUE, 
							ssl.verifyhost = FALSE, ssl.verifypeer = FALSE, 
							cookiejar = cookieFile, cookiefile = cookieFile)
				} else {
					.self$webCurl <- getCurlHandle()
				}
				.self$webName <- "" 
				.self$webUser <- ""
				
			},	
			expiresIn = function(byAPI = FALSE) {
				if (byAPI) {
					strURL <- "https://api.weibo.com/oauth2/get_token_info"
					OUT <- .post(strURL, .self$oauthToken)
					OUT <- OUT$expire_in
				} else {
					if (as.numeric(.self$oauthLife) < 0) {
						OUT <- -1
					} else {
						oauthtimediff <- difftime(Sys.time(), as.POSIXlt(.self$oauthTime, format = "%Y-%m-%d %H:%M:%S"), units = "secs")
						OUT <- as.numeric(.self$oauthLife) - floor(as.numeric(oauthtimediff))
					}
				}
				return(OUT)
			}, 
			getLimits = function(byAPI = FALSE) {
				if (byAPI) {
					strURL <- "https://api.weibo.com/2/account/rate_limit_status.json"
					outlist <- .get(strURL, .self$oauthToken)
					if ("error" %in% names(outlist)) stop(outlist$error)
					outdf1 <- do.call("rbind", lapply(outlist$api_rate_limits, 
									FUN = function(X) {
										data.frame(API = X[[1]], 
												LimitUnit = X[[3]],
												Limit = X[[2]], 											 
												RemainingHits = X[[4]], stringsAsFactors = FALSE)
									}
							))
					outdf2 <- data.frame(API = c("ip_limit", "user_limit"),
							LimitUnit = c("HOURS", "HOURS"),
							Limit = c(outlist$ip_limit, outlist$user_limit),
							RemainingHits = c(outlist$remaining_ip_hits, outlist$remaining_user_hits),
							stringsAsFactors = FALSE
					)
					OUT <- rbind(outdf1, outdf2)
					ResetTime <- outlist$reset_time
				} else {
					if (Sys.time() > as.POSIXlt(.self$oauthResetTime, format = "%Y-%m-%d %H:%M:%S")) {
						.self$oauthLimits <- .self$getLimits(TRUE)
						.self$oauthResetTime <- .hourtime(1)
					} 
					OUT <- .self$oauthLimits
					ResetTime <- .self$oauthResetTime
				}
				cat(paste("Reset time: ", ResetTime, "\n", sep = ""))
				return(OUT)
			},
			authorize = function(forcelogin = FALSE) {
				oldport <- tools:::httpdPort
				if (is.null(getOption("redirect_uri"))) .setCallback()
				verifyURL <- paste(.self$authURL, "?client_id=", .self$appKey, "&response_type=code&redirect_uri=", 
						getOption("redirect_uri"), sep= "")
				if (forcelogin) verifyURL <- paste(verifyURL, "&forcelogin=true", sep = "")
				
				browseURL(verifyURL)
				msg <- paste("Please input the codes here\n",
						"CODE: ", sep='')
				verifierCode <- readline(prompt=msg)
				if (oldport != 0) .setHttpPort(oldport)
				
				curl <- getCurlHandle()
				reader <- dynCurlReader(curl, baseURL = .self$accessURL, verbose = FALSE)
				fields <- paste("client_id=", .self$appKey, "&client_secret=", .self$appSecret, 
						"&grant_type=authorization_code&redirect_uri=", getOption("redirect_uri"), 
						"&code=", verifierCode, sep = "")
				curlPerform(curl = curl, URL = .self$accessURL, postfields = fields, writefunction = reader$update, ssl.verifypeer = FALSE)
				tokenList <- .fromJSON(reader$value())
				
				try(.self$oauthToken <- tokenList$access_token, silent = TRUE)
				try(.self$oauthUser <- as.character(tokenList$uid), silent = TRUE)
				try(.self$oauthLife <- as.character(tokenList$expires_in), silent = TRUE)
				try(.self$oauthTime <- format(Sys.time(), "%Y-%m-%d %H:%M:%S"), silent = TRUE)
			},
			login = function(username, password) {
				base64_username <- RCurl:::base64(URLencode(username, reserved=TRUE))[[1]]
				preloginURL <- paste("http://login.sina.com.cn/sso/prelogin.php", 
						"?entry=miniblog&callback=sinaSSOController.preloginCallBack&su=", 
						base64_username, "&client=ssologin.js(v1.4.5)", sep = "")
				loginURL <- "http://login.sina.com.cn/sso/login.php?client=ssologin.js(v1.4.5)"
				
				preloginJson <- .post(preloginURL, oauthKey = "", transjson = FALSE)
				preloginList <- .fromJSON(.strextract(preloginJson, "\\{.*\\}")[[1]])
				enPassword <- paste(digest(digest(password, algo= "sha1", serialize = FALSE), algo= "sha1", serialize = FALSE),
						as.character(preloginList$servertime), preloginList$nonce, sep = "")
				enPassword <- digest(enPassword, algo= "sha1", serialize = FALSE)
				params <- base::list(
						entry = "weibo",
						gateway = "1",
						from = "",
						savestate = "7",
						useticket = "1",
						pagerefer = "http://login.sina.com.cn/sso/logout.php?entry=miniblog&r=http%3A%2F%2Fweibo.com%2Flogout.php%3Fbackurl%3D%2F",
						vsnf = "1", 
						su = base64_username,
						service = "miniblog", 
						servertime = as.character(preloginList$servertime),
						nonce = preloginList$nonce,
						pwencode = "wsse",
						rsakv = "",
						sp = enPassword,
						encoding = "utf-8", 
						prelt = "",
						url = URLencode("http://weibo.com/ajaxlogin.php?framelogin=1&callback=parent.sinaSSOController.feedBackUrlCallBack"),
						returntype = "META"
				)
				
				loginXML <- .post(loginURL, oauthKey = "", params = params, transjson = FALSE)
				loginRetcode <- sapply(strsplit(.strextract(loginXML, "retcode=[0-9]+")[[1]], split = "="), 
						FUN = function(X) as.numeric(X[2]))
				if (loginRetcode[1] != 0) {
					strReason <- .strextract(loginXML, "reason=[^;]*;")[[1]][1]
					strReason <- iconv(URLdecode(strReason), "GBK", "UTF-8")
					cat(strReason)
					cat(" -- ")
					stop("")
				}
				loginURL <- .strextract(loginXML, "location.replace\\(.*?\\)")[[1]]
				loginURL <- gsub("^.*?\"|\".*?$", "", loginURL)
				
				cookieFile <- file.path(.self$appPath, paste(.self$oauthName, ".cookie", sep = ""))
				loginCurl <- getCurlHandle(followlocation = TRUE, verbose = TRUE, 
						ssl.verifyhost = FALSE, ssl.verifypeer = FALSE, 
						cookiejar = cookieFile, cookiefile = cookieFile)
				
				resXML <- getURL(loginURL, curl = loginCurl)
				rm(loginCurl)
				tmp <- gc(FALSE)
				.self$webCurl <- getCurlHandle(followlocation = TRUE, verbose = TRUE, 
						ssl.verifyhost = FALSE, ssl.verifypeer = FALSE, 
						cookiejar = cookieFile, cookiefile = cookieFile)
			},
			save = function() {
				applist <- listApp(.self$appName)
				if (.self$oauthName %in% names(applist$app_token)) {
					.modifyAccess(.self$appName, .self$oauthName, .self$oauthToken, .self$oauthUser, .self$oauthTime, .self$oauthLife)
				} else {
					.addAccess(.self$appName, .self$oauthName, .self$oauthToken, .self$oauthUser, .self$oauthTime, .self$oauthLife)
				}
				cat("Saved!\n")
			},
			list = function() {
				OUT <- base::list(
						"appPath" = .self$appPath, 
						"appName" = .self$appName, 
						"appKey" = .self$appKey, 
						"appSecret" = .self$appSecret, 
						"oauthName" = .self$oauthName, 
						"oauthToken" = .self$oauthToken, 
						"oauthUser" = .self$oauthUser, 
						"webName" = .self$webName, 
						"webUser" = .self$webUser
				)
				return(OUT)
			}
		)
)


setMethod("show", signature="weibo2.0", 
		function(object) {
			print(paste("Application: ", object$appName, " (", object$appKey, ")", sep = ""))
			print(paste("Access: ", object$oauthName, " (", object$oauthUser, ")", sep = ""))
			if (object$expiresIn() > 0) {
				print(gsub("HOURS", round(object$expiresIn()/3600, 2), object$oauthMsg))
			} else {
				print("oauth was expired, please use '$authorize()' to authorize.")
			}
			if (nzchar(object$webUser)) {
				print(gsub("COOKIE", object$oauthName, object$webMsg))
			} else {
				#print("not logged in, please use '$login(username, password)' to login.")
			}
		}
)






