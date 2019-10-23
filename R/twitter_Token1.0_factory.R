#' Token1.0オブジェクトを作成するFactoryクラス
#'
#' @usage
#' token_factory <- twitter_Token1.0_factory$new(consumer_key = "XXXXXX", consumer_secret = "XXXXXXXXXX")
#'
#' @examples
#' # インスタンス化する
#' token_factory <- twitter_Token1.0_factory$new(consumer_key = "XXXXXX", consumer_secret = "XXXXXXXXXX")
#' # 認証ページURLを作成する
#' auth_url <- token_factory$create_authorize_url()
#' # 認証ページURL auth_url をブラウザで開いて認証を行い、リダイレクトURLをブラウザのURLバーから読み取る
#' # 読み取ったリダイレクトURLを格納する
#' redirect_url <- 'http://127.0.0.1:1410/?oauth_token=fQvZDgAAAAAAXXXXX&oauth_verifier=XXXXXXXXXX'
#' # リダイレクトURLをパースする
#' params <- token_factory$parse_redirect_url(redirect_url)
#' # アクセストークンを取得する
#' twitter_token <- token_factory$create_token(params)
#'
#' @export
twitter_Token1.0_factory <- R6::R6Class("twitter_Token1.0_factory", public = list(

  # プロパティ
  app = NULL,
  endpoint =NULL,

  # コンストラクタ
  initialize = function(appname = "mytwitterapp", consumer_key, consumer_secret, redirect_uri = "http://127.0.0.1") {
    self$endpoint <- httr::oauth_endpoints("twitter")
    self$app <- httr::oauth_app(appname = appname, key = consumer_key, secret = consumer_secret, redirect_uri = redirect_uri)
  },

  # 認証URL作成メソッド
  create_authorize_url = function() {
    oauth_sig <- function(url, method,
                          token = NULL,
                          token_secret = NULL,
                          private_key = NULL,
                          oauth_callback = "http://127.0.0.1", ...) {
      httr::oauth_header(httr::oauth_signature(url, method, self$app, token,
                                               token_secret, private_key, other_params = c(list(...),
                                                                                           oauth_callback = oauth_callback)))
    }
    response <- httr::POST(self$endpoint$request, oauth_sig(self$endpoint$request,
                                                            "POST", private_key = NULL, oauth_callback = self$app$redirect_uri))
    httr::stop_for_status(response)
    params <- httr::content(response, type = "application/x-www-form-urlencoded")
    token <- params$oauth_token
    secret <- params$oauth_token_secret
    httr::modify_url(self$endpoint$authorize, query = list(oauth_token = token, permission = "read"))
  },

  # リダイレクトURLのパースメソッド
  parse_redirect_url = function(url) {
    query <- stringr::str_extract(url, "\\?[^#]*")
    if (!is.character(query) || identical(query, "")) {
      return(NA)
    } else {
      params <- parse_query(gsub("^\\?", "", query))
      oauth_verifier <- params$oauth_verifier %||% params[[1]]
      oauth_token <- params$oauth_token %||% params[[2]]
      params <- list(oauth_verifier = oauth_verifier, oauth_token = oauth_token)
      return(params)
    }
  },

  # アクセストークン取得メソッド
  create_token = function(params) {
    stopifnot(is.list(params), c("oauth_token", "oauth_verifier") %in% names(params))

    verifier <- params$oauth_verifier
    token <- params$oauth_token
    secret <- NULL
    private_key <- NULL

    oauth_sig <- function(url, method,
                          token = NULL,
                          token_secret = NULL,
                          private_key = NULL,
                          oauth_callback = "http://127.0.0.1", ...) {
      httr::oauth_header(httr::oauth_signature(url, method, self$app, token,
                                               token_secret, private_key, other_params = c(list(...),
                                                                                           oauth_callback = oauth_callback)))
    }
    response <- httr::POST(self$endpoint$access, oauth_sig(self$endpoint$access,
                                                           "POST", token, secret, oauth_verifier = verifier, private_key = private_key),
                           body = "")
    httr::stop_for_status(response)
    credentials <- httr::content(response, type = "application/x-www-form-urlencoded")

    stopifnot(c("oauth_token", "oauth_token_secret") %in% names(credentials))

    params <- list(as_header = TRUE)
    twitter_Token1.0$new(self$app, self$endpoint, params = params, credentials = credentials, cache = FALSE)
  }
))

parse_query <- function(query) {
  params <- vapply(strsplit(query, "&")[[1]], str_split_fixed, "=", 2,
                   FUN.VALUE = character(2)
  )

  values <- as.list(curl::curl_unescape(params[2, ]))
  names(values) <- curl::curl_unescape(params[1, ])
  values
}

twitter_Token1.0 <- R6::R6Class("Token1.0", inherit = httr::Token, list(
  init_credentials = function(force = FALSE) {
    stop("Not implemented")
  },
  can_refresh = function() {
    FALSE
  },
  refresh = function() {
    stop("Not implemented")
  },
  sign = function(method, url) {
    oauth <- httr::oauth_signature(url, method, self$app,
                                   self$credentials$oauth_token, self$credentials$oauth_token_secret,
                                   self$private_key)
    if (isTRUE(self$params$as_header)) {
      c(request(url = url), httr::oauth_header(oauth))
    } else {
      url <- httr::parse_url(url)
      url$query <- c(url$query, oauth)
      request(url = httr::build_url(url))
    }
  }
))

keep_last <- function (...) {
  x <- c(...)
  x[!duplicated(names(x), fromLast = TRUE)]
}

is_empty <- function (x) length(x) == 0

compact <- function (x) {
  empty <- vapply(x, is_empty, logical(1))
  x[!empty]
}

request <- function (method = NULL, url = NULL, headers = NULL, fields = NULL,
                     options = NULL, auth_token = NULL, output = NULL) {
  if (!is.null(method))
    stopifnot(is.character(method), length(method) == 1)
  if (!is.null(url))
    stopifnot(is.character(url), length(url) == 1)
  if (!is.null(headers))
    stopifnot(is.character(headers))
  if (!is.null(fields))
    stopifnot(is.list(fields))
  if (!is.null(output))
    stopifnot(inherits(output, "write_function"))
  structure(list(method = method, url = url, headers = keep_last(headers),
                 fields = fields, options = compact(keep_last(options)),
                 auth_token = auth_token, output = output), class = "request")
}

str_split_fixed <- function(string, pattern, n) {
  if (length(string) == 0) return(matrix(character(), nrow = 1, ncol = n))
  m <- gregexpr(pattern, string)[[1]]
  if (length(m) == 1 && m == -1) {
    res <- string
  } else {
    m_starts <- m
    m_ends <- m + attr(m, "match.length") - 1L
    starts <- c(1, m_ends + 1L)[seq_len(n)]
    ends <- c((m_starts - 1L)[seq_len(n - 1)], nchar(string))
    res <- lapply(string, function(x)
      unlist(Map(substr, x, starts, ends, USE.NAMES = FALSE))
    )
  }

  mat <- matrix("", nrow = length(res), ncol = n, byrow = TRUE)
  mat[seq_along(unlist(res))] <- unlist(res)
  mat[, seq_len(n), drop = FALSE]
}

"%||%" <- function(a, b) {
  if (length(a) > 0) a else b
}
