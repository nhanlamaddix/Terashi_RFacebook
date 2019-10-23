#' Token2.0オブジェクトを作成するFactoryクラス
#'
#' @usage
#' token_factory <- facebook_Token2.0_factory$new(consumer_key = "XXXXXX", consumer_secret = "XXXXXXXXXX")
#'
#' @examples
#' # Facebookアプリを作成し、リダイレクトURIに https://127.0.0.1:1410/ を加えておく。HTTPSであることに注意。
#' token_factory <- facebook_Token2.0_factory$new(consumer_key = "0000000000", consumer_secret = "00000000000000000000", redirect_uri = "https://127.0.0.1:1410/")
#'
#' # 認証ページURLを作成する
#' auth_url <- token_factory$create_authorize_url()
#'
#' auth_url
#' # 認証ページURL auth_url をブラウザで開いて認証を行い、リダイレクトURLをブラウザのURLバーから読み取る
#'
#' # 読み取ったリダイレクトURLを格納する
#' redirect_url <- 'https://127.0.0.1:1410/?code=XXXXXXXXXXXXX&state=AAAAAAA#_=_'
#'
#' # リダイレクトURLをパースする
#' params <- token_factory$parse_redirect_url(redirect_url)
#' # アクセストークンを取得する
#' facebook_token <- token_factory$create_token(params)
#'
#' @export
facebook_Token2.0_factory <- R6::R6Class("facebook_Token2.0_factory", public = list(

  # プロパティ
  app = NULL,
  endpoint =NULL,

  # コンストラクタ
  initialize = function(appname = "myfacebookapp", consumer_key, consumer_secret, redirect_uri = "http://127.0.0.1:1410/") {
    self$endpoint <- httr::oauth_endpoint(authorize = "https://www.facebook.com/dialog/oauth",
                                          access = "https://graph.facebook.com/oauth/access_token")
    self$app <- httr::oauth_app(appname = appname, key = consumer_key, secret = consumer_secret, redirect_uri = redirect_uri)
  },

  # 認証URL作成メソッド
  create_authorize_url = function(scope = NULL, state = nonce()) {
    if (is.null(scope)) {
      scope <- c("public_profile", "user_friends")
    }

    oauth2.0_authorize_url(
      self$endpoint,
      self$app,
      scope,
      redirect_uri = self$app$redirect_uri,
      state = state,
      query_extra = list()
    )
  },

  # リダイレクトURLのパースメソッド
  parse_redirect_url = function(url) {
    query <- stringr::str_extract(url, "\\?[^#]*")
    if (!is.character(query) || identical(query, "")) {
      return(NA)
    } else {
      params <- parse_query(gsub("^\\?", "", query))
      code <- params$code %||% params[[1]]
      state <- params$state %||% params[[2]]
      params <- list(code = code, state = state)
      return(params)
    }
  },

  # アクセストークン取得メソッド
  create_token = function(params, scope = NULL) {
    stopifnot(is.list(params), c("code") %in% names(params))

    if (is.null(scope)) {
      scope <- c("public_profile", "user_friends")
    }
    scope <- check_scope(scope)

    code <- params$code

    #
    req_params <- compact(list(
      client_id = self$app$key,
      redirect_uri = self$app$redirect_uri,
      code = code
    ))

    req_params$client_secret <- self$app$secret
    req <- httr::GET("https://graph.facebook.com/v3.2/oauth/access_token",
                      encode = "form",
                      query = req_params,
                      config = list())
    credentials <- httr::content(req, type = NULL)

    facebook_Token2.0$new(self$app, self$endpoint, params = params, credentials = credentials, cache = FALSE)
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

facebook_Token2.0 <- R6::R6Class("facebook_Token2.0", inherit = httr::Token2.0, list(
  init_credentials = function(force = FALSE) {
    stop("Not implemented")
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

oauth2.0_authorize_url <- function(endpoint, app, scope,
                                   redirect_uri = NULL,
                                   state = nonce(),
                                   query_extra = list()) {
  # TODO might need to put some params before and some after...

  scope <- check_scope(scope)
  query_extra <- query_extra %||% list() # i.e. make list if query_extra is null

  if (is.null(redirect_uri)) {
    redirect_uri <- app$redirect_uri
  }

  default_query <- list(
    client_id = app$key,
    scope = scope,
    redirect_uri = redirect_uri,
    response_type = "code",
    state = state
  )

  query <- compact(utils::modifyList(default_query, query_extra))

  httr::modify_url(
    endpoint$authorize,
    query = query
  )
}

nonce <- function(length = 10) {
  paste(sample(c(letters, LETTERS, 0:9), length, replace = TRUE),
        collapse = ""
  )
}

check_scope <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }

  if (!is.character(x)) {
    stop("`scope` must be a character vector", call. = FALSE)
  }
  paste(x, collapse = " ")
}

