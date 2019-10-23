#' R6 environment to store authentication credentials
#'
#' Used to keep persistent state.
#' @export
FacebookAuthentication <- R6::R6Class(
  "FacebookAuthentication",
  public = list(
    token = NULL,
    method = NULL
  ),
  lock_objects = FALSE,
  parent_env = emptyenv()
)

#' トークンをファイルに保存する
#'
#' @param token Tokenオブジェクト
#' @noRd
saveAccessToken <- function(token) {
  name <- guessTokenPlatform(token)
  if (is.null(name)) {
    return(NULL)
  }
  path <- getOption(stringr::str_c("gftShinyAuthR.", name, ".oauth_cache"))
  saveRDS(token, path)
}

#' 保存済みのトークンを読み取る
#'
#' @param name "google", "facebook", or "twitter"
#' @return Token
#' @noRd
readSavedAccessToken <- function(name) {
  path <- getOption(stringr::str_c("gftShinyAuthR.", name, ".oauth_cache"))
  if (!file.exists(path)) {
    return(NULL)
  }
  readRDS(path)
}

#' ファイルに保存されたトークンをメモリ上にロードする
#'
#' @return list
#' @noRd
loadAccessTokens <- function() {
  loadAccessToken("google")
  loadAccessToken("facebook")
  loadAccessToken("twitter")

  return(getLoadedAccessTokens())
}

loadAccessToken <- function(name) {
  if (name == "google") {
    return(loadGoogleToken())
  }

  if (name == "facebook") {
    return(loadFacebookToken())
  }

  if (name == "twitter") {
    return(loadTwitterToken())
  }
}

loadGoogleToken <- function() {
  token <- readSavedAccessToken("google")
  if (is.null(token)) {
    return(NULL)
  }
  googleAuthR::gar_auth(token = token)
}

loadFacebookToken <- function() {
  token <- readSavedAccessToken("facebook")
  if (is.null(token)) {
    return(NULL)
  }
  FacebookAuthentication$set("public", "token", token, overwrite = TRUE)
}

loadTwitterToken <- function() {
  token <- readSavedAccessToken("twitter")
  if (is.null(token)) {
    return(NULL)
  }
  return(rtweet::get_tokens())
}

#' メモリにロードされているアクセストークンを取得する
#'
#' @return Tokenオブジェクトのリスト。 .$google, .$facebook, .$twitter
#' @examples
#' token_list <- getLoadedAccessTokens()
#' print(token_list$google)
#'
#' @export
getLoadedAccessTokens <- function() {
  list(
    google = getLoadedAccessToken("google"),
    facebook = getLoadedAccessToken("facebook"),
    twitter = getLoadedAccessToken("twitter")
  )
}

#' メモリにロードされているアクセストークンを取得する
#'
#' @param name 文字列. "google", "facebook" または "twitter"
#' @return Tokenオブジェクト.
#'
#' @export
getLoadedAccessToken <- function(name) {
  if (name == "google") {
    return(googleAuthR::Authentication$public_fields$token)
  }

  if (name == "facebook") {
    return(FacebookAuthentication$public_fields$token)
  }

  if (name == "twitter") {
    return(loadTwitterToken())
  }
}
