#' Tokenオブジェクトからプラットフォームを判定する
#'
#' @param token Token
#' @return "google", "facebook" or "twitter"
#' @noRd
guessTokenPlatform <- function(token) {
  if (is.null(token$endpoint) || is.null(token$endpoint$authorize)) {
    return(NULL)
  }

  auth_endpoint <- token$endpoint$authorize

  for (name in c("google", "facebook", "twitter")) {
    if (auth_endpoint == httr::oauth_endpoints(name)$authorize) {
      return(name)
    }
  }

  return("unknown")
}
