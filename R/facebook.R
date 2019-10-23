#' Facebook APIを呼び出す
#'
#' @param path 呼び出したいAPIエンドポイントのパスからバージョン部分を抜いたもの。例: "/me?fields=id,name"
#' @param token アクセストークンをTokenオブジェクトまたは文字列で渡す
#' @param api_version APIバージョン。例: "v3.2"
#' @return APIレスポンスのJSONをリストオブジェクトとしてパースしたもの
#' @examples
#' callFacebookAPI("/me?fields=id,name")
#' @export
callFacebookAPI <- function(path, token = NULL, api_version = "v3.2") {
  url <- httr::parse_url(path)
  url$scheme <- "https"
  url$hostname <- "graph.facebook.com"
  if (!stringr::str_detect(url$path, "^/?v[\\d\\.]+/")) {
    url$path <- stringr::str_c("/", api_version, url$path)
  }

  if (is.null(token)) {
    token <- FacebookAuthentication$public_fields$token
  }

  if ("Token" %in% class(token)) {
    token <- token$credentials$access_token
  }
  if ("access_token" %in% names(url$query)) {
    token <- url$query$access_token
  }
  if (!is.character(token)) {
    stop("token should be a Token object or a character vector")
  }
  url$query$access_token <- token

  url <- httr::build_url(url)
  response <- httr::GET(url)
  content <- rawToChar(response$content)
  jsonlite::fromJSON(content)
}
