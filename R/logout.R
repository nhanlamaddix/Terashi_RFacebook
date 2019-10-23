#' @export
deleteAccessToken <- function(platform) {
  path <- getOption(stringr::str_c("gftShinyAuthR.", platform, ".oauth_cache"))
  if (file.exists(path)) {
    unlink(path)
  }

  if (platform == "google") {
    googleAuthR::Authentication$set("public", "token", NULL, overwrite = TRUE)
  }
  if (platform == "facebook") {
    FacebookAuthentication$set("public", "token", NULL, overwrite = TRUE)
  }
  if (platform == "twitter") {
    # TODO
  }
}
