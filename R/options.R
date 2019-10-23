.onLoad <- function(libname, pkgname) {
  op <- options()

  google_securityCode <- getOption("googleAuthR.securitycode")
  if (is.null(google_securityCode)) {
    google_securityCode <- createCode()
  }

  op.gftShinyAuthR <- list(
    gftShinyAuthR.google.securitycode = google_securityCode,
    gftShinyAuthR.facebook.securitycode = createCode(),
    gftShinyAuthR.google.client_id = getOption("googleAuthR.webapp.client_id"),
    gftShinyAuthR.google.client_secret = getOption("googleAuthR.webapp.client_secret"),
    gftShinyAuthR.google.scopes.selected = getOption("googleAuthR.scopes.selected"),
    gftShinyAuthR.google.oauth_cache = ".httr-oauth-google",
    gftShinyAuthR.facebook.oauth_cache = ".httr-oauth-facebook",
    gftShinyAuthR.twitter.oauth_cache = ".httr-oauth-twitter",
    gftShinyAuthR.redirect_uri = "https://127.0.0.1:4155/"
  )
  toset <- !(names(op.gftShinyAuthR) %in% names(op))
  if (any(toset)) options(op.gftShinyAuthR[toset])

  Sys.setenv(TWITTER_PAT = getOption("gftShinyAuthR.twitter.oauth_cache"))

  invisible()
}

createCode <- function(seed=NULL, num=20){
  if (!is.null(seed)) set.seed(seed)

  paste0(sample(c(1:9, LETTERS, letters), num, replace = T), collapse='')
}
