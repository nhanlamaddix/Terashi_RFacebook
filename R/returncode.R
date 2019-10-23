
has_oauth2.0_return_code <- function(params) {
  all(c("code", "state") %in% names(params))
}

has_oauth1.0_return_code <- function(params) {
  all(c("oauth_verifier", "oauth_token") %in% names(params))
}

check_security_code <- function(params,securityCode) {
  has_oauth2.0_return_code(params) && (params$state == securityCode)
}

is_twitter_return_params <- function(params) {
  # OAuth1.0なら無条件でTwitterと判定している。
  # 本来はリダイレクトURLやoauth_tokenを使って判断する。
  has_oauth1.0_return_code(params)
}

google_return_code <- function(params,
                               securityCode = getOption("gftShinyAuthR.google.securitycode")) {
  if (check_security_code(params, securityCode)) {
    return(params$code)
  } else {
    return(NULL)
  }
}

facebook_return_code <- function(params,
                                 securityCode = getOption("gftShinyAuthR.facebook.securitycode")) {
  if (check_security_code(params, securityCode)) {
    return(params$code)
  } else {
    return(NULL)
  }
}

twitter_return_verifier <- function(params) {
  if (is_twitter_return_params(params)) {
    return(params$oauth_verifier)
  } else {
    return(NULL)
  }
}

twitter_return_token <- function(params) {
  if (is_twitter_return_params(params)) {
    return(params$oauth_token)
  } else {
    return(NULL)
  }
}

authReturnCode <- function(session,
                           google.securityCode = getOption("gftShinyAuthR.google.securitycode"),
                           facebook.securityCode = getOption("gftShinyAuthR.facebook.securitycode")) {
  check_package_loaded("shiny")
  pars <- shiny::parseQueryString(session$clientData$url_search)

  code <- google_return_code(pars, securityCode = google.securityCode)
  if (!is.null(code)) {
    result <- c("code" = code)
    attr(result, "platform") <- "google"
    return(result)
  }

  code <- facebook_return_code(pars, securityCode = facebook.securityCode)
  if (!is.null(code)) {
    result <- c("code" = code)
    attr(result, "platform") <- "facebook"
    return(result)
  }

  verifier <- twitter_return_verifier(pars)
  token <- twitter_return_verifier(pars)
  if (!is.null(verifier) && !is.null(token)) {
    result <- c("token" = token, "verifier" = verifier)
    attr(result, "platform") <- "twitter"
    return(result)
  }

  return(NULL)
}
