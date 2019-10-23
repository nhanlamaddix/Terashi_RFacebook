google_shiny_getToken <- function(code,
                                  redirect.uri,
                                  client.id     = getOption("gftShinyAuthR.google.client_id"),
                                  client.secret = getOption("gftShinyAuthR.google.client_secret")){

  gar_app <- httr::oauth_app("google", key = client.id, secret = client.secret)

  scope_list <- getOption("gftShinyAuthR.google.selected")

  req <-
    httr::POST("https://accounts.google.com/o/oauth2/token",
         body = list(code = code,
                     client_id = client.id,
                     client_secret = client.secret,
                     redirect_uri = redirect.uri,
                     grant_type = "authorization_code"))

  stopifnot(identical(httr::headers(req)$`content-type`,
                      "application/json; charset=utf-8"))
  # content of req will contain access_token, token_type, expires_in
  token <- httr::content(req, type = "application/json")
  if(!is.null(token$error)){
    stop("Authentication error: ", token$error, token$error_description, call. = FALSE)
  }
  # Create a Token2.0 object consistent with the token obtained from gar_auth()
  httr::Token2.0$new(app = gar_app,
               endpoint = httr::oauth_endpoints("google"),
               credentials = list(access_token = token$access_token,
                                  token_type = token$token_type,
                                  expires_in = token$expires_in,
                                  refresh_token = token$refresh_token),
               params = list(scope = scope_list, type = NULL,
                             use_oob = FALSE, as_header = TRUE),
               cache_path = FALSE)

}

facebook_shiny_getToken <-
  function(params,
           redirect.uri,
           client.id     = getOption("gftShinyAuthR.facebook.client_id"),
           client.secret = getOption("gftShinyAuthR.facebook.client_secret")) {

    token_factory <- facebook_Token2.0_factory$new(consumer_key = client.id,
                                                   consumer_secret = client.secret,
                                                   redirect_uri = redirect.uri)
    token_factory$create_token(params)
  }

twitter_shiny_getToken <-
  function(params,
           client.id     = getOption("gftShinyAuthR.twitter.client_id"),
           client.secret = getOption("gftShinyAuthR.twitter.client_secret")) {

    token_factory <- twitter_Token1.0_factory$new(consumer_key = client.id,
                                                  consumer_secret = client.secret)
    token_factory$create_token(params)
  }
