google_shiny_getAuthUrl <-
  function(redirect.uri,
           state = getOption("gftShinyAuthR.google.securitycode"),
           client.id     = getOption("gftShinyAuthR.google.client_id"),
           client.secret = getOption("gftShinyAuthR.google.client_secret"),
           scope         = getOption("gftShinyAuthR.google.scopes.selected"),
           access_type   = c("online","offline"),
           approval_prompt = c("auto","force")) {

    access_type <- match.arg(access_type)
    approval_prompt <- match.arg(approval_prompt)

    scopeEnc <- paste(scope, sep='', collapse=' ')

    ## httr friendly version
    url <- httr::modify_url(
      httr::oauth_endpoints("google")$authorize,
      query = list(response_type = "code",
                   client_id = client.id,
                   redirect_uri = redirect.uri,
                   scope = scopeEnc,
                   state = state,
                   access_type = access_type,
                   approval_prompt = approval_prompt))
    url
  }

facebook_shiny_getAuthUrl <-
  function(redirect.uri,
           state = getOption("gftShinyAuthR.facebook.securitycode"),
           client.id     = getOption("gftShinyAuthR.facebook.client_id"),
           client.secret = getOption("gftShinyAuthR.facebook.client_secret"),
           scope         = getOption("gftShinyAuthR.facebook.scopes.selected")) {

    token_factory <- facebook_Token2.0_factory$new(consumer_key = client.id,
                                                   consumer_secret = client.secret,
                                                   redirect_uri = redirect.uri)
    token_factory$create_authorize_url(scope = scope, state = state)
  }

twitter_shiny_getAuthUrl <-
  function(redirect.uri,
           client.id     = getOption("gftShinyAuthR.twitter.client_id"),
           client.secret = getOption("gftShinyAuthR.twitter.client_secret")) {
    token_factory <- twitter_Token1.0_factory$new(consumer_key = client.id,
                                                  consumer_secret = client.secret,
                                                  redirect_uri = redirect.uri)
    token_factory$create_authorize_url()
  }

