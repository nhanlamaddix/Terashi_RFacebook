#' @export
gftShinyAuthUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::uiOutput(ns("ui"))
}

#' @export
gftShinyAuth <- function(input, output, session, platform = "google", redirect.uri = getOption("gftShinyAuthR.redirect_uri"),
                         login_text = "Login via {platform}", login_class = "btn btn-primary",
                         logout_text = "Logout from {platform}", logout_class = "btn btn-default"){

  shiny::observeEvent(1, {
    params <- shiny::parseQueryString(session$clientData$url_search)
    code <- authReturnCode(session)
    if(is.null(code)){
      return(NULL)
    }

    if (platform != attr(code, "platform")) {
      return(NULL)
    }

    access_token <- NULL
    if(platform == "google") {
      access_token <- google_shiny_getToken(code = code, redirect.uri = redirect.uri)
    } else if (platform == "facebook") {
      access_token <- facebook_shiny_getToken(params = params, redirect.uri = redirect.uri)
    } else if (platform == "twitter") {
      access_token <- twitter_shiny_getToken(params)
      if (!is.null(access_token$credentials$screen_name) && is.character(access_token$credentials$screen_name)) {
        Sys.setenv(TWITTER_SCREEN_NAME = access_token$credentials$screen_name)
      }
    }

    saveAccessToken(access_token)
    loadAccessToken(platform)

    return(access_token)
  })

  logoutCount <- shiny::reactiveVal(0)
  shiny::observeEvent(input$logout, {
    deleteAccessToken(platform)
    logoutCount(shiny::isolate(logoutCount()) + 1)
  })

  accessToken <- shiny::reactive({
    logoutCount()
    getLoadedAccessToken(platform)
  })

  loggedIn <- shiny::reactive({
    !is.null(accessToken())
  })

  output$ui <- shiny::renderUI({
    if (loggedIn()) {
      logout_text <- stringr::str_replace_all(logout_text, "\\{platform\\}", stringr::str_to_title(platform))
      button <- shiny::actionButton(inputId = session$ns("logout"), label = logout_text, class = logout_class, role = "button")
    } else {
      # OAuth認証ページURLを生成する
      if (platform == "google") {
        authUrl <- google_shiny_getAuthUrl(redirect.uri = redirect.uri)
      }
      if (platform == "facebook") {
        # Facebook Appの仕様上、リダイレクトURIはHTTPSになっている必要がある。
        # しかしローカル環境で起動されるShinyのURLはHTTP (e.g. http://127.0.0.1:4155) になってしまう。
        # リバースプロキシなどを通したShiny Server環境ならばHTTPSのURLが利用できる。
        # ローカル環境の場合は https://127.0.0.1:4155 のURLにリダイレクトされてきたとき
        # 手でURLをhttpに書き換えるなどの対応が必要になる。
        authUrl <- facebook_shiny_getAuthUrl(redirect.uri = redirect.uri)
      }
      if (platform == "twitter") {
        authUrl <- twitter_shiny_getAuthUrl(redirect.uri)
      }

      # ボタンを作成
      login_text <- stringr::str_replace_all(login_text, "\\{platform\\}", stringr::str_to_title(platform))
      button <- shiny::actionLink(
        inputId = session$ns("login"),
        label = shiny::a(login_text, href = authUrl, class = login_class, role = "button")
      )
    }

    # UI
    shiny::tagList(
      button
    )
  })

  return(accessToken)
}
