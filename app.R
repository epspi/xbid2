# app.R
require(dplyr)
require(shiny)
require(shinyjs)
require(httr)
source("router.R")
source("global.R")

uiIndex <- tagList(
  htmlTemplate("www/main_slider.html"),
  htmlTemplate("www/_promo.html"),
  htmlTemplate("www/_top_cats.html"),
  # htmlTemplate("www/_footer.html"),
  tags$script("$('.owl-carousel').trigger('refresh.owl.carousel');")
)
uiCart <- tagList(
  htmlTemplate("www/_cart.html")
)
uiShopGrid <- tagList(
  htmlTemplate("www/_shop-grid-ls.html")
)
uiLogin <- tagList(
  htmlTemplate("www/_account-login.html")
)
routes <- list(
  "/" = uiIndex,
  "/cart" = uiCart,
  "/search" = uiShopGrid,
  "/login" = uiLogin
)

# ============== UI ================

ui <- htmlTemplate("www/shell.html", 
                   content = uiOutput("body", class = "offcanvas-wrapper"),
                   document_ = T
)

# ============== SERVER ================

# Some constants
auto_refresh_time <- 3600 * .5
ending_soon_time <- 3600 * 2

server <- function(input, output, session) {
  
  #  ------->> Startup ---------
  
  curTime  <- Sys.time()
  print("time")

  if (file.exists(timestamp_loc)) {
    lastTime <- GetLastTimestamp(timestamp_loc, kTimeFileFormat)
    cat("Last Scrape:  ", format(lastTime, kTimeFileFormat, usetz = T, tz = kTZ), "\n")
  } else {
    lastTime <- curTime - auto_refresh_time
    cat("No Scrape History Found!!\n\n")
  }
  print("print rescrape")

  # Status Updates
  cat("Current Time: ", format(curTime, kTimeFileFormat, usetz = T, tz = kTZ), "\n")
  cat("Refresh Due:  ",format(lastTime + auto_refresh_time, kTimeFileFormat,
                              usetz = T, tz = "EST5EDT"), "\n\n")

  # Rescrape if due time
  if (curTime >= lastTime + auto_refresh_time) Rescrape()

  # Filter out auctions that have already passed
  # Make sure auction expiration has right time zone
  # After loading auctions, filter out those which have expired
  auctions_df  <-  ReadAuctionsCSV(auctions_loc, kTimeFileFormat) %>%
    filter(date + kExpiredTimeOffset > curTime)

  # Filter out items from auctions that have passed
  items_df  <- ReadItemsCSV(items_loc) %>%
    filter(auction_id %in% auctions_df$auction_id) %>%
    mutate(MSRP = as.numeric(MSRP))

  
  
  #  ------->> Authentication ---------
  
  # observe(print(input$id_token))
  usr_profile <- eventReactive(input$id_token, {
    if(input$id_token == "") {
      cat("No token!\n")
      return(NULL)
      
    } else {
      cat("\nid_token:", input$id_token, "\n\n")
      # User info using id_token JWT
      res <- POST(url = "https://epspi.auth0.com/tokeninfo",
                  body = list(id_token = input$id_token),
                  encode = "form")
      
      # # User info using access_token
      # res <- GET(url = "https://epspi.auth0.com/userinfo",
      #            add_headers(Authorization = paste0("Bearer ", input$access_token))
      # )
      if (res$status_code == 200) {
        result <- content(res)
        session$sendCustomMessage("profile_handler", jsonlite::toJSON(result))
        cat("LOGGED IN PROFILE:\n")
        # print(result)
        print(jsonlite::toJSON(result, pretty = 4, auto_unbox = T))
        return(result)
        
      } else {
        session$sendCustomMessage("expired_handler", "")
        return(NULL)
      }
    }
  })
  
  observe({
    if(is.null(usr_profile())) {
      runjs("$('.account, .cart').remove();")
    }
  })
  
  
  #  ------->> Routing ---------
  
  # Register routes with director.js
  route_script <- MakeRouter(routes)
  # cat(route_script)
  path <- reactiveVal("/")
  runjs(route_script)
  
  
  observeEvent(input$route_clicked, {
    if (input$route_clicked != path()) {
      cat("route:", input$route_clicked, "\n")
      path(input$route_clicked)
    }
  })
  
  # --Body--
  output$body <- renderUI({
    if (is.null(usr_profile())) routes[["/login"]] else routes[[path()]]
  })
  # output$body <- renderUI({
  #   routes[[path()]]
  # })
  
  
  #  ------->> Search ---------
  
  ## REACTIVE: Search result set
  search_res <- reactiveValues(query = NULL, data = NULL)
  
  
  # ## OBSERVER: Filters
  # observe({print(input$locSelect)})
  # filtered_res <- reactive({
  # 
  #   res <- search_res$data
  #   if (!is.null(input$locSelect)) {
  #     res <- res %>%
  #       filter(location %in% input$locSelect)
  #   }
  #   res
  # })
  
  ## OBSERVER: Search input button
  observeEvent(input$searchSubmit , {
    shiny::validate(
      need(input$searchText, "" ),
      need(usr_profile(), "Please sign in" )
    )
    search_res$query <- input$searchText
    print(search_res$query)
  })
  
}


shinyApp(ui = ui, server = server)
