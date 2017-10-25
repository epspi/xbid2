# app.R
require(dplyr)
require(shiny)
require(shinyjs)
require(httr)
source("router.R")
source("global.R")


# Server-side rendered content definitions
uiSearchResults <- enclose(function() {
    grid_template <- read_file("www/_grid_product_sprintf.html")
    list_template <- read_file("www/_list_product_sprintf.html")
    search_template_grid <- read_file("www/_search_results.html")
    search_template_list <- read_file("www/_search_results_list.html")
    
  function(search_res, grid = T) {
    if (grid) {
      htmlTemplate(
        text_ = search_template_grid, 
        n_items = nrow(search_res),
        products =  GenProductsGrid(grid_template, search_res), 
        document_ = F)
    } else {
      htmlTemplate(
        text_ = search_template_list, 
        n_items = nrow(search_res),
        products =  GenProductsList(list_template, search_res), 
        document_ = F)
    }
  }
})
uiIndex <- tagList(
  htmlTemplate("www/_main_slider.html"),
  htmlTemplate("www/_promo.html"),
  htmlTemplate("www/_top_cats.html")
)
uiCart <- function() htmlTemplate("www/_cart.html")
uiLogin <- function() htmlTemplate("www/_account-login.html")
uiAccountOrders <- function() htmlTemplate("www/_account-orders.html")
uiAccountProfile <- function() htmlTemplate("www/_account-profile.html")
uiAccountWishlist <- function() htmlTemplate("www/_account-wishlist.html")

routenames <- list(
  "/",
  "/cart",
  "/account-orders",
  "/account-profile",
  "/account-wishlist",
  "/search",
  "/login"
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
  
  if (file.exists(timestamp_loc)) {
    lastTime <- GetLastTimestamp(timestamp_loc, kTimeFileFormat)
    cat("Last Scrape:  ", 
        format(lastTime, kTimeFileFormat, usetz = T, tz = kTZ), "\n")
  } else {
    lastTime <- curTime - auto_refresh_time
    cat("No Scrape History Found!!\n\n")
  }
  
  # Status Updates
  cat("Current Time: ", 
      format(curTime, kTimeFileFormat, usetz = T, tz = kTZ), "\n")
  cat("Refresh Due:  ",
      format(lastTime + auto_refresh_time, kTimeFileFormat,
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
      # cat("\nid_token:", input$id_token, "\n\n")
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
  route_script <- MakeRouter(routenames)
  cat(route_script)
  path <- reactiveVal("/")
  runjs(route_script)
  
  
  observeEvent(input$route_clicked, {
    if (input$route_clicked != path()) {
      cat("route:", input$route_clicked, "\n")
      path(input$route_clicked)
    }
  })
  
  # --Server routed page--
  output$body <- renderUI({
    shiny::validate(
      need(usr_profile(), "Please sign in" )
    )
    
    route <- path()
    switch(route,
           "/" = uiIndex,
           "/cart" = uiCart(),
           "/account-orders" = uiAccountOrders(),
           "/account-profile" = uiAccountProfile(),
           "/account-wishlist" = uiAccountWishlist(),
           "/search" = uiSearchResults(search_res$data),
           "/login" = uiLogin(),
           ""
    )
  })
  
  
  #  ------->> Search ---------
  
  ## REACTIVE: Search result set
  search_res <- reactiveValues(query = NULL, data = NULL)
  
  
  ## OBSERVER: Search input button
  observeEvent(input$searchSubmit, {
    shiny::validate(
      need(input$searchText, "" ),
      need(usr_profile(), "Please sign in" )
    )
    search_res$query <- input$searchText
  })
  
  ## OBSERVER: Do search based on new query
  observe({
    search_res$query
    # input$locSelect
    validate(
      need(search_res$query, "Nothing to search for..." )
    )
    
    isolate({
      search_res$data <- search_res$query %>% 
        SearchWrapper(search_df = items_df,
                      join_df = auctions_df,
                      # favs_df = favorites$data,
                      favs_df = NULL
        )
      
      # Go to list or grid view depending on result
      if (nrow(search_res$data) >= kMaxPins) {
        
      } else {
        
      }
    })
  })
  
  # observe({print(names(session$clientData))})
  observe({cat("url hash:", session$clientData$url_hash)})
  # , "\n",
  #              "url hash initial:", session$clientData$url_hash_initial, "\n")})
  # 
  observe({cat("url_search:", session$clientData$url_search, "\n")})
  
  
}


shinyApp(ui = ui, server = server)
