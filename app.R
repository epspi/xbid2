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
                   document_ = T)

# ============== SERVER ================

server <- function(input, output, session) {
  
  
  #  ------->> Startup ---------
  
  # Check if rescrape needed
  curTime  <- Sys.time()
  CheckRescrapeDue(curTime)
  
  ## Read in data
  # Make sure auction expiration has right time zone
  # After loading auctions, filter out those which have expired
  auctions_df  <-  ReadAuctionsCSV(auctions_loc, kTimeFileFormat) %>%
    filter(date + kExpiredTimeOffset > curTime)
  
  # Filter out items from auctions that have passed
  items_df  <- ReadItemsCSV(items_loc) %>%
    filter(auction_id %in% auctions_df$auction_id) %>%
    mutate(MSRP = as.numeric(MSRP))
  
  
  #  ------->> Authentication ---------
  
  # --User Profile--
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
  
  # Modify dom for logged-off users
  # (Shouldn't be necessary as authentication automatically pops up)
  observe({
    if(is.null(usr_profile())) {
      runjs("$('.account, .cart').remove();")
    }
  })
  
  
  #  ------->> Routing ---------
  # Set the initial starting location
  initial_path <- "/"
  path <- reactiveVal(initial_path)
  
  # --Hash observer--
  observe({
    shiny::validate(
      # need(input$searchText, "" ),
      need(usr_profile(), "Please sign in" )
    )
    
    hash <- session$clientData$url_hash
    newpath <- gsub("#", "", hash)
    newpath <- ifelse(newpath == "", "/", newpath)
    
    # If url changed
    if (newpath != path()) {
      route <- gsub("[?].*", "", newpath)
      cat("\n--New URL--\nRoute:", route, "\n")
      
      # Search submission
      if (route == "/search") {
        q <- parseQueryString(gsub(".*[?]", "", newpath))
        cat("Query:\n ", 
            paste(names(q), unlist(q), sep = "=", collapse = "\n  "), "\n")
        last_query(q$term)
      }
      path(route)
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
           "/search" = uiSearchResults(search_res$data, 
                                       search_res$grid,
                                       search_res$filters),
           "/login" = uiLogin(),
           ""
    )
  })
  
  
  #  ------->> Search ---------
  
  # --Search result set--
  last_query <- reactiveVal(NULL)
  search_res <- reactiveValues(data = NULL, grid = T, filters = NULL)
  
  # --Do Search--
  observe({
    validate(
      need(last_query(), "Nothing to search for..." )
    )
    isolate({
      search_res$data <- last_query() %>% 
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
  
  
  
}


shinyApp(ui = ui, server = server)
