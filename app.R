# app.R
require(dplyr)
require(shiny)
require(shinyjs)
require(httr)
source("router.R")

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
  "/shop-grid-ls" = uiShopGrid,
  "/login" = uiLogin
)

# ============== UI ================

ui <- htmlTemplate("www/shell.html", 
                   content = uiOutput("body", class = "offcanvas-wrapper"),
                   # content = uiIndex,
                   document_ = T
)

# ============== SERVER ================
server <- function(input, output, session) {
  
  #  ------->> Authentication ---------
  
  # observe(print(input$token))
  usr_profile <- eventReactive(input$token, {
    if(input$token == "") {
      cat("No token!\n")
      return(NULL)
      
    } else {
      cat("\ntoken:", input$token, "\n\n")
      res <- POST(url = "https://epspi.auth0.com/tokeninfo",
                  body = list(id_token = input$token),
                  encode = "form")
      if (res$status_code == 200) {
        result <- content(res)
        session$sendCustomMessage("profile_handler", jsonlite::toJSON(result))
        cat("LOGGED IN PROFILE:\n")
        # print(result)
        print(jsonlite::toJSON(result, pretty = 4, auto_unbox = T))
        return(result)
        
      } else {
        session$sendCustomMessage("expired_handler", "")
        runjs("lock.show();")
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
  cat(route_script)
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
}


shinyApp(ui = ui, server = server)
