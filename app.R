# app.R

require(jsonlite)
require(httr)
require(dplyr)

## ///////////////////////////////////////////// ##
## UI ---------------------------------------------
## ///////////////////////////////////////////// ##
ui <- htmlTemplate("www/index.html")


## ///////////////////////////////////////////// ##
## SERVER -----------------------------------------
## ///////////////////////////////////////////// ##
server <- function(input, output, session) {
  
  # ============================================================
  ## INPUT TOKEN -----------------------------------------------
  observe(print(usr_profile()))
  
  usr_profile <- eventReactive(input$token, {
    
    if(is.null(input$token)) {
      cat("No token!\n")
      return(NULL)
    } else {
      cat("\ntoken:", input$token, "\n\n")
      res <- POST(url = "https://epspi.auth0.com/tokeninfo",
                  body = list(id_token = input$token),
                  encode = "form")
      if (res$status_code == 200) {
        
        session$sendCustomMessage("profile_handler",
                                  jsonlite::toJSON(content(res))
        )
        
        result <- content(res)
        # result$favorites_loc <- result$user_id %>% 
        #   sha1 %>% 
        #   file.path(user_data_loc, .) %>% 
        #   paste0(".csv")
        # 
        # favorites$data <- GetFavorites(result)
        # print(favorites$data)
        
        return(result)
        
      } else {
        session$sendCustomMessage("expired_handler", "")
        return(NULL)
      }
    }
  })

}


shinyApp(ui = ui, server = server)
