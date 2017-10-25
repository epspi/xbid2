html_str2 <- readr::read_file("www/_grid_product.html")

htmlTemplate(text_ = html_str2, document_ = F,
             item_link = "abc.html",
             img_link = "def.html",
             short_desc = "some cool product",
             msrp = "223.53"
)
ui_closure <- function(template) {
  html_text <- readr::read_file(template)
  
  function(...) {
    htmlTemplate(text_ = html_text, document_ = F, ...)
  }
}
GenProductsGrid <- function(products, template) {
  products %>%
    apply(1, function(product) {
      htmlTemplate(text_ = template, document_ = F,
                   item_link = "abc.html",
                   img_link = "def.html",
                   short_desc = "some cool product",
                   msrp = "223.53"
      )
    })
}

sprintf_factory <- function(template, reactive){
  text <- readr::read_file(template)
  
  function() {
    res <- reactive()
    
    sprintf(text,
            strftime(res$date, format = "%a %r", tz = kTZ),
            res$link.item, 
            res$img_src,
            res$description_html, 
            res$buttons_html)
  }
}

sprintf_factory("www/_grid_product_sprintf.html")

GenProductsGrid <- function(template_str, res) {
  sprintf(template_str,
          res$link.item,
          res$img_src,
          res$link.item,
          res$Item,
          res$MSRP
  )
}
  