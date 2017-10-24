html_str2 <- readr::read_file("www/_grid_product.html")

htmlTemplate(text_ = html_str2, document_ = F,
             item_link = "abc.html",
             img_link = "def.html",
             short_desc = "some cool product",
             msrp = "223.53"
)

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

renderRoute <- function() {


}
