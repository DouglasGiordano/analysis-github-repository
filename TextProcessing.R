library(RCurl)
library(XML)
text.clear.text <- function(text){
  #text.html = read_html(text) %>% html_nodes('p')
  tryCatch(
    {
      doc = htmlParse(text)
      text.html <- xpathSApply(doc, "//p[not(code)]", xmlValue)#ignore code
      text.html <- paste(text.html, xpathSApply(doc, "//li[not(code)]", xmlValue), collapse = "\n")#ignore code
      text.html <- paste(text.html, collapse = "\n")
      
      return(as.character(text.html))
    },
    error=function(error_message) {
      message("Error clear text: ",text," - ", error_message)
    }
  )

}

test <- function(test){
  test = "<p>I got the same error in V10 enterprise.</p>
<p>What I did to fix the issue :</p>
  <ol>
  <li>re-Installed  all dependencies again</li>
  <li>Removed custom_addons from the path (So only load odoo standard modules)</li>
  <li>Updated the odoo to the latest V10 and also updated odoo enterprise folder to latest</li>
  <li>reinstall nodeJS using different methods.</li>
  <li>Removed all development databases and started fresh by creating a blank new db</li>
  </ol>
  <p>When the blank db is created I tried to open apps menu and below is the error.</p>
  <p><a target=\"_blank\" rel=\"noopener oreferrer\" href=\"https://cloud.githubusercontent.com/assets/4558819/21097776/311f5ea2-c0a1-11e6-92bd-0eb221a87bc4.png\"><img src=\"https://cloud.githubusercontent.com/assets/4558819/21097776/311f5ea2-c0a1-11e6-92bd-0eb221a87bc4.png\" alt=\"image\" style=\"max-width:100%;\"></a></p>"
  
  test2 = "          <p>okay, fixed here. One import in a module was wrong.</p>
<p><code>openerp.addons.web.http import Controller, route, request</code></p>
<p>made it to search http class in \"/openerp/addons/web/\". Changing it to</p>
<p><code>from openerp.http import request, route, Controller</code></p>
<p>helped so it searches the class in \"/odoo/addons/web/\"</p>"
  message(text.clear.text(test2))  
}

#examples for test

#https://github.com/php/php-src/pull/1927  codes
#https://github.com/odoo/odoo/issues/29824  codes
#https://github.com/odoo/odoo/issues/29820  lists
#https://github.com/odoo/odoo/issues/29735  images
#https://github.com/odoo/odoo/issues/11627 all situations