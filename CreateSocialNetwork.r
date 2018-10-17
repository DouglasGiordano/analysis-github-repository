create.social.network <- function(){
  library(dplyr)
  mydb = dbConnect(MySQL(), user='root', password='root', dbname='simple_github', host='127.0.0.1')
  rsI = dbSendQuery(mydb, "select id, title as text, author, createdat, id as parent from issue")
  issues = fetch(rsI, n=-1)
  rsIC = dbSendQuery(mydb, "select id, bodyhtml as text, author, createdat, issue as parent from issuecomment")
  comments = fetch(rsIC, n=-1)
  for(row in 1:nrow(issues)){
    comment = filter(comments, parent == issues$id[row])
    comment$createdat = as.POSIXct(comment$createdat, "UTC", "%Y-%m-%dT%H:%M:%SZ")
    print(nrow(comment))
    for(i in 1:nrow(comment)){
      # An ISO-8601 encoded UTC date string.
      data = comment$createdat[i]
      commentLast = filter(comment, createdat < data)
      text = comment$text[i]
      print(nchar(text))
      mentions <- NULL


    }
  }
}

#procura as menções diretas no texto do comentário em HTML
search.direct.mention <- function(text){
  mentions = c()
  if(grepl("user-mention", text, fixed=TRUE)){ #verifica se a issue tem alguma menção direta
    memtions.html = read_html(text) %>% html_nodes('.user-mention') 
    for(h in 1:nrow(memtions)){ #extrai cada uma das menções
      mention = memtions.html[i] %>% html_text()
    }
  }
}

#procura as menções indiretas no texto do comentário em HTML
#toda mençaõ indireta é encontrada na tag HTML blockquote
#toda vez que um blockquote for encontrar devemos pesquisar 
#se o texto é uma referencia a outros comentários
search.indirect.mention <- function(text){
  if(grepl("<blockquote>", text, fixed=TRUE)){
    res <- read_html(text) %>% html_nodes('blockquote') %>% html_text()
    if(is.character(res)){
      trecho <- read_html(text) %>% html_nodes('blockquote') %>% html_nodes('p')
      
      if(is.list(trecho)){
        message("is list")
        if(grepl("wrote:", trecho[1], fixed=TRUE)){
          trecho <- trecho[2] %>%
            html_text()
          message(trecho)
        } else {
          trecho <- trecho[1] %>%
            html_text()
        }
        
        for(h in 1:nrow(comment)){
          if(comment$id[i]!=comment$id[h]){
            message("Texto corresponde ", grepl(trecho, comment$text[h], fixed=TRUE))
            if(grepl(trecho, comment$text[h], fixed=TRUE)){
              memtions <- comment$author[h]
              message("Menção indireta: ", memtions)
            }
          }
        }
      }
      
    }
  }
}
