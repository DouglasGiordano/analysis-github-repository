library(dplyr)
create.social.network <- function(name, owner){
  library(plyr)
  library(RMySQL)
  library(rvest)#https://www.datacamp.com/community/tutorials/r-web-scraping-rvest use for scraping data html
  library(purrr)#function map to get unique element html
  mydb = dbConnect(MySQL(), user='root', password='root', dbname='simple_github', host='127.0.0.1')
  
  #issue
  rsI = dbSendQuery(mydb, paste0("select  id, title as text, author, createdat, id as parent 
                                 from issue 
                                 where owner = '",owner,"' and name = '",name,"'"))
  issues = fetch(rsI, n=-1)
  rsIC = dbSendQuery(mydb, paste0("select id, bodyhtml as text, author, createdat, issue as parent 
                     from issuecomment 
                     where owner = '",owner,"' and name = '",name,"'"))
  comments = fetch(rsIC, n=-1)
  
  #pull
  rsP = dbSendQuery(mydb, paste0("select id, title as text, author, createdat, id as parent 
                    from pullrequest
                    where owner = '",owner,"' and name = '",name,"'"))
  pulls = fetch(rsP, n=-1)
  rsPC = dbSendQuery(mydb, paste0("select id, bodyhtml as text, author, createdat, pull as parent 
                     from pullcomment 
                     where owner = '",owner,"' and name = '",name,"'"))
  pullcomments = fetch(rsPC, n=-1)

  all <- rbind(issues, pulls)
  allcomment <- rbind(comments, pullcomments)
  nrow.all <- nrow(all)
  for(row in 1:nrow.all){
    edges <- data.frame()
    #names(edges)<-c("project_name","project_owner", "user_source", "user_target", "type", "date_time", "source")
    comment = filter(allcomment, parent == all$id[row])
    comment$createdat = as.POSIXct(comment$createdat, "UTC", "%Y-%m-%dT%H:%M:%SZ")
    length.comment = nrow(comment)
    if(length.comment > 1){
      for(i in 1:length.comment){
        # An ISO-8601 encoded UTC date string.
        data = comment$createdat[i]
        text = comment$text[i]
        user = comment$author[i]
        commentLast = filter(comment, createdat < data)
        edge = NULL
        
        direct.mentions = search.direct.mention(text)
        indirect.mentions = search.indirect.mention(text,commentLast)
        not.mentions = search.not.mention(commentLast, direct.mentions, indirect.mentions)
        edges = rbind(edges, get.edges.mention(comment[i,], direct.mentions, indirect.mentions, not.mentions))
      }
    }
    edges = na.omit(edges)
    if(!is.null(edges) & !empty(edges)){
      save.edges(edges, mydb)
    }
    message("process ",row,"/ ",nrow.all)
  }
}

save.edges <-function(edges, mydb){
  values <- paste0(apply(edges, 1, function(x) paste0("('", paste0(x, collapse = "', '"), "')")), collapse = ", ")
  query <- paste0("INSERT INTO edge (user_target,type,project_name,project_owner,date_time,source,user_source) VALUES ", values, " ON DUPLICATE KEY UPDATE user_target = user_target;")
  dbSendQuery(mydb, query)
}

get.edges.mention <- function(comment, direct.mentions, indirect.mentions, not.mentions){
  #https://www.pmg.com/blog/insert-r-data-frame-sql%EF%BB%BF/
  data.frame.edges = data.frame()
  #names(data.frame.edges)<-c("user_target","type")
  if(!is.null(direct.mentions)){
    for(i in 1:length(direct.mentions)){
      user = gsub("@", "", direct.mentions[i], fixed = TRUE)
      data.frame.edges<-rbind(data.frame.edges, data.frame(user_target = user,
                                                           type = "D"))
    }
  }
  
  if(!is.null(indirect.mentions)){
    for(i in 1:length(indirect.mentions)){
      user = gsub("@", "", indirect.mentions[i], fixed = TRUE)
      data.frame.edges<-rbind(data.frame.edges, data.frame(user_target = user,
                                                           type = "I"))
    }
  }
  
  if(!is.null(not.mentions)){
    for(i in 1:length(not.mentions)){
      user = gsub("@", "", not.mentions[i], fixed = TRUE)
      data.frame.edges<-rbind(data.frame.edges, data.frame(user_target = user,
                                                           type = "S"))
    } 
  }
  if(nrow(data.frame.edges) == 0){
    return(NULL)
  }
  data.frame.edges$project_name = "name"
  data.frame.edges$project_owner = "owner"
  data.frame.edges$date_time = comment$createdat
  data.frame.edges$source = comment$parent
  data.frame.edges$user_source = comment$author
  return(data.frame.edges)
}

search.not.mention <- function(commentLast, direct.mentions, indirect.mentions){
  not.mentions = c()
  if(length(direct.mentions)== 0 & length(indirect.mentions)== 0){
    for(h in 1:nrow(commentLast)){
      not.mentions = c(not.mentions, commentLast$author[h])
    }
  }
  return(not.mentions)
}

#procura as menções diretas no texto do comentário em HTML
search.direct.mention <- function(text){
  mentions = c()
  if(grepl("user-mention", text, fixed=TRUE)){
    mentions.html = read_html(text) %>% html_nodes('.user-mention')
    mentions.length = length(mentions.html)
    for(i in 1:mentions.length){
      mention = mentions.html[i] %>% html_text()
      mentions = c(mentions,mention)
    }
  }
  return(mentions)
}

#procura as menções indiretas no texto do comentário em HTML
#toda menção indireta é encontrada na tag HTML blockquote
#toda vez que um blockquote for encontrar devemos pesquisar 
#se o texto é uma referencia a outros coment?rios
search.indirect.mention <- function(text, comment){
  mentions = c()
  if(grepl("<blockquote>", text, fixed=TRUE)){
    blockquote.html = read_html(text) %>% html_nodes('blockquote')
    p.html = blockquote.html  %>% html_nodes('p')
    comment.p = NULL
    if(length(p.html) != 0){
      if(grepl("wrote:", p.html[1], fixed=TRUE)){
        comment.p <- p.html[2] %>% html_text()
      } else {
        comment.p <- p.html[1] %>% html_text()
      }
    } else {
      return()
    }
    
    length.comments = nrow(comment)
    if(length.comments > 1){
      for(h in 1:length.comments){
        if(grepl(comment.p, comment$text[h], fixed=TRUE)){
          mention <- comment$author[h]
          mentions = c(mentions,mention)
        }
      }
    } else if(length.comments == 1){
        if(grepl(comment.p, comment$text, fixed=TRUE)){
          mention <- comment$author
          mentions = c(mentions,mention)
        }
    }
      
  }
  return(mentions)
}
