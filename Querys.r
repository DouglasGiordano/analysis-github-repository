query.issue.project <- function(mydb, owner, name){
  rsI = dbSendQuery(mydb, paste0("select  id, title as text, author, createdat, id as parent 
                                 from issue 
                                 where owner = '",owner,"' and name = '",name,"'"))
  issues = fetch(rsI, n=-1)
  return(issues)
}

query.issue.comment.project <- function(mydb, owner, name){
  rsIC = dbSendQuery(mydb, paste0("select id, bodyhtml as text, author, createdat, issue as parent 
                     from issuecomment 
                                  where owner = '",owner,"' and name = '",name,"'"))
  comments = fetch(rsIC, n=-1)
  return(comments)
}

query.pullrequest.project <- function(mydb, owner, name){
  rsP = dbSendQuery(mydb, paste0("select id, title as text, author, createdat, id as parent 
                    from pullrequest
                                 where owner = '",owner,"' and name = '",name,"'"))
  pulls = fetch(rsP, n=-1)
  return(pulls)
}

query.pullrequest.comment.project <- function(mydb, owner, name){
  rsPC = dbSendQuery(mydb, paste0("select id, bodyhtml as text, author, createdat, pull as parent 
                     from pullcomment 
                                  where owner = '",owner,"' and name = '",name,"'"))
  pullcomments = fetch(rsPC, n=-1)
  return(pullcomments)
}

query.edge.project <- function(mydb, owner, name){
  rs = dbSendQuery(mydb, paste0("select user_source as source, user_target as target, date_time as time
                     from edge 
                                  where project_owner = '",owner,"' and project_name = '",name,"' order by date_time asc"))
  edges = fetch(rs, n=-1)
  return(edges)
}


query.interaction.project <- function(mydb, owner, name){
  rs = dbSendQuery(mydb, paste0("SELECT 
                                commit.author AS user,
                                commit.authoredDate AS date
                                FROM
                                commit 
                                WHERE commit.owner = '",owner,"' and name = '",name,"'
                                UNION SELECT 
                                issue.author AS user,
                                issue.createdAt AS date
                                FROM
                                issue 
                                WHERE issue.owner = '",owner,"' and issue.name = '",name,"'
                                UNION SELECT 
                                pullrequest.author AS user,
                                pullrequest.createdAt AS date
                                FROM
                                pullrequest 
                                WHERE pullrequest.owner = '",owner,"' and pullrequest.name = '",name,"'
                                UNION SELECT 
                                pullcomment.author AS user,
                                pullcomment.createdAt AS date
                                FROM
                                pullcomment 
                                WHERE pullcomment.owner = '",owner,"' and pullcomment.name = '",name,"'
                                UNION SELECT 
                                issuecomment.author AS user,
                                issuecomment.createdAt AS date
                                FROM
                                issuecomment 
                                WHERE issuecomment.owner = '",owner,"' and issuecomment.name = '",name,"'"))
  edges = fetch(rs, n=-1)
  return(edges)
}

save.metric <-function(metrics, mydb){
  dbWriteTable(mydb, "metricproject", metrics, append = TRUE,row.names=FALSE)
}

save.metric.link <-function(metrics, mydb){
  dbWriteTable(mydb, "metricuser", metrics, append = TRUE,row.names=FALSE,header = TRUE)
}

save.metric.turnover <-function(metrics, mydb){
  dbWriteTable(mydb, "metricuser", metrics, append = TRUE,row.names=FALSE)
}
