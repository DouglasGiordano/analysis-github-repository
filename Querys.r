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
