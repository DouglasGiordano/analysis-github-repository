get.turnover.metric = function(interactions, users){
  interactions$date  = gsub("[A-Za-z]"," " , interactions$date ,ignore.case = TRUE)
  interactions$date = substring(interactions$date, 1, 19)
  interactions$date = as.Date(as.POSIXct(interactions$date, "UTC", "%Y-%m-%d %H:%M:%S"))
  #define dates project
  date.start =  min(interactions$date)
  date.end =  max(interactions$date)
  significance = c()
  #looping users
  for (row in 1:length(users)) {
    user = users[row]
    user.interactions = interactions[interactions$user == user,]
    if(nrow(user.interactions) < 3){
      significance = c(significance, NA)
    } else {
      user.date.end = max(user.interactions$date)
      list.interval = diff(sort(user.interactions$date))
      days.nointeraction = difftime(date.end, user.date.end, units = "days")
      significance = c(significance, get.turnover.significance(as.numeric(list.interval), days.nointeraction))
    }
  }
  return(significance)
}

get.turnover.significance <- function(list.interval, interval){
  significance <- NA
  significance <- ((as.numeric(interval) - mean(list.interval))/sd(list.interval))
  return(significance)
}