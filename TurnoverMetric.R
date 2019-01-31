get.turnover.metric = function(interactions, users){
  interactions$date  = gsub("[A-Za-z]"," " , interactions$date ,ignore.case = TRUE)
  interactions$date = substring(interactions$date, 1, 19)
  interactions$date = as.Date(as.POSIXct(interactions$date, "UTC", "%Y-%m-%d %H:%M:%S"))
  #define dates project
  date.start =  min(interactions$date)
  date.end =  max(interactions$date)
  significance = c()
  list.days.nointeraction = c()
  #looping users
  for (row in 1:length(users)) {
    user = users[row]
    user.interactions = interactions[interactions$user == user,]
    user.date.end = max(user.interactions$date)
    days.nointeraction = difftime(date.end, user.date.end, units = "days")
    if(nrow(user.interactions) < 3){
      significance = c(significance, NA)
    } else {
      list.interval = diff(sort(user.interactions$date))
      significance = c(significance, get.turnover.significance(as.numeric(list.interval), days.nointeraction))
    }
    list.days.nointeraction = c(list.days.nointeraction, days.nointeraction)
  }
  
  metrics = data.frame(matrix(NA, nrow = length(users), ncol = 1))
  metrics$user = users
  metrics$days_no_interaction = list.days.nointeraction
  metrics$significance = significance
  return(metrics)
}

get.turnover.significance <- function(list.interval, interval){
  significance <- NA
  significance <- ((as.numeric(interval) - mean(list.interval))/sd(list.interval))
  return(significance)
}



