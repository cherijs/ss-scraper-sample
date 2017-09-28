
library(magrittr)

pacman::p_load(dplyr, ggplot2, jsonlite, pander)

joinAll <- function() {  
  df <- read_json("data/cars-20170927.json", flatten=T, simplifyVector=T)
  
  df$url <- NULL
  df$vin <- NULL
  df$mileage <- as.numeric(as.character((df$mileage)))
  
  brands <- read.csv("data/brands.csv")
  
  brand2country <- setNames(brands[,2], brands[,1])
  brand2region <- setNames(brands[,3], brands[,1])
  
  df$brand_country <- sapply(df$brand, function (x) { brand2country[[x]] })
  df$brand_egion <- sapply(df$brand, function (x) { brand2region[[x]] })
  
  df %>%
    select(-starts_with('_')) %>%
    unclass %>%
    as.data.frame(stringsAsFactors=T) %>%
    mutate(age_mins = difftime(crawled, created, units="mins")) %>%
    mutate(age_mins = ifelse(age_mins > 0, age_mins, NA)) %>%
    mutate(age_days = round(age_mins / (24 * 60))) %>%
    mutate(age_weeks = round(age_mins / (24 * 60 * 7))) %>%
    mutate(created_weekday = factor(weekdays(created), levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>%
    mutate(created_week_number = strftime(df$created, format="%W"))
}

top10agg <- function(df, f, var1, var2) {
  prc <- data.frame(
    Var1=var1,
    Var2=var2
  )
  
  ordered.prc <- prc[order(prc[,2], decreasing = T),]
  ordered.prc[,1] <- factor(ordered.prc[[1]], levels=rev(ordered.prc[,1]))
  top10 <- ordered.prc[1:10,]
  
  formula.attrs <- attributes(terms(as.formula(f)))
  
  colnames(top10) <- rev(rownames(formula.attrs$factors)[1:2])
  return(top10)
}

top10length <- function(df, f) {
  res <- aggregate(f, data=df, length)
  
  return(top10agg(df, f, res[,1], res[,2]))
}

top10mean <- function(df, f, digits=2) {
  totals <- aggregate(f, data=df, sum)
  satisfying <- aggregate(f, data=df, length)
  res <- round(totals[,2] / satisfying[,2], digits)
  
  return(top10agg(df, f, satisfying[,1], res))
}

top10countprc <- function(df, f) {
  totals <- aggregate(f, data=df, sum)
  satisfying <- aggregate(f, data=df, length)
  res <- round(totals[,2] / satisfying[,2] * 100, 2)
  
  return(top10agg(df, f, satisfying[,1], res))
}

plotAndGrid <- function(df, var1, var2) {
  library(gridExtra)
  
  scatter <- ggplot(df) + 
    aes_string(var1, var2) +
    geom_point(position = "jitter") + 
    theme(legend.position=c(1,1),legend.justification=c(1,1)) 
  
  plot_top <- ggplot(df.correct.mileage) + 
    aes_string(var1) +
    geom_density(alpha=.5) + 
    theme(legend.position = "none")     
  
  grid.arrange(plot_top, scatter, ncol=1, nrow=2, widths=c(4), heights=c(2, 4)) 
}