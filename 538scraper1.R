#538scraper
library(dplyr)
library(jsonlite)
library(httr)
library(rvest)

url <-"https://projects.fivethirtyeight.com/2020-nba-predictions/games/"
RAPpred <- html_text(html_nodes(read_html(url), ".number"))
RAPpred <- matrix(RAPpred, nrow = 100, byrow = T)  

for(i in 1:nrow(RAPpred)){
RAPpred[i,2] <- as.numeric(gsub("%", "", RAPpred[i,2]))
RAPpred[i,5] <- as.numeric(gsub("%", "", RAPpred[i,5]))
}



r <- httr::GET("https://projects.fivethirtyeight.com/2020-nba-predictions/data.json")

dat <- jsonlite::fromJSON(content(r, as = "text")) %>% 
purrr::pluck("games")



dat <- dat[dat$status=="post",]
dat$true_spread <- dat$score2 - dat$score1


RightWin <- function(score1, score2, prob1){
  if(prob1 >= .5){
    if(score1 > score2){
      return('Y')
    } else{return('N')}
  } else if(score2 > score1){
    return('Y')
  } else return('N')
}

#apply rightwin on score1 score2 prob1 

dat$elowin <- RightWin(dat$score1, dat$score2, dat$elo_prob1)
dat$eloRight <- mapply(RightWin, dat$score1, dat$score2, dat$elo_prob1)
dat$predRight <- mapply(RightWin, dat$score1, dat$score2, dat$rating_prob1)


table(dat$eloRight)
table(dat$predRight)

dat$elopred <- mapply(identical, dat$eloRight, dat$predRight)
table(dat$elopred)

table(dat[dat$eloRight=='N', 5])
table(dat[dat$predRight=='N', 5])
table(dat[dat$eloRight=='N', 6])
table(dat[dat$predRight=='N', 6])
table(dat[((dat$predRight=='N') && (dat$elopred == T)), 5])

hist(dat$true_spread)
hist(dat$elo_spread)

dat$dist_from_even_elo <- abs(.5 - dat$elo_prob1)
dat$dist_from_even_pred <- abs(.5 - dat$rating_prob1)
boxplot(dat$dist_from_even_elo ~ dat$eloRight)
boxplot(dat$dist_from_even_pred ~ dat$predRight)
boxplot(dat$dist_from_even_pred ~ dat$both_right, xlab = "Both Correct", ylab = "Distance from even probability, RAPTOR model", col = c("lightcoral","cyan"))

dat$both_wrong <- mapply(BothWrong, dat$eloRight, dat$predRight)
dat$both_right <- mapply(BothRight, dat$eloRight, dat$predRight)

table(dat[dat$both_wrong==T,5]) + table(dat[dat$both_wrong==T,6])
table(dat$both_wrong)

dat$hybrid_prob1 <- (dat$elo_prob1 + dat$rating_prob1) * .5
dat$hybridRight <- mapply(RightWin, dat$score1, dat$score2, dat$hybrid_prob1)

dat$fav_coversE <- mapply(FavoriteCover, dat$true_spread, dat$elo_spread)
dat$fav_coversR <- mapply(FavoriteCover, dat$true_spread, dat$rating_spread)

table(dat$fav_coversE)
table(dat$fav_coversR)

plot(dat$elo_prob1, dat$elo_spread)
plot(dat$rating_prob1, dat$rating_spread)

boxplot(dat$dist_from_even_elo ~ dat$fav_coversE)
boxplot(dat$dist_from_even_pred ~ dat$fav_coversR)

a <- hist(dat[dat$fav_coversR==T, "rating_prob1"])
b <- hist(dat$rating_prob1)
a$counts / b$counts

c <- hist(dat[dat$fav_coversR==T, "dist_from_even_elo"])
d <- hist(dat$dist_from_even_elo, breaks = 9)
c$counts / d$counts

BothWrong <- function(eloRight, predRight){
if((eloRight=='N') && (predRight=='N')){
  return(T)
}else return(F)
}

BothRight <- function(eloRight, predRight){
  if((eloRight=='Y') && (predRight=='Y')){
    return(T)
  }else return(F)
}


c(1,2,3,7,8)

#positive implies team2 favored
#negative implies team1 favored
#true spread is team2MOV

#if spread pk, NA

FavoriteCover <- function(true_spread, pred_spread){
  if((pred_spread > 0) && (pred_spread > true_spread)){
    return(T)
  }else if((pred_spread < 0) && (pred_spread < true_spread )){
    return(T)
  }else if(pred_spread == 0){
    return(NA)
  }else return(F)
}

