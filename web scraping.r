## ----packages------------------------------------------------------------
library(robotstxt)
library(rvest)
library(selectr)#working with html and xml
library(xml2)#memory management
library(stringr)#data cleaning
library(magrittr)#%>% forward pipe operator
library(tidyverse)

## ----check robots.txt----------------------------------------------------
paths_allowed(
  paths = c("https://www.imdb.com/search/title?groups=top_250&sort=user_rating")
)

## ----read html-----------------------------------------------------------
imdb <- read_html("https://www.imdb.com/search/title?groups=top_250&sort=user_rating")
imdb

## ----extract movie title-------------------------------------------------
imdb %>%
  html_nodes(".mode-advanced:nth-child(10) .lister-item-header a , .mode-advanced:nth-child(9) .lister-item-header a , .mode-advanced:nth-child(8) .lister-item-header a , .mode-advanced:nth-child(7) .lister-item-header a , .mode-advanced:nth-child(6) .lister-item-header a , .mode-advanced:nth-child(5) .lister-item-header a , .mode-advanced:nth-child(4) .lister-item-header a , .mode-advanced:nth-child(3) .lister-item-header a , .mode-advanced:nth-child(2) .lister-item-header a , .mode-advanced:nth-child(1) .lister-item-header a") %>%
  html_text() -> movie_title

movie_title


## ----extract movie director-------------------------------------------------
imdb %>%
  html_nodes(".mode-advanced:nth-child(10) .text-muted+ p a:nth-child(1) , .mode-advanced:nth-child(9) .text-muted+ p a:nth-child(1) , .mode-advanced:nth-child(8) .text-muted+ p a:nth-child(1) , .mode-advanced:nth-child(7) .text-muted+ p a:nth-child(1) , .mode-advanced:nth-child(6) .text-muted+ p a:nth-child(1) , .mode-advanced:nth-child(5) .text-muted+ p a:nth-child(1) , .mode-advanced:nth-child(4) .text-muted+ p a:nth-child(1) , .mode-advanced:nth-child(3) .text-muted+ p a:nth-child(1) , .mode-advanced:nth-child(2) .text-muted+ p a:nth-child(1) , .mode-advanced:nth-child(1) .text-muted+ p a:nth-child(1)") %>%
  html_text() -> movie_director

movie_director

## ----extract year of release---------------------------------------------
imdb %>%
  html_nodes(".mode-advanced:nth-child(10) .text-muted.unbold , .mode-advanced:nth-child(9) .text-muted.unbold , .mode-advanced:nth-child(8) .text-muted.unbold , .mode-advanced:nth-child(7) .text-muted.unbold , .mode-advanced:nth-child(6) .text-muted.unbold , .mode-advanced:nth-child(5) .text-muted.unbold , .mode-advanced:nth-child(4) .text-muted.unbold , .mode-advanced:nth-child(3) .text-muted.unbold , .mode-advanced:nth-child(2) .text-muted.unbold , .mode-advanced:nth-child(1) .text-muted.unbold") %>%
  html_text() %>%
  str_sub(start = 2, end = 5) %>%
  as.Date(format = "%Y") %>%
  year() -> movie_year

movie_year



## ----extract runtime-----------------------------------------------------
imdb %>%
  html_nodes(".mode-advanced:nth-child(10) .runtime , .mode-advanced:nth-child(9) .runtime , .mode-advanced:nth-child(8) .runtime , .mode-advanced:nth-child(7) .runtime , .mode-advanced:nth-child(6) .runtime , .mode-advanced:nth-child(5) .runtime , .mode-advanced:nth-child(4) .runtime , .mode-advanced:nth-child(3) .runtime , .mode-advanced:nth-child(2) .runtime , .mode-advanced:nth-child(1) .runtime") %>%
  html_text() %>%
  str_split(" ") %>%
  map_chr(1) %>%
  as.numeric() -> movie_runtime

movie_runtime

## ----extract genre-------------------------------------------------------
imdb %>%
  html_nodes(".mode-advanced:nth-child(10) .genre,.mode-advanced:nth-child(9) .genre,.mode-advanced:nth-child(8) .genre,.mode-advanced:nth-child(7) .genre,.mode-advanced:nth-child(6) .genre,.mode-advanced:nth-child(5) .genre,.mode-advanced:nth-child(4) .genre,.mode-advanced:nth-child(3) .genre,.mode-advanced:nth-child(2) .genre,.mode-advanced:nth-child(1) .genre") %>%
  html_text() %>%
  str_trim() -> movie_genre

movie_genre

## ----extract rating------------------------------------------------------
imdb %>%
  html_nodes(".mode-advanced:nth-child(10) .ratings-imdb-rating , .mode-advanced:nth-child(9) .ratings-imdb-rating , .mode-advanced:nth-child(8) .ratings-imdb-rating , .mode-advanced:nth-child(7) .ratings-imdb-rating , .mode-advanced:nth-child(6) .ratings-imdb-rating,.mode-advanced:nth-child(5) .ratings-imdb-rating , .mode-advanced:nth-child(4) .ratings-imdb-rating , .mode-advanced:nth-child(3) .ratings-imdb-rating , .mode-advanced:nth-child(2) .ratings-imdb-rating , .mode-advanced:nth-child(1) .ratings-imdb-rating")%>%
  html_attr("data-value")%>% 
  as.numeric() -> movie_rating

movie_rating

## ----extract votes-------------------------------------------------------
imdb %>%
  html_nodes(".mode-advanced:nth-child(10) .sort-num_votes-visible span:nth-child(2) , .mode-advanced:nth-child(9) .sort-num_votes-visible span:nth-child(2) , .mode-advanced:nth-child(8) .sort-num_votes-visible span:nth-child(2) , .mode-advanced:nth-child(7) .sort-num_votes-visible span:nth-child(2) , .mode-advanced:nth-child(6) .sort-num_votes-visible span:nth-child(2),.mode-advanced:nth-child(5) .sort-num_votes-visible span:nth-child(2) , .mode-advanced:nth-child(4) .sort-num_votes-visible span:nth-child(2) , .mode-advanced:nth-child(3) .sort-num_votes-visible span:nth-child(2) , .mode-advanced:nth-child(2) .sort-num_votes-visible span:nth-child(2) , .mode-advanced:nth-child(1) .sort-num_votes-visible span:nth-child(2)") %>% 
  html_attr('data-value') %>% 
  as.numeric() -> movie_votes

movie_votes



## ----top 10 movie details------------------------------------------------
top_10 <- tibble(title = movie_title,director=movie_director, release = movie_year,'runtime (mins)' = movie_runtime 
                 , genre = movie_genre, rating = movie_rating, 
                 votes = movie_votes)

top_10


barplot(as.matrix(movie_votes), main="movie_votes", ylab= "Total",
        beside=TRUE, col=rainbow(10))
legend("topleft", c("	
The Shawshank Redemption","	
The Godfather","	
The Dark Knight","	
The Lord of the Rings: The Return of the King","
Schindler's List","
The Godfather: Part II","
12 Angry Men","
The Lord of the Rings: The Fellowship of the Ring",
"Inception"), cex=0.6, bty="n", fill=rainbow(10))

barplot(as.matrix(movie_runtime), main="movie_runtime", ylab= "Total",
        beside=TRUE, col=rainbow(10))
legend("topleft", c("	
The Shawshank Redemption","	
The Godfather","	
The Dark Knight","	
The Lord of the Rings: The Return of the King","
Schindler's List","
The Godfather: Part II","
12 Angry Men","
The Lord of the Rings: The Fellowship of the Ring",
                    "Inception"), cex=0.6, bty="n", fill=rainbow(10))


pie(movie_rating, main="movie_rating", col=rainbow(length(movie_rating)),
    labels=c("The Shawshank Redemption(9.3)","	
             The Godfather(9.2)","	
             The Dark Knight(9.1)","	
             The Lord of the Rings: The Return of the King(9.0)","
             Schindler's List(9.0)","
The Godfather: Part II(9.0)","12 Angry Men(9.0)
","The Lord of the Rings: The Fellowship of the Ring(8.9)","	
Pulp Fiction(8.9)",
"Inception(8.8)"))

write.csv(top_10,"imdb.csv")

