# from Lucy's post
# http://livefreeordichotomize.com/2017/02/09/the-prevalence-of-drunk-podcasts/

# need emoGG
#devtools::install_github("dill/emoGG")
library(emoGG)
library('dplyr')
library('ggplot2')


req <- httr::GET(url = "https://itunes.apple.com/search",
                 query = list(
                   term = "drunk",
                   media = "podcast",
                   limit = 200
                 ))

itunes <- jsonlite::fromJSON(httr::content(req))$results

itunes %>%
  mutate(date = as.Date(releaseDate),monyear = zoo::as.yearmon(date)) %>%
  group_by(monyear) %>%
  summarise(n = n()) %>%
  mutate(date = zoo::as.Date(monyear)) %>%
  ggplot(aes(x = date,y=n)) +
  scale_x_date() +
  emoGG::geom_emoji(emoji="1f37a") + 
  ylab("Number of 'Drunk' podcasts released") +
  theme_minimal()

itunes %>%
  mutate(date = as.Date(releaseDate),monyear = zoo::as.yearmon(date)) %>%
  filter(date < as.Date('2017-02-01')) %>% #add a filter!
  group_by(monyear) %>%
  summarise(n = n()) %>%
  mutate(date = zoo::as.Date(monyear)) %>%
  ggplot(aes(x = date, y = n)) +
  scale_x_date() +
  emoGG::geom_emoji(emoji="1f37a") + 
  ylab("Number of 'Drunk' podcasts released") +
  theme_minimal()

