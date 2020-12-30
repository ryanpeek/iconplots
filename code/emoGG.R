# from Lucy's post
# http://livefreeordichotomize.com/2017/02/09/the-prevalence-of-drunk-podcasts/


# Libraries ---------------------------------------------------------------

# need emoGG
#devtools::install_github("dill/emoGG")
library(emoGG)
library('dplyr')
library('ggplot2')


# Get Data ----------------------------------------------------------------

req <- httr::GET(url = "https://itunes.apple.com/search",
                 query = list(
                   term = "beer",
                   media = "podcast",
                   limit = 200
                 ))


# Extract -----------------------------------------------------------------

itunes <- jsonlite::fromJSON(httr::content(req))$results


# Search for GG -----------------------------------------------------------

emoji_search("beer")

## llama: 🦙

# Plot --------------------------------------------------------------------

# make a plot
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


# Use Custom icons --------------------------------------------------------

library(ggimage)
library(ggrepel)


itunes %>%
  mutate(date = as.Date(releaseDate),monyear = zoo::as.yearmon(date)) %>%
  group_by(monyear) %>%
  summarise(n = n()) %>%
  mutate(date = zoo::as.Date(monyear)) %>%
  ggplot(aes(x = date,y=n)) +
  scale_x_date() +
  geom_image(aes(image = "icons/llama_1f999_google.png"), size = 0.1) +
  theme_minimal()
