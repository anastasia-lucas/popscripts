
###Modified from
###https://github.com/ewenme/geniusr
###https://drsimonj.svbtle.com/ordering-categories-within-ggplot2-facets

library(geniusr)
library(purrr)
library(ggplot2)
library(tidytext)
library(dplyr)
library(scales)

bing <- get_sentiments("bing")

#Arular
search_song("Galang")
#181262
tracklist_arular <- scrape_tracklist(album_id = get_song_meta(song_id = 181262)$album_id)
lyrics_arular <- map_df(tracklist_arular$song_lyrics_url, scrape_lyrics_url)

sentiment_arular <- lyrics_arular %>%
  unnest_tokens(word, line) %>%
  # remove stop words
  anti_join(stop_words) %>%
  # join afinn score
  inner_join(bing) %>%
  # count negative / positive words
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

#Kala
search_song("Paper Planes")
#2171
tracklist_kala <- scrape_tracklist(album_id = get_song_meta(song_id = 2171)$album_id)
lyrics_kala <- map_df(tracklist_kala$song_lyrics_url, scrape_lyrics_url)

sentiment_kala <- lyrics_kala %>%
  unnest_tokens(word, line) %>%
  # remove stop words
  anti_join(stop_words) %>%
  # join afinn score
  inner_join(bing) %>%
  # count negative / positive words
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

#MAYA
search_song("XXXO")
#121247
tracklist_maya <- scrape_tracklist(album_id = get_song_meta(song_id=121247)$album_id)
lyrics_maya <- map_df(tracklist_maya$song_lyrics_url, scrape_lyrics_url)

sentiment_maya <- lyrics_maya %>%
  unnest_tokens(word, line) %>%
  # remove stop words
  anti_join(stop_words) %>%
  # join afinn score
  inner_join(bing) %>%
  # count negative / positive words
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

#Matangi
search_song("Bring The Noize")
#123137
tracklist_matangi <- scrape_tracklist(album_id = get_song_meta(song_id = 123137)$album_id)
lyrics_matangi <- map_df(tracklist_matangi$song_lyrics_url, scrape_lyrics_url)

sentiment_matangi <- lyrics_matangi %>%
  unnest_tokens(word, line) %>%
  # remove stop words
  anti_join(stop_words) %>%
  # join afinn score
  inner_join(bing) %>%
  # count negative / positive words
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

#AIM
search_song("Borders")
#2314374
tracklist_aim <- scrape_tracklist(album_id = get_song_meta(song_id = 2314374)$album_id)
lyrics_aim <- map_df(tracklist_aim$song_lyrics_url, scrape_lyrics_url)

sentiment_aim <- lyrics_aim %>%
  unnest_tokens(word, line) %>%
  # remove stop words
  anti_join(stop_words) %>%
  # join afinn score
  inner_join(bing) %>%
  # count negative / positive words
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

#Plot
sentiment_arular$Album <- "Arular"
sentiment_kala$Album <- "Kala"
sentiment_maya$Album <- "/\\/\\/\\Y/\\"
sentiment_matangi$Album <- "Matangi"
sentiment_aim$Album <- "AIM"

full <- rbind(sentiment_arular[sentiment_arular$n>=3, ], sentiment_kala[sentiment_kala$n>=3, ], sentiment_maya[sentiment_maya$n>=3, ], sentiment_matangi[sentiment_matangi$n>=3, ], sentiment_aim[sentiment_aim$n>=3, ])
full$Album <- factor(full$Album, levels=c("Arular", "Kala", "/\\/\\/\\Y/\\", "Matangi", "AIM"))
full$word <- factor(full$word, levels=unique(full$word[order(full$n, decreasing = FALSE)]), ordered=TRUE)

fullorder <- full %>% group_by(Album) %>% ungroup() %>% arrange(Album, n) %>% mutate(order = row_number())
p1 <- ggplot(data=fullorder, aes(x=order, y=n, fill=sentiment)) + geom_bar(stat="identity") 
p1 <- p1 +scale_x_continuous(breaks = fullorder$order,labels=fullorder$word,expand = c(0,0)) + scale_y_continuous(breaks= pretty_breaks())
p1 <- p1 + coord_flip() + facet_wrap(~Album, scales="free")
#Change colors to match most recent album
p1 <- p1 + scale_fill_manual("", values=c("#230A05", "#FE5E22")) + theme(legend.position = c(0.85, 0.25)) + xlab("Word") + ylab("Word Count") + ggtitle("Sentiment Analysis of M.I.A.'s Most Used Words per Album")
ggsave(p1, filename="word_sentiment_by_album.png", dpi=300, units="in")

#Get overall percentages
ovrl <- rbind(sentiment_arular, sentiment_kala, sentiment_maya, sentiment_matangi, sentiment_aim)
ovrlcnts <- ovrl %>% group_by(Album, sentiment) %>% summarise(counts = sum(n))
ovrlcnts$Album <- factor(ovrlcnts$Album, levels=c("Arular", "Kala", "/\\/\\/\\Y/\\", "Matangi", "AIM"))

p2 <- ggplot(data=ovrlcnts, aes(x=Album, y=counts,fill=sentiment)) + geom_bar( position = "dodge", stat="identity") + scale_fill_manual("", values=c("#230A05", "#FE5E22")) + ylab("(Non-unique) Word Count") + ggtitle("Sentiment Analysis of M.I.A.'s Lyrics")
ggsave(p2, filename="overall_sentiment_by_album.png", dpi=300, units="in")
