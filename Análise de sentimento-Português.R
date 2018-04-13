library(readxl)
library(dplyr)

getwd()
df <- read_excel("/Users/HenriqueLispector/Desktop/Wordcloud Impeachment/todos.xlsx")
df <- as.data.frame(df)
Sys.setlocale("LC_ALL", "pt_BR.ISO8859-1")


class(df$Deputado)

library(stringi)
df$Deputado <- stri_trans_general(df$Deputado, "Latin-ASCII")
df$Fala <- stri_trans_general(df$Fala, "Latin-ASCII")
head(df)

df1 <- df %>% select(Deputado, Fala)

head(df1)

df1 <- df1 %>% mutate(comment_id = row_number())
head(df1)

library(lexiconPT)
library(tidytext)

df1_unnested <- df1 %>% unnest_tokens(term, Fala)
head(df1_unnested)

df1_unnested %>%
  select(comment_id, term) %>%
  head(20)


data("oplexicon_v3.0")
data("sentiLex_lem_PT02")

op30 <- oplexicon_v3.0
sent <- sentiLex_lem_PT02


x <- df1_unnested %>% 
  left_join(op30, by = "term") %>% 
  left_join(sent %>% select(term, lex_polarity = polarity), by = "term") %>% 
  select(comment_id, term, polarity, lex_polarity)

nrow(x)
sum(is.na(x$polarity))
sum(is.na(x$lex_polarity))

x1 <- x %>%
  filter(polarity != 0) %>%
  mutate(sentiment_polarity = ifelse(polarity < 0, "Negativo","Positivo"))

x1.1 <- table(x1$sentiment_polarity)

barplot(x1.1, main="Sentimento dos discursos - oplexicon_v3.0", 
        ylab="Quantidade de palavras", col = c('red','blue'))

x2 <- x %>%
  filter(lex_polarity != 0) %>%
  mutate(sentiment_lex_polarity = ifelse(lex_polarity < 0, "Negativo", "Positivo"))

x2 %>% count(sentiment_lex_polarity)

x2.1 <- table(x2$sentiment_lex_polarity)

barplot(x2.1, main="Sentimento dos discursos - sentiLex_lem_PT02", 
        ylab="Quantidade de palavras", col = c('red','blue'))


#get_word_sentiment("word")