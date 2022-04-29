# load the libraries ----------
library(tidyverse)
library(janitor)
library(ggthemes)
library(grid)
library(tidytext)
library(wordcloud2)
library(htmlwidgets)
library(webshot)

# run this command as a one time install of phantom js ------
# webshot::install_phantomjs()


# set a variable to store the top N value to be used later -------
n_top <- 8



# read the data file  --------------

if(!("big_dave" %in% ls())){
  
  print(" ========= Loading Big Dave data set ========= ")
  
  big_dave <- readr::read_csv(file = 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-19/big_dave.csv',
                            col_types = cols()) |>
  janitor::clean_names() |> janitor::remove_empty(which = "rows")
} else print(" ========= Already loaded !!! Big Dave data set ========= ")


if(!("times" %in% ls())){
  
  print(" ========= Loading Times data set ========= ")
  
  times <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-19/times.csv', 
                         col_types = cols()) |> 
  janitor::clean_names() |> janitor::remove_empty(which = "rows")
} else print(" ========= Already loaded !!! Times data set ========= ")




# create the data frame for plotting ----------
df.answer.dist <- big_dave |> 
  mutate(answers = as.character(nchar(answer))) |> 
  count(answers, sort = TRUE) |> select(answers, nos = n) |>
  top_n(n = n_top, wt = nos)


# create the plot ---------------
p.answer.dist <- ggplot(data = df.answer.dist) + 
  geom_segment(mapping = aes(x = 0, 
                             y = reorder(answers, nos), 
                             xend = nos, 
                             yend = answers, 
                             color = answers), 
               size = 1.0, 
               lineend = "round", 
               show.legend = FALSE, 
               na.rm = TRUE) +
  geom_point(mapping = aes(x = nos, 
                           y = answers, 
                           colour = answers), 
             size = 3.0, 
             show.legend = FALSE, 
             na.rm = TRUE) + 
  scale_x_continuous(breaks = seq(from = 0, 
                                  to = 40000, 
                                  by = 5000), 
                     expand = c(0.01,0.01)) + 
  scale_color_brewer(palette = "Dark2") + 
  theme(axis.title = element_text(colour = "black"), 
        axis.text = element_text(colour = "black"), 
        plot.title = element_text(color = "black"), 
        legend.title = element_text(colour = "black"),
        plot.subtitle = element_text(color = "black"), 
        legend.position = "top", 
        axis.line = element_line(colour = "black")) + 
  labs(title = "Crossword Puzzles and Clues", 
       subtitle = paste0("Big Dave - Answer Words - Top ", n_top, " - How long & frequent ?"), 
       x = "Number of words", 
       y = "Word Length", 
       caption = "Tidy Tuesday 2022 - Week 16\nArnab Panja")


# add custom annotations -------------------

my_text_grob <- grid::grobTree(textGrob(label = paste0(df.answer.dist[df.answer.dist$nos == max(df.answer.dist$nos), ]$answers, " letter words \n appear the most"), 
                                        x = 0.75, y = 0.3, hjust = 0, 
                                        gp = gpar(col = "black", 
                                                  fontface = "bold", 
                                                  fontsize = 9)))
my_arrow_grob <- grid::grobTree(curveGrob(x1 = 0.90, 
                                          y1 = 0.38, 
                                          x2 = 0.95, 
                                          y2 = 0.90, 
                                          curvature = 0.05, 
                                          arrow = arrow(angle = 10, 
                                                        ends = "last", 
                                                        type = "closed"), 
                                          gp = gpar(lty = "solid", 
                                                    col = "black", 
                                                    fill = "black", 
                                                    lwd = 1.5)))

p.answer.dist <- p.answer.dist + annotation_custom(grob = my_text_grob) + 
  annotation_custom(grob = my_arrow_grob)

# view the plot --------------
p.answer.dist


# save the plot ---------------

ggsave(filename = "tidy_tuesday/2022/week16/crossword_plot.png", 
       plot = p.answer.dist)


# Generate a word cloud of the clues ---------------------

df.bigdave.trx <- big_dave |> select(rowid, clue, answer) |> 
  filter(!is.na(clue) & !is.na(answer)) |> 
  mutate(answer = str_to_lower(answer), 
         clue = str_to_lower(clue)) |> 
  (\(x){cbind(x, str_locate(string = x$clue, pattern = '\\([:digit:]'))})() |> 
  mutate(new_clue = if_else(is.na(start), 
                            str_trim(clue), 
                            str_trim(str_sub(clue, 1, start - 1)))) |> 
  select(rowid, clue = new_clue, answer) |> 
  (\(x){cbind(rowid = as.numeric(x$rowid), 
              answer = x$answer, clue = str_trim(str_replace_all(string = x$clue, 
                                                                 pattern = '[:digit:]', 
                                                                 replacement = '')))})() |> 
  as_tibble() |> 
  (\(x){cbind(rowid = x$rowid, 
              answer = x$answer, 
              clue = str_trim(str_replace_all(string = x$clue, 
                                              pattern = '\\_', 
                                              replacement = '')))})() |> 
  as_tibble() |> 
  (\(x){cbind(rowid = x$rowid, # to deal with UTF-8 encoding issues
             answer = x$answer, 
             clue = str_replace_all(string = x$clue, 
                                    pattern = '[[:punct:]]', 
                                    replacement = '\''))})() |> 
  as_tibble()




p.bigdave.wordcloud <- df.bigdave.trx |> select(clue) |> 
  unnest_tokens(output = word, input = clue) |> 
  anti_join(stop_words, by = "word") |> 
  group_by(word) |> 
  summarise(count = n(), .groups = "drop") |> ungroup() |> 
  filter(count > 100) |> 
  select(word, freq = count) |> 
  wordcloud2::wordcloud2(size = 0.8, 
                         color = "random-dark")


saveWidget(widget = p.bigdave.wordcloud, 
           file = "tidy_tuesday/2022/week16/p_bigdave_wordcloud.html", 
           selfcontained = F, 
           title = "Wordcloud - Big Dave - Clues")

webshot(url = "tidy_tuesday/2022/week16/p_bigdave_wordcloud.html", 
        file = "tidy_tuesday/2022/week16/p_bigdave_wordcloud.png", 
        delay = 25, 
        vwidth = 1000, 
        vheight = 1000)

p.bigdave.wordcloud


df.bigdave.clues <- df.bigdave.trx |> select(clue) |> 
  unnest_tokens(output = word, input = clue) |> 
  (\(x){str_replace_all(string = x$word, # to deal with UTF-8 encoding issues
                       pattern = '[[:punct:]]', 
                       replacement = '\'')})() |> 
  as_tibble() |> setNames("word") |> 
  anti_join(stop_words, by = "word") |> 
  group_by(word) |> 
  summarise(count = n()) |> ungroup() |>
  top_n(n = n_top + 2, 
        wt = count) |> 
  mutate(type = "Clues")



# -------------------

  
df.bigdave.answers <- df.bigdave.trx |> select(answer) |> 
  unnest_tokens(output = word, input = answer) |> 
  anti_join(stop_words, by = "word") |> 
  group_by(word) |> 
  summarise(count = n(), .groups = "drop") |> ungroup() |> 
  top_n(n = n_top + 2, 
        wt = count) |> 
  mutate(type = "Answers")


p.bigdave.answers.clues <- rbind(df.bigdave.clues, df.bigdave.answers) |> 
  ggplot() + 
  geom_col(mapping = aes(x = count, y = reorder_within(x = word, 
                                                       by = count, 
                                                       within = type), fill = word), 
           show.legend = FALSE, width = 0.8) + 
  scale_y_reordered() +
  facet_wrap(facets = ~type, ncol = 2, scales = "free") + 
  labs(x = NULL, 
       y = NULL, 
       title = "Crossword - Big Dave", 
       subtitle = "Most frequent - Answers & Clues", 
       caption = "Tidy Tuesday - Week 16- Arnab Panja")

ggsave(filename = 'tidy_tuesday/2022/week16/p_bigdave_answers_clues.png', 
       plot = p.bigdave.answers.clues)


p.bigdave.answers.clues

# times data set analysis -------------------

# clue words distribution of times data ----------------------------

df.times.trx <- times |> select(rowid, clue, answer) |> 
  filter(!is.na(clue) & !is.na(answer)) |> 
  mutate(answer = str_to_lower(answer), 
         clue = str_to_lower(clue)) |> 
  (\(x){cbind(x, str_locate(string = x$clue, pattern = '\\([:digit:]'))})() |> 
  mutate(new_clue = if_else(is.na(start), 
                            str_trim(clue), 
                            str_trim(str_sub(clue, 1, start - 1)))) |> 
  select(rowid, clue = new_clue, answer) |> 
  (\(x){cbind(rowid = as.numeric(x$rowid), 
              answer = x$answer, clue = str_trim(str_replace_all(string = x$clue, 
                                                                 pattern = '[:digit:]', 
                                                                 replacement = '')))})() |> 
  as_tibble() |> 
  (\(x){cbind(rowid = x$rowid, 
              answer = x$answer, 
              clue = str_trim(str_replace_all(string = x$clue, 
                                              pattern = '\\_', 
                                              replacement = '')))})() |> 
  as_tibble()

# Word cloud plot of the times data set ---------------------

p.times.wordcloud <- df.times.trx |> select(clue) |> 
  unnest_tokens(output = word, input = clue) |> 
  anti_join(stop_words, by = "word") |> 
  group_by(word) |> 
  summarise(count = n(), .groups = "drop") |> ungroup() |> 
  filter(count > 100) |> 
  select(word, freq = count) |> 
  wordcloud2::wordcloud2(size = 0.8, 
                         color = "random-dark")


saveWidget(widget = p.times.wordcloud, 
           file = "tidy_tuesday/2022/week16/p_times_wordcloud.html", 
           selfcontained = F, 
           title = "Wordcloud - Times - Clues")

webshot(url = "tidy_tuesday/2022/week16/p_times_wordcloud.html", 
        file = "tidy_tuesday/2022/week16/p_times_wordcloud.png", 
        delay = 25, 
        vwidth = 1200, 
        vheight = 1200)


p.times.wordcloud

# Times Data Set Facet plot of most frequent answers and clues ------------------

df.times.clues <- df.times.trx |> select(clue) |> 
  unnest_tokens(output = word, input = clue) |> 
  anti_join(stop_words, by = "word") |> 
  group_by(word) |> 
  summarise(count = n(), .groups = "drop") |> ungroup() |> 
  top_n(n = n_top + 2, 
        wt = count) |> 
  mutate(type = "Clues")

df.times.answers <- df.times.trx |> select(answer) |> 
  unnest_tokens(output = word, input = answer) |> 
  anti_join(stop_words, by = "word") |> 
  group_by(word) |> 
  summarise(count = n(), .groups = "drop") |> ungroup() |> 
  top_n(n = n_top + 2, 
        wt = count) |> 
  mutate(type = "Answers")


p.times.answers.clues <- rbind(df.times.clues, df.times.answers) |> 
  ggplot() + 
  geom_col(mapping = aes(x = count, y = reorder_within(x = word, 
                                                       by = count, 
                                                       within = type), fill = word), 
           show.legend = FALSE, width = 0.8) + 
  scale_y_reordered() +
  facet_wrap(facets = ~type, ncol = 2, scales = "free") + 
  labs(x = NULL, 
       y = NULL, 
       title = "Crossword - Times", 
       subtitle = "Most frequent - Answers & Clues", 
       caption = "Tidy Tuesday - Week 16- Arnab Panja")

ggsave(filename = 'tidy_tuesday/2022/week16/p_times_answers_clues.png', 
       plot = p.times.answers.clues)


p.times.answers.clues
