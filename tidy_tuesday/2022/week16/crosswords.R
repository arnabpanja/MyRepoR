# load the libraries ----------
library(readr)
library(ggplot2)
library(dplyr)
library(janitor)
library(ggthemes)
library(grid)
library(tidytext)



# set a variable to store the top N value to be used later -------
n_top <- 8



# read the data file  --------------

if(!("big_dave" %in% ls())){

  big_dave <- readr::read_csv(file = 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-19/big_dave.csv',
                            col_types = cols()) |>
  janitor::clean_names() |> janitor::remove_empty(which = "rows")
}

if(!("times" %in% ls())){

  times <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-19/times.csv', 
                         col_types = cols()) |> 
  janitor::clean_names() |> janitor::remove_empty(which = "rows")
}



# create the data frame for plotting ----------
answer_dist_df <- big_dave |> 
  mutate(answers = as.character(nchar(answer))) |> 
  count(answers, sort = TRUE) |> select(answers, nos = n) |>
  top_n(n = n_top, wt = nos)


# create the plot ---------------
p_answer_dist <- ggplot(data = answer_dist_df) + 
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
       subtitle = paste0("Answer Words - Top ", n_top, " - How long & frequent ?"), 
       x = "Number of words", 
       y = "Word Length", 
       caption = "Tidy Tuesday 2022 - Week 16\nArnab Panja")


# add custom annotations -------------------

my_text_grob <- grid::grobTree(textGrob(label = paste0(answer_dist_df[answer_dist_df$nos == max(answer_dist_df$nos), ]$answers, " letter words \n appear the most"), 
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

p_answer_dist <- p_answer_dist + annotation_custom(grob = my_text_grob) + 
  annotation_custom(grob = my_arrow_grob)

# view the plot --------------
p_answer_dist


# save the plot ---------------

ggsave(filename = "tidy_tuesday/2022/week16/crossword_plot.png", 
       plot = last_plot())



# highest words as the answers 

times |> filter(!is.na(answer)) |> group_by(answer) |> 
  summarise(count = n()) |> ungroup() |> 
  top_n(n = 8, wt = count)


# lowest words as the answers   

times |> filter(!is.na(answer)) |> group_by(answer) |> 
  summarise(count = n()) |> ungroup() |> 
  arrange(count) |> mutate(row_n = row_number()) |> 
  filter(row_n <= 4)
  
# clue words distribution of times data ----------------------------

p_clue_plot <- times |> filter(!is.na(clue)) |> 
  select(rowid, clue, answer) |> 
cbind(str_locate(subset.times$clue, '\\([:digit:]')) |> 
  mutate(new_clue = str_sub(clue, 1, start - 1)) |> 
  unnest_tokens(output = word, input = new_clue) |> 
  group_by(word) |> 
  summarise(count = n()) |> ungroup() |> 
  filter(!is.na(word)) |> 
  anti_join(stop_words, by = "word") |> 
  top_n(n = n_top, wt = count) |> 
  ggplot() + 
  geom_col(mapping = aes(x = count, 
                         y = reorder(word, count), 
                         fill = word), 
           show.legend = FALSE) + 
  scale_x_continuous(expand = c(0.02, 0.02)) + 
  scale_fill_brewer(palette = "Dark2") + 
  labs(x = "Count", 
       y = "Clue Words", 
       title = "Crossword Puzzles and Clues", 
       subtitle = "Times - Clue Words Distribution", caption = "Tidy Tuesday 2022 - Week 16\nArnab Panja")

# add custom annotations -------------------

my_text_grob <- grid::grobTree(textGrob(label = "The word that appears\nthe most in the clues", 
                                        x = 0.75, y = 0.3, hjust = 0, 
                                        gp = gpar(col = "black", 
                                                  fontface = "bold", 
                                                  fontsize = 9)))


my_arrow_grob <- grid::grobTree(curveGrob(x1 = 0.90, 
                                          y1 = 0.35, 
                                          x2 = 0.95, 
                                          y2 = 0.85, 
                                          curvature = 0.05, 
                                          arrow = arrow(angle = 10, 
                                                        ends = "last", 
                                                        type = "closed"), 
                                          gp = gpar(lty = "solid", 
                                                    col = "black", 
                                                    fill = "black", 
                                                    lwd = 1.5)))

p_clue_plot <- p_clue_plot + annotation_custom(grob = my_text_grob) + 
  annotation_custom(grob = my_arrow_grob)

p_clue_plot

ggsave(filename = "tidy_tuesday/2022/week16/clue_plot.png", 
       plot = last_plot())





