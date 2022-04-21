# load the libraries ----------
library(readr)
library(ggplot2)
library(dplyr)
library(janitor)
library(ggthemes)
library(grid)


# set a variable to store the top N value to be used later -------
n_top <- 10

# read the data file  --------------
big_dave <- readr::read_csv(file = 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-19/big_dave.csv', 
                            col_types = cols()) |> 
  janitor::clean_names() |> janitor::remove_empty(which = "rows")

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
               show.legend = TRUE, 
               na.rm = TRUE) +
  geom_point(mapping = aes(x = nos, 
                           y = answers, 
                           colour = answers), 
             size = 3.0, 
             show.legend = FALSE, 
             na.rm = TRUE) + 
  scale_x_continuous(breaks = seq(from = 0, to = 40000, by = 5000)) + 
  theme_solarized_2(light = FALSE) + 
  theme(axis.title = element_text(colour = "white"), 
        axis.text = element_text(colour = "white"), 
        plot.title = element_text(color = "white"), 
        legend.text = element_text(colour = "white"), 
        legend.title = element_text(colour = "white"),
        plot.subtitle = element_text(color = "white"), 
        legend.position = "top", 
        axis.line = element_line(colour = "white")) + 
  labs(color = "Answer Words", 
       title = "Crossword Puzzles and Clues", 
       subtitle = paste0("Answer Words - Top", n_top, " - How long & frequent ?"), 
       x = "Number of words", 
       y = "Word Length", 
       caption = "Tidy Tuesday 2022 - Week 16\nArnab Panja")


# add custom annotations -------------------

my_text_grob <- grid::grobTree(textGrob(label = paste0(answer_dist_df[answer_dist_df$nos == max(answer_dist_df$nos), ]$answers, " letter words \n appear the most"), 
                                        x = 0.75, y = 0.3, hjust = 0, 
                                        gp = gpar(col = "goldenrod1", 
                                                  fontface = "bold", 
                                                  fontsize = 9)))
my_arrow_grob <- grid::grobTree(curveGrob(x1 = 0.90, 
                                          y1 = 0.38, 
                                          x2 = 0.95, 
                                          y2 = 0.90, 
                                          curvature = 0.1, 
                                          arrow = arrow(angle = 20, 
                                                        ends = "last", 
                                                        type = "closed"), 
                                          gp = gpar(lty = "solid", 
                                                    col = "goldenrod1", 
                                                    fill = "goldenrod1", 
                                                    lwd = 2)))

p_answer_dist <- p_answer_dist + annotation_custom(grob = my_text_grob) + 
  annotation_custom(grob = my_arrow_grob)

# view the plot --------------
p_answer_dist


# save the plot ---------------

ggsave(filename = "tidy_tuesday/2022/week16/crossword_plot.png", 
       plot = last_plot())

