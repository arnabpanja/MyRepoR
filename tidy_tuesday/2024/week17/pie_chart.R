# Tidy Tuesday - 2024 - Week 17 
# Objects Launched into Space 

# Load the libraries ----- 

library(readr, warn.conflicts = FALSE)
library(janitor, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
options(tibble.width = Inf)


# Load the dataset  ----- 

tt_data <- read_csv(file = "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-04-23/outer_space_objects.csv", 
                    col_types = cols(), 
                    show_col_types = FALSE) |> 
  clean_names() |> 
  remove_empty(which = "rows")

str_glue("The dataset has {nrow(tt_data)} observations and {ncol(tt_data)} variables")

str_glue("The variables in the dataset are\n")
cat(paste0(names(tt_data), "\n"))


# find the number of objects launched  ----
# per year for the last 8 years 
tt_sum <- tt_data |> 
  group_by(year) |> 
  summarize(sum_obj = sum(num_objects, 
                          na.rm = TRUE), 
                          .groups = "drop") |> 
  top_n(n = 8, wt = year) |>
  arrange(-year)

# View the data ----
tt_sum


# Plot a pie chart ---- 

p_pie_chart <- ggplot(data = tt_sum, mapping = aes(x = "", y = as.character(year))) + 
  geom_col(mapping = aes(fill = as.character(year)), show.legend = TRUE) + 
  geom_text(mapping = aes(label = sum_obj), 
            position = position_stack(vjust = 0.5), 
            size = 3, 
            color = "black", 
            fontface = "bold") + 
  scale_fill_brewer(palette = "Set3") + 
  labs(x = "", 
       y = "", 
       fill = "Year of Launch", 
       title = "Tidy Tuesday - Objects Launched into Space by Year", 
       subtitle = str_glue("Almost {round(max(tt_sum$sum_obj)/min(tt_sum$sum_obj), digits = 0)} fold increase in the objects launched into space in the last {nrow(tt_sum)} years"), 
       caption = "Tidy Tuesday - 2024 - Week 17 - ArnabP") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        plot.background = element_blank(), 
        legend.position = "bottom") + 
  coord_polar(theta = "y")

# View the chart ---- 
p_pie_chart


# save the plot ---- 
ggsave(filename = "tidy_tuesday/2024/week17/p_pie_chart.png", 
       plot = last_plot())


