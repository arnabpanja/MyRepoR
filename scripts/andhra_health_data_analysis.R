suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(janitor))


# Load source data -------------------------
if(!("andhra_health_data" %in% ls())){
  print(" ======= Loading Source Data ======= ")
andhra_health_data <- read_csv(file = "data/andhra_health_insurance/Andhra_Health_Data.csv", 
                               col_types = cols()) |> 
  clean_names() |> remove_empty(which = "rows")
} else{
  print(" ======= Data Already Present ======= ")
  print(" ======= Data Loading Skipped ======= ")
}


# Check summary information ----------------
dim(andhra_health_data)
names(andhra_health_data)

# data clean up ----------------------------



andhra_health_data <- andhra_health_data |> mutate(new_sex = case_when(
  sex == "Male(Child)" ~ "Male", 
  sex == "Female(Child)" ~ "Female", 
  TRUE ~ sex
)) |> mutate(new_sex = str_to_lower(new_sex))




# Plot Age distributions of Claimants ------------

p_age_distribution <- andhra_health_data |> ggplot() + 
  geom_histogram(mapping = aes(x = age), 
                 bins = 30, 
                 na.rm = TRUE, 
                 show.legend = FALSE, 
                 color = "greenyellow", 
                 fill = "green") +  
  theme_minimal() + 
  scale_y_continuous(name = "Count", 
                     breaks = seq(0, 60000, 10000)) + 
  scale_x_continuous(name = "Age", 
                     breaks = seq(0, 100, 10)) + 
  labs(title = "Health Claims - Age Distribution of claimants", 
       subtitle = "Health Claims are maximum in the Age Group 45 - 55")

p_age_distribution

ggsave(filename = "plots/andhra_health_insurance/p_age_distribution.png", 
       plot = p_age_distribution)



# Plot Age distributions of Claimants by Sex ------------

vec.colors <- c("aquamarine", "coral1")


p_age_distribution_sex <- andhra_health_data |> ggplot() +  
      geom_density(mapping = aes(x = age, fill = new_sex, color = new_sex), 
                   alpha = 0.6,  
                   show.legend = TRUE, 
                   na.rm = TRUE) + 
  theme_minimal() + 
  scale_fill_manual(values = vec.colors) + 
  scale_color_manual(values = vec.colors) + 
  scale_x_continuous(name = "Age", breaks = seq(0, 100, 10)) + 
  scale_y_continuous(name = "Density") + 
  theme(legend.position = "bottom") + 
  labs(fill = "Sex", 
       color = "Sex", 
       title = "Health Claims - Age Distribution of claimants by Sex", 
       subtitle = "Females have claimed slightly more than Males", 
       )

p_age_distribution_sex

ggsave(filename = "plots/andhra_health_insurance/p_age_distribution_sex.png", 
       plot = p_age_distribution_sex)