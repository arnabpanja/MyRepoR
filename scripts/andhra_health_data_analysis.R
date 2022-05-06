suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(janitor))
suppressPackageStartupMessages(library(ggbump))
suppressPackageStartupMessages(library(ggridges))





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

# Data clean up ----------------------------



andhra_health_data <- andhra_health_data |> mutate(new_sex = case_when(
  sex == "Male(Child)" ~ "Male", 
  sex == "Female(Child)" ~ "Female", 
  TRUE ~ sex
)) |> mutate(new_sex = str_to_title(new_sex))




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






# Plot Bump Chart -----
# to show no of surgeries by district every year 

p_surgeries_district <- andhra_health_data |> select(surgery_date, district_name) |> 
  filter(!is.na(surgery_date) & !is.na(district_name)) |> 
  mutate(surgery_year = str_sub(surgery_date, 7, 10)) |> 
  group_by(surgery_year, district_name) |> 
  summarise(count = n(), .groups = "drop") |> ungroup() |> 
  mutate(surgery_year = as.numeric(surgery_year)) |> 
  ggplot() + 
  geom_bump(mapping = aes(x = surgery_year, 
                                  y = count, 
                                  color = district_name), size = 1) + 
  geom_point(mapping = aes(x = surgery_year, 
                           y = count, 
                           color = district_name), 
             size = 3) + 
  theme_minimal() + 
  labs(color = "District", 
       x = "Year of Surgery", 
       y = "Number of Surgeries", 
       title = "Health Claims - Surgeries by District", 
       subtitle = "Big bump in Surgeries in some districts")


p_surgeries_district

ggsave(filename = "plots/andhra_health_insurance/p_surgeries_district.png", 
       plot = p_surgeries_district)
  
# Let us see the variation in 2 of these districts 

andhra_health_data |> select(surgery_date, district_name) |> 
  filter(!is.na(surgery_date) & !is.na(district_name)) |> 
  mutate(surgery_year = str_sub(surgery_date, 7, 10)) |> 
  group_by(surgery_year, district_name) |> 
  summarise(count = n(), .groups = "drop") |> ungroup() |> 
  mutate(surgery_year = as.numeric(surgery_year)) |> 
  filter(district_name == "East Godavari" | district_name == "Chittoor") |> 
  arrange(district_name, surgery_year)


# Plot Stacked Bar Chart -----
# to show no of surgeries by surgery type and hospital type 

p_surgery_hospital <- andhra_health_data |> group_by(category_name, hosp_type) |> 
  summarise(count = n(), .groups = "drop") |> ungroup() |> 
  mutate(category_name = str_trim(str_replace_all(string = category_name, 
                                         pattern = "SURGERY|AND|SURGERIES|DISEASES|SURGICAL|PROCEDURES", 
                                         replacement = "")), 
         hosp_type = case_when(
           hosp_type == "C" ~ "C - Private", 
           hosp_type == "G" ~ "G - Government", 
           TRUE ~ hosp_type
         )) |> 
  ggplot() + 
  geom_bar(mapping = aes(x = count, 
                         y = category_name, 
                         fill = hosp_type), 
           stat = "identity", 
           na.rm = TRUE, 
           position = "stack") + 
  scale_fill_brewer(palette = "Set2") + 
  theme_minimal() + 
  theme(legend.position = "top") + 
  labs(fill = "Hospital Type", 
       title = "Health Claims - Surgery by Hospital Type", 
       y = NULL, 
       x = "Number of Surgeries")

p_surgery_hospital

ggsave(filename = "plots/andhra_health_insurance/p_surgery_hospital.png", 
       plot = p_surgery_hospital)



# Ridge Chart -----
# to show age distribution by sex and mortality 

p_age_mortality_sex <- andhra_health_data |> select(new_sex, mortality_y_n, age) |> 
  ggplot() + 
  ggridges::geom_density_ridges2(mapping = aes(x = age, 
                                              y = mortality_y_n, 
                                              fill = new_sex), 
                                na.rm = TRUE, 
                                alpha = 0.6, 
                                scale = 0.75, 
                                bandwidth = 4) +
  scale_fill_brewer(palette = "Spectral") + 
  scale_x_continuous(breaks = seq(0, 120, 20)) + 
  theme_minimal() + 
  theme(legend.position = "top") + 
  labs(fill = "Sex", 
       x = "Age", 
       y = "Mortality", 
       title = "Health Claims - Mortatlity Distribution with Age & Sex")

p_age_mortality_sex

ggsave(filename = "plots/andhra_health_insurance/p_age_mortality_sex.png", 
       plot = p_age_mortality_sex)



# Dumbbell Chart -----
# to show minimum and maximum claim amounts per surgery category 


p_claim_amt_surgery <- suppressMessages(andhra_health_data |> select(category_name, claim_amount) |> 
  mutate(category_name = str_trim(str_replace_all(string = category_name, 
                                         pattern = "SURGERY|AND|SURGERIES|DISEASES|SURGICAL|PROCEDURES", 
                                         replacement = ""))) |>
    filter(!is.na(claim_amount) & claim_amount != 0) |> 
  group_by(category_name) |> 
  summarise(min_claim = min(claim_amount), 
            max_claim = max(claim_amount), .groups = "drop") |>  
  ggplot() + 
  ggalt::geom_dumbbell(mapping = aes(y = category_name, 
                                     x = min_claim, 
                                     xend = max_claim), 
                       size = 1.5, 
                       color = "blue", 
                       size_x = 3, 
                       size_xend = 4, 
                       colour_x = "green", 
                       colour_xend = "magenta") + 
  scale_x_continuous(labels = scales::comma) + 
  theme_minimal() + 
  labs(x = "Claim Amounts (in Indian Rupees)", 
       y = NULL,  
       title = "Health Claims - Claims Amounts", 
       subtitle = "Variation of Claim Amounts by Surgery Category"))

p_claim_amt_surgery

ggsave(filename = "plots/andhra_health_insurance/p_claim_amt_surgery.png", 
       plot = p_claim_amt_surgery)
