library(tidyverse)
imd18 <- readxl::read_excel("IMD2018.xlsx", sheet = "IMD18")
imd18

read_bind_csv <- function(x, y){
  x <- read_csv(y) %>%
    bind_rows(x, .)
}

# Importing and cleaning education data
education <- read_csv("2018-census-place-summaries-csv/2018-census-place-summaries-education-table2-2018-csv.csv") %>%
  filter(Area_type == "Statistical Area 2")
education <- education %>%
  filter(Maori_ethnic_group_indicator_summary_description == "Total") %>%
  select(Year, Area_type, Area_code, Highest_qualification_description, Highest_qualification_percent) %>%
  pivot_wider(names_from = Highest_qualification_description, values_from = Highest_qualification_percent) %>%
  select(!c(`Not elsewhere included`, Total))


# Importing and cleaning ethnicity data
ethnicity <- read_csv("2018-census-place-summaries-csv/2018-census-place-summaries-ethnicity-table1-2018-csv.csv") %>%
  filter(Area_type == "Statistical Area 2")
ethnicity <- ethnicity %>%
  select(Year, Area_type, Area_code, Maori_descent_description, Maori_descent_indicator_percent) %>%
  pivot_wider(names_from = Maori_descent_description, values_from = Maori_descent_indicator_percent) %>%
  select(!c(`Response unidentifiable`, `Not stated`, `Total`))

# Cleaning transport data, spitting out the percentage of modes used for work, selecting SA2
transport <- read_csv("2018-census-place-summaries-csv/2018-census-place-summaries-transport-table1-2018-csv.csv")
transport <- transport %>%
  select(Year, Area_type, Area_code, `Main_means_of_travel_to_work_description`, `Main_means_of_travel_to_work_percent`) %>%
  pivot_wider(names_from = `Main_means_of_travel_to_work_description`, values_from = `Main_means_of_travel_to_work_percent`) %>%
  filter(Area_type == "Statistical Area 2") %>%
  select(!c(`Did not go to work today`, `Not elsewhere included`))

# Binding the census data together
vars <- left_join(education, ethnicity, by = "Area_code") %>%
  left_join(., transport, by = "Area_code")
vars <- vars %>%
  select(!c(Year, Year.y, Area_type.y, Area_type.x, Area_type)) %>%
  rename(
    `Total Education` = `Total stated.x`,
    `Year` = Year.x,
    `Total Ethnicity` = `Total stated.y`,
    `Total Transport` = `Total stated`
  )

# Creating groupings for education
vars <- vars %>%
  mutate(
    Secondary = as.numeric(`Level 1 certificate`) + as.numeric(`Level 2 certificate`) + as.numeric(`Level 3 certificate`) + as.numeric(`Overseas secondary school qualification`),
    `Some University` = as.numeric(`Level 4 certificate`) + as.numeric(`Level 5 diploma`) + as.numeric(`Level 6 diploma`),
    Tertiary = as.numeric(`Bachelor's degree and level 7 qualification`) + as.numeric(`Post-graduate and honours degrees`),
    `Post-tertiary` = as.numeric(`Master's degree`) + as.numeric(`Doctorate degree`),
    `Any University` = as.numeric(`Level 4 certificate`) + as.numeric(`Level 5 diploma`) + as.numeric(`Bachelor's degree and level 7 qualification`) + as.numeric(`Post-graduate and honours degrees`) + as.numeric(`Master's degree`) + as.numeric(`Doctorate degree`)
    ) %>%
  select(!c(`Level 1 certificate`, `Level 2 certificate`, `Level 3 certificate`,
            `Level 4 certificate`, `Level 5 diploma`, `Level 6 diploma`,
            `Bachelor's degree and level 7 qualification`, `Post-graduate and honours degrees`,
            `Master's degree`, `Doctorate degree`))

vars <- vars %>%
  mutate(
    Area_code = as.character(Area_code)
  )

write_csv(vars, "census.csv")


imd18
colnames(imd18)

# Maybe can interpolate data from the larger one using the thiessen polygon method?
max(imd18$Rank_IMD18)

