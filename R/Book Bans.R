library(tidyverse)
library(readxl)
library(SKTools)
library(stats)
library(caret)
library(corrplot)
library(janitor)
library(broom)
library(sur)
library(openxlsx)
library(googlesheets4)



books <- read_xlsx("book_index.xlsx", sheet = 1)

books <- books %>% arrange(`Date of Challenge/Removal`) %>% mutate(challenge_id = row_number())


books %>% select(Author, Title, State) %>% sapply(., n_distinct)



titles <- books %>% group_by(Title) %>%
  summarise(Challenges = n(), Perc = (Challenges/as.integer(count(books)))*100) %>%
  arrange(desc(Challenges))

titles  <- titles %>% mutate(Cum_Perc = cumsum(titles$Perc))

slice_max(titles, order_by = Challenges, n = 50)

authors <- books %>% group_by(Author) %>%
  summarise(Challenges = n()) %>%
  arrange(desc(Challenges))


states <- books %>% group_by(State) %>%
  summarise(Challenges = n()) %>%
  arrange(desc(Challenges))



books_dataset_names <- list("Books" = books, "Titles" = titles, "States" = states, "Authors" = authors)

sheet_id <- "https://docs.google.com/spreadsheets/d/1U4-FsOkqh5-ulNaEafP0qaermKDI8toVK39iEUwpUXQ"

sheet_write(books, ss = sheet_id, sheet = "Books")

sheet_write(titles, ss = sheet_id, sheet = "Titles")

sheet_write(authors, ss = sheet_id, sheet = "Authors")
