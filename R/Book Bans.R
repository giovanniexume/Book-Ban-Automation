

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




# Load PEN America dataset
books <- read_xlsx("inst/books.xlsx", sheet = 1)


# Deauth google drive

googledrive::drive_auth(path = gargle::secret_decrypt_json("inst/googledrive_encrypt.json", "GOOGLEDRIVE_KEY"))

# Deauth googlesheet 

googlesheets4::gs4_auth(path = gargle::secret_decrypt_json("inst/googledrive_encrypt.json", "GOOGLEDRIVE_KEY"))


# Sort by date of book ban and create unique id using row number
books <- books %>% arrange(`Date of Challenge/Removal`) %>% mutate(challenge_id = row_number())

#get a distinct count of authors, titles and states with bans

books %>% select(Author, Title, State) %>% sapply(., n_distinct)


#group challenges by the book title, get a count of challenges per title and percentage.

titles <- books %>% group_by(Title) %>%
  summarise(Challenges = n(), Perc = (Challenges/as.integer(count(books)))*100) %>%
  arrange(desc(Challenges))

#get the cumulative percentage of bans for each title

titles  <- titles %>% mutate(Cum_Perc = cumsum(titles$Perc))

#look at the top 50 books with the most challenges
slice_max(titles, order_by = Challenges, n = 50)


#get a count of authors with the most book bans

authors <- books %>% group_by(Author) %>%
  summarise(Challenges = n()) %>%
  arrange(desc(Challenges))

#get a count of states with the most book bans
states <- books %>% group_by(State) %>%
  summarise(Challenges = n()) %>%
  arrange(desc(Challenges))

#save the counts for challenges, unique titles, unique authors and states into datasets

books_dataset_names <- list("Books" = books, "Titles" = titles, "States" = states, "Authors" = authors)

#write to google sheet

sheet_id <- "https://docs.google.com/spreadsheets/d/1U4-FsOkqh5-ulNaEafP0qaermKDI8toVK39iEUwpUXQ"

sheet_write(books, ss = sheet_id, sheet = "Books")

sheet_write(titles, ss = sheet_id, sheet = "Titles")

sheet_write(authors, ss = sheet_id, sheet = "Authors")

sheet_write(states, ss = sheet_id, sheet = "States")

