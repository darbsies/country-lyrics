library(tidyverse)
library(geniusR)
library(stringr)

country_csv <- "country_output.csv"
hot_csv <- "hot_output.csv"

country_og <- read_csv(country_csv)
  

country_df <-
  read_csv(country_csv) %>%
  mutate(artist = str_sub(artist, 2, -2)) %>%
  mutate(title = str_sub(title, 2, -2)) %>%
  filter(!artist %in% c("Adam Cunningham & Red Marlow", "Chloe Kohanski & Blake Shelton", "Emily Ann Roberts", "Emily Ann Roberts & Blake Shelton", "Jake Worthington & Blake Shelton", "Jake Worthington", "Billy Gilman", "Brennley Brown", "Zach Seabaugh", "Pryor Baird", "Josh Gallagher", "Barrett Baber", "Barrett Baber & Blake Shelton", "Shelby Brown", "Meghan Linsey", "Kristen Merlin", "Adam Wakefield", "Mary Sarah", "Sawyer Fredericks", "Sundance Head", "Sundance Head & Blake Shelton", "The Nashville Nuggets", "Cole Vosbury", "Christina Aguilera", "Corey Kent White", "Josh Halverson", "Audra McLaughlin")) %>%
  filter(!title %in% c("A Girl Like You", "At Last", "Black Roses", "Darlin' Don't Go", "Delta Dawn", "Ghost In This House", "Go Rest High On That Mountain", "The Dance", "To Make You Feel My Love", "Hairy Christmas", "Hearts I Leave Behind", "How Great Thou Art", "Wake Up When It's Over", "Lord, I Hope This Day Is Good", "Somewhere In My Broken Heart", "Tell Me Why", "There's A Tear In My Beer", "What I'm Thankful For (The Thanksgiving Song)")) %>%
  mutate(artist = str_replace_all(artist, "\\-", " ")) %>%
  mutate(title = str_replace_all(title, "\\-", " ")) %>%
  mutate(title = str_replace_all(title, "\\&", "and")) %>%
  mutate(artist = str_replace_all(artist, "[:punct:]", "")) %>%
  mutate(title = str_replace_all(title, "[:punct:]", "")) %>%
  mutate(artist = str_replace_all(artist, "\\+", "")) %>%
  mutate(title = str_replace_all(title, "\\+", "")) %>%
  mutate(artist = str_remove(artist, "Featuring.*")) %>%
  mutate(artist = str_replace_all(artist, "Bebe Rexha  F.*", "Bebe Rexha")) %>%
  mutate(title = str_replace_all(title, "Ones Who Got Me Here", "The Ones Who Got Me Here")) %>%
  mutate(title = str_replace_all(title, "Take Me Home Country Roads", "Take Me Home Country Roads original trailer soundtrack")) %>%
  mutate(artist = str_remove(artist, " With Thomas Rhett")) %>%
  mutate(artist = str_remove(artist, "  Lauren Daigle")) %>%
  mutate(artist = str_remove(artist, "  Kenny Chesney")) %>%
  mutate(artist = str_remove(artist, "  Blake Shelton")) %>%
  mutate(artist = str_remove(artist, " With Catherine Dunn")) %>%
  mutate(artist = str_remove(artist, " With Chris Young")) %>%
  mutate(artist = str_remove(artist, "  Sia")) %>%
  mutate(artist = str_remove(artist, "With Kix Brooks")) %>%
  mutate(artist = str_remove(artist, "Duet .*")) %>%
  mutate(artist = str_remove(artist, " With .*")) %>%
  mutate(artist = str_replace_all(artist, "Levi Hummon", "Levi Hummon scooter carusoe chris destefano")) %>%
  mutate(artist = str_replace_all(artist, "Tim McGraw  Faith Hill", "Tim McGraw and Faith Hill")) %>%
  mutate(artist = str_replace_all(artist, "Adam Calhoun  Hosier", "Calhoun and hosier")) %>%
  mutate(artist = str_replace_all(artist, "Big  Rich", "Big and Rich")) %>%
  mutate(title = str_replace_all(title, "Ones That Like Me", "The Ones That Like Me")) %>%
  mutate(title = str_replace_all(title, "  The Truth", " and the truth")) %>%
  mutate(artist = str_replace_all(artist, "Lennon  Maisy", "Lennon and Maisy")) %>%
  mutate(artist = str_replace_all(artist, "Jerrod Niemann  Lee Brice", "Jerrod Niemann")) %>%
  mutate(title = str_replace_all(title, "Boy  A Girl Thing", "Boy and A Girl Thing")) %>%
  mutate(artist = str_replace_all(artist, "Maddie  Tae", "Maddie and Tae")) %>%
  mutate(artist = str_replace_all(artist, "Artists Of Then Now  Forever", "Artists Of Then Now and Forever")) %>%
  mutate(artist = str_replace(artist, "Reba$", "Reba McEntire")) %>%
  mutate(artist = str_replace(artist, "Connie Britton  Hayden Panettiere", "Nashville Cast")) %>%
  mutate(title = str_replace_all(title, "Huntin Fishin  Lovin Every Day", "Huntin Fishin and Lovin Every Day")) %>%
  mutate(artist = str_replace_all(artist, "Clare Bowen  Sam Palladio", "Nashville Cast")) %>%
  mutate(artist = str_replace_all(artist, "Sam Palladio.*", "Nashville Cast")) %>%
  mutate(artist = str_replace_all(artist, "Todd Chrisley  Sara Evans", "Sara Evans")) %>%
  mutate(artist = str_replace_all(artist, "Zac Brown  Sir Rosevelt", "Sir Rosevelt")) %>%
  mutate(artist = str_replace_all(artist, "Willie Nelson  Merle Haggard", "Merle Haggard and Willie Nelson")) %>%
  mutate(title = str_replace_all(title, "John Cougar John Deere John 316", "John Cougar John Deere John 3 16")) %>%
  mutate(title = str_replace_all(title, "Last Thing I Needed First This Morning", "Last Thing I Needed First Thing This Morning")) %>%
  mutate(title = str_replace_all(title, "Deja Vu", "Deja Vu the voice performance")) %>%
  mutate(title = str_replace_all(title, "  YOU", " and YOU")) %>%
  mutate(artist = str_remove(artist, "  Colt Ford")) %>%
  mutate(artist = str_remove(artist, "  Ryan Upchurch")) %>%
  mutate(title = str_replace_all(title, "Speakers Bleachers And Preachers", "Speakers Preachers And Bleachers")) %>%
  mutate(artist = str_replace_all(artist, "Hillary Scott  The Scott Family", "Hillary Scott and The Scott Family")) %>%
  mutate(artist = str_remove(artist, " And Miranda Lambert"))

country_partial <-
  country_df %>%
  group_by(artist, title) %>%
  summarize(top_peak = min(peak)) %>%
  arrange(title)

for (i in 1:797) {
  print(i)
  print(country_partial$title[i])
  lyrics <- genius_lyrics(artist = country_partial$artist[i], song = country_partial$title[i])
}