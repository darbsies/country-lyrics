library(tidyverse)
library(geniusR)
library(stringr)
library(reshape2)
library(proxy)
library(lsa)

country_csv <- "country_output.csv"
hot_csv <- "hot_output.csv"
country_gender_csv <- "country_genders.csv"

country_hot_compare <- "country_hot_compare_csv.csv"

countryness_df <-
  read_csv(country_hot_compare) %>%
  dplyr::select(lyrics = Lyric_both, countryness = country_over_hot)

country_cosine <-
  country_partial %>%
  mutate(cosine_sim = 0)

countryness_for_vec <-
  countryness_df %>%
  arrange(lyrics) %>%
  dplyr::select(countryness)

countryness_vec <- countryness_for_vec$countryness

for (i in 1:length(country_partial$title)) {
  print(i)
  print(country_partial$title[i])
  song_lyrics <- genius_lyrics(artist = country_partial$artist[i], song = country_partial$title[i])
  
  len <- length(song_lyrics$lyric)
  countryness_list <- list()
  #creating the list that each song lyric will be added to
  for (k in 1:length(countryness_df$lyrics)) {
    word <- countryness_df$lyrics[k]
    #val <- countryness_df$countryness[k]
    countryness_list[[word]] <- 0
  }
  for (j in 1:len) {
    line <- str_to_lower(song_lyrics$lyric[j])
    line <- str_remove_all(line, "[:punct:]|[:digit:]")
    words_in_line <- str_split(line, " ")
    line_length <- length(words_in_line[[1]])
    for (k in 1:line_length) {
      word <- words_in_line[[1]][k]
      val <- countryness_list[[word]]
      if (is.null(val)) {
        #do nothing
      } else {
        #update value (add one)
        countryness_list[[word]] <- val + 1
      }
    }
  }
  #now convert to a vector and do cosine similarity
  lyric_to_compare <- melt(countryness_list)
  lyric_df <- as_data_frame(lyric_to_compare) %>%
    dplyr::select(word = L1, count = value) %>%
    arrange(word)
  lyric_vec <- lyric_df$count
  cosine_simi <- lsa::cosine(lyric_vec, countryness_vec)
  country_cosine$cosine_sim[i] <- cosine_simi[1][1]
}

country_cosine <-
  country_cosine %>%
  arrange(cosine_sim)
write_csv(country_cosine, path = "country_cosine.csv")


#hot music cosine w countryness

hot_cosine <-
  hot_edited %>%
  mutate(cosine_sim = 0)

for (i in 695:length(hot_edited$title)) {
  print(i)
  print(hot_edited$title[i])
  song_lyrics <- genius_lyrics(artist = hot_edited$artist[i], song = hot_edited$title[i])
  
  len <- length(song_lyrics$lyric)
  countryness_list <- list()
  #creating the list that each song lyric will be added to
  for (k in 1:length(countryness_df$lyrics)) {
    word <- countryness_df$lyrics[k]
    #val <- countryness_df$countryness[k]
    countryness_list[[word]] <- 0
  }
  for (j in 1:len) {
    line <- str_to_lower(song_lyrics$lyric[j])
    line <- str_remove_all(line, "[:punct:]|[:digit:]")
    words_in_line <- str_split(line, " ")
    line_length <- length(words_in_line[[1]])
    for (k in 1:line_length) {
      word <- words_in_line[[1]][k]
      val <- countryness_list[[word]]
      if (is.null(val)) {
        #do nothing
      } else {
        #update value (add one)
        countryness_list[[word]] <- val + 1
      }
    }
  }
  #now convert to a vector and do cosine similarity
  lyric_to_compare <- melt(countryness_list)
  lyric_df <- as_data_frame(lyric_to_compare) %>%
    dplyr::select(word = L1, count = value) %>%
    arrange(word)
  lyric_vec <- lyric_df$count
  cosine_simi <- lsa::cosine(lyric_vec, countryness_vec)
  hot_cosine$cosine_sim[i] <- cosine_simi[1][1]
}

hot_cosine <-
  read_csv("hot_cosine.csv") %>%
  mutate(in_country = 0) %>%
  arrange(title)
  left_join(country_songs_temp, by = c("artist", "title")) %>%
  select(artist, title, cosine_sim, in_country) %>%
  filter(in_country != 1) %>%
  arrange(cosine_sim)

for (i in 1:length(hot_edited$title)) {
  #check if in country
  hot_url <- gen_song_url(artist = hot_edited$artist[i], song = hot_edited$title[i])
  in_country <- country_urls[[hot_url]]
  if (is.null(in_country)) {
    hot_cosine$in_country[i] <- 0
  } else {
    hot_cosine$in_country[i] <- 1
  }
}


hot_cosine_edited <-
  hot_cosine %>%
  filter(in_country == 0) %>%
  select(artist, title, cosine_sim, in_country) %>%
  arrange(cosine_sim)
write_csv(hot_cosine_edited, path = "hot_cosine_edited.csv")

hot_cosine <-
  hot_cosine %>%
  left_join(country_songs_temp, by = c("artist", "title")) %>%
  
  
write_csv(hot_cosine, path = "hot_cosine.csv")

country_songs_temp <-
  country_partial %>%
  dplyr::select(artist, title) %>%
  mutate(in_country = 1)
  

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

country_gender <-
  read_csv(country_gender_csv)

country_w_gender <-
  country_partial %>%
  full_join(country_gender, by = c("artist", "title")) %>%
  filter(gender != "group")

country_female <-
  country_w_gender %>%
  filter(gender == "female")

country_male <-
  country_w_gender %>%
  filter(gender == "male")


#FEMALE LYRICS

#initialize list of country lyrics
female_lyrics <- list()
female_lyrics_count <- 0

#for loop of looping though songs
for (i in 1:length(country_female$title)) {
  print(i)
  print(country_female$title[i])
  song_lyrics <- genius_lyrics(artist = country_female$artist[i], song = country_female$title[i])
  
  len <- length(song_lyrics$lyric)
  for (j in 1:len) {
    line <- str_to_lower(song_lyrics$lyric[j])
    line <- str_remove_all(line, "[:punct:]|[:digit:]")
    words_in_line <- str_split(line, " ")
    line_length <- length(words_in_line[[1]])
    for (k in 1:line_length) {
      word <- words_in_line[[1]][k]
      val <- female_lyrics[[word]]
      if (is.null(val)) {
        #add word to vector
        female_lyrics[[word]] <- 1
      } else {
        #update value (add one)
        female_lyrics[[word]] <- val + 1
      }
      female_lyrics_count <- female_lyrics_count + 1
    }
  }
}


#converting list of lyrics to a vector for export
female_final <- melt(female_lyrics)
female_df <- as_data_frame(female_final) %>%
  dplyr::select(word = L1, count = value) %>%
  mutate(occurrence = (count / sum(count)) * 10000) %>%
  filter(!word %in% c("", " ", "  ", "   "))
write.csv(female_df, file = "female_occurrence.csv")



#MALE LYRICS

#initialize list of country lyrics
male_lyrics <- list()
male_lyrics_count <- 0

#for loop of looping though songs
for (i in 1:length(country_male$title)) {
  print(i)
  print(country_male$title[i])
  song_lyrics <- genius_lyrics(artist = country_male$artist[i], song = country_male$title[i])
  
  len <- length(song_lyrics$lyric)
  for (j in 1:len) {
    line <- str_to_lower(song_lyrics$lyric[j])
    line <- str_remove_all(line, "[:punct:]|[:digit:]")
    words_in_line <- str_split(line, " ")
    line_length <- length(words_in_line[[1]])
    for (k in 1:line_length) {
      word <- words_in_line[[1]][k]
      val <- male_lyrics[[word]]
      if (is.null(val)) {
        #add word to vector
        male_lyrics[[word]] <- 1
      } else {
        #update value (add one)
        male_lyrics[[word]] <- val + 1
      }
      male_lyrics_count <- male_lyrics_count + 1
    }
  }
}



#converting list of lyrics to a vector for export
male_final <- melt(male_lyrics)
male_df <- as_data_frame(male_final) %>%
  dplyr::select(word = L1, count = value) %>%
  mutate(occurrence = (count / sum(count)) * 10000) %>%
  filter(!word %in% c("", " ", "  ", "   "))
write.csv(male_df, file = "male_occurrence.csv")

#combining male and female
gender_lyrics <-
  male_df %>%
  dplyr::select(word, male_count = count, male_occurrence = occurrence) %>%
  full_join(female_df, by = c("word")) %>%
  dplyr::select(word, male_count, male_occurrence, female_count = count, female_occurrence = occurrence) %>%
  filter(male_count >= 2) %>%
  filter(female_count >= 2) %>%
  mutate(
    female_over_male = female_occurrence / male_occurrence,
    male_over_female = male_occurrence / female_occurrence
  )

write_csv(gender_lyrics, path = "gender_lyrics.csv")




#initialize list of country lyrics
country_lyrics <- list()
country_lyrics_count <- 0

#for loop of looping though songs
for (i in 1:length(country_partial$title)) {
  print(i)
  print(country_partial$title[i])
  song_lyrics <- genius_lyrics(artist = country_partial$artist[i], song = country_partial$title[i])
  
  len <- length(song_lyrics$lyric)
  for (j in 1:len) {
    line <- str_to_lower(song_lyrics$lyric[j])
    line <- str_remove_all(line, "[:punct:]|[:digit:]")
    words_in_line <- str_split(line, " ")
    line_length <- length(words_in_line[[1]])
    for (k in 1:line_length) {
      word <- words_in_line[[1]][k]
      val <- country_lyrics[[word]]
      if (is.null(val)) {
        #add word to vector
        country_lyrics[[word]] <- 1
      } else {
        #update value (add one)
        country_lyrics[[word]] <- val + 1
      }
      country_lyrics_count <- country_lyrics_count + 1
    }
  }
}


#converting list of lyrics to a vector for export
country_final <- melt(country_lyrics)
country_df <- as_data_frame(country_final) %>%
  dplyr::select(word = L1, count = value) %>%
  mutate(occurrence = (count / sum(count)) * 10000)
write.csv(country_df, file = "country_occurrence.csv")
  
country_urls <- list()
for (i in 1:length(country_partial$title)) {
  song_url <- gen_song_url(artist = country_partial$artist[i], song = country_partial$title[i])
  country_urls[[song_url]] <- 1
}





hot_og <- read_csv(hot_csv)

hot_df <-
  read_csv(hot_csv) %>%
  mutate(artist = str_sub(artist, 2, -2)) %>%
  mutate(title = str_sub(title, 2, -2)) %>%
  filter(!title %in% c("Vaina Loca", "A Girl Like You", "A Lonely Night", "Dame Tu Cosita", "Sin Pijama", "Despacito", "El Farsante", "Ella Quiere Beber", "Let It Be", "Te Bote", "What The World Needs Now Is Love", "When I Was Your Man", "Rotten To The Core")) %>%
  filter(!artist %in% c("Lindsey Stirling", "Sawyer Fredericks", "Jacquie Lee", "Sundance Head", "Emily Ann Roberts", "Christina Grimmie", "Chloe Kohanski", "Taylor John Williams", "Hannah Huston", "Lauren Duski", "Madi Davis", "Jordan Smith", "Jordan Smith & Adam Levine", "Jake Worthington", "Kimberly Nichole", "Josh Kaufman", "Barrett Baber", "Adam Wakefield", "Chris Jamison & Adam Levine", "Matt McAndrew", "Koryn Hawthorne", "PIKOTARO", "Christina Grimmie & Adam Levine")) %>%
  mutate(artist = str_replace_all(artist, "\\-", " ")) %>%
  mutate(title = str_replace_all(title, "\\-", " ")) %>%
  mutate(title = str_replace_all(title, "&", "and")) %>%
  mutate(artist = str_replace_all(artist, "&", "and")) %>%
  mutate(artist = str_replace_all(artist, "Featruing|Feauring", "Featuring")) %>%
  mutate(artist = str_remove(artist, "Featuring.*")) %>%
  mutate(artist = str_remove(artist, "and Kenny Chesney")) %>%
  mutate(title = str_replace(title, "\\(Freestyle\\)", "Freestyle")) %>%
  mutate(title = str_replace(title, "\\(Flex\\)", "Flex")) %>%
  mutate(title = str_replace(title, "\\(Lose It All\\)", "Lose It All")) %>%
  mutate(title = str_replace(title, "\\(To Your New Lover\\)", "To Your New Lover")) %>%
  mutate(title = str_replace(title, "\\(Cry\\)", "Cry")) %>%
  mutate(title = str_replace(title, "\\(Outro\\)", "Outro")) %>%
  mutate(title = str_replace(title, "\\(Saturday Night\\)", "Saturday Night")) %>%
  mutate(title = str_replace(title, "\\(Interlude\\)", "Interlude")) %>%
  mutate(title = str_replace(title, "\\(Instagram Models\\)", "Instagram Models")) %>%
  mutate(title = str_replace(title, "\\(Where Feet May Fail\\)", "Where Feet May Fail")) %>%
  mutate(title = str_replace(title, "\\(Where They From\\)", "Where They From")) %>%
  mutate(title = str_replace(title, "\\(Royalty\\)", "Royalty")) %>%
  mutate(title = ifelse(artist == "Bruno Mars and Cardi B", "Finesse Remix", title)) %>%
  mutate(title = str_remove(title, "\\(.*\\)")) %>%
  mutate(artist = str_replace_all(artist, "\\+", " ")) %>%
  mutate(artist = str_replace_all(artist, "\\/", "and")) %>%
  mutate(title = str_replace_all(title, "\\+", " ")) %>%
  mutate(title = str_replace_all(title, "1 Night", "1night")) %>%
  mutate(artist = str_replace_all(artist, "Burl Ives", "Burt Ives")) %>%
  mutate(title = str_remove_all(title, "\\.")) %>%
  mutate(artist = str_remove_all(artist, "\\.")) %>%
  mutate(artist = str_remove_all(artist, "\\'")) %>%
  mutate(title = str_remove_all(title, "\\'")) %>%
  mutate(artist = str_replace(artist, "Fat Joe, Remy Ma and Jay Z", "Fat joe and remy ma")) %>%
  mutate(title = str_replace_all(title, "Apes\\*\\*t", "Apeshit")) %>%
  mutate(artist = str_replace(artist, " x ", " and ")) %>%
  mutate(artist = str_replace(artist, " X ", " and ")) %>%
  mutate(title = str_replace(title, "B\\*\\*\\*\\*|B\\*tch|B\\*\\*ch", "Bitch")) %>%
  mutate(title = str_replace(title, "N\\*\\*\\*\\*z", "Niggaz")) %>%
  mutate(title = str_replace(title, "N\\*\\*\\*\\*s", "Niggas")) %>%
  mutate(title = str_replace(title, "P\\*\\*\\*\\*", "Pussy")) %>%
  mutate(title = str_replace(title, "F\\*\\*k", "Fuck")) %>%
  mutate(title = str_replace(title, "S\\*\\*t|Sh\\*t", "Shit")) %>%
  mutate(artist = str_remove(artist, "Duet .*")) %>%
  mutate(artist = str_remove(artist, " With .*")) %>%
  mutate(title = str_replace(title, "BBO", "BBO bad bitches only")) %>%
  mutate(title = str_replace(title, ">", "than")) %>%
  mutate(artist = str_replace(artist, "\\$", "s")) %>%
  mutate(artist = str_replace(artist, "2 Chainz, Drake and Quavo", "2 Chainz")) %>%
  mutate(artist = str_replace(artist, "CurrensY", "Curren Y")) %>%
  mutate(artist = str_replace(artist, "DJ Suede.*", "DJ Suede")) %>%
  mutate(artist = str_replace(artist, "Kanye West, Gu.*", "Kanye West")) %>%
  mutate(title = str_replace(title, "Chanel ", "Chanel go get it")) %>%
  mutate(artist = str_replace(artist, "Sofia Carson,.*", "Sofia Carson")) %>%
  mutate(artist = str_replace(artist, "Rae Sremmurd a.*", "Rae Sremmurd")) %>%
  mutate(artist = str_replace(artist, "The White Buffalo a.*", "The White Buffalo")) %>%
  mutate(title = str_replace(title, "Commas", "Fuck Up Some Commas")) %>%
  mutate(artist = str_replace(artist, "Natti Natasha a.*", "Natti Natasha")) %>%
  mutate(artist = str_replace(artist, "KCamp", "K Camp")) %>%
  mutate(title = str_replace(title, "Delirious.*", "Delirious Boneless")) %>%
  mutate(artist = str_replace(artist, "Tyga a.*", "Tyga")) %>%
  mutate(artist = str_replace(artist, "Kristen Bell.*", "Walt Disney Records")) %>%
  mutate(title = str_replace(title, "El Perdon", "El Perdon Forgiveness")) %>%
  mutate(artist = str_replace(artist, "Nicky Jam a.*", "Nicky Jam")) %>%
  mutate(artist = str_replace(artist, "AsAP", "A Ap")) %>%
  mutate(artist = str_replace(artist, "Lin Manuel Miranda and Ben Platt", "Ben Platt and Lin Manuel Miranda")) %>%
  mutate(title = str_replace(title, "Freeee .*", "Freeee Ghost Town pt 2")) %>%
  mutate(artist = str_replace(artist, "BloodPop", " and BloodPop")) %>%
  mutate(artist = str_replace(artist, "Missy \"Misdemeanor\" Elliott", "Missy Elliott")) %>%
  mutate(artist = str_remove(artist, "and John Legend|and Iggy Azalea|and Gucci Mane x Quavo")) %>%
  mutate(title = str_replace(title, "Good To Be Alive", "Good To Be Alive Hallelujah")) %>%
  mutate(title = str_replace(title, "Habits", "Habits Stay High")) %>%
  mutate(title = str_replace(title, "Higher We Go", "Higher We Go Intro")) %>%
  mutate(artist = str_replace(artist, "Machine Gun Kelly, and Ambassadors and Bebe Rexha", "Machine Gun Kelly, X Ambassadors and Bebe Rexha")) %>%
  mutate(title = str_replace(title, "Hot Boy", "Hot Nigga")) %>%
  mutate(title = str_replace(title, "How Would You Feel ", "How Would You Feel Paean")) %>%
  mutate(title = str_replace(title, "I Have Nothing", "I Have Nothing The Voice Performance")) %>%
  mutate(title = str_replace(title, "I Need Your Love", "Habibi I Need Your Love")) %>%
  mutate(title = str_replace(title, "I Was Jack", "I Was Jack You Were Diane")) %>%
  mutate(artist = str_replace(artist, "Quavo and Lil Yachty|Quavo, Takeoff and Offset", "Quality Control")) %>%
  mutate(title = str_replace(title, "Infinity ", "Infinity 888")) %>%
  mutate(title = str_replace(title, "Juju On That Beat ", "Juju On That Beat TZ Anthem")) %>%
  mutate(artist = str_remove(artist, " and Zayion McCall")) %>%
  mutate(artist = str_remove(artist, " and and Ambassadors")) %>%
  mutate(title = str_replace(title, "Krippy Kush", "Krippy Kush Remix")) %>%
  mutate(artist = str_replace(artist, "Farruko, Nicki Minaj, Bad Bunny, 21 Savage and Rvssian", "Rvssian Farruko and Bad Bunny")) %>%
  mutate(artist = str_remove(artist, " and Shakira")) %>%
  mutate(artist = str_remove(artist, " and Zedd")) %>%
  mutate(artist = str_remove(artist, " and Zendaya")) %>%
  mutate(artist = str_remove(artist, " and The Weeknd")) %>%
  mutate(artist = str_remove(artist, " and Justin Timberlake")) %>%
  mutate(artist = str_remove(artist, " And Miranda Lambert")) %>%
  mutate(artist = str_remove(artist, " and Lin Manuel Miranda")) %>%
  mutate(artist = str_remove(artist, " and Cedric Gervais")) %>%
  mutate(artist = str_replace(artist, "N\\*E\\*R\\*D", "NERD")) %>%
  mutate(artist = str_replace(artist, "Janet.*", "Janet Jackson")) %>%
  mutate(artist = str_replace(artist, "Matt McAndrew and Adam Levine", "Adam Levine")) %>%
  mutate(artist = str_replace(artist, "G Eazy and Bebe Rexha", "G Eazy")) %>%
  mutate(title = str_replace(title, "Move That Doh", "Move That Dope")) %>%
  mutate(artist = str_replace(artist, "A Trak   Milo and Otis ", "A Trak and Milo and Otis")) %>%
  mutate(artist = str_replace(artist, "SOB and RBE", "SOB x RBE")) %>%
  mutate(artist = str_replace(artist, "Lillywood and Robin Schulz", "Lilly Wood and The Prick")) %>%
  mutate(title = str_replace(title, "\\$ex", "Sex")) %>%
  mutate(artist = str_replace(artist, "Kid Ink,.*", "Kid Ink")) %>%
  mutate(artist = str_remove(artist, ", Wiz Khalifa and Ty Dolla sign")) %>%
  mutate(title = str_replace(title, "Pt1", "Pt 1")) %>%
  mutate(title = str_replace(title, "^Smile ", "Smile Bitch")) %>%
  mutate(title = str_replace(title, "Summertime Sadness", "Summertime Sadness Cedric Gervais Remix")) %>%
  mutate(title = str_replace(title, "The Fox", "The Fox What Does The Fox Say")) %>%
  mutate(artist = str_remove(artist, ", Keala Settle, Zac Efron, Zendaya and The Greatest Showman Ensemble")) %>%
  mutate(title = str_replace(title, "For A Broke Heart", "For A Broken Heart Why Am I So In Love")) %>%
  mutate(artist = str_remove(artist, "and The Greatest Showman Ensemble")) %>%
  mutate(title = str_replace(title, "Thunder/Young Dumb and Broke", "Thunder/Young Dumb and Broke Medley")) %>%
  mutate(artist = str_replace(artist, "  Khalid", " and Khalid")) %>%
  mutate(artist = str_replace(artist, "Labrinth, Sia and Diplo Present LSD", "LSD")) %>%
  mutate(artist = str_replace(artist, "Young Money", "Drake")) %>%
  mutate(title = str_replace(title, "Untitled 02 l 06232014", "Untitled 02 06232014")) %>%
  mutate(title = str_replace(title, "Untitled 07 l Levitate", "Untitled 07 Levitate")) %>%
  mutate(title = str_replace(title, "Up Down ", "Up Down Do This All Day")) %>%
  mutate(artist = str_replace(artist, "Gucci Mane and Bruno Mars X Kodak Black", "Gucci Mane Bruno Mars and Kodak Black")) %>%
  mutate(title = str_replace(title, "\\[The 2014 FIFA World Cup Official Song\\]", "Ole Ola")) %>%
  mutate(artist = str_remove(artist, ", Thomas Doherty and Dylan Playfair")) %>%
  mutate(artist = str_replace(artist, "Dwayne Johnson", "Dwayne The Rock Johnson"))




hot_edited <-
  hot_df %>%
  group_by(artist, title) %>%
  summarize(top_peak = min(peak)) %>%
  arrange(title)

for (i in 1639:2248) {
  print(i)
  print(hot_edited$title[i])
  lyrics <- genius_lyrics(artist = hot_edited$artist[i], song = hot_edited$title[i])
}

#MAKING HOT DATASET 

#initialize list of hot lyrics
hot_lyrics <- list()
hot_lyrics_count <- 0


#for loop of looping though songs
for (i in 1:length(hot_edited$title)) {
  print(i)
  print(hot_edited$title[i])
  #check if in country
  hot_url <- gen_song_url(artist = hot_edited$artist[i], song = hot_edited$title[i])
  in_country <- country_urls[[hot_url]]
  if (is.null(in_country)) {
    song_lyrics <- genius_lyrics(artist = hot_edited$artist[i], song = hot_edited$title[i])
    len <- length(song_lyrics$lyric)
    for (j in 1:len) {
      line <- str_to_lower(song_lyrics$lyric[j])
      line <- str_remove_all(line, "[:punct:]|[:digit:]")
      words_in_line <- str_split(line, " ")
      line_length <- length(words_in_line[[1]])
      for (k in 1:line_length) {
        word <- words_in_line[[1]][k]
        val <- hot_lyrics[[word]]
        if (is.null(val)) {
          #add word to vector
          hot_lyrics[[word]] <- 1
        } else {
          #update value (add one)
          hot_lyrics[[word]] <- val + 1
        }
        hot_lyrics_count <- hot_lyrics_count + 1
      }
    }
  } else {
    print("in country")
  }
}

#converting list of lyrics to a vector for export
hot_final <- melt(hot_lyrics)
hot_df <- as_data_frame(hot_final) %>%
  dplyr::select(word = L1, count = value) %>%
  mutate(occurrence = (count / sum(count)) * 10000)
write.csv(hot_df, file = "hot_occurrence.csv")


gen_song_url(artist = country_partial$artist[623], song = country_partial$title[623])