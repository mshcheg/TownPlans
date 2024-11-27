library(tidyverse)
library(pdftools)
library(stringr)
library(wordcloud)
library(tm)
library(SnowballC)
library(RColorBrewer)
library(sf)
library(mapview)
library(tesseract)

townDB <- read.csv("/Users/mshchegl/OneDrive - University of Vermont/Documents/Research/Town Plans/TownPlans.csv")
range(townDB$Year.Adopted, na.rm = T)
median(townDB$Year.Adopted, na.rm = T)

setwd("/Users/mshchegl/OneDrive - University of Vermont/Documents/Research/Town Plans/")
towns_shp <- st_read("ShapeFiles/FS_VCGI_OPENDATA_Boundary_BNDHASH_poly_towns_SP_v1_8415479404540794356/FS_VCGI_OPENDATA_Boundary_BNDHASH_poly_towns_SP_v1.shp")
Town_pop23 <- read.csv("HSI-STAT-Population-of-Vermont-towns-1930-2023.csv") %>%
  select(NAME, X2023)
  
townDB_sub <- townDB %>%
  filter(Type %in% c("Town", "City")) %>%
  filter(!is.na(Year.Adopted))

##
#Download PDFs Only Run Once
oldw <- getOption("warn")
options(warn = -1)

for (i in 1:nrow(townDB_sub)){
  file <- paste(townDB_sub[i,"Municipality"], ".pdf", sep = "")
  path <- "/Users/mshchegl/OneDrive - University of Vermont/Documents/Research/Town Plans/"
  URL <- townDB_sub[i,"Plan.PDF"]
  tryCatch(download.file(URL, paste(path, file, sep = ""), mode = "wb"),
           error = function(e) print(paste(file, 'did not work out')))
}

options(warn = oldw)
##

my_stop_words <- c("chapter", "home", "homes", "housing",
                   "can", "vermont", "town", "plan",
                   "’s", "page", "figure", "table", "area", "areas",
                   "uses", "shall", "also", "use", "city", "town")

#test workflow on Bennington
Bennington_text <- pdftools::pdf_text("Plan_PDFs/Bennington.pdf")
#Contents <- Bennington_text[grep("Table of Contents", Bennington_text)]
#Chapters <- Contents %>%
#  tolower() %>%
#  str_extract_all("chapter \\d.*\\w.*\\d") %>%
#  unlist() %>%
#  gsub("\\s+", " ", .) %>%
#  gsub("\n", " ", .)

#Bennington_Housing <- Bennington_text[grep("Chapter 5. Housing", Bennington_text)]

#Start <- tail(grep("Table of Contents", Bennington_text), 1)+1

#Bennington_string <- paste(Bennington_text, collapse = " ")
Bennington_words <- Bennington_text %>%
  removeNumbers() %>%
  as.data.frame() %>%
  rename(text = 1) %>%
  unnest_tokens(word, text) %>%
  count(word, sort = T) %>%
  mutate(total = sum(n),
         town = "bennington") %>%
  mutate(rank = row_number()) %>%
  filter(.,grepl("[A-Za-z]", word))

bennington_bigrams <- Bennington_text %>%
  removeNumbers() %>%
  as.data.frame() %>%
  rename(text = 1) %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 3) %>%
  mutate(town = "bennington") %>%
  filter(!is.na(bigram)) 

#Bennington_raw <-
#  Bennington_string %>%
#  # Remove the line breaks
#  gsub("\n", " ", .) %>%
#  # Collapse blank spaces down to single space
#  gsub("\\s+", " ", .) %>%
#  tolower() %>%
#  removeNumbers() %>%
#  removeWords(stopwords("english")) %>%
#  removeWords(c("bennington", my_stop_words)) %>%
#  gsub("–", " ", .) %>%
#  gsub("—*", "", .) %>%
#  gsub("•", " ", .) %>%
#  gsub("“", "", .) %>%
#  gsub("”", "", .) %>%
#  gsub("■", "", .) %>%
#  gsub("’", "", .) %>%
#  gsub("‘", "", .) %>%
#  removePunctuation() %>%
#  stripWhitespace()

#http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know

#Bennington_dtm <- TermDocumentMatrix(Bennington_raw)
#Bennington_m <- as.matrix(Bennington_dtm)
#Bennington_v <- sort(rowSums(Bennington_m),decreasing=TRUE)
#Bennington_d <- data.frame(word = names(Bennington_v),freq=Bennington_v) %>% filter(.,grepl("[A-Za-z]", word))

#head(Bennington_d, 100)

#set.seed(1234)
#wordcloud(words = Bennington_d$word, freq = Bennington_d$freq, min.freq = 2,
#          max.words=20, random.order=FALSE, rot.per=0.35, 
#          colors=brewer.pal(8, "Dark2"))

#test workflow on Royalton
Royalton_text <- pdftools::pdf_text("Plan_PDFs/Royalton.pdf")

Royalton_words <- Royalton_text %>%
  removeNumbers() %>%
  as.data.frame() %>%
  rename(text = 1) %>%
  unnest_tokens(word, text) %>%
  count(word, sort = T) %>%
  mutate(total = sum(n),
         town = "royalton") %>%
  mutate(rank = row_number()) %>%
  filter(.,grepl("[A-Za-z]", word))

Royalton_bigrams <- Royalton_text %>%
  removeNumbers() %>%
  as.data.frame() %>%
  rename(text = 1) %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 3) %>%
  mutate(town = "royalton") %>%
  filter(!is.na(bigram)) 

#Royalton_string <- paste(Royalton_text, collapse = " ")

#Royalton_raw <-
#  Royalton_string %>%
  # Remove the line breaks
#  gsub("\n", " ", .) %>%
  # Collapse blank spaces down to single space
#  gsub("\\s+", " ", .) %>%
#  tolower() %>%
#  removeNumbers() %>%
#  removeWords(stopwords("english")) %>%
#  removeWords(c("royalton", my_stop_words)) %>%
#  gsub("–", " ", .) %>%
#  gsub("—*", "", .) %>%
#  gsub("•", " ", .) %>%
#  gsub("“", "", .) %>%
#  gsub("”", "", .) %>%
#  gsub("■", "", .) %>%
#  gsub("’", "", .) %>%
#  gsub("‘", "", .) %>%
#  removePunctuation() %>%
#  stripWhitespace()

#http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know

#Royalton_dtm <- TermDocumentMatrix(Royalton_raw)
#Royalton_m <- as.matrix(Royalton_dtm)
#Royalton_v <- sort(rowSums(Royalton_m),decreasing=TRUE)
#Royalton_d <- data.frame(word = names(Royalton_v),freq=Royalton_v)

View(rbind(Bennington_words, Royalton_words))

set.seed(1234)
wordcloud(words = Royalton_d$word, freq = Royalton_d$freq, min.freq = 2,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
####

Freq_Tab_All <- rbind(Bennington_words, Royalton_words)
Freq_Tab_Bigram <- rbind(bennington_bigrams, Royalton_bigrams)

files <- list.files("Plan_PDFs/")
sub_files <- files[!files %in% c("Bennington.pdf", "Royalton.pdf")]

for (i in sub_files){
  my_town <- str_replace(i, ".pdf", "") %>% tolower()
  if (grepl("_", my_town) == TRUE){
    my_town = gsub("_", " ", my_town)}
  print(my_town)
  town_text <- pdftools::pdf_text(paste("Plan_PDFs/", i, sep=""))
  if (town_text[1] == "" & town_text[2] == "") {
    town_text <- pdf_ocr_text(paste("Plan_PDFs/", i, sep=""))
    if (town_text[1] == "" & town_text[2] == ""){
    print(paste("reading file", i, "failed."))
    next}
  }
  #  town_Housing <- town_text[grep("Housing", town_text)]
  #  if (length(town_Housing) == 0) next
  #  town_string <- paste(town_Housing, collapse = " ")
  town_words <- town_text %>%
    removeNumbers() %>%
    as.data.frame() %>%
    rename(text = 1) %>%
    unnest_tokens(word, text) %>%
    count(word, sort = T) %>%
    mutate(total = sum(n),
           town = my_town) %>%
    mutate(rank = row_number()) %>%
    filter(.,grepl("[A-Za-z]", word))
  
  town_bigrams <- town_text %>%
    removeNumbers() %>%
    as.data.frame() %>%
    rename(text = 1) %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 3) %>%
    mutate(town = my_town) %>%
    filter(!is.na(bigram))
  
  #  Freq_Tab <- full_join(Freq_Tab, town_d, by = "word") %>%
  #    rename(!!my_town := freq)
  Freq_Tab_All <- rbind(Freq_Tab_All, town_words)
  Freq_Tab_Bigram <- rbind(Freq_Tab_Bigram, town_bigrams)
}

Town_tf_idf <- Freq_Tab_All %>%
  mutate(word = gsub("’s", "", word)) %>%
  mutate(word = gsub("'s", "", word)) %>%
  mutate(word = gsub("___", "", word)) %>% 
  mutate(word = gsub("__", "", word)) %>%
  mutate(word = gsub("_", "", word)) %>%
  select(word, town, n) %>%
  group_by(word, town) %>%
  summarize(n = sum(n,na.rm = T)) %>%
  filter(nchar(word) > 2 & nchar(word) < 20) %>%
  filter(!str_detect(word, pattern = "[0123456789]")) %>%
  bind_tf_idf(word, town,n)

Town_tf_idf %>%
  arrange(desc(tf_idf)) %>%
  View

library(forcats)

towns <- unique(Town_tf_idf$town)

Town_tf_idf <- Town_tf_idf %>%
  filter(nchar(word) > 2 & nchar(word) < 20) %>%
  filter(!str_detect(word, pattern = "[0123456789]"))

sentiments <- get_sentiments("nrc")

Town_tf_idf %>%
  inner_join(sentiments) %>%
  group_by(sentiment, town) %>%
  summarise(count = n()) %>%
  filter(sentiment == "positive" | sentiment == "negative") %>%
  View

write.csv(Town_tf_idf, "WordFreqIDF.csv")
write.csv(Freq_Tab_Bigram, "BigramFreq.csv")

library(igraph)
library(ggraph)
library(ggrepel)
set.seed(2017)
set.seed(2024)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
my_stop_words <- c("m", "h", "d", "royalton", "rbes", "iii", "viii", "vii", "xiv",
                   "royalton's", "e", "g", "c", "k", "windsor", "crawford", "drb",
                   "rb", "essex")

towns <- unique(Freq_Tab_Bigram$town)

Freq_Tab_Bigram %>%
#  filter(str_detect(bigram, "development")) %>%
  filter(str_detect(bigram, "recreation")) %>%
#   filter(str_detect(bigram, "forest")) %>%
  separate(bigram, c("word1", "word2", "word3"), sep = " ") %>%
  count(word1, word2, word3, sort = TRUE) %>%
#  filter(str_detect(word1, "forest") | 
#   str_detect(word2, "forest") | str_detect(word3, "forest")) %>%
#  filter(str_detect(word1, "development") | 
#  str_detect(word2, "development") | str_detect(word3, "development")) %>%
  filter(str_detect(word1, "recreation") | 
          str_detect(word2, "recreation") | str_detect(word3, "recreation")) %>%
  filter(!word1 %in% stopwords("english") & 
           !word2 %in% stopwords("english") & !word3 %in% stopwords("english")) %>%
  filter(!word1 %in% my_stop_words & 
           !word2 %in% my_stop_words & 
           !word3 %in% my_stop_words) %>%
# for forest / development
#  filter(n > 20) %>%
#for recreation
  filter(n > 10) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = T) +
  theme_void()

cities <- c("vergennes", "burlington", "south burlington", "winooski", 	
            "montpelier")

VT_population_summary <- VT_population %>%
  select(GEOID, NAME, total_popE) %>%
  separate(NAME, c("town", "county", "state"), ",") %>%
  mutate(town = tolower(town))

VT_population_summary %>%
  mutate(pop_group = case_when(total_popE <= 100 ~ "<= 100",
                               total_popE > 100 & total_popE <= 1000 ~ "100-1000",
                               total_popE > 1000 & total_popE <=5000 ~ "1000-5000",
                               total_popE > 5000 & total_popE <=10000 ~ "5000-10000",
                               total_popE > 10000 ~ "> 10000")) %>%
  group_by(pop_group) %>%
  summarise(count = n()) %>%
  View

Freq_Tab_Bigram_pop <- Freq_Tab_Bigram %>%
  mutate(town = case_when(str_detect(town, "city") ~ town,
                          str_detect(town, " town") ~ town,
                          town %in% cities ~ paste(town, "city", sep = " "),
                          .default = paste(town, "town", sep = " "))) %>%
  left_join(., VT_population_summary, by = "town")

range(VT_population_summary$total_popE)
plot(VT_population_summary$total_popE)

Freq_Tab_Bigram_pop %>%
  filter(!is.na(total_popE)) %>%
#  filter(total_popE <= 100) %>%
#  filter(total_popE > 100 & total_popE <= 1000) %>%
#  filter(total_popE > 1000 & total_popE <= 5000) %>%
#  filter(total_popE > 5000 & total_popE <= 10000) %>%
  filter(total_popE > 10000) %>%
  filter(str_detect(bigram, "development")) %>%
  separate(bigram, c("word1", "word2", "word3"), sep = " ") %>%
  count(word1, word2, word3, sort = TRUE) %>%
  filter(str_detect(word1, "development") | 
           str_detect(word2, "development") | str_detect(word3, "development")) %>%
  filter(!word1 %in% stopwords("english") & 
           !word2 %in% stopwords("english") & !word3 %in% stopwords("english")) %>%
  filter(!word1 %in% my_stop_words & 
           !word2 %in% my_stop_words & 
           !word3 %in% my_stop_words) %>%
  #no filter for 100, 8 filter for 100-1000, 20 filter for 1000-5000, 
  filter(n > 5) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = T) +
  theme_void()

my_stop_words_freq <- c("use", "town", "plan", "areas", "vermont",
                        "area", "village", "will", "adopted")

towns <- unique(Freq_Tab_All$town)

Freq_Tab_All %>%
  mutate(town = case_when(str_detect(town, "city") ~ town,
                          str_detect(town, " town") ~ town,
                          town %in% cities ~ paste(town, "city", sep = " "),
                          .default = paste(town, "town", sep = " "))) %>%
  left_join(., VT_population_summary, by = "town") %>%
  mutate(pop_group = case_when(total_popE <= 100 ~ "<= 100",
                               total_popE > 100 & total_popE <= 1000 ~ "100-1000",
                               total_popE > 1000 & total_popE <=5000 ~ "1000-5000",
                               total_popE > 5000 & total_popE <=10000 ~ "5000-10000",
                               total_popE > 10000 ~ "> 10000")) %>%
  filter(!word %in% stopwords("english")) %>%
  filter(!word %in% my_stop_words_freq) %>%
  group_by(word, pop_group) %>%
  summarise(total = sum(n)) %>%
  filter(pop_group == "<= 100") %>%
  filter(nchar(word) > 2) %>%
  filter(!word %in% towns) %>%
  arrange(desc(total)) %>%
  head(10) %>%
  ggplot(aes(y=reorder(word, -total), x=total)) +
  geom_col() +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_y_discrete(limits = rev) +
  theme(axis.text.y = element_text(size=10)) +
  #  coord_flip() +
  labs(x="Total", y="") +
  theme_bw() +
  theme(panel.grid.major.y = element_blank())
  
  

Freq_Tab_All %>%
  mutate(town = case_when(str_detect(town, "city") ~ town,
                          str_detect(town, " town") ~ town,
                          town %in% cities ~ paste(town, "city", sep = " "),
                          .default = paste(town, "town", sep = " "))) %>%
  left_join(., VT_population_summary, by = "town") %>%
  mutate(pop_group = case_when(total_popE <= 100 ~ "<= 100",
                               total_popE > 100 & total_popE <= 1000 ~ "100-1000",
                               total_popE > 1000 & total_popE <=5000 ~ "1000-5000",
                               total_popE > 5000 & total_popE <=10000 ~ "5000-10000",
                               total_popE > 10000 ~ "> 10000")) %>%
  filter(!word %in% stopwords("english")) %>%
  filter(!word %in% my_stop_words_freq) %>%
  filter(nchar(word) > 2) %>%
  filter(pop_group == "<= 100") %>%
  View

AT_Towns <- c("bennington", "manchester", "norwich")
Velomont_Towns <- c("readsboro", "wilmington", "dover", "manchester", 
                    "dorset", "wells", "poultney", "rutland town", 
                    "rochester", "randolph", "waitsfield", "richmond", 
                    "waterbury", "stowe", "morrisville", "hardwick",
                    "greensboro", "st. johnsbury", "west burke", "derby")  %>%
  sort()
Ski_Towns <- c("dover", "stratton", "peru", "londonerry",
               "ludlow", "killington", "warren", "buels gore",
               "bolton", "stowe", "morses mill", "jay" )

Recreation <- Freq_Tab_All %>%
  add_row(word = "Total", summarise(.,across(where(is.numeric), ~sum(.x, na.rm=T)))) %>%
  filter(word %in% c(
    #General
    "recreation", "recreating", "recreate",
    "trail", "trails",
    #Environments
    "forest", "forests",
    "mountain", "mountains",
    "appalachian",
    "river", "rivers", 
    "wetland", "wetlands", 
    "lake", "lakes", 
    "stream", "streams",
    #Vermont Specific
    "velomont",
    #Activities
    "hike", "hiking",
    "bike", "biking",
    "skateboard", "skateboarding",
    "run", "running",
    "backpacking", "backpack", 
    "camp","camping", 
    "ski", "skiing", "snowboard", "snowboarding",
    "ohv", "atv", "utv",
    "hunting", "hunt", 
    "fishing", "fish",
    "forage", "foraging", 
    "kayaking", "canoe", "canoeing", "boat", "boating", "raft", "rafting",
    #Totals                  
    "Total")) %>%
  pivot_longer(2:ncol(Freq_Tab_All), names_to = "town", values_to = "count" ) %>%
  pivot_wider(names_from = word, values_from = count) %>%
  group_by(town) %>%
  mutate(recreation_tot = sum(recreation, recreating, recreate, na.rm = T), 
         forest_tot = sum(forest, forests, na.rm = T),
         mountain_tot = sum(mountain, mountains, na.rm = T),
         trail_tot = sum(trail, trails, na.rm = T), 
         hike_tot = sum(hike, hiking, na.rm = T),
         bike_tot = sum(bike, biking, na.rm = T),
         skate_tot = sum(skateboard, skateboarding, na.rm=T),
         run_tot = sum(run, running, na.rm=T),
         backpack_tot = sum(backpacking, backpack, na.rm=T), 
         camp_tot = sum(camp, camping, na.rm=T), 
         snow_tot = sum(ski, skiing, snowboard, snowboarding, na.rm=T),
         rec_veh = sum(ohv, atv, utv, na.rm = T),
         hunt_tot = sum(hunting, hunt, fishing, fish, na.rm = T), 
         forag_tot = sum(forage, foraging, na.rm = T), 
         water_tot = sum(kayaking, canoe, canoeing, boat, boating, raft, rafting, na.rm=T),
         river_tot = sum(river, rivers, na.rm = T),
         wetland_tot = sum(wetland, wetlands, na.rm = T),
         lake_tot = sum(lake, lakes, na.rm = T),
         stream_tot = sum(stream, streams, na.rm = T)) %>%
  select(town, Total, recreation_tot, trail_tot, 
         forest_tot, mountain_tot, appalachian, river_tot,
         wetland_tot, lake_tot, stream_tot, velomont, hike_tot,
         bike_tot, skate_tot, run_tot, backpack_tot, 
         camp_tot, snow_tot, rec_veh, hunt_tot,
         water_tot) %>%
  rename(recreation = recreation_tot,
         trail = trail_tot,
         forest = forest_tot,
         mountain = mountain_tot,
         river = river_tot,
         wetland = wetland_tot,
         lake = lake_tot,
         stream = stream_tot,
         biking = bike_tot,
         skateboarding = skate_tot,
         hiking = hike_tot,
         runing = run_tot,
         backpacking = backpack_tot,
         camping = camp_tot,
         `snow sports` = snow_tot,
         `water sports` = water_tot,
         `off roading` = rec_veh,
         `hunting and fishing` = hunt_tot) %>%
  select(town, recreation, trail, 
         forest, mountain, appalachian, river,
         wetland, lake, stream, velomont, hiking,
         biking, skateboarding, runing, backpacking, 
         camping, `snow sports`, `water sports`, `off roading`, `hunting and fishing`)

View(Recreation)
  
Recreation %>%
  pivot_longer(cols = 2:ncol(Recreation), names_to = "word") %>%
  group_by(word) %>%
  summarise(total = sum(value, na.rm = T)) %>%
  filter(word != "skateboarding") %>%
  ggplot(aes(y=reorder(word, -total), x=total)) +
  geom_col() +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_y_discrete(limits = rev) +
  theme(axis.text.y = element_text(size=10)) +
#  coord_flip() +
  labs(x="Total", y="") +
  theme_bw() +
  theme(panel.grid.major.y = element_blank())

Recreation %>%
  filter(town %in% AT_Towns) %>%
  pivot_longer(cols = 2:ncol(Recreation), names_to = "word") %>%
  group_by(word) %>%
  summarise(total = sum(value, na.rm = T)) %>%
  filter(word != "skateboarding") %>%
  filter(total > 0) %>%
  ggplot(aes(y=reorder(word, -total), x=total)) +
  geom_col() +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_y_discrete(limits = rev) +
  theme(axis.text.y = element_text(size=10)) +
  #  coord_flip() +
  labs(x="Total", y="") +
  theme_bw() +
  theme(panel.grid.major.y = element_blank())

Recreation %>%
  filter(town %in% Ski_Towns) %>%
  pivot_longer(cols = 2:ncol(Recreation), names_to = "word") %>%
  group_by(word) %>%
  summarise(total = sum(value, na.rm = T)) %>%
  filter(word != "skateboarding") %>%
  filter(total > 0) %>%
  ggplot(aes(y=reorder(word, -total), x=total)) +
  geom_col() +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_y_discrete(limits = rev) +
  theme(axis.text.y = element_text(size=10)) +
  #  coord_flip() +
  labs(x="Total", y="") +
  theme_bw() +
  theme(panel.grid.major.y = element_blank())

Recreation %>%
  filter(town %in% AT_Towns) %>%
  pivot_longer(cols = 2:ncol(Recreation), names_to = "word") %>%
  filter(word != "skateboarding") %>%
  filter(value > 0 & !is.na(value)) %>%
  ggplot(aes(y=reorder(word, -value), x=value, fill = town)) +
  geom_col() +
  facet_grid(town ~ .) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_y_discrete(limits = rev) +
  theme(axis.text.y = element_text(size=10)) +
  #  coord_flip() +
  labs(x="Total", y="") +
  theme_bw() +
  theme(panel.grid.major.y = element_blank())

Recreation %>%
  filter(town %in% Velomont_Towns) %>%
  pivot_longer(cols = 2:ncol(Recreation), names_to = "word") %>%
  group_by(word) %>%
  summarise(total = sum(value, na.rm = T)) %>%
  filter(word != "skateboarding") %>%
  filter(total > 0) %>%
  ggplot(aes(y=reorder(word, -total), x=total)) +
  geom_col() +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_y_discrete(limits = rev) +
  theme(axis.text.y = element_text(size=10)) +
  #  coord_flip() +
  labs(x="Total", y="") +
  theme_bw() +
  theme(panel.grid.major.y = element_blank())

Rank <- Recreation %>%
  filter(town %in% Velomont_Towns) %>%
  pivot_longer(cols = 2:ncol(Recreation), names_to = "word") %>%
  group_by(word) %>%
  summarise(total = sum(value, na.rm = T))

Recreation %>%
  filter(town %in% Velomont_Towns) %>%
  pivot_longer(cols = 2:ncol(Recreation), names_to = "word") %>%
  filter(word != "skateboarding") %>%
  filter(value > 0 & !is.na(value)) %>%
  left_join(., Rank, by = "word") %>%
  filter(town %in% Velomont_Towns[1:4]) %>%
  ggplot(aes(y=reorder(word, -total), x=value, fill = town)) +
  geom_col() +
  facet_grid(town ~ .) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_y_discrete(limits = rev) +
  theme(axis.text.y = element_text(size=10)) +
  #  coord_flip() +
  labs(x="Total", y="") +
  theme_bw() +
  theme(panel.grid.major.y = element_blank())

Recreation %>%
  mutate(total = rowSums(across(where(is.numeric)), na.rm=T)) %>%
  arrange(-total) %>%
  head(10) %>%
  select(-total) %>%
  pivot_longer(cols = 2:ncol(Recreation), names_to = "word") %>%
  filter(word != "skateboarding") %>%
  filter(value > 0 & !is.na(value)) %>%
  ggplot(aes(y=reorder(word, -value), x=value, fill = town)) +
  geom_col() +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_y_discrete(limits = rev) +
  theme(axis.text.y = element_text(size=10)) +
  #  coord_flip() +
  labs(x="Total", y="") +
  theme_bw() +
  theme(panel.grid.major.y = element_blank())

#n-grams for housing and recreation - https://www.tidytextmining.com/ngrams.html
library(tidytext)

View(get_sentiments("nrc"))

