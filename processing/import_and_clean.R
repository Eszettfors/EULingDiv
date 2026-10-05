library(tidyverse)
library(readxl)
library(rnaturalearth)

# import and clean data from 2024 -----------

l1_24 = read_excel("data/raw/eu_lang_survey_2024.xlsx", sheet = "L1", skip = 7)
l2_24 = read_excel("data/raw/eu_lang_survey_2024.xlsx", sheet = "L2", skip = 7)
l3_24 = read_excel("data/raw/eu_lang_survey_2024.xlsx", sheet = "L3", skip = 7)
l4_24 = read_excel("data/raw/eu_lang_survey_2024.xlsx", sheet = "L4", skip = 7)

clean_and_long_2024 = function(df, speaker_type = "L1"){
  
  df = df[, 2:ncol(df)]
  df = df %>% select(-c(`D-W`, `D-E`)) #remove east and west germany
  df = df[-c(1,2, 93:98),-c(2)] # remove eu_sum, total, and "do not know" or "has no mother tongue" answers
  colnames(df)[1] = c("language") #change first col name
  
  df_countries = df %>%
    select(!language) %>%
    colnames() %>%
    as.data.frame() #gather countries acronymes
  
  eng_names = df[1:nrow(df) %% 2 == 0,] #Select rows with english language names
  language_names = eng_names$language #store language names
  
  df = df[1:nrow(df) %% 2 == 1,] #select rows with nominal values
  df$language = language_names # add language column
  
  df[,2:ncol(df)] = mapply(as.numeric ,df %>% select(BE:SE)) # make numeric
  df[,2:ncol(df)][is.na(df[,2:ncol(df)])] = 0 #replace NA with 0
  
  # pivot_longer
  df = df %>%
    pivot_longer(cols = !c(language), 
                 names_to = "country_code",
                 values_to = paste0(speaker_type, "_speakers"))
  
  return(df)
  
}

l1_24_long = clean_and_long_2024(l1_24, "L1")
l2_24_long = clean_and_long_2024(l2_24, "L2")
l3_24_long = clean_and_long_2024(l3_24, "L3")
l4_24_long = clean_and_long_2024(l4_24, "L4")

# join dfs
long_speakers_2024 = l1_24_long %>%
  full_join(l2_24_long) %>%
  full_join(l3_24_long) %>%
  full_join(l4_24_long)


# make speakers long
long_speakers_2024 = long_speakers_2024 %>%
  pivot_longer(cols = contains("speakers"),
               names_to = "speaker_type",
               values_to = "number_of_speakers") %>%
  filter(number_of_speakers != 0)

colSums(is.na(long_speakers_2024))

# import and clean data from 2012 
l1_12 = read_excel("data/raw/EU_lang_survey_2012.xls", sheet = "L1", skip = 7)
l2_12 = read_excel("data/raw/EU_lang_survey_2012.xls", sheet = "L2", skip = 7)
l3_12 = read_excel("data/raw/EU_lang_survey_2012.xls", sheet = "L3", skip = 7)
l4_12 = read_excel("data/raw/EU_lang_survey_2012.xls", sheet = "L4", skip = 7)

## Structure data for 2012

# rough cleanup
clean_and_long_2012 = function(df, speaker_type = "L1"){
  df = df %>%
    select(-c(`D-W`, `D-E`)) #remove east and west germany
  df = df[-c(1, 78:79),-c(2)] # remove eu_sum, total, and "other"
  colnames(df)[1] = c("language") #change first col name
  #print(df)
  df_lang = df[1:nrow(df) %% 2 == 0,] #Select rows with english language names
  language_names = df_lang$language #store language names
  
  df = df[1:nrow(df) %% 2 == 1,]
  df$language = language_names # add language column
  print(df)
  df[1:nrow(df) %% 2 == 1,]
  
  
  df[,2:ncol(df)] = mapply(as.numeric ,df %>% select(BE:UK)) # make numeric
  df[,2:ncol(df)][is.na(df[,2:ncol(df)])] = 0 #replace NA with 0
  
  print(df)
  
  df = df %>%
    pivot_longer(cols = !c(language), 
                 names_to = "country_code",
                 values_to = paste0(speaker_type, "_speakers"))
  
  return(df)
  
}

l1_12_long = clean_and_long_2012(l1_12, "L1")
l2_12_long = clean_and_long_2012(l2_12, "L2")
l3_12_long = clean_and_long_2012(l3_12, "L3")
l4_12_long = clean_and_long_2012(l4_12, "L4")

# adjust scottisch gaelic to match
l1_12_long = l1_12_long %>%
  mutate(language = ifelse(grepl("Scottish", language), "Scottish/Gaelic", language))
l2_12_long = l2_12_long %>%
  mutate(language = ifelse(grepl("Scottish", language), "Scottish/Gaelic", language))
l3_12_long = l3_12_long %>%
  mutate(language = ifelse(grepl("Scottish", language), "Scottish/Gaelic", language))
l4_12_long = l4_12_long %>%
  mutate(language = ifelse(grepl("Scottish", language), "Scottish/Gaelic", language))

#### join ####
long_speakers_2012 = l1_12_long %>%
  full_join(l2_12_long) %>%
  full_join(l3_12_long) %>%
  full_join(l4_12_long)

long_speakers_2012 = long_speakers_2012 %>%
  pivot_longer(cols = contains("speakers"),
               names_to = "speaker_type",
               values_to = "number_of_speakers") %>%
  filter(number_of_speakers != 0)


colSums(is.na(long_speakers_2012))


# remove NONE
long_speakers_2012 = long_speakers_2012 %>%
  filter(language != "None")


##### harmonize datasets ####

# adjust naming for gaelic languages to match
long_speakers_2012 = long_speakers_2012 %>%
  mutate(language = case_when(grepl("Irish", language) ~ "Irish/Gaelic",
                            TRUE ~ language))


long_speakers_2012 %>%
  filter(language == "Irish/Gaelic")

# remove none and other
long_speakers_2024 = long_speakers_2024 %>%
  filter(!language %in% c("None - don't speak any other language", "Other"))

long_speakers_2012 = long_speakers_2012 %>%
  filter(!language %in% c("None", "Other"))


langs_2024 = long_speakers_2024 %>%
  distinct(language) %>%
  pull(language)# 45 languages

langs_2012 = long_speakers_2012 %>%
  distinct(language) %>%
  pull(language) # 38 languages

# add ISO codes
all_langs = union(langs_2012, langs_2024)

# use macro identifier ara, sqi, kur, zho, srd, rom, kur, yid; sami has no macro identifier --> sme = norther sami

names(all_langs) = c("ara", "eus", "bul", "cat", "zho", "hrv", "ces", "dan", "nld", "eng","ekk", "fin", "fra", "glg", "deu", "ell", "hin", "hun", "gle", "ita", "jap", "kor", "lav", "lit" , "ltz", "mlt", "pol", "por", "ron",
                     "rus", "gla", "slk", "slv", "spa", "swe", "tur", "urd", "cym", "sqi", "fry", "fur", "kur", "nds", "oci", "rom", "sme", "srd", "hbs", "ukr", "yid")
all_langs

langs_map_df = as.tibble(as.list(all_langs))
langs_map_df = langs_map_df %>%
  pivot_longer(cols = everything(), names_to = "ISO6393", values_to = "language")


# join ISOcode maping with dataframe
long_speakers_2012 = long_speakers_2012 %>%
  left_join(langs_map_df)

long_speakers_2024 = long_speakers_2024 %>%
  left_join(langs_map_df)


# add country name

countries_2024 = long_speakers_2024 %>%
  distinct(country_code) %>%
  pull(country_code) # 27

countries_2012 = long_speakers_2012 %>%
  distinct(country_code) %>%
  pull(country_code) # 27

length(union(countries_2024, countries_2012)) # 28 in union; difference is UK in 2012 and Croatia in 2024

df_geo = ne_countries(scale = "large")
df_geo = df_geo %>%
  as_tibble() %>%
  select("iso_a2_eh", "name") %>%
  rename("country_code" = iso_a2_eh,
         "country_name" = name)

# resolve france
df_geo = df_geo %>%
  group_by(country_code) %>%
  summarize(country_name = first(country_name))

# join with 2024 data

# adjust greece in lang data
long_speakers_2024 = long_speakers_2024 %>%
  mutate(country_code = ifelse(country_code == "EL", "GR", country_code))


long_speakers_2024 = long_speakers_2024 %>%
  left_join(df_geo)

long_speakers_2024 %>%
  filter(is.na(country_name)) %>%
  distinct(country_code) # problem with EL; should be GR


# join with 2012 data

# adjust UK to GB and EL to GR
long_speakers_2012 = long_speakers_2012 %>%
  mutate(country_code = ifelse(country_code == "UK", "GB", country_code)) %>%
  mutate(country_code = ifelse(country_code == "EL", "GR", country_code))

long_speakers_2012 = long_speakers_2012 %>%
  left_join(df_geo)

long_speakers_2012 %>%
  filter(is.na(country_name))

colSums(is.na(long_speakers_2012))


# add years and join dataframe

long_speakers_2012$year = 2012
long_speakers_2024$year = 2024

long_speakers_2012_2024 = long_speakers_2012 %>%
  rbind(long_speakers_2024) %>%
  rename("language_name" = language) %>%
  relocate(country_code, country_name, ISO6393, language_name, speaker_type, number_of_speakers, year)

colSums(is.na(long_speakers_2012_2024))

# write data
write_csv(long_speakers_2012_2024, "data/clean/EU_country_speakers_2012_2024.csv")

View(long_speakers_2012_2024)
