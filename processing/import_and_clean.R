library(tidyverse)
library(readxl)

# import and clean data from 2024 -----------

l1_24 = read_excel("data/raw/eu_lang_survey_2024.xlsx", sheet = "L1", skip = 7)
l2_24 = read_excel("data/raw/eu_lang_survey_2024.xlsx", sheet = "L2", skip = 7)
l3_24 = read_excel("data/raw/eu_lang_survey_2024.xlsx", sheet = "L3", skip = 7)
l4_24 = read_excel("data/raw/eu_lang_survey_2024.xlsx", sheet = "L3", skip = 7)

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
  print(df)
  df_lang = df[1:nrow(df) %% 2 == 0,] #Select rows with english language names
  language_names = df_lang$language #store language names
  
  
  df = 
  
  df$language = language_names # add language column
  
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

clean_and_long_2012(l1_12, "L1")
clean
# pivot_longer

l1_12_n %>%
  pivot_longer()


sums = apply(l1_12_p %>% select(!language), MARGIN = 2, FUN =sum ) #sanity check: sum of all fractions for each country is 1
print(sums)

l1_12_p = l1_12_p %>% mutate(language = ifelse(language == "Irish\\ Gaelic", "Irish/Gaelic", language)) #adjusting lang name for coherence


write_csv(l1_12_p, "clean_data/l1_12_p.csv")


## Data for 2012 survey #####



l2_12 = l2_12 %>% select(-c(`D-W`, `D-E`)) #remove east and west germany
l2_12 = l2_12[-c(1, 78:81),-c(2)] # remove eu_sum, total, and "do not know" or "has no mother tongue" answers
colnames(l2_12)[1] = c("language") #change first col name

l2_12_eng = l2_12[1:nrow(l2_12) %% 2 == 0,] #Select rows with english language names

l2_12_n = l2_12[1:nrow(l2_12) %% 2 == 1,] #select rows with nominal values

l2_12_n[,2:ncol(l2_12_n)] = mapply(as.numeric ,l2_12_n %>% select(BE:UK)) # make numeric
l2_12_n[,2:ncol(l2_12_n)][is.na(l2_12_n[,2:ncol(l2_12_n)])] = 0 #replace NA with 0

#combine number of speakers of l1 and l2
l1_l2_12_n = l1_12_n %>% select(!language) + l2_12_n %>% select(!language)


l1_l2_12_p = l1_l2_12_n %>% apply(MARGIN = 2, FUN = fraction) # calculate fractions
l1_l2_12_p = as.data.frame(l1_l2_12_p)
l1_l2_12_p$language = language_names # add language column

sums = apply(l1_l2_12_p %>% select(!language), MARGIN = 2, FUN =sum ) #sanity check: sum of all fractions for each country is 1
print(sums)

l1_l2_12_p = l1_l2_12_p %>% mutate(language = ifelse(language == "Irish\\ Gaelic", "Irish/Gaelic", language)) #adjusting lang name for coherence


write_csv(l1_l2_12_p, "clean_data/l1_l2_12_p.csv")

