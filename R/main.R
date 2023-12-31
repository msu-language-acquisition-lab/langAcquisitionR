library(jsonlite)
library(plyr)
library(dplyr)
library(stringr)

plugins <- fromJSON("plugins.json")

trim_plugin_column <- function(df, plugin_name="html-slider-response", columns=c()) {
  excludes <- plugins[[plugin_name]]	# Columns added by the plugin

  df <- df %>% select(-c(any_of(excludes)))	# Exclude above columns from data frame
  df <- df %>% select(-c(any_of(columns)))	# Exclude any additional columns from data

  # Get trial data
  df <- filter(df,
               df$trial_type == plugin_name)

  return (df)
}

filter_by_first_lang <- function(df, lang="English") {
  # Remove any leading or trailing white space around first_lang column
  # Convert value to title
  df$first_lang <- str_to_title(trimws(df$first_lang), locale="en")

  df %>% distinct(first_lang) # show which distinct languages are there
  df <- filter(df, df$first_lang == lang)	#filter by English first language

  return (df)
}
