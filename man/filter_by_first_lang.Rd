\name{filter_by_first_lang}
\alias{filter_by_first_lang}
\title{Filter By A First Language}
\description{
  Filter rows in a DataFrame by a first language, by default, English
}
\usage{
filter_by_first_lang(df, lang = "English")
}
\arguments{
  \item{df}{DataFrame object to be filtered by first_lang}
  \item{lang}{Name of first language, "English" by default}
}
\details{
  Defaulting first_lang as the column name for first language
}
\value{
  A DataFrame with only one distinct first language, English by default
}
\author{
  Han Nguyen
}
\note{
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
library(plyr)
library(dplyr)
library(stringr)

## Orginal Data Frame
df <- data.frame(
  rt = c(1, 2, 1.5),
  browser = c("Firefox", "Internet Explorer", "Safari"),
  trial_type = c("html-slider-response", "html-slider-response", "survey-html-form"),
  response = c("Yes", "No", "No"),
  age = c(4, 6, 5),
  sibling_count = c(1, 0, 0),
  first_lang = c("English", "Vietnamese", "English")
)

## Filter data by English as first language
new_df <- filter_by_first_lang(df)

print(new_df)

## Filter data by Vietnamese as first language
new_df <- filter_by_first_lang(df, lang="Vietnamese")

print(new_df)
}
