\name{trim_plugin_column}
\alias{trim_plugin_column}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Trim Plugin Column}
\description{
  Remove unnessary columns associated by a specific plugin.
  Filter by main trials.
}
\usage{
trim_plugin_column(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{DataFrame object to be trimmed}
  \item{plugin_name}{Name of plugin used, "html-slider-response" by default}
  \item{columns}{Vector of additional columns to be trimmed}
}
\details{
  A map of plugins and column names can be found in plugins.json
}
\value{
  A DataFrame object where unnessary columns are excluded
}
\references{
}
\author{
  Han Nguyen
}
\note{
  More plugins can be added to plugins.json
}

\seealso{
}
\examples{
## Orginal Data Frame
df <- data.frame(
  rt = c(1, 2, 1.5),
  browser = c("Firefox", "Internet Explorer", "Safari"),
  trial_type = c("html-slider-response", "html-slider-response", "survey-html-form"),
  response = c("Yes", "No", "No"),
  age = c(4, 6, 5),
  sibling_count = c(1, 0, 0)
)

## Remove rt, browser, trials that are not html-slide-response, and number of siblings
new_df <- trim_plugin_column(df, columns=c("sibling_count"))
}
