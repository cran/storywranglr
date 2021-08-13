# storywranglr 0.2.0

* Bugfix: Empty ngrams queries no longer crash, and now return a zero-row tibble
  with the correct column types.
* Feature: storywranglr::ngrams() new parameter `fill_dates` gives the 
  option to fill in missing date rows with rows of NA values.
* Removed unnecessary dependencies on packages "stringi" and "pillar".

# storywranglr 0.1.0

* Initial release.
