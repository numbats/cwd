# Public Libraries Victoria teaching extract

`plv-library-visits.csv` is a prepared extract from Public Libraries Victoria's
**2024-25 Annual Statistical Survey - Final Datasets 2022-23 to 2024-25**.

Official workbook:
<https://www.plv.org.au/wp-content/uploads/2025/11/06.-2024-25-PLV-Annual-Statistical-Survey-Final-Datasets-2022-23-to-2024-25.xlsx>

The workbook was accessed on 18 August 2026. The extract retains five library
services and the published Victorian total for three financial years. It includes
the municipal population and in-person branch-library visit fields needed for the
Week 3 tutorial. The Victorian total is the total published in the workbook; it is
not calculated from the five example services in this extract.

The source workbook names the visit fields differently across sheets:

- `2022-23`: `25a. Total number of branch library visits`
- `2023-24`: `10a. Total number of branch library visits`
- `2024-25`: `10a. Total number of branch library visits`

The 2024-25 data-collection template defines this field as visits in person to a
branch library. It does not include mobile-library, collection-delivery, outreach,
website, catalogue, or library-app visits.

The following R code reproduces the extract after downloading the official workbook
as `plv-statistical-survey.xlsx`:

```r
library(dplyr)
library(readr)
library(readxl)

services <- c(
  "Bayside", "Melbourne", "Monash", "Yarra", "Yarra Plenty",
  "Victoria (Total)"
)

read_visits <- function(sheet, visit_column) {
  read_excel("plv-statistical-survey.xlsx", sheet = sheet, skip = 3) |>
    filter(trimws(Name) %in% services) |>
    transmute(
      service = trimws(Name),
      financial_year = sheet,
      municipal_population = as.numeric(`1. Municipal population`),
      branch_visits = as.numeric(.data[[visit_column]])
    )
}

library_visits <- bind_rows(
  read_visits("2022-23", "25a. Total number of branch library visits"),
  read_visits("2023-24", "10a. Total number of branch library visits"),
  read_visits("2024-25", "10a. Total number of branch library visits")
)

write_csv(library_visits, "plv-library-visits.csv")
```
