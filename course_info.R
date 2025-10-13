######## Course info ########
library(tidyverse)

# Start of semester
start_semester <- "2025-07-28"

# Week of mid-semester break
mid_semester_break <- "2025-09-29"

# Schedule
schedule <- tribble(
    ~Week, ~Topic, ~Reference, ~Reference_URL,
    1, "Basic communication theory and practice", "", "",
    2, "Basic Communitcation Theory and Presentations", "", "",
    3, "Data Storytelling on the Web", "", "",
    4, "Writing clearly and concisely", "", "", # Assignment 1 due
    5, "Statistical model outputs and data tables", "", "",
    6, "Effective data visualisation", "", "",
    7, "Introduction to web technologies and styling", "", "", # Assignment 2 due
    8, "R packages and documentation", "", "",
    9, "Communicating data with interactive web apps", "", "",
    10, "Communicating data interatively: Part 2", "", "", # Assignment 3 due
    11, "Automated systems for code communication", "", "",
    12, "Profsesional Skills Communication", "", "", # Assignment 4 due in Week 13
)

# Assignment 1: Breaking down published articles
# Assignment 2: Writing their own article then breaking it down
# Assignment 3: Creating a blog including styling and content
# Assignment 4: Turning their article into a dashboard + reflection


# Add mid-semester break
calendar <- tibble(
    Date = seq(as.Date(start_semester), by = "1 week", length.out = 13)
) |>
    mutate(
        Week = row_number(),
        Week = if_else(Date < mid_semester_break, Week, Week - 1),
        # Week =
    )

# Add calendar to schedule
schedule <- schedule |>
    left_join(calendar, by = "Week") |>
    mutate(
        Week = if_else(Date == mid_semester_break, NA, Week),
        Topic = if_else(Date == mid_semester_break, "Mid-semester break", Topic),
        # Reference = if_else(Date == mid_semester_break, NA, Reference),
        # Reference_URL = if_else(Date == mid_semester_break, NA, Reference_URL)
    ) |>
    select(Week, Date, everything())

# Add assignment details
lastmon <- function(x) {
    7 * floor(as.numeric(x - 1 + 4) / 7) + as.Date(1 - 4, origin = "1970-01-01")
}

assignments <- read_csv(here::here("assignments.csv")) |>
    mutate(
        Date = lastmon(Due),
        Moodle = paste0("https://learning.monash.edu/mod/assign/view.php?id=", Moodle),
        File = paste0("assignments/", File)
    )

schedule <- schedule |>
    left_join(assignments, by = "Date") |>
    mutate(Week = if_else(is.na(Week) & Date > "2025-05-20", 13, Week))

show_assignments <- function(week) {
    ass <- schedule |>
        filter(
            Week >= week & (week > Week - 3 | week > 8),
            !is.na(Assignment),
        ) |>
        select(Assignment:File)
    if (NROW(ass) > 0) {
        cat("\n\n## Assignments\n\n")
        for (i in seq(NROW(ass))) {
            cat("* [", ass$Assignment[i], "](../", ass$File[i], ") is due on ",
                format(ass$Due[i], "%A %d %B.\n"),
                sep = ""
            )
        }
    }
}


submit <- function(schedule, assignment) {
    ass <- schedule |>
        filter(Assignment == assignment)
    due <- format(ass$Due, "%e %B %Y") |> stringr::str_trim()
    url <- ass$Moodle
    button <- paste0(
        "<br><br><hr><b>Due: ", due, "</b><br>",
        "<a href=", url, " class = 'badge badge-large badge-blue'>",
        "<font size='+2'>&nbsp;&nbsp;<b>Submit</b>&nbsp;&nbsp;</font><br></a>"
    )
    cat(button)
}
