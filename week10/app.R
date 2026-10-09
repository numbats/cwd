library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(lubridate)

data_candidates <- c(
  "tutorial/solar_data.csv",
  "solar_data.csv",
  file.path("week10", "tutorial", "solar_data.csv")
)
data_path <- data_candidates[file.exists(data_candidates)][1]

if (is.na(data_path)) {
  stop("Could not find solar_data.csv. Run the app from the week10 folder or place the data beside app.R.")
}

consumption_data <- read_csv(data_path, show_col_types = FALSE) |>
  filter(con_gen == "Consumption") |>
  transmute(
    datetime = as.POSIXct(datetime, tz = "UTC"),
    date = as.Date(datetime),
    energy_kwh = as.numeric(energy_kwh)
  ) |>
  arrange(datetime)

add_solar_model <- function(data, system_kw) {
  interval_hours <- median(as.numeric(diff(data$datetime), units = "hours"), na.rm = TRUE)

  data |>
    mutate(
      interval_hours = interval_hours,
      hour_of_day = hour(datetime) + minute(datetime) / 60,
      daylight = pmax(sin(pi * (hour_of_day - 6) / 12), 0),
      seasonal = 0.70 + 0.30 * cos(2 * pi * (yday(datetime) - 15) / 365.25),
      solar_kwh = system_kw * daylight^1.5 * seasonal * interval_hours,
      net_demand_kwh = energy_kwh - solar_kwh,
      grid_import_without_battery_kwh = pmax(net_demand_kwh, 0),
      grid_export_without_battery_kwh = pmax(-net_demand_kwh, 0)
    )
}

dispatch_battery <- function(data, capacity_kwh, round_trip_efficiency) {
  if (capacity_kwh <= 0) {
    return(data |>
      mutate(
        battery_state_kwh = 0,
        grid_import_kwh = grid_import_without_battery_kwh,
        grid_export_kwh = grid_export_without_battery_kwh
      ))
  }

  one_way_efficiency <- sqrt(round_trip_efficiency)
  battery_state <- grid_import <- grid_export <- numeric(nrow(data))
  state <- 0

  for (i in seq_len(nrow(data))) {
    net <- data$net_demand_kwh[i]

    if (net < 0) {
      surplus <- -net
      energy_stored <- min(surplus * one_way_efficiency, capacity_kwh - state)
      state <- state + energy_stored
      grid_export[i] <- surplus - energy_stored / one_way_efficiency
    } else {
      energy_delivered <- min(net, state * one_way_efficiency)
      state <- state - energy_delivered / one_way_efficiency
      grid_import[i] <- net - energy_delivered
    }

    battery_state[i] <- state
  }

  data |>
    mutate(
      battery_state_kwh = battery_state,
      grid_import_kwh = grid_import,
      grid_export_kwh = grid_export
    )
}

minimum_date <- min(consumption_data$date)
maximum_date <- max(consumption_data$date)

ui <- page_sidebar(
  title = "One household, three solar stories",
  sidebar = sidebar(
      dateRangeInput(
        "date_range", "Period",
        start = minimum_date, end = maximum_date,
        min = minimum_date, max = maximum_date
      ),
      sliderInput("system_kw", "Solar system size (kW)", min = 1, max = 15, value = 5, step = 0.5),
      numericInput("import_tariff", "Import tariff ($/kWh)", value = 0.32, min = 0, step = 0.01),
      numericInput("feed_in_tariff", "Feed-in tariff ($/kWh)", value = 0.05, min = 0, step = 0.01),
      numericInput("solar_cost", "Solar installation cost ($)", value = 7000, min = 0, step = 500),
      checkboxInput("include_battery", "Include a battery", value = TRUE),
      conditionalPanel(
        condition = "input.include_battery",
        sliderInput("battery_kwh", "Usable battery capacity (kWh)", min = 1, max = 30, value = 10),
        sliderInput("battery_efficiency", "Round-trip efficiency", min = 0.70, max = 1, value = 0.90, step = 0.01),
        numericInput("battery_cost", "Battery installation cost ($)", value = 10000, min = 0, step = 500)
      )
  ),
  p(
    strong("Common evidence: "),
    "Every tab uses the same consumption records, modelled solar production, tariffs and battery dispatch. Only the framing changes."
  ),
  navset_card_tab(
    id = "frame",
    nav_panel(
      "Why invest",
      h3(textOutput("sales_headline", inline = TRUE)),
      textOutput("sales_message"),
      plotOutput("sales_plot", height = "430px")
    ),
    nav_panel(
      "Energy balance",
      h3(textOutput("neutral_headline", inline = TRUE)),
      textOutput("neutral_message"),
      plotOutput("neutral_plot", height = "430px")
    ),
    nav_panel(
      "Cautious case",
      h3(textOutput("sceptic_headline", inline = TRUE)),
      textOutput("sceptic_message"),
      plotOutput("sceptic_plot", height = "430px")
    ),
    nav_panel(
      "Evidence and assumptions",
      h3("The calculation behind all three views"),
      tableOutput("assumptions"),
      h4(textOutput("trace_title", inline = TRUE)),
      p("Read across each row, then down the battery column to follow the state carried through time."),
      tableOutput("calculation_trace"),
      downloadButton("download_data", "Download common scenario data")
    )
  )
)

server <- function(input, output, session) {
  scenario_data <- reactive({
    req(input$date_range, input$system_kw, input$import_tariff, input$feed_in_tariff)

    selected <- consumption_data |>
      filter(date >= input$date_range[1], date <= input$date_range[2])

    validate(need(
      nrow(selected) > 0,
      "No records match this period. Try a wider date range."
    ))

    capacity <- if (isTRUE(input$include_battery)) input$battery_kwh else 0
    efficiency <- if (isTRUE(input$include_battery)) input$battery_efficiency else 1

    selected |>
      add_solar_model(system_kw = input$system_kw) |>
      dispatch_battery(
        capacity_kwh = capacity,
        round_trip_efficiency = efficiency
      ) |>
      mutate(
        baseline_cost = energy_kwh * input$import_tariff,
        scenario_cost = grid_import_kwh * input$import_tariff -
          grid_export_kwh * input$feed_in_tariff,
        interval_saving = baseline_cost - scenario_cost
      )
  })

  metrics <- reactive({
    data <- scenario_data()
    selected_days <- as.integer(max(data$date) - min(data$date)) + 1
    total_saving <- sum(data$interval_saving)
    annual_saving <- total_saving / selected_days * 365.25
    capital_cost <- input$solar_cost +
      if (isTRUE(input$include_battery)) input$battery_cost else 0

    list(
      days = selected_days,
      consumption = sum(data$energy_kwh),
      production = sum(data$solar_kwh),
      grid_import = sum(data$grid_import_kwh),
      grid_export = sum(data$grid_export_kwh),
      saving = total_saving,
      annual_saving = annual_saving,
      capital_cost = capital_cost,
      payback = if (annual_saving > 0) capital_cost / annual_saving else Inf,
      self_sufficiency = 1 - sum(data$grid_import_kwh) / sum(data$energy_kwh),
      battery_shift = sum(
        data$grid_import_without_battery_kwh - data$grid_import_kwh
      )
    )
  })

  daily_data <- reactive({
    scenario_data() |>
      group_by(date) |>
      summarise(
        consumption_kwh = sum(energy_kwh),
        solar_kwh = sum(solar_kwh),
        grid_import_kwh = sum(grid_import_kwh),
        grid_export_kwh = sum(grid_export_kwh),
        daily_saving = sum(interval_saving),
        .groups = "drop"
      ) |>
      mutate(
        cumulative_saving = cumsum(daily_saving),
        net_position = -metrics()$capital_cost + cumulative_saving
      )
  })

  output$sales_headline <- renderText({
    paste0("Put ", scales::dollar(metrics()$saving), " back in the household budget")
  })

  output$sales_message <- renderText({
    battery_text <- if (isTRUE(input$include_battery)) {
      paste0(
        " The battery redirects about ",
        scales::number(metrics()$battery_shift, accuracy = 1),
        " kWh that would otherwise have been imported from the grid."
      )
    } else {
      ""
    }

    paste0(
      "Over the selected ", metrics()$days, " days, the modelled system supplies ",
      scales::percent(metrics()$self_sufficiency, accuracy = 1),
      " of consumption without grid imports.", battery_text
    )
  })

  output$neutral_headline <- renderText({
    "Production, consumption and grid demand follow different daily rhythms"
  })

  output$neutral_message <- renderText({
    paste0(
      "The household consumes ", scales::number(metrics()$consumption, accuracy = 1),
      " kWh. Modelled solar production is ",
      scales::number(metrics()$production, accuracy = 1),
      " kWh; ", scales::number(metrics()$grid_import, accuracy = 1),
      " kWh is imported and ", scales::number(metrics()$grid_export, accuracy = 1),
      " kWh is exported."
    )
  })

  output$sceptic_headline <- renderText({
    if (is.finite(metrics()$payback)) {
      paste0("Simple payback is approximately ", round(metrics()$payback, 1), " years")
    } else {
      "The selected assumptions do not produce a positive annual saving"
    }
  })

  output$sceptic_message <- renderText({
    paste0(
      "The headline saving excludes the up-front cost of ",
      scales::dollar(metrics()$capital_cost),
      ". It also assumes tariffs, consumption and modelled production continue unchanged."
    )
  })

  output$sales_plot <- renderPlot({
    ggplot(daily_data(), aes(date, cumulative_saving)) +
      geom_area(fill = "#72BF44", alpha = 0.35) +
      geom_line(colour = "#2E7D32", linewidth = 1) +
      scale_y_continuous(labels = scales::label_dollar()) +
      labs(
        x = NULL, y = "Cumulative bill saving",
        subtitle = "Avoided import costs minus foregone export credits"
      ) +
      theme_minimal(base_size = 15)
  })

  output$neutral_plot <- renderPlot({
    daily_data() |>
      select(
        date,
        Consumption = consumption_kwh,
        `Modelled solar` = solar_kwh,
        `Grid import` = grid_import_kwh
      ) |>
      pivot_longer(-date, names_to = "series", values_to = "energy_kwh") |>
      ggplot(aes(date, energy_kwh, colour = series)) +
      geom_line(linewidth = 0.7) +
      scale_colour_manual(values = c("#333333", "#006DAE", "#F28E2B")) +
      labs(x = NULL, y = "Daily energy (kWh)", colour = NULL) +
      theme_minimal(base_size = 15) +
      theme(legend.position = "top")
  })

  output$sceptic_plot <- renderPlot({
    ggplot(daily_data(), aes(date, net_position)) +
      geom_hline(yintercept = 0, linetype = "dashed", colour = "#555555") +
      geom_line(colour = "#9C2F2F", linewidth = 1) +
      scale_y_continuous(labels = scales::label_dollar()) +
      labs(
        x = NULL, y = "Cumulative position after up-front cost",
        subtitle = "Simple cash position; financing, maintenance and degradation are excluded"
      ) +
      theme_minimal(base_size = 15)
  })

  output$assumptions <- renderTable({
    data.frame(
      Assumption = c(
        "Selected period", "Solar system", "Battery", "Import tariff",
        "Feed-in tariff", "Up-front cost", "Solar production",
        "Battery initial state"
      ),
      Value = c(
        paste(input$date_range, collapse = " to "),
        paste(input$system_kw, "kW"),
        if (isTRUE(input$include_battery)) {
          paste(input$battery_kwh, "kWh at", scales::percent(input$battery_efficiency))
        } else {
          "Not included"
        },
        paste0(scales::dollar(input$import_tariff), "/kWh"),
        paste0(scales::dollar(input$feed_in_tariff), "/kWh"),
        scales::dollar(metrics()$capital_cost),
        "Simplified deterministic daylight and seasonal model",
        "Empty at the start of the selected period"
      ),
      check.names = FALSE
    )
  }, striped = TRUE, bordered = TRUE, spacing = "m")

  output$trace_title <- renderText({
    paste("Selected calculation trace for", min(scenario_data()$date))
  })

  output$calculation_trace <- renderTable({
    data <- scenario_data()
    first_date <- min(data$date)

    data |>
      filter(
        date == first_date,
        hour(datetime) %in% c(0, 6, 9, 12, 15, 18, 21),
        minute(datetime) == 0
      ) |>
      transmute(
        Time = format(datetime, "%H:%M"),
        Consumption = round(energy_kwh, 2),
        Solar = round(solar_kwh, 2),
        Net = round(net_demand_kwh, 2),
        Battery = round(battery_state_kwh, 2),
        Import = round(grid_import_kwh, 2),
        Export = round(grid_export_kwh, 2),
        Saving = scales::dollar(interval_saving)
      )
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$download_data <- downloadHandler(
    filename = function() paste0("solar-scenario-", Sys.Date(), ".csv"),
    content = function(file) {
      write_csv(scenario_data(), file)
    }
  )
}

shinyApp(ui, server)
