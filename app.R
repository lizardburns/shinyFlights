library(shiny)
library(DBI)
library(RPostgres)

config_file <- Sys.getenv(
  "SHINYFLIGHTS_ENV",
  unset = "/etc/shiny-server/shinyFlights.env"
)

data_source <- ifelse(file.exists(config_file), "postgres", "csv")

if (file.exists(config_file)) {
  readRenviron(config_file)
  required_variables <- c(
    "PGHOST", "PGPORT", "PGDATABASE", "PGUSER", "PGPASSWORD"
  )
  missing_variables <- required_variables[Sys.getenv(required_variables) == ""]
  
  if (length(missing_variables) > 0) {
    stop(
      "Missing database settings: ",
      paste(missing_variables, collapse = ", ")
    )
  }  
}

pg_connect <- function() {
  dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("PGHOST"),
    port = as.integer(Sys.getenv("PGPORT")),
    dbname = Sys.getenv("PGDATABASE"),
    user = Sys.getenv("PGUSER"),
    password = Sys.getenv("PGPASSWORD")
  )
}

ui <- fluidPage(
  tags$head(
    tags$title("shinyFlights"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  div(
    class = "page-header",
    h1("shinyFlights"),
    p("Explore direct routes stored in PostgreSQL.")
  ),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "source_airport",
        "Departure airport",
        choices = NULL
      ),
      selectInput(
        "country",
        "Destination country",
        choices = "All countries"
      ),
      helpText(
        "The app queries flights.route_details when a selection changes."
      )
    ),
    mainPanel(
      fluidRow(
        column(
          4,
          div(
            class = "summary-card",
            span(class = "summary-label", "Direct destinations"),
            textOutput("destination_count")
          )
        ),
        column(
          8,
          div(
            class = "summary-card",
            span(class = "summary-label", "Selected airport"),
            textOutput("selected_airport")
          )
        )
      ),
      h2("Destinations by country"),
      plotOutput("country_plot", height = "360px"),
      h2("Route details"),
      tableOutput("route_table")
    )
  )
)

server <- function(input, output, session) {
  if (data_source == "csv") {
    route_details <- read.csv(
      "data/flights-route-details.csv",
      stringsAsFactors = FALSE,
      na.strings = ""
    )

    airports <- unique(
      route_details[
        !is.na(route_details$source_code),
        c(
          "source_code",
          "source_airport",
          "source_city",
          "source_country"
        )
      ]
    )
    
    airports <- airports[
      order(
        airports$source_country,
        airports$source_city,
        airports$source_airport
      ),
    ]
    
    row.names(airports) <- NULL

  } else {
    con <- pg_connect()
    session$onSessionEnded(function() {
      if (dbIsValid(con)) {
        dbDisconnect(con)
      }
    })

    airports <- dbGetQuery(
      con,
      "
        SELECT DISTINCT
          source_code,
          source_airport,
          source_city,
          source_country
        FROM flights.route_details
        WHERE source_code IS NOT NULL
        ORDER BY source_country, source_city, source_airport
      "
    )  
  }

  airport_labels <- sprintf(
    "%s — %s, %s",
    airports$source_code,
    airports$source_city,
    airports$source_country
  )
  airport_choices <- stats::setNames(airports$source_code, airport_labels)

  default_airport <- if ("MAN" %in% airports$source_code) {
    "MAN"
  } else {
    airports$source_code[[1]]
  }

  updateSelectInput(
    session,
    "source_airport",
    choices = airport_choices,
    selected = default_airport
  )

  routes_for_airport <- reactive({
    req(input$source_airport)
  
    if (data_source == "postgres") {
      dbGetQuery(
        con,
        "
          SELECT DISTINCT
            destination_code,
            destination_airport,
            destination_city,
            destination_country,
            airline_name
          FROM flights.route_details
          WHERE source_code = $1
          ORDER BY destination_country, destination_city,
                   destination_airport, airline_name
        ",
        params = list(input$source_airport)
      )
  
    } else if (data_source == "csv") {
      result <- route_details[
        !is.na(route_details$source_code) &
          route_details$source_code == input$source_airport,
        c(
          "destination_code",
          "destination_airport",
          "destination_city",
          "destination_country",
          "airline_name"
        )
      ]
  
      result <- unique(result)
  
      result <- result[
        order(
          result$destination_country,
          result$destination_city,
          result$destination_airport,
          result$airline_name,
          na.last = TRUE
        ),
      ]
  
      row.names(result) <- NULL
      result
  
    } else {
      stop("data_source must be either 'postgres' or 'csv'")
    }
  })
  
  observeEvent(routes_for_airport(), {
    countries <- sort(unique(routes_for_airport()$destination_country))
    countries <- countries[!is.na(countries) & countries != ""]

    updateSelectInput(
      session,
      "country",
      choices = c("All countries", countries),
      selected = "All countries"
    )
  }, ignoreInit = FALSE)

  filtered_routes <- reactive({
    data <- routes_for_airport()

    if (!is.null(input$country) && input$country != "All countries") {
      data <- data[data$destination_country == input$country, , drop = FALSE]
    }

    data
  })

  output$destination_count <- renderText({
    length(unique(filtered_routes()$destination_code))
  })

  output$selected_airport <- renderText({
    req(input$source_airport)
    selected <- airports[airports$source_code == input$source_airport, ]

    sprintf(
      "%s (%s, %s)",
      selected$source_airport[[1]],
      selected$source_city[[1]],
      selected$source_country[[1]]
    )
  })

  output$country_plot <- renderPlot({
    data <- filtered_routes()
    validate(need(nrow(data) > 0, "No routes match this selection."))

    counts <- sort(
      table(data$destination_country),
      decreasing = TRUE
    )
    counts <- head(counts, 12)

    old_par <- par(mar = c(5, 10, 3, 1))
    on.exit(par(old_par))

    barplot(
      rev(counts),
      horiz = TRUE,
      las = 1,
      col = "#2563eb",
      border = NA,
      xlab = "Number of route records",
      main = "Top destination countries"
    )
  })

  output$route_table <- renderTable({
    data <- filtered_routes()
    validate(need(nrow(data) > 0, "No routes match this selection."))

    names(data) <- c(
      "Code", "Airport", "City", "Country", "Airline"
    )

    head(data, 100)
  }, striped = TRUE, hover = TRUE, spacing = "s")
}

shinyApp(ui, server)
