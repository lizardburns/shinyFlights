base_url <- "https://raw.githubusercontent.com/jpatokal/openflights/master/data"

for (filename in c("airports.dat", "airlines.dat", "routes.dat")) {
  download.file(paste0(base_url, "/", filename), filename, mode = "wb")
}

airports <- read.csv(
  "inst/extdata/airports.dat", header = FALSE, na.strings = "\\N",
  stringsAsFactors = FALSE,
  col.names = c(
    "airport_id", "name", "city", "country", "iata", "icao",
    "latitude", "longitude", "altitude", "timezone", "dst",
    "tz_database", "type", "source"
  )
)

airlines <- read.csv(
  "inst/extdata/airlines.dat", header = FALSE, na.strings = "\\N",
  stringsAsFactors = FALSE,
  col.names = c(
    "airline_id", "name", "alias", "iata", "icao", "callsign",
    "country", "active"
  )
)

routes <- read.csv(
  "inst/extdata/routes.dat", header = FALSE, na.strings = "\\N",
  stringsAsFactors = FALSE,
  col.names = c(
    "airline_code", "airline_id", "source_airport_code",
    "source_airport_id", "destination_airport_code",
    "destination_airport_id", "codeshare", "stops", "equipment"
  )
)

# Select and rename the source-airport fields
source_airports <- data.frame(
  source_airport_id = airports$airport_id,
  source_code = airports$iata,
  source_airport = airports$name,
  source_city = airports$city,
  source_country = airports$country
)

# Select and rename the destination-airport fields
destination_airports <- data.frame(
  destination_airport_id = airports$airport_id,
  destination_code = airports$iata,
  destination_airport = airports$name,
  destination_city = airports$city,
  destination_country = airports$country,
  destination_latitude = airports$latitude,
  destination_longitude = airports$longitude
)

# Select and rename the airline fields
airline_names <- data.frame(
  airline_id = airlines$airline_id,
  airline_name = airlines$name
)

# INNER JOIN routes to the source airport
route_details <- merge(
  routes,
  source_airports,
  by = "source_airport_id"
)

# INNER JOIN routes to the destination airport
route_details <- merge(
  route_details,
  destination_airports,
  by = "destination_airport_id"
)

# LEFT JOIN the airline name
route_details <- merge(
  route_details,
  airline_names,
  by = "airline_id",
  all.x = TRUE
)

# Keep the same columns and order as the PostgreSQL view
route_details <- route_details[
  c(
    "airline_id",
    "airline_name",
    "source_airport_id",
    "source_code",
    "source_airport",
    "source_city",
    "source_country",
    "destination_airport_id",
    "destination_code",
    "destination_airport",
    "destination_city",
    "destination_country",
    "destination_latitude",
    "destination_longitude"
  )
]

# Give the rows a predictable order
route_details <- route_details[
  order(
    route_details$source_code,
    route_details$destination_code,
    route_details$airline_name,
    na.last = TRUE
  ),
]

row.names(route_details) <- NULL

# Inspect and export
dim(route_details)
head(route_details)

write.csv(
  route_details,
  "flights-route-details.csv",
  row.names = FALSE,
  na = ""
)
