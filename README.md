# shinyFlights

A small Shiny application, exploring flights data, for demo purposes.

## Features

- departure-airport selector;
- destination-country filter;
- summary of direct destinations;
- base-R bar chart;
- route-details table;
- parameterised PostgreSQL query.

## Runtime requirements

The server needs these R packages:

- `shiny`
- `DBI`
- `RPostgres`

The app expects a PostgreSQL environment file at:

```text
/etc/shiny-server/shinyFlights.env
```

It will use the csv in `data` if it can't find the env file.

Alternatively, for local testing, select a different file with:

```bash
export SHINYFLIGHTS_ENV="$HOME/.config/dsi/database.env"
R -e "shiny::runApp('.', host='127.0.0.1', port=3839)"
```

The database must contain the view `flights.route_details`.

## Licence

MIT. The OpenFlights source data has its own licence and attribution requirements (see `inst/extdata/README.txt`)
