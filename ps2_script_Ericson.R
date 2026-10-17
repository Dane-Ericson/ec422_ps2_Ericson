library(tidyquant)
library(tidyverse)

#3.1
unemp_raw <- tq_get(
  c("UNRATE", "ORUR", "CAUR", "TXUR", "NYUR"),
  get  = "economic.data",
  from = "1976-01-01"
)

head(unemp_raw)

dir.create("data", showWarnings = FALSE)

write_rds(unemp_raw, "data/unemp_raw.rds")

#3.2
unemp_raw = read_rds("data/unemp_raw.rds")

#4.1
unemp <- unemp_raw |>
  rename(unemployment_rate = price) |>
  mutate(
    region = case_when(
      symbol == "UNRATE" ~ "United States",
      symbol == "ORUR" ~ "Oregon",
      symbol == "CAUR" ~ "California",
      symbol == "TXUR" ~ "Texas",
      symbol == "NYUR" ~ "New York"
    )
  )


#4.2
unemp <- unemp |>
  arrange(region, date) |>
  group_by(region) |>
  mutate(
    unemployment_rate = replace(
      unemployment_rate,
      date == as.Date("2025-10-01"),
      unemployment_rate[date == as.Date("2025-09-01")]
    )
  ) |>
  ungroup()

unemp <- unemp |>
  arrange(region, date) |>
  group_by(region) |>
  mutate(unemployment_rate_change = unemployment_rate - lag(unemployment_rate)) |>
  ungroup()

unemp_recent <- unemp |>
  filter(date >= as.Date("2015-01-01"))
head(unemp_recent)

unemp_summary <- unemp |>
  group_by(region)|>
  summarize(
    mean_rate = mean(unemployment_rate, na.rm = TRUE),
    min_rate = min(unemployment_rate, na.rm = TRUE),
    max_rate = max(unemployment_rate, na.rm = TRUE)
  )
head(unemp_summary)

#4.3
unemp_top_increases <- unemp |>
  slice_max(order_by = unemployment_rate_change, n = 5)
unemp_top_decreases <- unemp |>
  slice_min(order_by = unemployment_rate_change, n = 5)
unemp_top_increases
unemp_top_decreases


#4.4
unemp_no_covid <- unemp |>
  filter(date >= as.Date("2021-01-01")|
           date<as.Date("2020-01-01"))

unemp_no_covid


unemp_top_increases_no_covid <- unemp_no_covid |>
  slice_max(order_by = unemployment_rate_change, n = 5)
unemp_top_decreases_no_covid <- unemp_no_covid |>
  slice_min(order_by = unemployment_rate_change, n = 5)
unemp_top_increases_no_covid
unemp_top_decreases_no_covid


#5.1
ggplot(unemp, aes(x = date, y = unemployment_rate, color = region)) +
  geom_line() + geom_area() +
  labs(title = "Unemployment Rate Over Time By Region", x = "Date", y = "Unemployment Rate") +
  theme_minimal()

#5.2
ggplot(unemp, aes(x = date, y = unemployment_rate, color = region)) +
  geom_line() + geom_area(fill = "tan")+
  labs(title = "Unemployment Rate Over Time By Region", x = "Date", y = "Unemployment Rate") +
  theme_minimal() +
  facet_wrap(~ region)

#5.3
ggplot(unemp_summary, aes(x = fct_reorder(region, mean_rate), y = mean_rate, color = region)) +
  geom_col(fill = "white") +
  theme_classic()+
  labs(title = "Mean Unemployment by Region", y = "Mean Unemployment")

#5.4
unemp_changes <- bind_rows(
  unemp_top_increases, 
  unemp_top_decreases
)

ggplot(unemp_changes, aes(x = region, y = unemployment_rate_change, fill = unemployment_rate_change > 0))+
geom_col(position = "dodge") + 
  labs(
    title = "Largest Increases and Decreases in Employment by Region",
    x = "Region",
    y = "Change in Unemployment Rate",
    fill = ("Direction of Change"),
  ) +
  theme_classic()
  
#I chose to plot the largest unemployment increases and decreases over the entire dataset by region, as it displays which regions have seen the largest shocks to employment over time or had the largest periods of economic fluctuation.



       