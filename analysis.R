library(tidyverse)
library(readxl)
library(scales)

Pasta1 <- read_excel("Pasta1.xlsx")

scale_factor <- 100

ggplot(Pasta1, aes(x = Year)) +
  geom_line(
    aes(y = `Net Balance`, color = "Net Balance"),
    linewidth = 1.2
  ) +
  geom_line(
    aes(
      y = `Average gold prize` * scale_factor,
      color = "Average gold price"
    ),
    linewidth = 1.2
  ) +
  scale_y_continuous(
    name = "Net Balance",
    labels = scales::label_comma(),
    sec.axis = sec_axis(
      ~ . / scale_factor,
      name = "Average gold price"
    )
  ) +
  scale_color_manual(
    name = NULL,
    values = c(
      "Net Balance" = "steelblue",
      "Average gold price" = "darkred"
    )
  ) +
  labs(x = "Year") +
  theme_light() +
  theme(
    legend.position = "bottom"
  )

Series_7_ <- read_excel("Series(7).xlsx")

Series_7_ %>%
  filter(Date <= as.Date("1932-12-31")) %>%
  ggplot(aes(x = Date, y = Amount)) +
  geom_line(
    color = "lightblue",
    linewidth = 1
  ) +
  scale_y_continuous(
    labels = scales::label_comma()
  ) +
  labs(
    x = "Date",
    y = "Exchange rate"
  ) +
  theme_light()

graffisc <- read_excel("graffisc.xlsx")

graffisc$Spending <- as.numeric(
  gsub(",", "", graffisc$Spending)
)

graffisc$Result <- as.numeric(
  gsub(",", "", graffisc$Result)
)

data <- graffisc[-39, ]

data_long <- data %>%
  pivot_longer(
    cols = c(Revenue, Spending, Result),
    names_to = "Series",
    values_to = "Value"
  )

ggplot(
  data_long,
  aes(
    x = Year,
    y = Value,
    color = Series
  )
) +
  geom_line(linewidth = 1) +
  scale_color_manual(
    values = c(
      "Revenue" = "blue",
      "Spending" = "red",
      "Result" = "darkgreen"
    )
  ) +
  scale_y_continuous(
    labels = scales::label_comma()
  ) +
  theme_light()

events <- data.frame(
  year = c(1904, 1911, 1922, 1932),
  label = c(
    "1904\nRevolution",
    "1911\nCivil War",
    "1922\nCivil War",
    "Chaco War"
  )
)

ggplot(
  data_long,
  aes(
    x = Year,
    y = Value,
    color = Series
  )
) +
  geom_line(linewidth = 1) +
  scale_color_manual(
    values = c(
      "Revenue" = "blue",
      "Spending" = "red",
      "Result" = "darkgreen"
    )
  ) +
  scale_y_continuous(
    labels = scales::label_comma()
  ) +
  geom_vline(
    data = events,
    aes(xintercept = year),
    color = "gray50"
  ) +
  geom_text(
    data = events,
    aes(
      x = year,
      y = 7000000,
      label = label
    ),
    inherit.aes = FALSE,
    angle = 90,
    vjust = -0.5
  ) +
  theme_light()
