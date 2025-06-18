# Funktion zum umwandeln ins Wide-Format 
create_wide_table <- function(df) {
  df %>%
    mutate(value = round(value, 2)) %>%
    pivot_wider(
      names_from = statistic,
      values_from = value
    ) %>%
    # Bereinigen der Variablennamen für die Darstellung in der Tabelle
    mutate(
      variable = str_remove(variable, "_secs$"),
      variable = str_replace_all(variable, "_", " ")
    ) %>%
    rename(
      Prozess = variable,
      Mittelwert = mean,
      Median = median,
      Standardabweichung = sd,
      Varianz = var
    )
}