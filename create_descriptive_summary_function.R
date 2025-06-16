# Funktion für deskriptive Analyse
create_descriptive_summary <- function(df) {
  df %>%
    # Mittelwert, Median, Standardabweichung, Varianz für alle Spalten mit Sekundenangabe
    summarise(
      across(ends_with("_secs"), # bspl.: where(is.numeric) oder c(Kontakt_Anzahl_Anmeldung, Kontakt_Anzahl_Arztgespräch)
             list(
               mean = ~mean(., na.rm = TRUE),
               median = ~median(., na.rm = TRUE),
               sd = ~sd(., na.rm = TRUE),
               var = ~var(., na.rm = TRUE)
             ),
             .names = "{.col}_{.fn}"
      )
    ) %>%
    # Umwandeln in das Long-Format
    pivot_longer(
      cols = everything(),
      names_to = "variable_statistic",
      values_to = "value"
    ) %>%
    # Zerlegen der Spalte "variable_statistic" in "statistic" und "variable"
    mutate(
      statistic = str_extract(variable_statistic, "(mean|median|sd|var)$"),
      variable = str_remove(variable_statistic, "_(mean|median|sd|var)$")
    ) %>%
    # Auswählen und anordnen der Spalten
    select(variable, statistic, value)
}