# ============================================
# R-Skript: Datenbereinigung Bachelorarbeit
# Ziel: Import, Bereinigung und Vorbereitung der Excel-Daten
# Autor: Name
# Datum: 30.05.2025
# ============================================

# Pakete laden
library(readxl)
library(stringr)
library(dplyr)
library(tidyr)

# eigene Funktion laden
source("create_specializedDF_function.R")

# Daten einlesen
df <- read_xlsx("Daten/OPRA Forschungsprojekt 16.04.2025.xlsx")

# Überblick verschaffen
summary(df)
head(df)

### Data Cleaning

# Spaltennamen die >=1 aufeinanderfolgende Leerzeichen enthalten mit einem Unterstrich ersetzen 
names(df) <- str_replace_all(names(df), "\\s+", "_")

# Rechtschreibfehler in Spaltennamen korrigieren
colnames(df)[which(names(df) == "Ceckin_Start")] <- "Checkin_Start"
colnames(df)[which(names(df) == "Modaltiät")] <- "Modalität"

# Vereinheitlichen von Faktorvariablen
df$ErstelltVon <- tolower(df$ErstelltVon)
df$GeändertVon <- tolower(df$GeändertVon)

# Korrigieren der Spalte "Arztgespräch_Dauer"
df <- df %>%
  mutate(
    Arztgespräch_Dauer_ORIGINAL_RAW = Arztgespräch_Dauer,
    temp_numeric = as.numeric(Arztgespräch_Dauer),
    Arztgespräch_Dauer = hms::as_hms(temp_numeric * 86400)
  ) %>%
  select(-temp_numeric)

# Aufruf der Funktion, um die Dataframes für Anmeldung, Befundung und Klinik zu erstellen
list_of_dfs <- create_specialized_dfs(df)

# Zugreifen auf die einzelnen Dataframes
anmeldung <- list_of_dfs$Anmeldung
befundung <- list_of_dfs$Befundung
klinik <- list_of_dfs$Klinik

# Umwandeln der Dauer-Spalten in Sekunden
df <- df %>%
  mutate(
    # Für jede Spalte, die auf "Dauer" endet:
    across(ends_with("Dauer"),
           # Berechne die Sekunden seit Mitternacht (00:00:00) für jeden Zeitpunkt
           ~ as.numeric(format(.x, "%H")) * 3600 +
             as.numeric(format(.x, "%M")) * 60 +
             as.numeric(format(.x, "%S")),
           .names = "{.col}_secs"
    )
  )

#### Deskriptive Statistik 

# Mittelwert, Median, Standardabweichung, Varianz für alle numerischen Spalten
anmeldung_descriptive <- anmeldung %>%
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
  )

# Umwandeln in das Long-Format
anmeldung_descriptive_long <- anmeldung_descriptive %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable_statistic",
    values_to = "value"
  ) %>%
  # Zerlegen der Spalte "variable_statistic" in "variable" und "statistic"
  mutate(
    statistic = str_extract(variable_statistic, "(mean|median|sd|var)$"),
    variable = str_remove(variable_statistic, "_(mean|median|sd|var)$")
  ) %>%
  # Auswählen und anordnen der Spalten
  select(variable, statistic, value)
print("Deskriptive Statistiken im Long-Format:")
print(anmeldung_descriptive_long)


##### Kann eigentlich weg #####
###  Unausagekräftige Plots

# Boxplot nach Prüfverfahren
boxplot(Anmeldung_AX_Dauer ~ Prüfverfahren,
        data = anmeldung,
        main = "Anmeldedauer nach Prüfverfahren",
        xlab = "Prüfverfahren",
        ylab = "Dauer in Sekunden",
        col = "lightgreen")


# Zusammenhang Anmeldedauer und Kontaktanzahl
plot(anmeldung$Kontakt_Anzahl_Anmeldung,
     hms::as_hms(anmeldung$Anmeldung_AX_Dauer), # hms-Paket für Zeitkomponenten ohne Datum; evtl. Achsenskalierung ändern
     main = "Zusammenhang Kontaktanzahl und Dauer",
     xlab = "Anzahl Kontakte",
     ylab = "Anmeldedauer (Sekunden)",
     pch = 19, col = "steelblue")


summary(anmeldung$Anmeldung_AX_Dauer)
summary(anmeldung$Checkin_Dauer)
summary(anmeldung$Fallakte_Dauer)
summary(anmeldung$Kontakt_Anzahl_Anmeldung)

# Mittelwert pro Prüfverfahren
aggregate(Anmeldung_AX_Dauer ~ Prüfverfahren, data = anmeldung, FUN = mean, na.rm = TRUE)
aggregate(Checkin_Dauer ~ Prüfverfahren, data = anmeldung, FUN = mean, na.rm = TRUE)

# Kontaktanzahl nach Prüfverfahren
aggregate(Kontakt_Anzahl_Anmeldung ~ Prüfverfahren, data = anmeldung, FUN = mean, na.rm = TRUE)

# Histogramme
hist(anmeldung$Anmeldung_AX_Dauer, main = "Anmeldedauer", xlab = "Sekunden", col = "lightblue", breaks = 'secs')

# Boxplots nach Prüfverfahren
boxplot(Anmeldung_AX_Dauer ~ Prüfverfahren, data = anmeldung,
        main = "Anmeldedauer nach Prüfverfahren",
        ylab = "Dauer in Sekunden", col = "lightgreen")

# Mittlere Dauer je Modalität
aggregate(Anmeldung_AX_Dauer ~ Modalität, data = anmeldung, FUN = mean, na.rm = TRUE)


statistics_Anmeldung_Dauer <- anmeldung %>%
  group_by(Prüfverfahren) %>%
  summarise(
    n = n(),
    mean_Anmeldung = mean(Anmeldung_AX_Dauer, na.rm = TRUE),
    median_Anmeldung = median(Anmeldung_AX_Dauer, na.rm = TRUE),
    sd_Anmeldung = sd(Anmeldung_AX_Dauer, na.rm = TRUE),
    mean_Checkin = mean(Checkin_Dauer, na.rm = TRUE),
    mean_Kontakte = mean(Kontakt_Anzahl_Anmeldung, na.rm = TRUE)
  )


###### Statistische Tests mit P-Wert (Signifikanz usw.) #####

# ANOVA zur Prüfverfahren-Wirkung
aov_result <- aov(Anmeldung_AX_Dauer ~ Prüfverfahren, data = anmeldung)
summary(aov_result)

# Zusammenhang prüfen: Dauer vs. Kontaktanzahl
cor.test(as.numeric(
  difftime(anmeldung$Anmeldung_AX_Stop, anmeldung$Anmeldung_AX_Start, units = "secs")
), anmeldung$Kontakt_Anzahl_Anmeldung, use = "complete.obs")

plot(anmeldung$Kontakt_Anzahl_Anmeldung,
     anmeldung$Anmeldung_AX_Dauer,
     main = "Zusammenhang Kontakte und Anmeldedauer",
     xlab = "Anzahl Kontakte",
     ylab = "Dauer in Sekunden", pch = 19, col = "purple")
abline(lm(Anmeldung_AX_Dauer ~ Kontakt_Anzahl_Anmeldung, data = anmeldung), col = "orange", lwd=2)

## als nächstes ausreißer identifizieren (vlt. mit Hilfe von dem Buch von Warnat "Saur...")
