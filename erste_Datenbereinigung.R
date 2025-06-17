# ============================================
# R-Skript: Datenbereinigung Bachelorarbeit
# Ziel: Import, Bereinigung und Vorbereitung der Excel-Daten
# Autor: Name
# Datum: 17.06.2025
# ============================================

# Pakete laden
library(readxl)
library(stringr)
library(dplyr)
library(tidyr)

# eigene Funktionen laden
source("create_specialized_dfs_function.R")
source("create_descriptive_summary_function.R")

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

# Umwandeln der Excel-Zeitbrüche in "Arztgespräch_Dauer" in Sekunden (1 Tag = 86400 Sekunden)
df <- df %>%
  mutate(
    Arztgespräch_Dauer_secs = as.numeric(Arztgespräch_Dauer) * 86400
    )

# Umwandeln der Dauer-Spalten in Sekunden
df <- df %>%
  mutate(
    # Für jede Spalte, die auf "Dauer" endet:
    across(ends_with("Dauer") & !starts_with("Arztgespräch_Dauer"),
           # Berechne die Sekunden seit Mitternacht (00:00:00) für jeden Zeitpunkt
           ~ as.numeric(format(.x, "%H")) * 3600 +
             as.numeric(format(.x, "%M")) * 60 +
             as.numeric(format(.x, "%S")),
           .names = "{.col}_secs"
    )
  )

# Aufruf der Funktion, um die Dataframes für Anmeldung, Befundung und Klinik zu erstellen
list_of_dfs <- create_specialized_dfs(df)

# Zugreifen auf die einzelnen Dataframes
anmeldung <- list_of_dfs$Anmeldung
befundung <- list_of_dfs$Befundung
klinik <- list_of_dfs$Klinik


#### Deskriptive Statistik 


# Deskriptive Tabellen erzeugen
anmeldung_descriptive <- create_descriptive_summary(anmeldung)
befundung_descriptive <- create_descriptive_summary(befundung)
klinik_descriptive <- create_descriptive_summary(klinik)

### NEUES BEGINNT HIER ###

anmeldung_long_differentiated <- anmeldung %>%
  pivot_longer(
    cols = ends_with("_Dauer_secs"), # Alle Dauer-Spalten
    names_to = "Prozess",
    values_to = "Dauer_Sekunden"
  ) %>%
  mutate(
    # Kategorie hinzufügen, um zwischen "Gesamtdauer" und "Teilprozess" zu unterscheiden
    Kategorie = ifelse(Prozess == "Anmeldung_Dauer_secs", "Gesamtdauer", "Teilprozess"),
    # Prozessnamen bereinigen
    Prozess = str_replace_all(Prozess, "_Dauer_secs", ""),
    Prozess = str_replace_all(Prozess, "_", " ")
  )

ggplot(anmeldung_long_differentiated, aes(x = Prozess, y = Dauer_Sekunden, fill = Kategorie)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = "X") +
  scale_y_log10(labels = scales::label_comma()) +
  labs(
    title = "Verteilung der Prozessdauern in der Anmeldung",
    subtitle = "Vergleich von Teilprozessen und Gesamtdauer",
    x = "Prozessschritt",
    y = "Dauer in Sekunden",
    fill = "Dauer-Typ" # Legendentitel
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

ggplot(anmeldung_long_differentiated, aes(x = Prozess, y = Dauer_Sekunden, fill = Kategorie)) +
  geom_violin(trim = FALSE, alpha = 0.7) + # trim=FALSE zeigt die volle Dichte
  geom_boxplot(width = 0.1, outlier.colour = "red", outlier.shape = 8) + # Boxplot als Overlay
  #scale_y_log10(labels = scales::label_comma()) +
  labs(
    title = "Verteilung der Prozessdauern in der Anmeldung (Violin-Plot, Log-Skala)",
    subtitle = "Dauer in Sekunden",
    x = "Prozessschritt",
    y = "Dauer in Sekunden (log Skala)",
    fill = "Dauer-Typ"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

# Histogramm für Anmeldung_AX_Dauer_secs
ggplot(anmeldung %>% filter(!is.na(Anmeldung_AX_Dauer_secs)), aes(x = Anmeldung_AX_Dauer_secs)) +
  geom_histogram(binwidth = 10, fill = "lightblue", color = "black") + # Experimentiere mit binwidth
  labs(
    title = "Histogramm der Anmeldung_AX_Dauer",
    x = "Dauer in Sekunden",
    y = "Anzahl der Beobachtungen"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Dichtediagramm für Anmeldung_AX_Dauer_secs
ggplot(anmeldung %>% filter(!is.na(Anmeldung_AX_Dauer_secs)), aes(x = Anmeldung_AX_Dauer_secs)) +
  geom_density(fill = "lightblue", alpha = 0.7, color = "darkblue") +
  labs(
    title = "Dichteverteilung der Anmeldung_AX_Dauer",
    x = "Dauer in Sekunden",
    y = "Dichte"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

### NEUES ENDET HIER ###

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
## die neue funktion ist noch nicht funktionsfähig