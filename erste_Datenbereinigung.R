# ============================================
# R-Skript: Datenbereinigung Bachelorarbeit
# Ziel: Import, Bereinigung und Vorbereitung der Excel-Daten
# Autor: Name
# Datum: 12.05.2025
# ============================================

# Pakete laden
library(readxl)
library(stringr)
library(dplyr)

# eigene Funktion laden
source("create_specializedDF_function.R")

# Daten einlesen
df <- read_xlsx("Daten/OPRA Forschungsprojekt 16.04.2025.xlsx")

# Überblick verschaffen
summary(df)
head(df)

# Spaltennamen die >=1 aufeinanderfolgende Leerzeichen enthalten mit einem Unterstrich ersetzen 
names(df) <- str_replace_all(names(df), "\\s+", "_")

# Rechtschreibfehler in Spaltennamen korrigieren
colnames(df)[which(names(df) == "Ceckin_Start")] <- "Checkin_Start"
colnames(df)[which(names(df) == "Modaltiät")] <- "Modalität"

# Vereinheitlichen von Faktorvariablen
df$ErstelltVon <- tolower(df$ErstelltVon)
df$GeändertVon <- tolower(df$GeändertVon)

# Aufruf der Funktion, um die Dataframes für Anmeldung, Befundung und Klinik zu erstellen
list_of_dfs <- create_specialized_dfs(df)

# Zugreifen auf die einzelnen Dataframes
Anmeldung_df <- list_of_dfs$Anmeldung
Befundung_df <- list_of_dfs$Befundung
Klinik_df <- list_of_dfs$Klinik

# Stoppuhr-Spalten 
zeitvars <- c("Anmeldung AX Dauer", "Anmeldung AX Start", "Anmeldung AX Stop","Anmeldung Dauer",
              "Checkin Start", "Checkin Dauer", "Checkin Stop",
              "Fallakte Dauer", "Fallakte Start", "Fallakte Stop")

# Faktor-Spalten
factorvars <- c("ErstelltVon", "Modalität", "Gerät", "Körperregion",
                "Prüfverfahren", "Prüfverfahren global", "GeändertVon")

# Umwandeln von Variablen in Faktor um später besser damit arbeiten zu können (Korrekte Analyse, Richtige Visualisierung)
for (var in factorvars) {
  Anmeldung_df[[var]] <- as.factor(Anmeldung_df[[var]])
}


##### Kann eigentlich weg #####
###  Unausagekräftige Plots

# Boxplot nach Prüfverfahren
boxplot(Anmeldung_AX_Dauer ~ Prüfverfahren,
        data = Anmeldung_df,
        main = "Anmeldedauer nach Prüfverfahren",
        xlab = "Prüfverfahren",
        ylab = "Dauer in Sekunden",
        col = "lightgreen")

# Zusammenhang Anmeldedauer und Kontaktanzahl
plot(Anmeldung_df$Kontakt_Anzahl_Anmeldung,
     Anmeldung_df$Anmeldung_AX_Dauer,
     main = "Zusammenhang Kontaktanzahl und Dauer",
     xlab = "Anzahl Kontakte",
     ylab = "Anmeldedauer (Sekunden)",
     pch = 19, col = "steelblue")


summary(Anmeldung_df$Anmeldung_AX_Dauer)
summary(Anmeldung_df$Checkin_Dauer)
summary(Anmeldung_df$Fallakte_Dauer)
summary(Anmeldung_df$Kontakt_Anzahl_Anmeldung)

# Mittelwert pro Prüfverfahren
aggregate(Anmeldung_AX_Dauer ~ Prüfverfahren, data = Anmeldung_df, FUN = mean, na.rm = TRUE)
aggregate(Checkin_Dauer ~ Prüfverfahren, data = Anmeldung_df, FUN = mean, na.rm = TRUE)

# Kontaktanzahl nach Prüfverfahren
aggregate(Kontakt_Anzahl_Anmeldung ~ Prüfverfahren, data = Anmeldung_df, FUN = mean, na.rm = TRUE)

# Histogramme
hist(Anmeldung_df$Anmeldung_AX_Dauer, main = "Anmeldedauer", xlab = "Sekunden", col = "lightblue", breaks = 'secs')

# Boxplots nach Prüfverfahren
boxplot(Anmeldung_AX_Dauer ~ Prüfverfahren, data = Anmeldung_df,
        main = "Anmeldedauer nach Prüfverfahren",
        ylab = "Dauer in Sekunden", col = "lightgreen")

# Mittlere Dauer je Modalität
aggregate(Anmeldung_AX_Dauer ~ Modalität, data = Anmeldung_df, FUN = mean, na.rm = TRUE)


statistics_Anmeldung_Dauer <- Anmeldung_df %>%
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
aov_result <- aov(Anmeldung_AX_Dauer ~ Prüfverfahren, data = Anmeldung_df)
summary(aov_result)

# Zusammenhang prüfen: Dauer vs. Kontaktanzahl
cor.test(as.numeric(
  difftime(Anmeldung_df$Anmeldung_AX_Stop, Anmeldung_df$Anmeldung_AX_Start, units = "secs")
), Anmeldung_df$Kontakt_Anzahl_Anmeldung, use = "complete.obs")

plot(Anmeldung_df$Kontakt_Anzahl_Anmeldung,
     Anmeldung_df$Anmeldung_AX_Dauer,
     main = "Zusammenhang Kontakte und Anmeldedauer",
     xlab = "Anzahl Kontakte",
     ylab = "Dauer in Sekunden", pch = 19, col = "purple")
abline(lm(Anmeldung_AX_Dauer ~ Kontakt_Anzahl_Anmeldung, data = Anmeldung_df), col = "orange", lwd=2)

## eventuell noch umlaute entfernen
## als nächstes ausreißer identifizieren (vlt. mit Hilfe von dem Buch von Warnat "Saur...")
