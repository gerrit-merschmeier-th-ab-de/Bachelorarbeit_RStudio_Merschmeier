# ============================================
# R-Skript: Datenbereinigung Bachelorarbeit
# Ziel: Import, Bereinigung und Vorbereitung der Excel-Daten
# Autor: Name
# Datum: 12.05.2025
# ============================================

# Laden der Pakete
#install.packages("readxl") # nur beim ersten Mal notwendig
library(readxl)

# Einlesen der Daten
df <- read_xlsx("Daten/OPRA Forschungsprojekt 16.04.2025.xlsx")

# Überblick verschaffen
summary(df)
head(df)

# Auswählen von relevanten Spalten             (Fehler in "Modaltiät")
data_relevant <- df[, c("Änderungszeitstempel", "Datum", "ErstelltVon", "LfdNr", "Fall_ID",
                        "Fall_Foto_ID_foreignKey", "Modaltiät", "Gerät", "Körperregion", "Anmeldung AX Dauer",
                        "Anmeldung AX Start", "Anmeldung AX Stop", "Anmeldung Dauer", "Anmeldung Kommentar", "Ceckin Start",
                        "Checkin Dauer", "Checkin Stop", "Fallakte Dauer", "Fallakte Start", "Fallakte Stop", 
                        "Kontakt Anzahl Anmeldung", "Kontakt Anzahl Summe", "Primärschlüssel", "Prüfverfahren", "Prüfverfahren global", 
                        "TR_Index", "GeändertVon"
                        )]

# Stoppuhr-Spalten 
zeitvars <- c("Anmeldung AX Dauer", "Anmeldung AX Start", "Anmeldung AX Stop","Anmeldung Dauer",
              "Ceckin Start", "Checkin Dauer", "Checkin Stop",
              "Fallakte Dauer", "Fallakte Start", "Fallakte Stop")

# Faktor-Spalten
factorvars <- c("ErstelltVon", "Modaltiät", "Gerät", "Körperregion",
                "Prüfverfahren", "Prüfverfahren global", "GeändertVon")

# Umwandeln von Variablen in Faktor um später besser damit arbeiten zu können (Korrekte Analyse, Richtige Visualisierung)
for (var in factorvars) {
  data_relevant[[var]] <- as.factor(data_relevant[[var]])
}

# Vereinheitlichen von Faktorvariablen
data_relevant$ErstelltVon <- tolower(data_relevant$ErstelltVon)
data_relevant$GeändertVon <- tolower(data_relevant$GeändertVon)

# Alle Spaltennamen mit Leerzeichen in Unterstriche umwandeln
colnames(data_relevant) <- gsub(" ", "_", colnames(data_relevant))

##### Kann eigentlich weg #####
###  Unausagekräftige Plots
hist(dauer_sec,
     main = "Verteilung der Anmeldedauer",
     xlab = "Dauer in Sekunden",
     col = "skyblue")

# Boxplot nach Prüfverfahren
boxplot(`Anmeldung AX Dauer` ~ Prüfverfahren,
        data = data_relevant,
        main = "Anmeldedauer nach Prüfverfahren",
        xlab = "Prüfverfahren",
        ylab = "Dauer in Sekunden",
        col = "lightgreen")

# Zusammenhang Anmeldedauer und Kontaktanzahl
plot(data_relevant$`Kontakt Anzahl Anmeldung`,
     data_relevant$`Anmeldung AX Dauer`,
     main = "Zusammenhang Kontaktanzahl und Dauer",
     xlab = "Anzahl Kontakte",
     ylab = "Anmeldedauer (Sekunden)",
     pch = 19, col = "steelblue")

# als nächstes vielleicht die rohdaten nehmen und so splitten wie sie in der ipad-app sind -> anmeldung, klinik, befundung... 

summary(data_relevant$Anmeldung_AX_Dauer)
summary(data_relevant$Checkin_Dauer)
summary(data_relevant$Fallakte_Dauer)
summary(data_relevant$Kontakt_Anzahl_Anmeldung)

# Mittelwert pro Prüfverfahren
aggregate(Anmeldung_AX_Dauer ~ Prüfverfahren, data = data_relevant, FUN = mean, na.rm = TRUE)
aggregate(Checkin_Dauer ~ Prüfverfahren, data = data_relevant, FUN = mean, na.rm = TRUE)

# Kontaktanzahl nach Prüfverfahren
aggregate(Kontakt_Anzahl_Anmeldung ~ Prüfverfahren, data = data_relevant, FUN = mean, na.rm = TRUE)

# Histogramme
hist(data_relevant$Anmeldung_AX_Dauer, main = "Anmeldedauer", xlab = "Sekunden", col = "lightblue", breaks = 'secs')

# Boxplots nach Prüfverfahren
boxplot(Anmeldung_AX_Dauer ~ Prüfverfahren, data = data_relevant,
        main = "Anmeldedauer nach Prüfverfahren",
        ylab = "Dauer in Sekunden", col = "lightgreen")

# Mittlere Dauer je Modalität
aggregate(Anmeldung_AX_Dauer ~ Modaltiät, data = data_relevant, FUN = mean, na.rm = TRUE)

library(dplyr)

statistics_Anmeldung_Dauer <- data_relevant %>%
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
aov_result <- aov(Anmeldung_AX_Dauer ~ Prüfverfahren, data = data_relevant)
summary(aov_result)

# Zusammenhang prüfen: Dauer vs. Kontaktanzahl
cor.test(as.numeric(
  difftime(data_relevant$Anmeldung_AX_Stop, data_relevant$Anmeldung_AX_Start, units = "secs")
), data_relevant$Kontakt_Anzahl_Anmeldung, use = "complete.obs")

plot(data_relevant$Kontakt_Anzahl_Anmeldung,
     data_relevant$Anmeldung_AX_Dauer,
     main = "Zusammenhang Kontakte und Anmeldedauer",
     xlab = "Anzahl Kontakte",
     ylab = "Dauer in Sekunden", pch = 19, col = "darkred")
abline(lm(Anmeldung_AX_Dauer ~ Kontakt_Anzahl_Anmeldung, data = data_relevant), col = "blue")

# Regressionsmodell zum Einfluss mehrerer Faktoren
modell <- lm(Anmeldung_AX_Dauer ~ Kontakt_Anzahl_Anmeldung + Modaltiät + Gerät + Körperregion + Prüfverfahren, data = data_relevant)
summary(modell)

######## als letztes funktion für die erstellung von den spez. DFs gemacht. Jetzt hier eingliedern