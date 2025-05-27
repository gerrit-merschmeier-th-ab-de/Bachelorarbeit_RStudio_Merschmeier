# Definition der Funktion
create_specialized_dfs <- function(original_df) {
  
  # Basis-Spalten, die in allen drei Dataframes enthalten sein sollen
  base_columns_start <- c(
    "Änderungszeitstempel",
    "Datum",
    "ErstelltVon",
    "LfdNr",
    "Fall_ID",
    "Fall_Foto_ID_foreignKey",
    "Modaltiät",
    "Gerät",
    "Körperregion"
  )
  
  # Basis-Spalten, die am Ende aller drei Dataframes enthalten sein sollen
  base_columns_end <- c(
    "Primärschlüssel",
    "Prüfverfahren",
    "Prüfverfahren_global",
    "TR_Index",
    "GeändertVon"
  )
  
  ## Dataframe 'Anmeldung' erstellen
  anmeldung_specific_columns <- c(
    "Anmeldung_AX_Dauer",
    "Anmeldung_AX_Start",
    "Anmeldung_AX_Stop",
    "Anmeldung_Dauer",
    "Anmeldung_Kommentar",
    "Ceckin_Start",
    "Checkin_Dauer",
    "Checkin_Stop",
    "Fallakte_Dauer",
    "Fallakte_Start",
    "Fallakte_Stop",
    "Kontakt_Anzahl_Anmeldung",
    "Kontakt_Anzahl_Summe"
  )
  columns_anmeldung <- c(base_columns_start, anmeldung_specific_columns, base_columns_end)
  Anmeldung <- original_df[, columns_anmeldung]
  
  ## Dataframe 'Befundung' erstellen
  befundung_specific_columns <- c(
    "Befundung_Dauer",
    "Befundung_ohne_Reporter_B3_Dauer",
    "Befundung_ohne_Reporter_B3_Start",
    "Befundung_ohne_Reporter_B3_Stop",
    "Befundung_mit_Reporter_B4_Dauer",
    "Befundung_mit_Reporter_B4_Start",
    "Befundung_mit_Reporter_B4_Stop",
    "Kontakt_Anzahl_Befunder"
  )
  columns_befundung <- c(base_columns_start, befundung_specific_columns, base_columns_end)
  Befundung <- original_df[, columns_befundung]
  
  ## Dataframe 'Klinik' erstellen
  klinik_specific_columns <- c(
    "Arztgespräch_Dauer",
    "Klinik_mit_Arztgespräch_B1_Dauer",
    "Klinik_mit_Arztgespräch_B1_Start",
    "Klinik_mit_Arztgespräch_B1_Stop",
    "Klinik_ohne_Arztgespräch_B1_Dauer",
    "Klinik_ohne_Arztgespräch_B1_Start",
    "Klinik_ohne_Arztgespräch_B1_Stop",
    "Kontakt_Anzahl_Arztgespräch",
    "OPRA_Anamese_besser",
    "OPRA_Anamese_falsch",
    "OPRA_Anamese_gleich_gut",
    "OPRA_Anamese_Kommentar"
  )
  columns_klinik <- c(base_columns_start, klinik_specific_columns, base_columns_end)
  Klinik <- original_df[, columns_klinik]
  
  # Die drei Dataframes als Liste zurückgeben
  return(list(
    Anmeldung = Anmeldung,
    Befundung = Befundung,
    Klinik = Klinik
  ))
}