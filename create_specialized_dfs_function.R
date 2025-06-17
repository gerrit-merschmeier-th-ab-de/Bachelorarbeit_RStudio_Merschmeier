# Definition der Funktion
create_specialized_dfs <- function(original_df) {
  
  # Basis-Spalten, die in allen drei Dataframes enthalten sein sollen
  base_columns_start <- c(
    "Änderungszeitstempel",
    "Erstellungszeitstempel",
    "Datum",
    "ErstelltVon",
    "Fall_ID",
    "Modalität",
    "Gerät",
    "Körperregion",
    "Kontakt_Anzahl_Summe",
    "LfdNr"
  )
  
  # Basis-Spalten, die am Ende aller drei Dataframes enthalten sein sollen
  base_columns_end <- c(
    "Patienten_ID_foreignKey",
    "Fall_Foto_ID_foreignKey",
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
    "Aufklärung_A2_Start",
    "Aufklärung_A2_Stop",
    "Aufklärung_A2_Dauer",
    "Checkin_Start",
    "Checkin_Stop",
    "Checkin_Dauer",
    "Fallakte_Start",
    "Fallakte_Stop",
    "Fallakte_Dauer",
    "Kontakt_Anzahl_Anmeldung",
    "Anmeldung_AX_Dauer_secs",
    "Anmeldung_Dauer_secs",
    "Aufklärung_A2_Dauer_secs",
    "Checkin_Dauer_secs",
    "Fallakte_Dauer_secs"
  )
  columns_anmeldung <- c(base_columns_start, anmeldung_specific_columns, base_columns_end)
  Anmeldung <- original_df[, columns_anmeldung]
  
  ## Dataframe 'Befundung' erstellen
  befundung_specific_columns <- c(
    "Befundung_Dauer",
    "Befundung_mit_Reporter_B4_Start",
    "Befundung_mit_Reporter_B4_Stop",
    "Befundung_mit_Reporter_B4_Dauer",
    "Befundung_ohne_Reporter_B3_Start",
    "Befundung_ohne_Reporter_B3_Stop",
    "Befundung_ohne_Reporter_B3_Dauer",
    "Kontakt_Anzahl_Befunder",
    "Reporter_Befund_besser",
    "Reporter_Befund_gleich_gut",
    "Reporter_Befund_schlechter",
    "Reporter_Befund_richtig",
    "Reporter_Befund_falsch",
    "Reporter_Befund_Umfang_zu_kurz",
    "Reporter_Befund_Umfang_zu_lang",
    "Reporter_Befund_Umfang_angemessen",
    "Befundung_Dauer_secs",
    "Befundung_mit_Reporter_B4_Dauer_secs",
    "Befundung_ohne_Reporter_B3_Dauer_secs"
  )
  columns_befundung <- c(base_columns_start, befundung_specific_columns, base_columns_end)
  Befundung <- original_df[, columns_befundung]
  
  ## Dataframe 'Klinik' erstellen
  klinik_specific_columns <- c(
    "Klinik_mit_Arztgespräch_B1_Start",
    "Klinik_mit_Arztgespräch_B1_Stop",
    "Klinik_mit_Arztgespräch_B1_Dauer",
    "Klinik_ohne_Arztgespräch_B1_Start",
    "Klinik_ohne_Arztgespräch_B1_Stop",
    "Klinik_ohne_Arztgespräch_B1_Dauer",
    "Kontakt_Anzahl_Arztgespräch",
    "Arztgespräch_Dauer",
    "OPRA_Anamese_besser",
    "OPRA_Anamese_gleich_gut",
    "OPRA_Anamese_schlechter",
    "OPRA_Anamese_richtig",
    "OPRA_Anamese_falsch",
    "OPRA_Anamese_Umfang_zu_kurz",
    "OPRA_Anamese_Umfang_zu_lang",
    "OPRA_Anamese_Umfang_angemessen",
    "OPRA_Anamese_Kommentar",
    "Klinik_mit_Arztgespräch_B1_Dauer_secs",
    "Klinik_ohne_Arztgespräch_B1_Dauer_secs",
    "Arztgespräch_Dauer_secs"
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
