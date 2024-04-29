rename_bundesland <- function(bundesland_name) {
  bundesland_mapping <- c(
    "baden" = "Baden-W\u00fcrttemberg",
    "bayern" = "Bayern",
    "berlin" = "Berlin",
    "brandenburg" = "Brandenburg",
    "bremen" = "Bremen",
    "hamburg" = "Hamburg",
    "hessen" = "Hessen",
    "mecklenburg" = "Mecklenburg-Vorpommern",
    "nordrhein" = "Nordrhein-Westfalen",
    "rheinland" = "Rheinland-Pfalz",
    "saarland" = "Saarland",
    "niedersachsen" = "Nieder",
    "anhalt" = "S-Anhalt",
    "sachsen" = "Sachsen",
    "nieder" = "Niedersachsen",
    "S-anhalt" = "Sachsen-Anhalt",
    "schleswig" = "Schleswig-Holstein",
    "ringen" = "Th\u00fcringen",
    "bundestag" = "Bundestag"
  )
  
  for (pattern in names(bundesland_mapping)) {
    bundesland_name <-
      gsub(paste0(".*", pattern, ".*"),
           bundesland_mapping[pattern],
           bundesland_name,
           ignore.case = TRUE)
  }
  bundesland_name
}
