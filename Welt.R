rawdata = read.csv('COVID19.csv', header = TRUE)
rawdata$date = as.Date(rawdata$date)

Startzeit = min(rawdata$date)
Endzeit = max(rawdata$date)

Datum = seq(Startzeit, Endzeit, by="days")
Gesamte_Faelle = c()
Neue_Faelle = c()
Gesamte_Todesfaelle = c()
Neue_Todesfaelle = c()
Reproduktion = c()
Geimpften = c()
Bevoelkerung = c()

for (i in 1:length(Datum)){
  SUB = subset(rawdata, date == Datum[i]) #Passendes Datum
  SUB = subset(SUB, continent != "") #Nur Laender
  Gesamte_Faelle = c(Gesamte_Faelle, sum(SUB$total_cases, na.rm = TRUE))
  Neue_Faelle = c(Neue_Faelle, sum(SUB$new_cases, na.rm = TRUE))
  Gesamte_Todesfaelle = c(Gesamte_Todesfaelle, sum(SUB$total_deaths, na.rm = TRUE))
  Neue_Todesfaelle = c(Neue_Todesfaelle, sum(SUB$new_deaths, na.rm = TRUE))
  Reproduktion = c(Reproduktion, median(SUB$reproduction_rate, na.rm = TRUE))
  Geimpften = c(Geimpften, sum(SUB$people_vaccinated, na.rm = TRUE))
  Bevoelkerung = c(Bevoelkerung, sum(SUB$population, na.rm = TRUE))
}
 
Welt = data.frame(date = Datum,
                  total_cases = Gesamte_Faelle,
                  new_cases = Neue_Faelle,
                  total_deaths = Gesamte_Todesfaelle,
                  new_deaths = Neue_Todesfaelle,
                  reproduction_rate = Reproduktion,
                  people_vaccinated = Geimpften,
                  population = Bevoelkerung)

write.csv(Welt, "Welt.csv", row.names=TRUE)
