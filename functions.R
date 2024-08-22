# Funktionen für Plots

colormap = c("#666666", "#E69F00", "#009E73")

gmean = function(v) {
  g = 1
  for (v_i in v) {
    g = g * v_i
  }
  geomean = g ^ (1 / length(v))
  return(geomean)
}

options(warn = -1)


########################################
### Land
########################################

plot_new_cases = function(location_data, moving_average, checkMA) {
  p <-
    ggplot()
  if (!checkMA) {
    p <-
      p + geom_line(data = location_data, aes(x = date, y = new_cases, color = location))
  }
  p <- p + scale_color_manual(values = colormap)
  if (moving_average != 0) {
    p <-
      p + geom_ma(
        data = location_data,
        mapping = aes(x = date, y = new_cases, color = location),
        ma_fun = SMA,
        n = moving_average
      )
  }
  p <-
    p + xlim(min(location_data$date), max(location_data$date)) + ylim(0, max(location_data$new_cases)) + xlab("Datum") + ylab("Neue Fälle")
  p + theme_bw()
}

plot_reproduction = function(location_data, moving_average, checkMA) {
  p <-
    ggplot()
  if (!checkMA) {
    p <-
      p + geom_line(data = location_data, aes(x = date, y = reproduction_rate, color = location))
  }
  if (moving_average != 0) {
    p <-
      p + geom_ma(
        data = location_data,
        mapping = aes(x = date, y = reproduction_rate, color = location),
        ma_fun = SMA,
        n = moving_average
      )
  }
  p <-
    p + xlim(min(location_data$date), max(location_data$date)) + ylim(0, max(location_data$reproduction_rate)) +
    xlab("Datum") + ylab("Reproduktionszahl") + scale_color_manual(values = colormap)
  p + theme_bw()
}

plot_stringency = function(location_data, moving_average, checkMA) {
  p <-
    ggplot()
  if (!checkMA) {
    p <-
      p + geom_line(data = location_data, aes(x = date, y = stringency_index, color = location))
  }
  if (moving_average != 0) {
    p <-
      p + geom_ma(
        data = location_data,
        mapping = aes(x = date, y = stringency_index, color = location),
        ma_fun = SMA,
        n = moving_average
      )
  }
  p <-
    p + xlim(min(location_data$date), max(location_data$date)) + ylim(0, max(location_data$stringency_index)) +
    xlab("Datum") + ylab("Einschränkungen") + scale_color_manual(values = colormap)
  p + theme_bw()
}

plot_total_cases = function(location_data, moving_average, checkMA) {
  p <-
    ggplot()
  if (!checkMA) {
    p <-
      p + geom_line(data = location_data,
                    aes(
                      x = date,
                      y = total_cases / population * 100,
                      color = location
                    ))
  }
  if (moving_average != 0) {
    p <-
      p + geom_ma(
        data = location_data,
        mapping = aes(
          x = date,
          y = total_cases / population * 100,
          color = location
        ),
        ma_fun = SMA,
        n = moving_average
      )
  }
  p <-
    p + xlim(min(location_data$date), max(location_data$date)) + ylim(0,
                                                                      max(location_data$total_cases / location_data$population * 100)) + xlab("Datum") + ylab("Gesamte Fälle [% Population]") + scale_color_manual(values = colormap)
  p + theme_bw()
}

plot_total_death = function(location_data, moving_average, checkMA) {
  p <-
    ggplot()
  if (!checkMA) {
    p <-
      p + geom_line(data = location_data,
                    aes(
                      x = date,
                      y = total_deaths / population * 1000,
                      color = location
                    ))
  }
  if (moving_average != 0) {
    p <-
      p + geom_ma(
        data = location_data,
        mapping = aes(
          x = date,
          y = total_deaths / population * 1000,
          color = location
        ),
        ma_fun = SMA,
        n = moving_average
      )
  }
  p <-
    p + xlim(min(location_data$date), max(location_data$date)) + ylim(0,
                                                                      max(location_data$total_deaths / location_data$population * 1000)) + xlab("Datum") + ylab("Gesamte Todesfälle [‰ Population]") + scale_color_manual(values = colormap)
  p + theme_bw()
}

plot_new_tests = function(location_data, moving_average, checkMA) {
  p <-
    ggplot()
  if (!checkMA) {
    p <-
      p + geom_line(data = location_data,
                    aes(x = date, y = new_tests_per_thousand, color = location))
  }
  p <- p + scale_color_manual(values = colormap)
  if (moving_average != 0) {
    p <-
      p + geom_ma(
        data = location_data,
        mapping = aes(x = date, y = new_tests_per_thousand, color = location),
        ma_fun = SMA,
        n = moving_average
      )
  }
  p <-
    p + xlim(min(location_data$date), max(location_data$date)) + ylim(0, max(location_data$new_tests_per_thousand)) +
    xlab("Datum") + ylab("Neue Tests pro 1000")
  p + theme_bw()
}

plot_test_positiverate = function(location_data, moving_average, checkMA) {
  p <-
    ggplot()
  if (!checkMA) {
    p <-
      p + geom_line(data = location_data, aes(x = date, y = positive_rate, color = location))
  }
  p <- p + scale_color_manual(values = colormap)
  if (moving_average != 0) {
    p <-
      p + geom_ma(
        data = location_data,
        mapping = aes(x = date, y = positive_rate, color = location),
        ma_fun = SMA,
        n = moving_average
      )
  }
  p <-
    p + xlim(min(location_data$date), max(location_data$date)) + ylim(0, max(location_data$positive_rate)) +
    xlab("Datum") + ylab("Positivrate")
  p + theme_bw()
}

plot_ICU = function(location_data, moving_average, checkMA) {
  p <-
    ggplot()
  if (!checkMA) {
    p <-
      p + geom_line(data = location_data, aes(x = date, y = icu_patients, color = location))
  }
  if (moving_average != 0) {
    p <-
      p + geom_ma(
        data = location_data,
        mapping = aes(x = date, y = icu_patients, color = location),
        ma_fun = SMA,
        n = moving_average
      )
  }
  p <-
    p + xlim(min(location_data$date), max(location_data$date)) + ylim(0, max(location_data$icu_patients)) +
    xlab("Datum") + ylab("Intensiv-Patienten") + scale_color_manual(values = colormap)
  p + theme_bw()
}

plot_hosp = function(location_data, moving_average, checkMA) {
  p <-
    ggplot()
  if (!checkMA) {
    p <-
      p + geom_line(data = location_data, aes(x = date, y = hosp_patients, color = location))
  }
  if (moving_average != 0) {
    p <-
      p + geom_ma(
        data = location_data,
        mapping = aes(x = date, y = hosp_patients, color = location),
        ma_fun = SMA,
        n = moving_average
      )
  }
  p <-
    p + xlim(min(location_data$date), max(location_data$date)) + ylim(0, max(location_data$hosp_patients)) +
    xlab("Datum") + ylab("Krankenhaus-Patienten") + scale_color_manual(values = colormap)
  p + theme_bw()
}

plot_bettenbelegung = function(location_data, moving_average, checkMA) {
  p <-
    ggplot()
  if (!checkMA) {
    p <-
      p + geom_line(data = location_data, aes(
        x = date,
        y = hosp_patients / (hospital_beds_per_thousand * population /
                               1000) * 100,
        color = location
      ))
  }
  if (moving_average != 0) {
    p <-
      p + geom_ma(
        data = location_data,
        mapping = aes(
          x = date,
          y = hosp_patients / (hospital_beds_per_thousand * population /
                                 1000) * 100,
          color = location
        ),
        ma_fun = SMA,
        n = moving_average
      )
  }
  p <-
    p + xlim(min(location_data$date), max(location_data$date)) + ylim(0, max(
      location_data$hosp_patients / (
        location_data$hospital_beds_per_thousand * location_data$population /
          1000
      ) * 100
    )) + xlab("Datum") + ylab("Bettenbelegung [%]") + scale_color_manual(values = colormap)
  p + theme_bw()
}

plot_vac = function(location_data, moving_average, checkMA) {
  p <-
    ggplot()
  if (!checkMA) {
    p <-
      p + geom_line(data = location_data,
                    aes(
                      x = date,
                      y = people_vaccinated / population * 100,
                      color = location
                    ))
  }
  if (moving_average != 0) {
    p <-
      p + geom_ma(
        data = location_data,
        mapping = aes(
          x = date,
          y = people_vaccinated / population * 100,
          color = location
        ),
        ma_fun = SMA,
        n = moving_average
      )
  }
  p <-
    p + xlim(min(location_data$date), max(location_data$date)) + ylim(0, 100) +
    xlab("Datum") + ylab("Impfquote [%]") + scale_color_manual(values = colormap)
  p + theme_bw()
}

plot_sterblichkeit = function(location_data, moving_average, checkMA) {
  p <-
    ggplot()
  if (!checkMA) {
    p <-
      p + geom_line(data = location_data,
                    aes(
                      x = date,
                      y = new_deaths / new_cases * 100,
                      color = location
                    ))
  }
  p <- p + scale_color_manual(values = colormap)
  if (moving_average != 0) {
    p <-
      p + geom_ma(
        data = location_data,
        mapping = aes(
          x = date,
          y = new_deaths / new_cases * 100,
          color = location
        ),
        ma_fun = SMA,
        n = moving_average
      )
  }
  p <-
    p + xlim(min(location_data$date), max(location_data$date)) + ylim(0,
                                                                      max(location_data$new_deaths / location_data$new_cases * 100)) + xlab("Datum") + ylab("Sterblichkeit [%]")
  p + theme_bw()
}



########################################
### Welt
########################################

plot_world_newcases = function(world_data, moving_average, checkMA) {
  n <- 1
  if (moving_average == "5 Tage") {
    n <- 5
  }
  if (moving_average == "7 Tage") {
    n <- 7
  }
  if (moving_average == "20 Tage") {
    n <- 20
  }
  
  p <-
    ggplot(world_data,
           aes(x = date,
               y = new_cases))
  if (!checkMA) {
    p <- p + geom_line()
  }
  if (moving_average != "Keiner") {
    p <-
      p + geom_ma(
        data = world_data,
        mapping = aes(x = date, y = new_cases),
        ma_fun = SMA,
        n = n
      )
  }
  p <- p + xlim(min(world_data$date), max(world_data$date)) + ylim(0, max(world_data$new_cases)) +xlab("Datum") + ylab("Neue Fälle")
  p + theme_bw()
}

plot_world_totalcases = function(world_data, moving_average, checkMA) {
  n <- 1
  if (moving_average == "5 Tage") {
    n <- 5
  }
  if (moving_average == "7 Tage") {
    n <- 7
  }
  if (moving_average == "20 Tage") {
    n <- 20
  }
  
  p <-
    ggplot(world_data,
           aes(x = date,
               y = total_cases / population * 100))
  if (!checkMA) {
    p <- p + geom_line()
  }
  if (moving_average != "Keiner") {
    p <-
      p + geom_ma(
        data = world_data,
        mapping = aes(x = date, y = total_cases / population * 100),
        ma_fun = SMA,
        n = n
      )
  }
  p <- p + xlim(min(world_data$date), max(world_data$date)) + ylim(0, max(world_data$total_cases / world_data$population * 100)) +xlab("Datum") + ylab("Gesamte Fälle [% Population]")
  p + theme_bw()
}

plot_world_newdeaths = function(world_data, moving_average, checkMA) {
  n <- 1
  if (moving_average == "5 Tage") {
    n <- 5
  }
  if (moving_average == "7 Tage") {
    n <- 7
  }
  if (moving_average == "20 Tage") {
    n <- 20
  }
  
  p <-
    ggplot(world_data,
           aes(x = date,
               y = new_deaths))
  if (!checkMA) {
    p <- p + geom_line()
  }
  if (moving_average != "Keiner") {
    p <-
      p + geom_ma(
        data = world_data,
        mapping = aes(x = date, y = new_deaths),
        ma_fun = SMA,
        n = n
      )
  }
  p <- p + xlim(min(world_data$date), max(world_data$date)) + ylim(0, max(world_data$new_deaths)) +xlab("Datum") + ylab("Neue Todesfälle")
  p + theme_bw()
}

plot_world_totaldeaths = function(world_data, moving_average, checkMA) {
  n <- 1
  if (moving_average == "5 Tage") {
    n <- 5
  }
  if (moving_average == "7 Tage") {
    n <- 7
  }
  if (moving_average == "20 Tage") {
    n <- 20
  }
  
  p <-
    ggplot(world_data,
           aes(x = date,
               y = total_deaths / population * 1000))
  if (!checkMA) {
    p <- p + geom_line()
  }
  if (moving_average != "Keiner") {
    p <-
      p + geom_ma(
        data = world_data,
        mapping = aes(x = date, y = total_deaths / population * 1000),
        ma_fun = SMA,
        n = n
      )
  }
  p <- p + xlim(min(world_data$date), max(world_data$date)) + ylim(0, max(world_data$total_deaths / world_data$population * 1000)) +xlab("Datum") + ylab("Gesamte Todesfälle [‰ Population]")
  p + theme_bw()
}

plot_world_r = function(world_data, moving_average, checkMA) {
  n <- 1
  if (moving_average == "5 Tage") {
    n <- 5
  }
  if (moving_average == "7 Tage") {
    n <- 7
  }
  if (moving_average == "20 Tage") {
    n <- 20
  }
  
  p <-
    ggplot(world_data,
           aes(x = date,
               y = reproduction_rate))
  if (!checkMA) {
    p <- p + geom_line()
  }
  if (moving_average != "Keiner") {
    p <-
      p + geom_ma(
        data = world_data,
        mapping = aes(x = date, y = reproduction_rate),
        ma_fun = SMA,
        n = n
      )
  }
  p <- p + xlim(min(world_data$date), max(world_data$date)) + ylim(0, max(world_data$new_tests_per_thousand)) +xlab("Datum") + ylab("Reproduktionszahl")
  p + theme_bw()
}

plot_world_vac = function(world_data, moving_average, checkMA) {
  n <- 1
  if (moving_average == "5 Tage") {
    n <- 5
  }
  if (moving_average == "7 Tage") {
    n <- 7
  }
  if (moving_average == "20 Tage") {
    n <- 20
  }
  
  p <-
    ggplot(world_data,
           aes(x = date,
               y = people_vaccinated / population * 100))
  if (!checkMA) {
    p <- p + geom_line()
  }
  if (moving_average != "Keiner") {
    p <-
      p + geom_ma(
        data = world_data,
        mapping = aes(x = date, y = people_vaccinated / population * 100),
        ma_fun = SMA,
        n = n
      )
  }
  p <- p + xlim(min(world_data$date), max(world_data$date)) + ylim(0, 100) +xlab("Datum") + ylab("Impfquote [%]")
  p + theme_bw()
}


########################################
### Prognose
########################################


wachstumsfaktor_linear = function(data) {
  ggplot() + geom_line(data = data, aes(x = date, y = new_cases)) + xlab("Datum") + ylab("Wachstumsfaktor") + theme_bw()
}


prognose_linear = function(data, prognosedauer, herdenimmunitaet) {
  Summand = c()
  for (i in 0:length(data$total_cases)) {
    Summand = c(Summand, data$total_cases[i + 1] - data$total_cases[i])
  }
  
  Prognose = c(data$total_cases[length(data$total_cases)])
  AvSummand = mean(Summand[length(Summand) - 30:length(Summand)])
  
  if (prognosedauer == "Bis Herdenimmunitaet") {
    sechzig = herdenimmunitaet / 100 * data$population[1]
    while (sechzig >= Prognose[length(Prognose)]) {
      Prognose = c(Prognose, Prognose[length(Prognose)] + AvSummand)
    }
  }
  else {
    n = 10
    if (prognosedauer == "100 Tage") {
      n = 100
    }
    if (prognosedauer == "1000 Tage") {
      n = 1000
    }
    
    for (i in 1:n) {
      Prognose = c(Prognose, Prognose[length(Prognose)] + AvSummand)
    }
  }
  
  Start = data$date[1]
  Tage = length(Prognose) + length(data$date)
  Datum = seq(from = Start,
              by = "day",
              length.out = Tage)
  
  Totalinfektionen = c(data$total_cases, Prognose)
  pop = data$population[1]
  Prognosendaten = data.frame(x = Datum, y = Totalinfektionen / pop * 100)
  
  ggplot(data = Prognosendaten, aes(x = x, y = y)) + geom_line()  + xlab("Datum") + ylab("Gesamte Fälle [% Population]") + theme_bw()
  
}

wachstumsfaktor_exp = function(data) {
  Faktor = c()
  for (i in 0:length(data$total_cases)) {
    Faktor = c(Faktor, data$total_cases[i + 1] / data$total_cases[i])
  }
  
  ggplot(data = data) +
    geom_line(aes(date, Faktor)) +
    xlab("Datum") + ylab("Wachstumsfaktor") +
    ylim(1, 1.1) + theme_bw()
}

prognose_exp = function(data, prognosedauer, herdenimmunitaet) {
  Faktor = c()
  for (i in 0:length(data$total_cases)) {
    Faktor = c(Faktor, data$total_cases[i + 1] / data$total_cases[i])
  }
  
  Prognose = c(data$total_cases[length(data$total_cases)])
  AvFaktor = median(Faktor[length(Faktor) - 30:length(Faktor)])
  
  if (prognosedauer == "Bis Herdenimmunitaet") {
    sechzig = herdenimmunitaet / 100 * data$population[1]
    while (sechzig >= Prognose[length(Prognose)]) {
      Prognose = c(Prognose, Prognose[length(Prognose)] * AvFaktor)
    }
  }
  else {
    n = 10
    if (prognosedauer == "100 Tage") {
      n = 100
    }
    if (prognosedauer == "1000 Tage") {
      n = 1000
    }
    
    for (i in 1:n) {
      Prognose = c(Prognose, Prognose[length(Prognose)] * AvFaktor)
    }
  }
  
  
  Start = data$date[1]
  Tage = length(Prognose) + length(data$date)
  Datum = seq(from = Start,
              by = "day",
              length.out = Tage)
  
  Totalinfektionen = c(data$total_cases, Prognose)
  
  Prognosendaten = data.frame(x = Datum, y = Totalinfektionen/data$population[1] * 100)
  
  p <-
    ggplot(data = Prognosendaten, aes(x = x, y = y)) + geom_line() + xlab("Datum") + ylab("Gesamte Fälle [% Population]")
  p + theme_bw()
}
