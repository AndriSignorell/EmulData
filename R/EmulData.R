

# ToDo:
#   
# rinder 
# cheese
# oeko


# Schneehöhe ****************************************

schnee <- function(n=507) {
  
  # set.seed(81)
  # schnee()
  
  d.set <- data.frame(proben_id=sample(1000:9999, n),
                      meereshöhe = RoundTo(abs(rnorm(n, 1200, 350))), 
                      hanglage= relevel(factor(sample(c("nord","süd", "west/ost"), n, 
                                                      replace=TRUE,
                                                      prob = c(0.4,.3, .3))), ref = "west/ost"),
                      kanton= factor(kt <- sample(c("VS","BE","GR","UR","TI"), n, 
                                                  replace=TRUE, 
                                                  prob=c(0.23, 0.2, .37, .14, 0.06))),
                      schneehöhe = 1)
  
  d.set$schneehöhe <- abs(round(10 + 40*d.set$meereshöhe + (d.set$hanglage=="nord")*15000 +
                                  + (d.set$hanglage=="süd")*(-8000) + (d.set$kanton == "GR") * 0.8
                                + rnorm(nrow(d.set), mean=0, sd=5000), -1))/200
  
  Labels(d.set) <- c("ID der Schneeprobe","Höhe des Messpunkts über Meer [m]",
                     "Ausrichtung des Hangs",
                     "Kanton","Maximale gemessene Schneehöhe während der Messperiode (in [cm])")
  
  
  Label(d.set) <- "Die Schneehöhe in den Bergen hängt massgeblich von der Meereshöhe ab. 
                   Weiter spielen auch die Ausrichtung des Hangs eine Rolle, an Nordhängen 
                   entsteht typischerweise eine dickere Schneedecke.
                   Dieser Zusammenhang soll anhand eines dafür erhobenen Datensatzes genauer 
                   untersucht werden."
  
  return(d.set)
  
} 




# Alzheimer ******************************

alzheimer <- function(n=554) {
  
  # alzheimer()
  
  # create dataset
  d.set <- data.frame(id          = sample(1000:9999, n), 
                      geschlecht  = sample(c("m","w"), n, replace=TRUE, prob=c(0.37, .73)),
                      alter       = sample(58:87, n, replace=TRUE),
                      gruppe      = (g <- sample(c("case","control"), n, 
                                                 replace=TRUE, 
                                                 prob=c(0.45, .55))),
                      items       = rpois(n, lambda = c("case"=12,"control"=4)[g])) 
  
  Labels(d.set) <- c("Proband", "Geschlecht", "Alter", 
                     "Studiengruppe (Case/Control)", "Anzahl erinnerte Begriffe im Gedächtnistest")
  
  Label(d.set) <-  "In einer Alzheimerstudie sollen die Erinnerungsleistungen von 
                    Alzheimer-Patienten (Gruppe <em>case</em>) mit jenen von gesunden 
                    Kontrollen (Gruppe <em>control</em>) verglichen werden. Den Probanden 
                    werden hierfür 30 Gegenstände vorgelegt, die sie hinterher auswendig 
                    niederzuschreiben haben."
  
  return(d.set)
  
}




rinder <- function(n=554) {
  
  # Schlachtalter:  18-24 Monate
  # Schlachtgewicht: 334 kg
  
  d.set <- .PackageData("rinder.xlsx")
  
  d.set$alter <- round(rnorm(nrow(d.set), mean = 21, sd=2), 0)
  d.set$gewicht <- round(c(0,-23,-30)[Nf(d.set$rasse)] + 
                           + 0.4* d.set$alter + 
                           + c(0,+30,+15)[Nf(d.set$futter)] + 
                           + c(0,-23)[Nf(d.set$sömmerung)] + 
                           + 290 + rnorm(nrow(d.set), 0, 5), 0)
  d.set$rasse <- factor(d.set$rasse)
  d.set$futter <- factor(d.set$futter)
  d.set$sömmerung <- factor(d.set$sömmerung)
  
  Labels(d.set) <- c("Schlachtgewicht des Rinds in [kg]", "Alter des Rinds [in Monaten]", 
                     "Rasse des Rinds", "Futtertyp", "Lage der Sömmerung")

  Label(d.set) <- "
          Für die Fleischproduktion werden typischerweise diverse Rinderrassen verwendet, 
          die mit 
          verschiedenenartigem Futter ernährt werden. Sowohl die Rasse, das 
          Futter, die Art der
          Sömmerung (wo sich die Tiere im Sommer aufhalten) aber auch 
          das Alter der Rinder haben einen Einfluss auf das Schlachtgewicht.  
          Der Datensatz&nbsp;&nbsp;<strong>&link&</strong>&nbsp;&nbsp;enthält 
          Daten, die bei einem
          Fleischproduzenten in einem Produktionszyklus erfasst wurden."
                           
  return(d.set)
                           
          
}






# Miete *******************************************

miete <- function() {
  
  # miete()
  
  d.set <- .PackageData("miete.xlsx")
  
  Labels(d.set) <- c("Monatsmiete in [EUR]", "Fläche der Wohnung in [m<sup>2</sup>]", 
                     "Bad frisch renoviert (j/n)", "Zentralheizung (j/n)","Ausbaustandard der Küche",
                     "Mietvertragsdauer", "Baujahr kategorisiert", "Wohngegend")
  
  Label(d.set) <- "Dieser Datensatz enthält einen repräsentativen Auszug 
                   aus Daten, die anlässlich der Erstellung 
                   eines Mietspiegels in einer grösseren europäischen Stadt erhoben wurden. Ziel 
                   eines Mietspiegels ist die Bestimmung der sogenannten ortsüblichen Miete, deren 
                   Höhe in der Regel von Ausstattungs- und Lagemerkmalen der Mietwohnung 
                   abhängt."
  
  
  return(d.set)
  
}




# Kalorien *******************************************

kalorien <- function() {
  
  # kalorien()
  
  d.set <- .PackageData("kalorien.xlsx")
  
  Labels(d.set) <- c("Studien Gruppe", "Anzahl aufgenommene Kalorien")
  
  Label(d.set) <-  "In einer Studie soll untersucht werden, ob Personen mit fleischloser Ernährung 
                    am Tag weniger Kalorien zu sich nehmen als Personen, die Fleisch konsumieren. 
                    Dabei wird angenommen, dass die tägliche Kalorienmenge normalverteilt ist und dass 
                    die Varianz bei beiden Gruppen übereinstimmt."

  return(d.set)
  
}





# Schokolade **********************************************************

choco <- function(n=300) {
  
  d.set <- data.frame(id=sample(1000:9999, n), 
                      geschlecht=sample(c("m","w"), n, replace=TRUE, prob=c(0.37, .73)),
                      alter = sample(18:75, n, replace=TRUE),
                      region= region <- sample(c("de","fr","it"), n, replace=TRUE, 
                                               prob=c(0.14, 0.24, .62)),
                      menge=menge <- rpois(n, lambda = c("de"=1.8,"it"=3,"fr"=2.2)[region]),
                      mengekat= cut(menge, breaks=c(0,1,3,5, Inf), right=FALSE, 
                                    labels = c("0", "1-2", "3-4", "5.."))
                      )
  
  Labels(d.set) <- c("ID", "Geschlecht", "Alter", 
                     "Wohnregion", "Bezogene Packungen Schokolade", 
                     "Mengen-Kategorie")
  
    Label(d.set) <- "
          Ein Schweizer Detailhändler gibt eine Umfrage in Auftrag. Es soll 
          herausgefunden werden, ob die Absatzmenge einer bestimmten Schokoladensorte 
          durch die regional unterschiedlichen ökonomischen, sozialen und 
          kulturellen Gegebenheiten beeinflusst wird. Dazu wurden Personen 
          in verschiedenen Sprachregionen der Schweiz gefragt, wie viele 
          Packungen der betreffenden Sorte sie pro Monat kaufen."
  
    
    # Die entsprechende Variable <strong>mengekat</strong> hat vier 
    # Ausprägungen <em>"0", "1-2", "3-4"</em> und <em>"5.."</em> (letzere 
    # 5 oder mehr Packungen). 
  
  
  return(d.set)
  
}



# Tierheim  ******************************************

tierklinik <- function(n=150) {
  
  d.set <- data.frame(id=sample(1000:9999, n), 
                      alter = sample(1:8, n, replace=TRUE),
                      # produce associated ordinal variables
                      RndPairs(n = n, r=0.65, prop = list(c(.3, .15, .35, .20), 
                                                          c(.35, .45, .2))))
  
  levels(d.set$x) <- c("Deutscher Schäferhund", "Terrier", 
                       "Rauhaardackel", "Dalmatiner")
  levels(d.set$y) <- c("Trockenfutter", "Dosenfutter", "Mischung")
  
  colnames(d.set)[3:4] <- c("rasse", "futter")
  Labels(d.set) <- c(
    "Identifikationsnummer",
    "Alter",
    "Hunderasse",
    "Benötigtes Futter")    
  
  Label(d.set) <- gettextf("Eine Tierklinik ist auf die tierärztliche Versorgung der vier Hunderassen 
            %s spezialisiert. Die Tiere erhalten entweder Trockenfutter, 
            Dosenfutter oder eine Mischung von beidem, je nachdem was die 
            Besitzer verwenden. Für die Beschaffung des Futters will die Klinik prüfen, 
            ob der Futtertyp von der Hunderasse abhängt. 
            Wäre dies nicht der Fall, kann die Klinik das Futter nur auf der Grundlage 
            der Gesamtzahl der Hunde bestellen, ohne die Rassen zu berücksichtigen.
            Die Klinik sammelte über eine gewisse Zeit die Angaben zum Hund und zum Futter.
            ", paste(gettextf("<em>%s</em>", levels(d.set$rasse)), collapse=", "))
  
  return(d.set)
  
}




# Social Media

socialmedia <- function(n = 1224){
  
  d.set <- data.frame(id=sample(1000:9999, n), 
                      region=sample(c("land","stadt"), n, replace=TRUE, prob=c(0.37, .73)),
                      alter = sample(16:60, n, replace=TRUE),
                      # produce associated ordinal variables
                      RndPairs(n = n, r=0.8, prop = list(c(.3, .15, .3, .25), 
                                                         c(.45, .55))))
  levels(d.set$x) <- c("FaceBook", "LinkedIn", "WhatsApp", "Instagram")
  levels(d.set$y) <- c("m","w")
  colnames(d.set)[4:5] <- c("app", "geschlecht")
  d.set <- d.set[,c("id","geschlecht","alter","region","app")]
  
  Labels(d.set) <- c(
    "Identifikationsnummer",
    "Geschlecht der Person",
    "Alter",
    "Wohnregion (stadt/land)",
    "Am häufigsten verwendete Social Media Applikation")    
  
  
  Label(d.set) <- "Der Datensatz&nbsp;&nbsp;<strong>&link&</strong>&nbsp;&nbsp;
        enthält die Ergebnisse einer Studie, 
        die untersuchen sollte, welche Social Media Applikationen von 
        unterschiedlichen Zielgruppen 
        genutzt werden."
  
  return(d.set)
  
}




# Smoke *************************************

smoke <- function(vars=c("educ","cigpric","white","age","income","cigs","restaurn","cigcat")) {
  
  d.set <- .PackageData("smoke.xlsx")
  
  d.set$pers_id <- sample(1000:9999, nrow(d.set))
  
  Labels(d.set) <- c("Anzahl Schuljahre",
                     "Durchschnittlicher Preis pro Packung",
                     "Hautfarbe weiss (1=ja, 0=nein)",
                     "Alter der Person",
                     "Einkommen in [$/Jahr]",
                     "Anzahl gerauchte Zigaretten pro Tag",
                     "Gilt ein Rauchverbot in Restaurants im Staat (1=ja, 0=nein)",
                     "Anzahl gerauchte Zigaretten pro Tag (kategorisiert)",
                     "ID der Person")
  
  d.set <- d.set[, vars]
  
  Label(d.set) <- "
        Der Datensatz&nbsp;&nbsp;<strong>&link&</strong>&nbsp;enthält Daten zu einer Raucher-Studie, die in 
        den Vereinigten Staaten durchgeführt wurde. In dieser Studie interessierte man sich dafür, 
        welche Effekte die Anzahl pro Tag gerauchter Zigaretten beeinflussen. "
  
  return(d.set)
  
}



# Eier ******************************************************


eier <- function(vars=c("gewicht","alter","rasse","futter","farbe")){
  
  d.set <- .PackageData("eier.xlsx", stringsAsFactors = TRUE)
  
  
  Labels(d.set) <- c("Gewicht des Eis in [g]", "Alter des Huhns in Tagen", 
                     "Rasse des Huhns", "Futtertyp", "Farbton des Eigelbs")
  
  d.set <- d.set[, vars]
  
  Label(d.set) <- gettextf('
            Für die Produktion von Eiern werden typischerweise diverse Huhnrassen 
            verwendet, die mit 
            verschiedenenartigem Futter ernährt werden. Sowohl die Rasse, 
            das Futter, aber auch das Alter 
            der Hühner haben einen Einfluss auf das Gewicht und die Qualität 
            der Eier. Eier mit 
            dunkelgelbem Dotter werden von den Konsumenten gemeinhin als 
            irgendwie "gesünder" wahrgenommen. 
            Der Datensatz&nbsp;&nbsp;<strong>&link&</strong>&nbsp;&nbsp;enthält 
            Daten, die bei einem
            Eierproduzenten in einem Produktionszyklus erfasst wurden.')
  
  return(d.set)
  
}



# Gämsen

gams <- function() {
  
  d.set <- .PackageData("gemsen.xlsx", stringsAsFactors = TRUE)
  
  d.set <- na.omit(d.set)
  d.set$banngebiet <- relevel(factor(d.set$ref, labels=c("ja", "nein")), ref="nein")
  d.set <- d.set[, c("jahr","geschlecht","alter","gewicht","krucken","banngebiet")]
  
  Labels(d.set) <- c("Abschussjahr",
                     "Geschlecht des Tieres",
                     "Alter des erlegten Tieres",
                     "Gewicht des erlegten Tieres in [kg]",
                     "Kruckenlänge (Hornlänge) in [mm]",
                     "Abschussgebiet ist Jagdbanngebiet"
  )
  
  Label(d.set) <- gettextf("          
                   Zur Beurteilung des Zustands der Gamspopulation wurden im Jagdinspektorat 
                   die in der regulären Jagd erlegten Gämsen vermessen.")
  
  return(d.set)
  
  
}


# Hotdog
hotdog <- function(){
  
  # get base table
  m <- SetNames(rbind("männlich" = c(53,46,15), 
                      "weiblich" = c(37,72,14)
  ),
  colnames=c("Senf", "Ketchup","Mayonnaise")) 
  names(dimnames(m)) <- c("Geschlecht", "Sauce")
  
  d.set <- data.frame(id=seq(sum(m)), Untable(m))
  
  Label(d.set) <- Mgsub(c("&level_x&", "&level_y&"),
                c(paste(gettextf("<em>%s</em>", gsub("&nbsp;","", rownames(m))), collapse=", "),
                  paste(gettextf("<em>%s</em>", colnames(m)), collapse=", ")),
                "Die Betreiberin einer Würstchenbude möchte feststellen, ob es 
           einen Zusammenang zwischen dem Geschlecht ihrer Kundschaft (&level_x&) 
           und der bevorzugten 
           Sauce (&level_y&) gibt. Sie führt über eine Woche lang detailliert Buch über 
           ihre Verkäufe."
  ) 
  
  return(d.set)
  
}



# Vegetarische Ernährung
vegi <- function(){
  
  # get base table
  m <- SetNames(rbind("Migros" = c(196,22,5), 
                      "Coop" = c(202,30,11),
                      "Lidl" = c(153,13,2),
                      "Aldi" = c(140,9,2)
  ),
  colnames=c("Fleisch", "Vegetarisch", "Vegan")) 
  names(dimnames(m)) <- c("Grossverteiler", "Ernährung")
  
  d.set <- data.frame(id=seq(sum(m)), Untable(m))
  
  Label(d.set) <- Mgsub(c("&level_x&", "&level_y&"),
                        c(paste(gettextf("<em>%s</em>", gsub("&nbsp;","", rownames(m))), collapse=", "),
                          paste(gettextf("<em>%s</em>", colnames(m)), collapse=", ")),
                        "Anhand von Einkaufsbelegen verschiedener Grossverteiler haben 
                         Forschende untersucht, wie viel 
                         Fleisch Schweizer Haushalte konsumieren. Es interessiert, ob es 
                         sich die Kundschaft der Grossverteiler sich in ihren 
                         Ernährungsgewohnheiten unterscheidet."
                         ) 
  
  return(d.set)
  
}




lohn <- function() {
  
  # use for: Klasseneinteilung, LinReg
  d.set <- .PackageData("lohn.xlsx")

  Label(d.set) <- " Der Datensatz&nbsp;&nbsp;&nbsp;<strong>&link&</strong>&nbsp;&nbsp;&nbsp;enthält
                    Lohndaten, die in einer Stichprobe im Raum Edinburgh (Schottland) erhoben wurden.
                    <br>
                    Die Variable&nbsp;<em>stdlohn</em> gibt den Stundenlohn für den jeweiligen 
                    Arbeitnehmer/jeweilige Arbeitnehmerin an."
  
  return(d.set)
  
}

# lohn()


# Aeschen
aeschen <- function(mu =c(1, 0.8), s=c(0.19, 0.22)){
  
  # get base dataset
  d.set <- TwoSamp(n=c(101, 123), colnames=c("gewicht", "jahr"),
                   glevels=c("2023","2024"),
                   DIST=c(function(n) round(rnorm(n, mean=mu[1], sd=s[1]), 3),
                          function(n) round(rnorm(n, mean=mu[2], sd=s[2]), 3)))[, c(2,1)]
  d.set$jahr <- N(d.set$jahr)

  Label(d.set) <- "Fischer im Bodensee äusserten den Verdacht, dass das Gewicht der Aeschen 
      im 2024 deutlich kleiner sei, als im vorherigen Jahr (2023). Dieses war speziell
      warm gewesen und man vermutete, dass vor allem ältere (und schwerere) Fische besonders unter 
      dem in wärmerem Wasser verringerten Sauerstoffgehalt gelitten hätten und allenfalls vorzeitig 
      eingegangen sein könnten.<br>
      Der Datensatz&nbsp;&nbsp;&nbsp;<strong>&link&</strong>&nbsp;&nbsp;&nbsp;enthält
      das Gewicht der Fische für je eine Stichprobe aus den Jahren 2023 und 2024."

  return(d.set)
  
}


# Lineare Regression - Bluthochdruck 

blood <- function() {
  
  d.set <- rbind(
    data.frame(age=(a <- sample(20:70, 187, replace=TRUE)),
               sex="m", 
               bmi=(b <- round(rnorm(187, 25, 5), 1)),
               systol=a * 0.5 + 115 + (b-25) * 0.3 + rnorm(length(a), mean = 0, sd=7)),
    
    data.frame(age=(a <- sample(20:70, 157, replace=TRUE)),
               sex="f",
               bmi=(b <- round(rnorm(157, 25, 5), 1)),
               systol=a * 0.7 + 110  + (b-25) * 0.2 + rnorm(length(a), mean = 0, sd=5)))
  
  d.set$systol <- round(d.set$systol, 1)
  d.set <- data.frame(id=seq(nrow(d.set)), DescTools::Sample(d.set))
  rownames(d.set) <- NULL
  
  Label(d.set) <- "Mit zunehmendem Alter nimmt der Blutdruck im Allgemeinen zu. 
                   In einer Studie wurde für 
                   eine Stichprobe mit Probanden beider Geschlechter der 
                   systolische Blutdruck in [mmHg] gemessen."
  
  return(d.set)
  
  # Berechnen Sie eine lineare Regression.
  # Wie gross ist die mittlere Zunahme pro Lebensjahr? 
  # Ist der Blutdruck von Frauen und Männern unterschiedlich? 
  # Ist die Zunahme für Frauen und Männer unterschiedlich. 
  #   
  # 1 mmHg = 1.33322 mbar
  # v <- "A"
  
}  

# lineare Regression

dose <- function(){
  
  d.set <- data.frame(proben_id=sample(1000:9999, n <- 507),
                      wartezeit = RoundTo(abs(rnorm(n, 12, 4))), 
                      lage= relevel(factor(sample(c("aufrecht","kopfüber"), n, r=T,
                                                  prob = c(0.5,.5))), ref = "aufrecht"),
                      reinigung= relevel(factor(sample(c("ja","nein"), n, r=T,
                                                       prob = c(0.5,.5))), ref = "ja"),
                      dosis = 1)
  
  d.set$dosis <- abs(round(10 + 4*d.set$wartezeit + (d.set$lage=="aufrecht")*500 +
                             + (d.set$reinigung == "ja") * 0.8
                           + rnorm(nrow(d.set), mean=0, sd=500), -1))/2000
  Labels(d.set) <- c("ID der Probe","Wartezeit in [s] zwischen 2 Inhalationen",
                     "Ausrichtung der Sprühdose",
                     "Reinigungsstoss vor der Benutzung", "Abgegebene Dosis in [mg/l]")
  
  Label(d.set) <- 'Eine Sprühdose (sog. Inhalator) mit dem Wirkstoff "Fenoterol", 
                   ein Medikament für Asthmatiker, sollte bei Betätigung des Dosierventils
                   möglichst gleichmässige Dosen abgeben. 
                   Die Stellung der Dose und der zeitliche Abstand zwischen zwei Inhalationen 
                   dürfen aus Sicherheitsgründen keine Rolle spielen. In einer Kontrolle 
                   wurde eine Stichprobe solcher Inhalatoren von einem Analyseinstitut daraufhin 
                   geprüft, ob die abgegebene Dosis diesen Anforderungen entsprach.'
  
  return(d.set)

}



schulnote <- function(){
  
    d.dat <- within(
      data.frame(
        bildschirmzeit      = round(runif(n, min = 2, max = 8), 2),
        schlafdauer         = round(runif(n, min = 5, max = 9), 2),
        freizeitaktivitäten = ordered(sample(1:3, size = n, replace = TRUE),
                                      labels=c("selten", "gelegentlich", "häufig"))
      ),
      note <- Winsorize(
        round(2 + -0.3*bildschirmzeit + 0.4*schlafdauer + 
                0.2* N(freizeitaktivitäten) + rnorm(n, mean = 0, sd = 0.5), 
              2), val = c(1,6))
    )
    
    Labels(d.dat) <- c("Bildschirmzeit [Stunden pro Tag]",
                       "Schlafdauer [Stunden pro Nacht]",
                       "Häufigkeit von Freizeitaktivitäten","Schulische Leistung")
    
    
    Label(d.dat) <- "
    Ein Sozialwissenschaftler möchte untersuchen, wie verschiedene Faktoren die 
    schulischen Leistungen von Jugendlichen beeinflussen.
    Als Zielvariable Y dienen die schulischen Leistungen, 
    gemessen als Durchschnittsnote auf einer Skala von 1 (sehr schwach) bis 6 (sehr gut).
    Erklärende Variablen sind:<br><br>&vartab&<br><br>
      
    Es wurde eine Stichprobe von &n& Jugendlichen untersucht
    (Datensatz&nbsp;&nbsp;&nbsp;<strong>&link&</strong>&nbsp;&nbsp;&nbsp;). 
    "
    
    # Führen Sie eine multiple lineare Regression durch, um den Einfluss dieser Variablen 
    # auf die schulischen Leistungen zu analysieren.
    
    return(d.dat)

}



einkauf <- function(n = 427){
  
  d.dat <- within(
    data.frame(
      filiale  = factor(sample(c("Ost", "Nordwest", "Süd"), 
                               size = n, replace = TRUE)), 
      wagen    = factor(sample(c("L", "XL"), 
                               size = n, replace = TRUE))
    ),
      
    einkauf <- 
      DescTools::RoundTo((rpois(n, 
                    lambda = c(7, 9, 14)[DescTools::N(filiale)]*4 + 
                      c(10, 15)[DescTools::N(wagen)]) + runif(n)), 0.05)
  )
  
  Labels(d.dat) <- c("Filiale",
                     "Einkaufswagengrösse",
                     "Einkaufsbetrag [in CHF]")
  
  Label(d.dat) <- gettextf("
  Ein Detailhändler möchte untersuchen, ob unterschiedliche Grössen der Einkaufswagen
  die Höhe des Einkaufsbetrags beeinflussen. Einer Hypothese nach könnten grosse 
  Einkaufswagen die Kunden animieren, mehr einzukaufen.<br>
  Für die Prüfung der Hypothese wurden Daten von %s Einkäufen in drei Filialen <em>Ost</em>, 
  <em>Nordwest</em>
  und <em>Süd</em> jeweils während 6 Wochen mit normalen Einkaufswagen (<em>L</em>) erfasst, und 6 Wochen mit 
  grossen Einkaufswagen (<em>XL</em>).<br>", n)

  return(d.dat)
  
}



kredit <- function(n){
  
  d.dat <- within(
    data.frame(
      finanzwissen   = round(runif(n, min = 1, max = 10), 0), 
      einkommen      = round(runif(n, min = 40, max = 150), 0),  
      beschäftigung  = factor(sample(c("teilzeit", "vollzeit"), 
                                     size = n, replace = TRUE)) 
    ),
    kredit <- 
      round(10 + 5 * finanzwissen + 0.8 * einkommen + 
              15 * N(beschäftigung) + rnorm(n, mean = 0, sd = 5), 
            0)
  )
  
  Labels(d.dat) <- c("Finanzwissen [numerisch, auf einer Skala von 1 bis 10]",
                     "Jahreseinkommen [in Tausend CHF]",
                     "Beschäftigungsstatus",
                     "Kredithöhe")

  Label(d.dat) <- gettextf("
  Eine Bank möchte untersuchen, welche Faktoren die Höhe eines gewährten Kredits 
  beeinflussen. Dazu wurden Daten von %s Kunden erfasst. 
  Die Zielvariable y ist die Höhe des gewährten Kredits (in Tausend CHF). <br>
  Folgende erklärende Variablen wurden im Datensatz&nbsp;&nbsp;&nbsp;<strong>&link&</strong>&nbsp;&nbsp;&nbsp;
  erfasst: <br><br> &vartab&", n)
  
  # Führen Sie eine multiple lineare Regression durch, um den Einfluss dieser Variablen 
  # auf die schulischen Leistungen zu analysieren.
  
  return(d.dat)
  
}




# Retouren 
retouren <- function(n=1248, p=0.016, name_x){
  
  hersteller <- function(n, p, name_x){
    
    data.frame(
      garantiefall = sample(c("nein", "ja"), size = n, replace = TRUE, 
                     prob = c(1-p, p)),
      hersteller = name_x,
      produkt    = sample(c("TV", "DVD-Player", "Beamer", "HiFi"), 
                            size = n, replace = TRUE)
      
    )
  }
  
  
  d.ctr <- data.frame(name_x = c("LG", "Samsung", "Sony","Panasonic"),
                      p = c(0.014, 0.015, 0.018, 0.02)*4,
                      n = round(c(.18, .33, .39, .10)*n))
  

  d.dat <- do.call(rbind, lapply(seq(nrow(d.ctr)), 
                   function(i) with(d.ctr[i, ], 
                                    hersteller(n, p, name_x)))  )

  d.dat$verkaufsnr <- round(runif(n=n)*1000+1000)
  
  Label(d.dat) <- "
    Ein Onlineanbieter elektronischer Geräte zählt die Garantiefälle einer Marke innerhalb 
    einer bestimmten Kategorie (Bsp. «TV»), die im After Sales abgewickelt werden müssen.
    So soll die Wahrscheinlichkeit für einen Garantiedefekt während der gesetzlichen 
    Garantiezeit transparent gemacht werden und auch der Vergleich zwischen  
    unterschiedlichen Herstellern ermöglicht werden. Ein Hersteller, der eventuell 
    mehr auf eine nachhaltige Bauweise setzt, ist so leichter zu erkennen.
    
    Es liegen die Verkaufsdaten zweier Jahre vor: 
    &nbsp;&nbsp;&nbsp;<strong>&link&</strong>&nbsp;&nbsp;&nbsp;. 
    "

  return(Sample(d.dat[, c(4,1:3)]))
  
}



birthweight <- function() {

  d.set <- .PackageData("birthweight.xlsx")

  Labels(d.set) <- c("Personen-ID",
                     "Familieneinkommen [1'000 $]",
                     "Geburtsgewicht [kg]",
                     "Schuljahre Vater [Jahre]",
                     "Schuljahre Mutter [Jahre]",
                     "Geschlecht des Kindes (0=weiblich, 1=männlich)",
                     "Hautfarbe weiss (0=nein, 1=ja)",
                     "Anzahl während der Schwangerschaft pro Tag <br>von der Mutter gerauchter Zigaretten" 
  )
  
  Label(d.set) <- "Der Datensatz&nbsp;&nbsp;<strong>&link&</strong>&nbsp;enthält Daten zu einer Raucher-Studie, die in 
        den Vereinigten Staaten durchgeführt wurde. In dieser Studie interessierte man unter anderem 
        sich dafür, welchen Einfluss die Anzahl pro Tag gerauchter Zigaretten von Schwangeren auf das 
        Geburtsgewicht der Kinder hatten."

  return(d.set)
  
  
}




sports <- function() {
  
  d.set <- .PackageData("sports.xlsx")
  
  Labels(d.set) <- c(
    "Anzahl Schuljahre",
    "Wohnhaft in einer Stadt (1=ja, 0=nein)",
    "Alter der Person",
    "Anzahl Sport-Einheiten pro Woche"
  )
  

  Label(d.set) <- "
    In einer Studie zum Thema 'Sport und Gesundheit' interessierte man 
    sich dafür, welche persönlichen Eigenschaften die Anzahl absolvierter 
    Sporteinheiten beeinflussen.<br>
    Der Datensatz &nbsp;&nbsp;<strong>&link&</strong>&nbsp; 
    enthält die Angaben."
  
  return(d.set)
  
  
}




haushaltschaden <- function(n, mu_small=1000, sd_small=500, 
                            scale=2500, shape=2.5){
  # example:
  
  # haushaltschaden(n=1000, shape=2)
  
  rpareto <- function (n, scale = 1, shape) 
  {
    ans <- scale/runif(n)^(1/shape)
    ans[scale <= 0] <- NaN
    ans[shape <= 0] <- NaN
    ans
  }
  
  small_damages <- abs(rnorm(n, mean = mu_small, sd = sigma_small))
  large_damages <- rpareto(n, scale = scale, shape = shape)
  
  # Kombinierte Verteilung (80% kleine, 20% grosse Schäden)
  weights <- sample(c(TRUE, FALSE), size = n, replace = TRUE, prob = c(0.8, 0.2))
  damages <- ifelse(weights, 
                    sample(small_damages, size = n, replace = TRUE), 
                    sample(large_damages, size = n, replace = TRUE))
  
  Label(damages) <- "Versicherungen verzeichnen häufig eine hohe Frequenz 
               kleiner Schäden und eine seltene, aber hohe Schadenslast durch 
               Extremereignisse. Hausratversicherungen in der Schweiz decken 
               übliche Gefahren wie Diebstahl, Feuer und Wasser ab, wobei 
               kleine Schäden oft einfach reguliert werden, während grosse
               Schäden besondere Versicherungsfälle sind."
  
  return(round(damages, 0))
  
  
}



autismus <- function(noise=FALSE){
  
  # Erstellen der Kontingenztabelle
  dat <- matrix(
    c(241, 198, 164, 215,   # Row: Autism Yes
       20,  25,  27,  44    # Row: Autism No
    ), # Totals
    nrow = 2, byrow = TRUE, 
    dimnames=list( 
      Autismus  = c("Ja", "Nein"),
      Stillzeit = c("0 Monate", "<2 Monate", "2-6 Monate", 
                    ">6 Monate")))
  
  if(noise)
    dat <- round(jitter(dat, amount = 4))
  
  Label(dat) <- "Gibt es einen Zusammenhang zwischen Autismus und Stillen? Um dies 
      festzustellen, wurden Mütter von autistischen und nicht-autistischen 
      Kindern befragt, ob und wenn ja bis zu welchem Zeitpunkt sie ihre Kinder gestillt haben. 
      Die Daten sind in folgender Tabelle dargestellt. Bieten die Daten 
      genügend Hinweise darauf, dass Stillen und Autismus unabhängig 
      voneinander sind?"
  
  return(dat)

}



knochendichte <- function() {
  
  d.set <- EmulData:::.PackageData("knochendichte.xlsx")
  
  Labels(d.set) <- c("Knochendichte [g/cm<sup>3</sup>]",
                     "Vitamin Dosis Gruppe"
                    )

  Label(d.set) <- "
      Vitamin C trägt zu einer normalen Kollagenbildung für eine normale Funktion der 
      Blutgefässe bei.
      Bei Insassen in Pflegeheimen ist die Versorgung besonders wichtig für den 
      Knochenaufbau. In einer Studie wurde eine Gruppe von
      Insassen zufällig je einer von drei Behandlungen zugeteilt, die über 6&nbsp;Wochen unterschiedliche 
      Dosen Vitamin C erhielten. Die Knochendichte in [g/cm<sup>3</sup>] wurde dann abschliessend bestimmt.
      Ein hoher Wert gilt als erstrebenswert."

  return(d.set)
  
  
}


krankenversicherer <- function(){

  d.set <- EmulData:::.PackageData("krankenversicherer.xlsx")
  
  Labels(d.set) <- c("Name der Versicherers",      # Versicherer
                     "CH Marktanteil in [Prozent]",      # Marktanteil
                     "Grössenklasse",              # Typ
                     "Anzahl Versicherte",         # Versicherte
                     "Anzahl versicherte Kinder",  # Kinder
                     "Anzahl versicherte Junge Erwachsene",# Junge Erwachsene 
                     "Anzahl versicherte Erwachsene",# Erwachsene 
                     "Mittlere Zahlungsdauer [in Tagen]" # ZahlDauer
                      )

  Label(d.set) <- "
              Für die Kunden der Krankenkassen kann es ein wichtiges 
              Merkmal sein, wie schnell dass eingereichte Rechnungen vergütet 
              werden. Für das Jahr 2014 veröffentlichte das Bundesamt für Gesundheit 
              eine entsprechende Studie, bei der pro Versicherer die mittlere Anzahl 
              Tage ausgewiesen wurde, die zwischen dem Einreichen der Rechnung und 
              der Auszahlung lag. Vermutet wird, dass die Auszahlungsgeschwindigkeit von
              der Grösse des Versicherers abhängen könnte.<br>
              "
  
  
  return(d.set)

}


bankhr <- function(){
  
  d.set <- EmulData:::.PackageData("bankhr.xlsx")
  
  Labels(d.set) <- c("Name der Bank", 
                     "Anzahl Angestellte",
                     "Mittlere Reaktionsdauer [in Tagen]"
  )
  
  Label(d.set) <- "
              Um die Personalabteilungen von Banken zu vergleichen, wurde in einer 
              experimentellen Studie &n& Banken jeweils 4 Bewerbungen zugestellt und 
              die mittlere Anzahl Tage festgehalten, die zwischen dem Einreichen 
              der Bewerbung und der Reaktion lag. Vermutet wird, dass die 
              Reaktionsgeschwindigkeit von
              der Grösse der Bank (Anzahl Angestellten) abhängen könnte.<br>
              "
  
  return(d.set)
  
}


detailhändler <- function(){
  
  d.set <- EmulData:::.PackageData("detailhändler.xlsx")
  
  Labels(d.set) <- c("Identifikationsnummer des Unternehmens", 
                     "Name des Unternehmens",
                     "geschätzter Jahresumsatz in [Mio CHF]"
  )
  
  Label(d.set) <- "
              Die Liste enthält den Jahresumsatz von &n& auf dem Schweizer Markt 
              aktiven Unternehmen aus dem Detailhandel.<br>
              "
  
  return(d.set)
  
}


comcorp <- function(){
  
  d.set <- EmulData:::.PackageData("comcorp.xlsx")
  
  Labels(d.set) <- c("Identifikationsnummer des Unternehmens", 
                     "Name des Unternehmens",
                     "geschätzter Jahresumsatz in [Mio CHF]",
                     "geschätzter Anzahl Kunden"
  )
  
  Label(d.set) <- "
              Die Liste enthält den Jahresumsatz von &n& auf dem Schweizer Markt 
              aktiven Unternehmen aus dem Detailhandel.<br>
              "
  
  return(d.set)
  
}


skigebiet <- function(){
  
  d.set <- EmulData:::.PackageData("skigebiet.xlsx")
  
  Labels(d.set) <- c("Name des Skigebiets",
                     "Pistenkilometer in [km]",
                     "geschätzter Anzahl Besucher pro Jahr",
                     "geschätzter Jahresumsatz in [Mio CHF]"
  )
  
  Label(d.set) <- "
              Die Liste enthält den die Anzahl Pistenkilometer, die Anzahl 
              Besucher und den geschätzten Jahresumsatz von &n& Schweizer
              Skigebiete.<br>
              "
  
  return(d.set)
  
}



kunden <- function(){

  d.set <- EmulData:::.PackageData("kunden.xlsx")
  
  Labels(d.set) <- c("Identifikationsnummer",     
                     "Alter der Person in [Jahren]",
                     "Geschlecht der Person",
                     "Wohnregion",        
                     "Weiterempfehlungsbereitschaft"
  )
  
  Label(d.set) <- '
        Für ein Möbelgeschäft wurde von einem Institut eine Kundenumfrage durchgeführt. Dabei wurden 
        in einer Stichprobe die Variablen <em>alter, geschlecht, wohnregion</em> erfragt. 
        Danach wurde die Frage:<br><br>
        <em>"Wie wahrscheinlich auf einer Skala von 1 (unwahrscheinlich) - 10 (sicher) 
        ist es, dass Sie uns weiterempfehlen werden?"</em><br><br>
        gestellt. <br>Bewertungen zwischen 1 und 6 werden als <em>"detraktor"</em>, 7 und 8 
        als <em>"passiv"</em> und 9-10 als <em>"promotor"</em> interpretiert.'

  return(d.set)
}



bip <- function(n = 20){
  
  d.set <- EmulData:::.PackageData("bip.xlsx")[1:n, ]
  
  Labels(d.set) <- c("Land",     
                     "Bruttoinlandprodukt 2023"
                    )
  
  Label(d.set) <- gettextf('
        Das Bruttoinlandprodukt (BIP) misst den Gesamtwert aller 
        Waren und Dienstleistungen, die in einem Land in einem bestimmten 
        Zeitraum produziert werden. Der vorliegende Datensatz umfasst das BIP für die
        %s grössten Volkswirtschaften weltweit im Jahr 2023 (in Milliarden US-Dollar).',
        n)
  
  return(d.set)
  
}




operation <- function(){
  
  d.set <- EmulData:::.PackageData("operation.xlsx")
  
  Labels(d.set) <- c("Verspätung vor dem Memo",     
                     "Verspätung nach Versenden des Memo"
                     )
  
  Label(d.set) <- "Der administrativen Leitung eines Spitals war aufgefallen, dass die erste nicht 
      notfallmässige Operation, die täglich angesetzt war, häufig mit Verspätung begann. 
      Wenn sich indes der erste geplante Eingriff verzögerte, verzögerten sich auch 
      alle anderen für diesen Tag geplanten Eingriffe. Über &n& Tage wurde in der Folge
      aufgezeichnet, wie viele Minuten nach der geplanten Zeit die erste
      Operation an jedem Tag begann. 
      <br>Danach wurde ein Memo an das gesamte chirurgische Personal des 
      Krankenhauses versandt, in dem alle Beteiligte aufgefordert wurden, 
      die Verzögerung des Beginns der ersten nicht dringenden Operation 
      pro Tag zu verringern. Eine Woche nach dem Versand des Memos wurde wiederum an &n& Tagen
      geprüft, wie viele Minuten Verzögerung die erste geplante Operation 
      aufwies."

  return(d.set)
  
}



vocabular <- function(){
  
  d.set <- EmulData:::.PackageData("vocabular.xlsx")
  
  Labels(d.set) <- c("Jahr der Prüfung",     
                     "Geschlecht des Prüflings",
                     "Anzahl Schul-/Ausbildungsjahre", 
                     "Ergebnis im Wörtertest"
                     )

  Label(d.set) <- "Der Datensatz&nbsp;&nbsp;<strong>&link&</strong>&nbsp;&nbsp;
                  enthält die Ergebnisse eines Wörter-Tests aus zwei verschiedenen Jahren.
                  Die Lehrer fragen sich, ob es Faktoren bei den Prüflingen gibt, die die 
                  Ergebnisse erklären können. Sie vermuten (oder hoffen zumindest), dass 
                  die Ausbildungszeit einen (positiven) Einfluss hat. In der Frage, ob das 
                  Geschlecht relevant ist, sind sie sich uneins.
                  Zudem wurden in den beiden betrachteten Jahren unterschiedliche 
                  didaktische Konzepte verfolgt, sodass auch dies einen Einfluss auf 
                  die Prüfungsergebnisse gehabt haben könnte."

  return(d.set)
  
}




fitness <- function(n){

  d.set <- within(
    data.frame(
      geschlecht  = factor(sample(c("m", "w"), 
                                  size = n, replace = TRUE)), 
      alter     = round(runif(n, min = 18, max = 40), 0),  
      training  = factor(sample(c("HYP", "INT","FUN"), 
                                     size = n, replace = TRUE)) 
    ),
    zuwachs <- 
        round(( - 0.05 * alter  - 4 * (geschlecht=="w") + 
                30 * N(training)/3 + rnorm(n, mean = 0, sd = 5)), 
              0)
  )
  d.set$kgewicht <- rnorm(nrow(d.set), mean = 70, sd=6.2)
  d.set$kgewicht[d.set$geschlecht == "w"] <- rnorm(length(d.set$kgewicht[d.set$geschlecht == "w"]), 
                                                  mean=65, sd=4.2)
  d.set$kgewicht <- round(d.set$kgewicht, 1)
    
  Labels(d.set) <- c("Geschlecht",     
                     "Alter",
                     "Trainingtyp", 
                     "Leistungszuwachs [in kg]",
                     "Körpergewicht [in kg]"
  )
  
  Label(d.set) <- 'In einem achtwöchigen Experiment soll untersucht werden, 
                  welche von drei Trainingsmethoden den grössten Leistungszuwachs 
                  im Fitnessstudio bewirkt. Dazu nehmen &n& gesunde, untrainierte 
                  Erwachsene im Alter von 18 bis 40 Jahren teil, die per Zufall auf 
                  drei Gruppen aufgeteilt werden. Die erste Gruppe 
                  absolviert klassisches "Hypertrophietraining" (<em>HYP</em>). 
                  Die zweite Gruppe trainiert nach dem "High-Intensity-Training-Prinzip" 
                  (<em>INT</em>) mit einem Satz bis zur Muskelerschöpfung. 
                  Die dritte Gruppe führt "Functional Training" mit instabilen 
                  Übungen wie TRX oder Bosu Ball durch (<em>FUN</em>). Alle Teilnehmenden 
                  trainieren dreimal pro Woche unter Aufsicht von Fitnesstrainern, 
                  um eine korrekte Übungsausführung sicherzustellen. Der 
                  Leistungszuwachs (<em>zuwachs</em>) wird primär anhand der Maximalkraft 
                  (Mittelwert aus Bankdrücken und Beinpresse) gemessen. 
                  Der Datensatz&nbsp;&nbsp;<strong>&link&</strong>&nbsp;&nbsp;
                  enthält die Ergebnisse.'
  
  # Sekundäre Endpunkte sind Veränderungen im Muskelumfang 
  # (Oberarm/Oberschenkel).
  
  return(d.set)
  
}


alpvieh <- function(n=100){
  
  set.seed(42)
  
  # n <- 100
  fuetterung <- factor(sample(c("keine", "heu", "kraftfutter"), n, replace = TRUE),
                       levels=c("keine", "heu", "kraftfutter"))
  alp <- rbinom(n, 1, 0.6)
  startgewicht <- round(rnorm(n, mean = 125, sd = 6), 1)
  alter <- round(rnorm(n, mean = 6, sd = 0.5), 1)
  geschlecht <- factor(c("m","w")[rbinom(n, 1, 0.5)+1], levels=c("m","w"))

  # Regressionsstruktur (wahre Effekte)
  beta0 <- 50
  beta_fuet <- c(keine = 0, Heu = 10, Kraftfutter = 25)
  beta_alp <- 5
  beta_start <- 0.6
  beta_alter <- 2
  
  schlachtgewicht <- round(
      beta0 +
      beta_fuet[fuetterung] +
      beta_alp * alp +
      beta_start * startgewicht +
      beta_alter * alter +
      rnorm(n, 0, 5), 
    1)
  
  d.set <- data.frame(schlachtgewicht, fuetterung, alp, startgewicht, alter, geschlecht)
  
  Labels(d.set) <- c("Schlachtgewicht in [kg]",
                     "die Fütterungsart während der Sömmerung",
                     "1 wenn das Kalb auf einer Alp gesömmert wurde",
                     "das Startgewicht zu Beginn der Sömmerung in [kg]",
                     "das Alter bei Beginn der Sömmerung (Monate)",
                     "das Geschlecht des Kalbes")
  
  Label(d.set) <- as.html("In einer Untersuchung sollte analysiert werden, welche Einflussgrössen 
  das Schlachtgewicht (Zielvariable) in [kg] von Kälbern nach der Sömmerung bestimmen. 
  Die Fütterungsart beschreibt, ob und in welchem Umfang während der Sömmerung 
  zugefüttert wurde. Es wird angenommen, dass zusätzliche Fütterung – insbesondere 
  mit Kraftfutter – zu einem höheren Schlachtgewicht führen könnte. Auch ein 
  Alpaufenthalt könnte sich positiv auswirken, da Kälber dort oft bessere 
  Weidebedingungen vorfinden. Startgewicht und Alter dienen als kontinuierliche Grössen, 
  die das Wachstumspotenzial der Tiere abbilden sollen.<br>
  Folgende potenzielle erklärende Variablen liegen vor 
  (Datensatz&nbsp;&nbsp;<strong>&link&</strong>&nbsp;&nbsp;):
  <br><br> &vartab&<br><br>                          ")

  return(d.set)
}




akku <- function(){
  
  d.set <- EmulData:::.PackageData("akku.xlsx")
  
  Labels(d.set) <- c("Typ des Akkus",     
                     "Anzahl Ladezyklen bis zum Ersatz" 
  )
  
  Label(d.set) <- "
    Ein Batterie-Hersteller bietet 2 Typen von Akkus A und B an und 
    verspricht in der Werbung, dass beide Akkutypen gleich viele Ladezyklen
    vertragen, bevor sie ersetzt werden müssen.
    Eine Konsumentenorganisation will das überprüfen und bildet eine Stichprobe
    mit Typ-A und Typ-B Akkus. Die Akkus werden soviele Male geladen, bis
    die Kapazität nicht mehr über einen vorher bestimmten Schwellwert kommt.<br>
    Die Daten finden sich in &nbsp;&nbsp;<strong>&link&</strong>&nbsp;&nbsp;."
  
  return(d.set)
  
}



sonnenblumen <- function(n=60) {
  
  dünger <- rep(c("A", "B"), each = n/2)
  sorte <- rep(c("Classic", "Giant", "Mini"), length.out = n)
  sonne <- sample(c("hoch", "mittel", "gering"), size = n, replace = TRUE)
  
  # Baseline für Sorten (in cm)
  sorten_effekt <- c(Classic = 150, Giant = 180, Mini = 120)
  
  # Sonneneinstrahlungseffekt
  sonnen_effekt <- c(hoch = 20, mittel = 10, gering = 0)
  
  # Generierung der Pflanzenhöhe
  höhe <- sorten_effekt[sorte] +
    sonnen_effekt[sonne] +
    ifelse(dünger == "A", 5, 0) +     # Dünger A leicht besser
    rnorm(n, mean = 0, sd = 8)        # zufällige Streuung
  
  # Zusammenführen
  daten <- data.frame(dünger, sorte, sonne, höhe)
  
  Labels(daten) <- c("Düngertyp", 
                     "Sonnenblumensorte",
                     "Intensität der Sonneneinstrahlung",
                     "gemessene Pflanzenhöhe nach definierter Wachstumsperiode")

  Label(daten) <- "Führt die Verwendung von Dünger A im Vergleich zu Dünger B 
    zu einem höheren Wachstum (Pflanzenhöhe) bei Sonnenblumen, unter 
    Berücksichtigung verschiedener Sorten und unterschiedlicher 
    Sonneneinstrahlung?<br>
    Die Daten finden sich in &nbsp;&nbsp;<strong>&link&</strong>&nbsp;&nbsp;."
  
  return(daten)
  
}



verzögerung <- function(){
  
  d.set <- EmulData:::.PackageData("verzögerung.xlsx")
  
  Labels(d.set) <- c("Messung vor dem Memo",     
                     "Messung nach dem Memo" 
                     )
  
  Label(d.set) <- '
      In einem Speditionsbetrieb fiel dem Disponenten auf, dass die erste
      Lieferung häufig mit Verspätung erfolgte.
      Wenn sich die erste Lieferung verzögerte, verzögerten sich auch
      alle anderen für diesen Tag geplanten Lieferungen. Über 15 Tage wurde in der Folge
      aufgezeichnet, wie viele Minuten nach der geplanten Zeit die erste
      Lieferung an jedem Tag vom Lager abging.
      <br>
      Danach wurde ein Memo an das gesamte Logistik-Personal versandt,
      in dem alle Beteiligte aufgefordert wurden,
      die Verzögerung der Erstlieferung pro Tag zu verringern. Eine Woche nach dem Versand des Memos wurde
      erneut geprüft, wie viele Minuten Verzögerung die Erstlieferung
      aufwies.
  
      Die Daten finden sich in der Tabelle&nbsp;&nbsp;<strong>&link&</strong>&nbsp;&nbsp;. Eine negative Zahl
      in der Tabelle bedeutet dass die Lieferng früher als geplant vom Lager abging.
      Es interessiert nun, ob das Memo zu geringeren Verspätungen geführt hat.'

}




bike <- function(){
  
  d.set <- EmulData:::.PackageData("bike.xlsx")
  
  Labels(d.set) <- c("Datum","Wochentag","Monat","Ferientag","Jahreszeit",
                     "Wetter","Temperatur in °C","gefühlte Temperatur in °C",
                     "Luftfeuchtigkeit in %","Windgeschwindigkeit",
                     "Anzahl Vermietungen")

  Label(d.set) <- "
          Der Verleih von Fahrrädern hängt stark mit dem Wetter und den saisonalen Gegebenheiten zusammen. 
          Typischerweise beeinflussen Wetterbedingungen, Niederschlag, Wochentag, 
          Jahreszeit, Tageszeit usw. die Anzahl Vermietungen.
          <br>Der Datensatz&nbsp;&nbsp;&link&&nbsp;&nbsp;enthält die Vermietungen eines grossen 
          Velo-Verleihers für jeden Tag während 2er Jahre."
 
  return(d.set)
                            
}




arbeitsweg <- function(){
  
  d.set <- EmulData:::.PackageData("arbeitsweg.xlsx")
  
  Labels(d.set) <- c("ID der befragten Person", "Agglomeration (<em>ZH, BE, BS</em>)", 
                     "Transportmittel", "Benötigte Zeit für den Arbeitsweg")
  
  Label(d.set) <- "
          Für eine Mobilitätsuntersuchung zuhanden der Verkehrskommission sollte die Dauer
          des Arbeitswegs für unterschiedliche Agglomerationen und Verkehrsmittel ermittelt werden.
          "
  
  return(d.set)
  
}



# ---------------------------------------------------
# Funktion zur zufälligen Kundengenerierung
# ---------------------------------------------------
reisekunden <- function(n = 500) {
    
  # ---------------------------------------------------
  # 🧳 Simulation eines Kundendatensatzes für ein Reisebüro
  # ---------------------------------------------------
  
  set.seed(123)  # für Reproduzierbarkeit
  
  # Beispielhafte Zielgruppen mit Wahrscheinlichkeiten
  zielgruppen <- c("Frauen", "Männer", "Jüngere (<30)", "Mittlere (30–50)", "Ältere (60+)")
  p_zielgruppen <- c(0.25, 0.25, 0.20, 0.20, 0.10)
  
  # Basis-Tabelle mit typischen Reisedestinationen und Motivationen
  reiseideen <- data.frame(
    Zielgruppe = c(
      "Frauen", "Frauen", "Männer", "Männer",
      "Jüngere (<30)", "Mittlere (30–50)", "Ältere (60+)"
    ),
    Reiseziel = c(
      "Bali", "Südfrankreich", "Graubünden", "Kanada",
      "Ibiza", "Südafrika", "Donaukreuzfahrt"
    ),
    Motivation = c(
      "Wellness & Natur", "Erholung & Kulinarik",
      "Berge & Sport", "Abenteuer & Roadtrip",
      "Party & Sonne", "Familienzeit & Komfort",
      "Kultur & Erholung"
    ),
    stringsAsFactors = FALSE
  )
  
  
  # Zufällige Zielgruppe pro Kunde
  gruppe <- sample(zielgruppen, n, replace = TRUE, prob = p_zielgruppen)
  
  # Geschlecht aus Zielgruppe ableiten
  geschlecht <- ifelse(gruppe == "Frauen", "weiblich",
                       ifelse(gruppe == "Männer", "männlich",
                              sample(c("weiblich", "männlich"), n, replace = TRUE)))
  
  # Alter je nach Gruppe simulieren
  alter <- sapply(gruppe, function(g) {
    if (g == "Jüngere (<30)") rnorm(1, 25, 3)
    else if (g == "Mittlere (30–50)") rnorm(1, 40, 5)
    else if (g == "Ältere (60+)") rnorm(1, 68, 4)
    else rnorm(1, 45, 10)
  })
  
  # Reiseziel & Motivation passend zur Zielgruppe
  reisedaten <- sapply(gruppe, function(g) {
    sample(reiseideen$Reiseziel[reiseideen$Zielgruppe == g], 1)
  })
  
  motive <- sapply(gruppe, function(g) {
    sample(reiseideen$Motivation[reiseideen$Zielgruppe == g], 1)
  })
  
  # Zusammenführen
  daten <- data.frame(
    KundenID = 1:n,
    Geschlecht = geschlecht,
    Alter = round(alter),
    Zielgruppe = gruppe,
    Reiseziel = reisedaten,
    Motivation = motive,
    stringsAsFactors = FALSE
  )
  
  
  Label(daten) <- "Ein Reisebüro will die Unterschiede in den Reisepräferenzen 
          nach Geschlecht und Altersgruppe abbilden, wie sie typischerweise 
          in Marktanalysen beobachtet werden."
  
  Labels(daten) <- c("die ID des Kunden", "das Alter", "die Zielgruppe", "das letzte Reiseziel",
                     "die geäusserte Motivation")
  
  return(daten)
  
}


kreisel <- function(){
  
  d.set <- EmulData:::.PackageData("kreisel.xlsx")
  
  Labels(d.set) <- c("Staulänge ohne Verkehrsleitung", 
                     "Staulänge mit Verkehrsleitung")
  
  Label(d.set) <- "
      Am Kreisel in Fällanden staut sich der Verkehr an jedem Morgen zuverlässig. 
      Die Gemeinde versuchte als Ansatz, die Wartezeiten für die Autofahrer 
      durch den Einsatz von Verkehrskadetten zu verkürzen. 
      Um den Einfluss der Verkehrsleitung zu prüfen, mass man an &nA& normalen Tagen 
      die Länge des Staus in [m] zu jeweils 8 vorgängig zufällig bestimmten Zeitpunkten 
      innerhalb der kritischen Zeitphase von 06:00-09:00 Uhr (A - <em>ohne</em>). 
      Das Gleiche tat man an weiteren &nB& Tagen, an denen die Verkehrskadetten 
      zum Einsatz kamen (B - <em>mit</em>). 
      Die Mittelwerte des Staus pro Tag finden sich in der 
      Datei &nbsp;<strong>&link&</strong>&nbsp;&nbsp;.
      "
  
  return(d.set)
  
}









# Tennisschläger
"Einer Gruppe von 20 Tennisspielern mittleren Niveaus werden je zwei Tennisschläger 
zum Testen ausgehändigt. 
Einer der Schläger ist jeweils mit einer Nylon-Saite
bespannt, der andere mit einer synthetischen Darm-Saite. Nach einigen Wochen Testzeit
wird jeder Spieler gefragt, ob er Nylon- oder Darm-Saiten bevorzugt. Es sei p der Anteil aller 
Tennisspieler mittleren Niveaus, die Darm-Saiten bevorzugen und X sei die
Anzahl der Spieler unter den 20 Testspielern, die Darm-Saiten bevorzugen. 
Da Darm-Saiten teurer 
sind als Nylon-Saiten, betrachten wir die Nullhypothese, dass höchstens
die Hälfte der Spieler Darm-Saiten bevorzugt. 
Wir vereinfachen dies zu H0 : p = 0.5 und
werden H0 nur ablehnen, falls der Versuchsausgang eindeutig Darm-Saiten bevorzugt.

Marktabschätzung, mindestens 30% Kunden für eine Markteinführung von neuen Saiten."



"Die Post hat festgestellt, dass normalerweise 5% aller Sendungen auf
dem Postweg verloren gehen. Der Online-Shop Azamon.com möchte diese Information
benutzen, um betrügerische Kunden zu erkennen.
a) Welche Verteilung können wir benutzen, um die Anzahl X der verlorenen Pakete
fur einen Kunde, der n Bestellungen gemacht hat, zu modellieren?
  b) Shopper99 hat 15 Bestellungen gemacht und zwei von ihnen als 
  'auf dem Postweg verloren gegangen' angezeigt. Azamon.com möchte testen, ob dieser Kunde
betrügerisch ist."


"Unterhalb einer Kläranlage wurden 16 unabhängige Wasserproben aus einem
Fluss entnommen und jeweils deren Ammoniumkonzentration Xi (angegeben in
µgNH4-N/`) mit einem Messgerät bestimmt. Der Mittelwert der Proben ergab
x = 204.2.
Wir wollen nun wissen, ob mit diesem Experiment eine Überschreitung des
Grenzwerts von 200 µgNH4-N/` nachgewiesen werden kann (auf dem 5%-Niveau).
a) (2 Punkte) Nimm an, die Standardabweichung der Messungen sei im Voraus aufgrund 
früherer Studien bekannt. Sie betrage 10 µgNH4-N/`. 
Finde einen geeigneten statistischen Test, um zu überprüfen, ob eine Grenzwertüberschreitung 
nachgewiesen werden kann. Wie lauten die Modellannahmen?"






