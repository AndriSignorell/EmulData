



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






