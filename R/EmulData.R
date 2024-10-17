



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
                   Höhe in der Regel von Ausstattungs- und Lagemerkmalen der Mietwohnung abhängt."
  
  
  return(d.set)
  
}


