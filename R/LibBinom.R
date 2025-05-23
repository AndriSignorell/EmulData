
# Binomial Library


LibBinomial <- list(
  zigaretten = list(
    PRE_TXT = "In einer Studie sollte die These geprüft werden, dass Raucher ihre
                  Zigarettenmarke aufgrund des Geschmacks zuverlässig identifizieren können. 
                  In einem Experiment wurden &n&&nbsp;Probanden je 5&nbsp;Zigaretten unterschiedlicher Marke 
                  zum Rauchen gegeben, von denen jeweils eine von der bevorzugten Marke war. X bezeichne die 
                  Anzahl Personen, die ihre Marke richtig erkannt haben.
                  <br>
                  Unter der Annahme, dass alle Probanden nur geraten haben, wie gross ist 
                  die Wahrscheinlichkeit, dass ...&nbsp;<br><br>
                  ",
    prob_txt = "Probanden ihre Marke richtig erkannt haben.",
    quant_txt = "Wie gross ist die höchste Anzahl richtig erkannter Zigarettenmarken, der %s TeilnehmerInnen 
                    mit den am wenigsten richtig erkannten Zigaretten?",  
    prob = c(1/5, 1/5),  # probability from ... to
    n = c(12,15)
  ), 
  
  
  fahrschule = list(
    PRE_TXT =  "Eine Fahrschule behauptet, dass &p& ihrer Fahrschüler beim ersten Versuch 
               die Fahrprüfung bestehen.  <br>
               Berechnen Sie die Wahrscheinlichkeit, dass bei dieser Erfolgsquote bei einer 
               Gruppe von &n&&nbsp;Fahrschülern ... <br><br>
              ",
    prob_txt="Fahrschüler die Prüfung bestehen.",
    quant_txt = "", 
    prob = c(0.7,0.9),  # probability from ... to
    n = c(6, 8)
    
  ), 
  
  sumpfkrebs = list(
    PRE_TXT =  "
        In der wissenschaftlichen Literatur findet sich die Schätzung, 
        dass unter normalen Umständen p&nbsp;=&nbsp;&p& der Sumpfkrebse 
        nicht fortpflanzungsfähig 
        sind. Ein Team von Biologie-Studenten fängt n&nbsp;=&nbsp;&n&&nbsp; erwachsene Krebse.
        <br>
               Berechnen Sie die Wahrscheinlichkeit, dass davon ... <br><br>
              ",
    prob_txt="Krebse fortpflanzungsfähig sind.",
    quant_txt = "", 
    prob = c(0.8, 0.9),  # probability from ... to
    n = c(12, 15)
    
  ), 
  
  biathlon = list(
    PRE_TXT =  "
    
      Eine Biathletin startet mit einer Trefferwahrscheinlichkeit von p&nbsp;=&nbsp;&p& 
      in die Saison, bestimmt anhand ihrer Trainingsdaten. 
      In den ersten Rennen gibt sie n&nbsp;=&nbsp;&n&&nbsp; Schüsse ab.
      <br>
      Berechnen Sie die Wahrscheinlichkeit, dass davon ... <br><br>
      ",
    prob_txt="Schüsse Treffer sind.",
    quant_txt = "", 
    prob = c(0.85, 0.95),  # probability from ... to
    n = c(12, 15)
    
  )

  
)

