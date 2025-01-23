
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
    
  )
)

