


RndWord <- function(size, length, x = LETTERS, replace = TRUE, prob = NULL){
  sapply(1:size, function(i) paste(sample(x=x, size=length, replace=replace, prob=prob), collapse=""))
}
