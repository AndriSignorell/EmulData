

.PackageData <- function(filename) {
  
  data.frame(readxl::read_xlsx(
    gettextf("%s/%s", 
             file.path(find.package("EmulData"), "extdata"), 
             filename)))
  
}

