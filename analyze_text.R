library(udpipe)

# download czech language model
dlCac <- udpipe_download_model(language = c("czech-cac"))
udmodel_czech_cac <- udpipe_load_model(file = "czech-cac-ud-2.3-181115.udpipe")

# language coding for macOS
Sys.setlocale(category = "LC_ALL", locale = "en_US.UTF-8")

#firstly download titles from czech wiki - https://dumps.wikimedia.org/cswiki/latest/?fbclid=IwAR3Bd0UvsQFuxf90Ncp9t7YBe7dRbyT6XE70llYip9KW9MIk9MLux85UXGg



setwd("~/....../eRko/knihovna projekt")
library(udpipe)
udmodel_czech_cac <- udpipe_load_model(file = "czech-cac-ud-2.3-181115.udpipe")
Sys.setlocale(category = "LC_ALL", locale = "en_US.UTF-8")
wiki_names <- readLines("wiki_names")


anotate_text <- function(file_name){
  lines <- readLines(file_name)
  vektor <- as.vector(lines)
  anotation  <- udpipe_annotate(udmodel_czech_cac, vektor)
  frame <- as.data.frame(anotation)
  return(frame)
} 

subset_text <- function(anotation){
  subset <- subset(anotation, upos %in% c("PROPN"))
  return(subset)
} 

intersect_text <- function(anotation){
  propn <- subset(anotation, upos %in% c("PROPN"))
  intersection <- intersect(propn$lemma, wiki_names)
  frame <- as.data.frame(intersection)
  return(frame)
} 

intersect_text_all <- function(anotation) {
  intersection_all <- intersect(anotation$lemma, wiki_names)
  frame_all <- as.data.frame(intersection_all)
  return(frame_all)
}

count_word <- function(intersection, subset){
  intersection <- (intersection$intersection)
  subset <- (subset$lemma)
  proprn <- integer(length(intersection))
  for(i in 1:length(intersection)){
    word <- intersection[i]
    proprn[i] <- sum(subset == word)
  }
  df <- data.frame(intersection, proprn)
  return(df)
}


analyze_text <- function(file_name) {
  anotation <- anotate_text(file_name)
  intersection <- intersect_text(anotation)
  intersection_all <- intersect_text_all(anotation)
  subset <- subset_text(anotation)
  count <- count_word (intersection,subset)
  return(list('anotation'=anotation, 'intersection'=intersection,'intersection_all'=intersection_all, 'subset'=subset, 'count'=count))
}
#anotate_dir function never worked 
anotate_dir <- function(input_dir) {
  all_annotations <- list()
  files = list.files(input_dir)
  for(i in c(6,8)){ #1:length(files)){
    name = files[i]
    print(name)
    path <- paste(input_dir, "/", name, sep="")
    analysis <- analyze_text(path)
    print(analysis)
    print(all_annotations)
    all_annotations$name = analysis
    print(all_annotations)
  }
  return(all_annotations)
}
#all_anotations <- anotate_dir('texty')

arabesky_1893 <- analyze_text("texty/arabesky_1893_8.txt")
babicka_1920 <- analyze_text("texty/babicka_1920.txt")
basne_macha_1897 <- analyze_text("texty/basne_macha_1897.txt")
...

write.csv2(basne_macha_1897[["intersection"]], file="basne_macha_1897_i.csv", fileEncoding = "UTF-8")
write.csv2(basne_macha_1897[["intersection_all"]], file="basne_macha_1897_all.csv", fileEncoding = "UTF-8")
write.csv2(basne_macha_1897[["count"]], file="basne_macha_1897_sub.csv", fileEncoding = "UTF-8")
...


#from intersection with counted mentions we choose real locations by hand - that is biggest weakness of the whole project. 
#to the locations we give latitude and longitude parameters using GeoCoder - http://www.gpsvisualizer.com/geocoder/
#places which wasn't found, we localize by hand using http://geonames.org

We complete dataset with information about authors and writings and using https://carto.com make a amazing map.

Like we did here: http://bit.ly/romanticti_spisovatele .
