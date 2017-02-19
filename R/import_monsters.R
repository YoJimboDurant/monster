
# Libraries ---------------------------------------------------------------

library(readr)
library(stringr)
library(magrittr)
library(dplyr)


# load monsters -----------------------------------------------------------

monsters <- read.csv("../data/d20pfsrd-Bestiary - Updated 23Feb2014.csv")


# create function to calculate hp based on hit die ------------------------

roll_hit_points <- function(hit_die, n=1){
  HD <- as.character(rep(hit_die,n))
  
  grepl("[0-9]+d[0-9]+(\\+[0-9]+)?(\\-[0-9]+)?", HD)
  
  ndie <- as.numeric(str_extract(HD, "^[0-9]+"))
  die <- as.numeric(str_extract(HD, regex('(?<=d)[0-9]+')))
  
  bonus <- str_extract(HD, "[+-][0-9]+")
  bonus[is.na(bonus)] <- 0 
  bonus <- as.numeric(bonus)
  
  horror_bonus <- as.numeric(str_extract(HD, regex('(?<=plus) [0-9]+')))
  horror_bonus[is.na(horror_bonus)] <- 0 
  
  bonus <- bonus + horror_bonus
  
  lx <- data.frame(ndie=ndie,die=die,bonus=bonus)
  generate_hp <- function(lx){
    sample_die <- 1:lx$die
    rolls <- sample(sample_die, size = lx$ndie, replace = TRUE)
    hit_points <- sum(rolls) + lx$bonus
    return(hit_points)  
  }
  
  hp <- sapply(seq_along(1:dim(lx)[1]), function(i){
    generate_hp(lx[i,])
    })
  
  hp[hp<1] <- 1
  return(paste(hp, col=" "))
}



dungeon_monsters <- 
  monsters[grep("^goblin$|Buggane|Junk Golem|Fachen", monsters$Name, ignore.case = T),]


dungeon_monsters$hp <- sapply(seq_along(dungeon_monsters$HD), function(i) roll_hit_points(dungeon_monsters$HD[i],  dungeon_monsters$N[i]))

write_room = function(i, outfile = "./markdown/outfile.rmd"){
  roomlines <- NULL
  roomlines <- c("\n", paste("###Room", i), "\n")
  room_monsters <- dungeon_monsters[i,]
  room_monsters <- as.data.frame(t(room_monsters))
  names(room_monsters) <- paste(dungeon_monsters[i,c("Name")], collapse=" ") 
#  room_monsters <- room_monsters[room_monsters$N!="",]
  monsters <- knitr::kable(room_monsters, format="markdown")
  roomlines <- c(roomlines, monsters)
  return(roomlines)
}

head_lines <- readLines("./markdown/header.rmd")

outfile = "./markdown/kargstower.rmd"

head_lines <- gsub("!TITLE!", "Karg's Tower", head_lines)



rooms <- lapply(seq_along(1:dim(dungeon_monsters)[1]), function(i) write_room(i))

write(head_lines, outfile)

lapply(rooms, write, outfile, append=TRUE)
