library(readxl)
library(dplyr)

# Read Data  ------------------------------------------------------------------
path_file <- "C:/Users/hanna/Downloads/Magisterka_marki_policzone.xlsx"

fb_blab <- read_excel(path_file, sheet=3 ,range = "D2:O192", col_names = TRUE)[-4][-7]
fb_kaya <- read_excel(path_file, sheet=4 ,range = "D1:M115", col_names = TRUE)[-4][-7]
fb_tolpa <- read_excel(path_file, sheet=5 ,range = "D1:N93", col_names = TRUE)[-4][-7]
fb_miyo <- read_excel(path_file, sheet=6 ,range = "D1:N15", col_names = TRUE)[-4][-7]
fb_htc <- read_excel(path_file, sheet=7 ,range = "D1:N12", col_names = TRUE)[-4][-7]

insta_blab <- read_excel(path_file, sheet=11 ,range = "E1:N190", col_names = TRUE)[-3]
insta_kaya <- read_excel(path_file, sheet=10 ,range = "D1:N162", col_names = TRUE)[-3]
insta_tolpa <- read_excel(path_file, sheet=12 ,range = "D1:M144", col_names = TRUE)[-3]
insta_miyo <-read_excel(path_file, sheet=9 ,range = "D1:M107", col_names = TRUE)[-3]
insta_htc <- read_excel(path_file, sheet=13 ,range = "C1:L78", col_names = TRUE)[-3]

# Number of profile followers ------------------------------------------------
n_fb_blab <- 54000
n_fb_kaya <- 68000
n_fb_tolpa <- 162000
n_fb_miyo <- 41000
n_fb_htc <- 8000

n_insta_blab <- 85700
n_insta_kaya <- 106000
n_insta_tolpa <- 74300
n_insta_miyo <- 73700
n_insta_htc <- 40200

# Data transformations  -------------------------------------------------------
fb_blab['suma reakcji'] <- rowSums(fb_blab[, c('liczba komentarzy', 'polubienia', 'liczba udostępnień')], na.rm=T)
colnames(fb_blab)[8] ="ER od posta"
fb_kaya['suma reakcji'] <- rowSums(fb_kaya[, c('comments', 'likes', 'shares')], na.rm=T)
colnames(fb_kaya)[4] ="typ posta"
fb_tolpa['suma reakcji'] <- rowSums(fb_tolpa[, c('comments', 'likes', 'shares')], na.rm=T)
colnames(fb_tolpa)[4] ="typ posta"
fb_miyo['suma reakcji'] <- rowSums(fb_miyo[, c('comments', 'likes', 'shares')], na.rm=T)
colnames(fb_miyo)[4] ="typ posta"
fb_htc['suma reakcji'] <- rowSums(fb_htc[, c('comments', 'likes', 'shares')], na.rm=T)
colnames(fb_htc)[4] ="typ posta"

# Outliers handling:
fb_tolpa <- fb_tolpa[-58, ]
insta_tolpa[61, 2] <- c('Video')


insta_blab['suma reakcji'] <- rowSums(insta_blab[, c('likesCount', 'commentsCount')], na.rm=T)
insta_kaya['suma reakcji'] <- rowSums(insta_kaya[, c('likesCount', 'commentsCount')], na.rm=T)
insta_tolpa['suma reakcji'] <- rowSums(insta_tolpa[, c('likesCount', 'commentsCount')], na.rm=T)
insta_miyo['suma reakcji'] <- rowSums(insta_miyo[, c('likesCount', 'commentsCount')], na.rm=T)
insta_htc['suma reakcji'] <- rowSums(insta_htc[, c('likesCount', 'commentsCount')], na.rm=T)
colnames(insta_blab)[2] ="typ posta"
colnames(insta_kaya)[2] ="typ posta"
colnames(insta_tolpa)[2] ="typ posta"
colnames(insta_miyo)[2] ="typ posta"
colnames(insta_htc)[2] ="typ posta"


for (tab in c('fb_blab', 'fb_kaya', 'fb_tolpa', 'fb_miyo', 'fb_htc', 'insta_blab', 
              'insta_kaya', 'insta_tolpa', 'insta_miyo', 'insta_htc')){
  tab <- get(tab)
  tab$'ER od posta' <- tab$`ER od posta`*100
}

fb_blab$source <- c('Facebook')
fb_blab$brand <- c('BasicLab')
fb_kaya$source <- c('Facebook')
fb_kaya$brand <- c('Kaya')
fb_tolpa$source <- c('Facebook')
fb_tolpa$brand <- c('Tolpa')
fb_miyo$source <- c('Facebook')
fb_miyo$brand <- c('Miyo')
fb_htc$source <- c('Facebook')
fb_htc$brand <- c('HTC')

insta_blab$source <- c('Instagram')
insta_blab$brand <- c('BasicLab')
insta_kaya$source <- c('Instagram')
insta_kaya$brand <- c('Kaya')
insta_tolpa$source <- c('Instagram')
insta_tolpa$brand <- c('Tolpa')
insta_miyo$source <- c('Instagram')
insta_miyo$brand <- c('Miyo')
insta_htc$source <- c('Instagram')
insta_htc$brand <- c('HTC')

# Zamień 'Image' na 'Photo
insta_blab$'typ posta' <- gsub('Image', 'Photo', insta_blab$'typ posta')
insta_kaya$'typ posta' <- gsub('Image', 'Photo', insta_kaya$'typ posta')
insta_tolpa$'typ posta' <- gsub('Image', 'Photo', insta_tolpa$'typ posta')
insta_miyo$'typ posta' <- gsub('Image', 'Photo', insta_miyo$'typ posta')
insta_htc$'typ posta' <- gsub('Image', 'Photo', insta_htc$'typ posta')
insta_blab$'typ posta' <- gsub('Sidecar', 'Photo', insta_blab$'typ posta')
insta_kaya$'typ posta' <- gsub('Sidecar', 'Photo', insta_kaya$'typ posta')
insta_tolpa$'typ posta' <- gsub('Sidecar', 'Photo', insta_tolpa$'typ posta')
insta_miyo$'typ posta' <- gsub('Sidecar', 'Photo', insta_miyo$'typ posta')
insta_htc$'typ posta' <- gsub('Sidecar', 'Photo', insta_htc$'typ posta')

# ER --------------------------------------------------------------------------
fb_blab$ER <- (fb_blab$'suma reakcji'/n_fb_blab)*100
fb_kaya$ER <- (fb_kaya$'suma reakcji'/n_fb_kaya)*100
fb_tolpa$ER <- (fb_tolpa$'suma reakcji'/n_fb_tolpa)*100
fb_miyo$ER <- (fb_miyo$'suma reakcji'/n_fb_miyo)*100
fb_htc$ER <- (fb_htc$'suma reakcji'/n_fb_htc)*100

insta_blab$ER <- (insta_blab$'suma reakcji'/n_insta_blab)*100
insta_kaya$ER <- (insta_kaya$'suma reakcji'/n_insta_kaya)*100
insta_tolpa$ER <- (insta_tolpa$'suma reakcji'/n_insta_tolpa)*100
insta_miyo$ER <- (insta_miyo$'suma reakcji'/n_insta_miyo)*100
insta_htc$ER <- (insta_htc$'suma reakcji'/n_insta_htc)*100

# FB: Sprawdzenie normalności rozkładu zmiennej 'suma reakcji na post' w grupach ---------------------
for (category in unique(fb_blab$'typ posta')){
  typ <- subset(fb_blab, `typ posta` == category)
  shapiro.p <- shapiro.test(typ$'suma reakcji')$p.value
  cat(category, ': ', shapiro.p, '\n')
}
#Normalnośc tylko dla Text

for (category in unique(fb_kaya$'typ posta')){
  typ <- subset(fb_kaya, `typ posta` == category)
  shapiro.p <- shapiro.test(typ$'suma reakcji')$p.value
  cat(category, ': ', shapiro.p, '\n')
}
#Normalność tylko dla Video

for (category in unique(fb_tolpa$'typ posta')){
  typ <- subset(fb_tolpa, `typ posta` == category)
  shapiro.p <- shapiro.test(typ$'suma reakcji')$p.value
  cat(category, ': ', shapiro.p, '\n')
}
#Normalność dla Video i Text

for (category in unique(fb_miyo$'typ posta')){
  typ <- subset(fb_miyo, `typ posta` == category)
  shapiro.p <- shapiro.test(typ$'suma reakcji')$p.value
  cat(category, ': ', shapiro.p, '\n')
}
#Normalność dla Photo

#HTC for 'Video' - not enough data; checking only for 'Photo'
temp <- subset(fb_htc, `typ posta` == 'Photo')
shapiro.test(temp$'suma reakcji')$p.value
#Brak normalności



# A czy normalność reakcji jest zachowana w markach?
shapiro.test(fb_blab$'suma reakcji')$p.value
shapiro.test(fb_kaya$'suma reakcji')$p.value
shapiro.test(fb_tolpa$'suma reakcji')$p.value
shapiro.test(fb_miyo$'suma reakcji')$p.value
shapiro.test(fb_htc$'suma reakcji')$p.value
# Tylko Miyo ma rozkład normalny.



# INSTA: Sprawdzenie normalności rozkładu zmiennej w grupach ---------------------

for (category in unique(insta_blab$'typ posta')){
  typ <- subset(insta_blab, `typ posta` == category)
  shapiro.p <- shapiro.test(typ$'suma reakcji')$p.value
  cat(category, ': ', shapiro.p, '\n')
}
#Brak normalności dla wszystkich grup

for (category in unique(insta_kaya$'typ posta')){
  typ <- subset(insta_kaya, `typ posta` == category)
  shapiro.p <- shapiro.test(typ$'suma reakcji')$p.value
  cat(category, ': ', shapiro.p, '\n')
}
# Brak normalności dla wszystkich grup

for (category in unique(insta_tolpa$'typ posta')){
  typ <- subset(insta_tolpa, `typ posta` == category)
  shapiro.p <- shapiro.test(typ$'suma reakcji')$p.value
  cat(category, ': ', shapiro.p, '\n')
}
# Brak normalności dla wszystkich grup

for (category in unique(insta_miyo$'typ posta')){
  typ <- subset(insta_miyo, `typ posta` == category)
  shapiro.p <- shapiro.test(typ$'suma reakcji')$p.value
  cat(category, ': ', shapiro.p, '\n')
}
# Brak normalności dla wszystkich grup

for (category in unique(insta_htc$'typ posta')){
  typ <- subset(insta_htc, `typ posta` == category)
  shapiro.p <- shapiro.test(typ$'suma reakcji')$p.value
  cat(category, ': ', shapiro.p, '\n')
}
# Brak normalności dla wszystkich grup

# Z powyższych wynika, że normalnośc nie jest zachowana dla żadnej z grup.

# A czy normalność reakcji jest zachowana w markach?
shapiro.test(insta_blab$'suma reakcji')$p.value
shapiro.test(insta_kaya$'suma reakcji')$p.value
shapiro.test(insta_tolpa$'suma reakcji')$p.value
shapiro.test(insta_miyo$'suma reakcji')$p.value
shapiro.test(insta_htc$'suma reakcji')$p.value

# Normalność dla sumy reakcji wszystkich postów na instagramie jest zachowana
# dla każdej z marek. 


