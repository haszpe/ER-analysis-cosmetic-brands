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


#for (tab in c('fb_blab', 'fb_kaya', 'fb_tolpa', 'fb_miyo', 'fb_htc', 'insta_blab', 
#              'insta_kaya', 'insta_tolpa', 'insta_miyo', 'insta_htc')){
#  tab <- get(tab)
#  tab$'ER od posta' <- tab$`ER od posta`*100
#}


# FB: Sprawdzenie normalności rozkładu zmiennej 'suma reakcji na post' w grupach ---------------------
for (category in unique(fb_blab$'typ posta')){
  typ <- subset(fb_blab, `typ posta` == category)
  shapiro.p <- shapiro.test(typ$'suma reakcji')$p.value
  cat(category, ': ', shapiro.p, '\n')
}

for (category in unique(fb_kaya$'typ posta')){
  typ <- subset(fb_kaya, `typ posta` == category)
  shapiro.p <- shapiro.test(typ$'suma reakcji')$p.value
  cat(category, ': ', shapiro.p, '\n')
}


#drop outlier -> GenericAttachementMedia
fb_tolpa <- fb_tolpa[-58, ]
for (category in unique(fb_tolpa$'typ posta')){
  typ <- subset(fb_tolpa, `typ posta` == category)
  shapiro.p <- shapiro.test(typ$'suma reakcji')$p.value
  cat(category, ': ', shapiro.p, '\n')
}

for (category in unique(fb_miyo$'typ posta')){
  typ <- subset(fb_miyo, `typ posta` == category)
  shapiro.p <- shapiro.test(typ$'suma reakcji')$p.value
  cat(category, ': ', shapiro.p, '\n')
}

#HTC for 'Video' - not enough data; checking only for 'Photo'
temp <- subset(fb_htc, `typ posta` == 'Photo')
shapiro.test(temp$'suma reakcji')$p.value

# Z powyższych wynika, że normalnośc (przy poziomie istoności = 0.05) jest 
# spełniona jedynie dla grup:
# FB: BasicLab: Video & Photo
# FB: Kaya: Photo & Text
# FB: Tolpa: ---
# FB: Miyo: --- 
# FB: HTC: Photo

# A czy normalność reakcji jest zachowana w markach?
shapiro.test(fb_blab$'suma reakcji')$p.value
shapiro.test(fb_kaya$'suma reakcji')$p.value
shapiro.test(fb_tolpa$'suma reakcji')$p.value
shapiro.test(fb_miyo$'suma reakcji')$p.value
shapiro.test(fb_htc$'suma reakcji')$p.value
# No i prawie jest, ale Miyo nie ma rozkładu normalnego.

# INSTA: Sprawdzenie normalności rozkładu zmiennej w grupach ---------------------

for (category in unique(insta_blab$'typ posta')){
  typ <- subset(insta_blab, `typ posta` == category)
  shapiro.p <- shapiro.test(typ$'suma reakcji')$p.value
  cat(category, ': ', shapiro.p, '\n')
}

for (category in unique(insta_kaya$'typ posta')){
  typ <- subset(insta_kaya, `typ posta` == category)
  shapiro.p <- shapiro.test(typ$'suma reakcji')$p.value
  cat(category, ': ', shapiro.p, '\n')
}

for (category in unique(insta_tolpa$'typ posta')){
  typ <- subset(insta_tolpa, `typ posta` == category)
  shapiro.p <- shapiro.test(typ$'suma reakcji')$p.value
  cat(category, ': ', shapiro.p, '\n')
}

#Miya Sidecars - not enough data
for (category in c('Image', 'Video')){
  typ <- subset(insta_miyo, `typ posta` == category)
  shapiro.p <- shapiro.test(typ$'suma reakcji')$p.value
  cat(category, ': ', shapiro.p, '\n')
}

for (category in unique(insta_htc$'typ posta')){
  typ <- subset(insta_htc, `typ posta` == category)
  shapiro.p <- shapiro.test(typ$'suma reakcji')$p.value
  cat(category, ': ', shapiro.p, '\n')
}

# Z powyższych wynika, że normalnośc (przy poziomie istoności = 0.05) jest
# zachowana dla wszystkich grup poza Sidecar'em w HairyTaleCosmetics.

# A czy normalność reakcji jest zachowana w markach?
shapiro.test(insta_blab$'suma reakcji')$p.value
shapiro.test(insta_kaya$'suma reakcji')$p.value
shapiro.test(insta_tolpa$'suma reakcji')$p.value
shapiro.test(insta_miyo$'suma reakcji')$p.value
shapiro.test(insta_htc$'suma reakcji')$p.value

# Normalność dla sumy reakcji wszystkich postów na instagramie jest zachowana
# dla każdej z marek. 

# Ze względu na brak normalności w niekórych gurpach, nie mozemy zastosować 
# parametrycznej wariancji. Dlatego zastosujemy nieparamteryczną, wieloczynnikową
# analizę wariancji.

