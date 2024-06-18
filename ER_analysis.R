library(readxl)
library(dplyr)

# Read Data  ------------------------------------------------------------------
path_file <- "Data/Magisterka_marki_policzone.xlsx"

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
fb_blab['followers']<- 54000
fb_kaya['followers']<- 68000
fb_tolpa['followers']<- 162000
fb_miyo['followers']<- 41000
fb_htc['followers']<- 8000

insta_blab['followers']<- 85700
insta_kaya['followers']<- 106000
insta_tolpa['followers']<- 74300
insta_miyo['followers']<- 73700
insta_htc['followers']<- 40200

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
for (tab in c('fb_blab', 'fb_kaya', 'fb_tolpa', 'fb_miyo', 'fb_htc', 'insta_blab', 
              'insta_kaya', 'insta_tolpa', 'insta_miyo', 'insta_htc')){
  temp <- get(tab)
  temp$'ER od posta' <- temp$`ER od posta`*100
  temp['ER'] <- (temp$'suma reakcji'/temp$followers)*100
  assign(tab, temp)
}

# Normalność ------------------------------------------------------------------
# FB: Sprawdzenie normalności rozkładu zmiennej 'suma reakcji na post' w grupach
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

# INSTA: Sprawdzenie normalności rozkładu zmiennej w grupach 

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
# Brak normalności dla wszystkich marek.


# Ze względu na brak normalności nie mozemy zastosować testów parametrycznych.
# Dlatego zastosujemy nieparamteryczną, wieloczynnikową analizę wariancji.


# Zgrupowane dane ------------------------------------------------------------
data <- rbind(fb_blab[c('source', 'brand', 'typ posta', 'suma reakcji', 'ER')], 
              fb_kaya[c('source', 'brand', 'typ posta', 'suma reakcji', 'ER')],
              fb_tolpa[c('source', 'brand', 'typ posta', 'suma reakcji', 'ER')],
              fb_miyo[c('source', 'brand', 'typ posta', 'suma reakcji', 'ER')],
              fb_htc[c('source', 'brand', 'typ posta', 'suma reakcji', 'ER')],
              insta_blab[c('source', 'brand', 'typ posta', 'suma reakcji', 'ER')],
              insta_kaya[c('source', 'brand', 'typ posta', 'suma reakcji', 'ER')],
              insta_tolpa[c('source', 'brand', 'typ posta', 'suma reakcji', 'ER')],
              insta_miyo[c('source', 'brand', 'typ posta', 'suma reakcji', 'ER')],
              insta_htc[c('source', 'brand', 'typ posta', 'suma reakcji', 'ER')])


# Groupby 'source', 'brand', 'typ posta' and calculate mean
średnie <- data %>% 
              group_by(source, brand, `typ posta`) %>%
              summarise(średnia_reakcji = mean(`suma reakcji`),
                        średnia_ER = mean(ER))
średnie_marek <- data %>% 
  group_by(source, brand) %>%
  summarise(średnia_reakcji = mean(`suma reakcji`),
            średnia_ER = mean(ER))

# Data export & CLeanup -------------------------------------------------------
# write.table(średnie, file = "Data/Means.csv", row.names=FALSE)
# write.table(średnie_marek, file = "Data/Means_brands.csv", row.names=FALSE)

rm(temp, typ, category, path_file, shapiro.p, tab)

# ANOVA nieparametryczna ------------------------------------------------------

                     
