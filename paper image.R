#Collect Flickr data ###########################################################################
#Install and library pacakges
install.packages("pacman")

pacman::p_load(data.table,
               dplyr,
               sf)

pacman::p_load_gh("ropensci/photosearcher")

#read in the shapefile
auvergne_sf <- sf::read_sf(".\\Auvergne\\Auvergne.shp") #directory to your shape file 

#reprojcet to wgs84
crs_reproject <- "+proj=longlat +datum=WGS84 +no_defs" #new crs to project to
auvergne_sf <- sf::st_transform(auvergne_sf, crs_reproject) # project shapefile

#search for all images within the shapefile
flickr_out <- photosearcher::photo_search(mindate_taken = "2017-01-01",
                                          maxdate_taken = "2021-01-01",
                                          maxdate_uploaded = "2021-02-01",
                                          sf_layer = auvergne_sf)

#filter results of text
flickr_out$text <- paste(flickr_out$title,
                         flickr_out$tags,
                         flickr_out$description) #create a col with all text metadata


####################################################################################################
#package_google vision----
library(imgrec)


#set up gvision
Sys.setenv(gvision_key = "XXX") #Your Google API Key
gvision_init()

#wikiloc_images_URL
#search google vision
results <- get_annotations(images = paste(flickrphoto$url), # image url
                           features = "label", # request all available features
                           max_res = 20, # maximum number of results per feature
                           mode = 'url') # determine image type

#parse results
temp_file_path <- tempfile(fileext = '.json')
save_json(results, temp_file_path)
img_data <- parse_annotations(results)
vision_results <- img_data$labels

#save the outputs
library("writexl")
write_xlsx(vision_results,"vision_flickr.xlsx")


#work with vision
#summary description
vision_summary <- data.frame(table(vision_results$description))
vision_summary <- vision_summary[order(-vision_summary$Freq),]
head(vision_summary)


###############################################################################################
#image analysis, statistics and data vizualisation 

#packages
library(tidyverse)
library(tidytext)
library(tidyr)
library(dplyr)
library(reshape2)
library(stringr)
library(ggplot2)
library(hunspell)
library(wordcloud)
library(forcats)
library(gutenbergr)
library(topicmodels)
library(textdata)
library(igraph)
library(ggraph)
library(widyr)
library(cowplot)
library(ggtext)
library(gridExtra)
library(grid)
library(broom)
library(purrr)
library(Hmisc)
library(data.table)
library(RColorBrewer)
library(cli)
library("writexl")
library(FactoMineR) #stat
library(gtools)
library(svglite)
library(multcomp)
library(multcompView)
library(rcompanion)
library(emmeans)
library(xlsx)
library("vegan")
library(wordcloud2)
library(ggwordcloud)
library("patchwork")
library(ggh4x)
library(scales)
library(ggsignif)
#package GIS
library(sf)  #spatail
library(biscale) #spatail
library(cowplot) #needed for biscale
library(raster)
library(ggspatial)  #needed for North and scale for map

#####################################################################################
#images and classification of tags ----
nature <- FinalTagsCoding %>% 
  group_by(url) %>% 
  summarise(
    prop_notnature = sum(class=="no")/n(), 
    prop_nature = sum(class != "no")/n())

#select prop_nature >=0.50 as nature images 
nature$imageNat <- ifelse(nature$prop_notnature>0.50, "no",
                          "yes")


#coded image as species or not 
species <- FinalTagsCoding %>%
  group_by(url) %>% 
  summarise(
    prop_species = sum(type=="species appreciation")/n(), 
    prop_notspecies = sum(type != "species appreciation")/n())


#select prop_species >=0.50 as species images 
species$imageSpecies <- ifelse(species$prop_notspecies>0.50, "no",
                               "yes")


################################################################################
#Distribution by land cover and trails 

#Flickr
countFlickr <- flickrSERVICEland %>%
  group_by(Trail) %>%
  count(LU)

countdataF <-  countFlickr %>%
  pivot_wider(
    names_from = LU,
    values_from = n,
    values_fill = 0,
    values_fn = sum)

summary(countdataF)

countdataF$total <- rowSums(countdataF[,2:8])#count number of pictures by TUD

Flickrpercentage <- countdataF %>%
  group_by(trail) %>% 
  mutate_each(funs(./total)) %>%
  mutate_at(vars(2:9),
            .funs = funs(. * 100))

summary(Flickrpercentage)

Flickfinal <- Flickrpercentage %>%
  mutate(platform = "flickr") %>%
  mutate(LU11 = 0)

Flickfinal <- Flickfinal[,c(1:9,11:12)]

#Wikiloc
countWikiloc <- wikilocSERVICEland %>%
  group_by(Trail) %>%
  count(LU)

countdataW <-  countWikiloc %>%
  pivot_wider(
    names_from = LU,
    values_from = n,
    values_fill = 0,
    values_fn = sum)

countdataW$total <- rowSums(countdataW[,2:9])#count number of pictures by TUD

wikilocpercentage <- countdataW %>%
  group_by(Trail) %>% 
  mutate_each(funs(./total)) %>%
  mutate_at(vars(2:9),
            .funs = funs(. * 100))

summary(wikilocpercentage)

Wikilocfinal <- wikilocpercentage %>%
  mutate(platform = "wikiloc") %>%
  mutate(LU5 = 0)

Wikilocfinal <- Wikilocfinal[,c(1:9,11:12)]

#group wikiloc and flickr
wikiflickr <- rbind(Flickfinal, Wikilocfinal)
summary(wikiflickr)

write_xlsx(wikiflickr,"WikilocFlickr.xlsx")


#images services and land cover NUMBER-----
#Flickr
serviceFlickr <- flickrSERVICEland %>%
  group_by(Trail) %>%
  count(LU,service)

servicecountdataF <-  serviceFlickr %>%
  pivot_wider(
    names_from = c(LU,service),
    values_from = n,
    values_fill = 0,
    values_fn = sum)
summary(servicecountdataF)
write_xlsx(servicecountdataF,"servicecountdataF.xlsx")

#WIKILOC
serviceWIKILOC <- wikilocSERVICEland %>%
  group_by(Trail) %>%
  count(LU,service)

servicecountdataW <-  serviceWIKILOC %>%
  pivot_wider(
    names_from = c(LU,service),
    values_from = n,
    values_fill = 0,
    values_fn = sum)


#tables species type and land cover by Trail Number----
#Flickr
FlickrspeciesTAGS <- wikflispeciesTAGS %>%
  filter(platform == "flickr")


speciesFcount <- FlickrspeciesTAGS %>%
  group_by(url) %>%
  count(LU,specietyp)

#count_ES in line0-1
speciesF01_count <- speciesFcount %>%
  pivot_wider(names_from = c(LU,specietyp), 
              values_from = n, 
              values_fill = 0,
              values_fn = function(x) 1)

#select url and TRAIL
selectFlickr <- FlickrspeciesTAGS[,c(4:5)]

selectFlickr1 <- selectFlickr %>%
  group_by(Trail) %>%
  distinct(url)

#joint TRAIL TO url in count 01
joinFlickr <- inner_join(speciesF01_count, 
                         selectFlickr1, 
                         by = "url")

joinFlickr_trail <- aggregate(joinFlickr[,2:24], 
                              by = list(joinFlickr$Trail), 
                              FUN = sum)

write_xlsx(joinFlickr_trail,"speciesFnumber.xlsx")

summary(joinFlickr_trail)

#wikiloc

WikilocspeciesTAGS <- wikflispeciesTAGS %>%
  filter(platform == "wikiloc")

speciesWcount <- WikilocspeciesTAGS %>%
  group_by(url) %>%
  count(LU,specietyp)


#count_ES in line0-1
speciesF01_count <- speciesWcount %>%
  pivot_wider(names_from = c(LU,specietyp), 
              values_from = n, 
              values_fill = 0,
              values_fn = function(x) 1)

#select url and TRAIL
selectWikiloc <- WikilocspeciesTAGS[,c(4:5)]

selectWikiloc1 <- selectWikiloc %>%
  group_by(Trail) %>%
  distinct(url)

#joint TRAIL TO url in count 01
joinWikiloc <- inner_join(speciesF01_count, 
                          selectWikiloc1, 
                          by = "url")

joinWikiloc_trail <- aggregate(joinWikiloc[,2:29], 
                               by = list(joinWikiloc$Trail), 
                               FUN = sum)

write_xlsx(joinWikiloc_trail,"speciesWnumber.xlsx")

##################################################################################
#statistics------------------------------------------------
#services
FlickrESland <- wikiflickrSERn %>%
  filter(platform == "flickr")

FlickrESland1 <- FlickrESland[,c(2:22)]

WikilocESland <- wikiflickrSERn %>%
  filter(platform == "wikiloc")

WikilocESland1 <- WikilocESland[,c(2:22)]

#wilcoxon test_Flickr
FlickrESland1 <- as.matrix(FlickrESland1)

pvalue <- matrix(nrow = length(colnames(FlickrESland1)), 
                 ncol = (length(colnames(FlickrESland1))))  

for(i in 1:length(colnames(FlickrESland1))){
  for(j in 1:length(colnames(FlickrESland1))){
    pvalue[i,j]<-wilcox.test(FlickrESland1[,i], 
                             FlickrESland1[,j], 
                             paired=TRUE)$p.value 
    colnames(pvalue) <- colnames(FlickrESland1)
    rownames(pvalue) <- colnames(FlickrESland1)} }


#select desired paired 
#turn matrix to dataframe by keeping only non duplicate and NA paired
pvalue[upper.tri(pvalue)] = NA
df4 <- reshape2::melt(pvalue, na.rm=T)

#select wanted paired
idx <- sub('.*\\_', '', df4$Var1) == sub('.*\\_', '', df4$Var2)

df5 <- df4[idx,]

df6 <- df4 %>% 
  filter(str_sub(Var1,1,1)==str_sub(Var2,1,1))

df7 <- df6[c(1,3,6:10,12,16,17,20:29), ]

Fpvalue <- rbind(df7, df5)

#Adjust P-values for Multiple Comparisons using BY

Fpvalue$pval_ajust <- p.adjust(Fpvalue$value, method="BY")

#pvalue to stars ***
Fpvalue$stars <- cut(Fpvalue[[4]], breaks = c(-Inf, 0.001, 
                                              0.01, 0.05, Inf), 
                     labels = c("***", "**", "*", "n.s."), 
                     right = FALSE)

write_xlsx(Fpvalue,"serviceFpvalue.xlsx")


#wilcoxon test_Wikiloc-----
WikilocESland1 <- as.matrix(WikilocESland1)

pvalue <- matrix(nrow = length(colnames(WikilocESland1)), 
                 ncol = (length(colnames(WikilocESland1))))  

for(i in 1:length(colnames(WikilocESland1))){
  for(j in 1:length(colnames(WikilocESland1))){
    pvalue[i,j]<-wilcox.test(WikilocESland1[,i], 
                             WikilocESland1[,j], 
                             paired=TRUE)$p.value 
    colnames(pvalue) <- colnames(WikilocESland1)
    rownames(pvalue) <- colnames(WikilocESland1)} }


#select desired paired 
#turn matrix to dataframe by keeping only non duplicate and NA paired
pvalue[upper.tri(pvalue)] = NA
df4 <- reshape2::melt(pvalue, na.rm=T)

#select wanted paired
idx <- sub('.*\\_', '', df4$Var1) == sub('.*\\_', '', df4$Var2)

df5 <- df4[idx,]

df6 <- df4 %>% 
  filter(str_sub(Var1,1,1)==str_sub(Var2,1,1))

df7 <- df6[c(1,3,6:10,12,16,17,20:30), ]

Wpvalue <- rbind(df7, df5)

#Adjust P-values for Multiple Comparisons using BY

Wpvalue$pval_ajust <- p.adjust(Wpvalue$value, method="BY")

#pvalue to stars ***
Wpvalue$stars <- cut(Wpvalue[[4]], breaks = c(-Inf, 0.001, 
                                              0.01, 0.05, Inf), 
                     labels = c("***", "**", "*", "n.s."), 
                     right = FALSE)

write_xlsx(Wpvalue,"serviceWpvalue.xlsx")


#compared Flicker vs Wikiloc (Wilcoxon paired = False)
#NUMBERS
wikiflickrSERn$platform <- as.factor(wikiflickrSERn$platform)
#speciesFW
speciesFW <- wikiflickrSERn[,c(8:10,16:17,19,22:23)]

#wilcoxon
Pvaluespecies <- speciesFW %>%
  select(-platform) %>% 
  map(~wilcox.test(.x~platform, 
                   paired = FALSE,
                   data=speciesFW)) %>% 
  map_dbl("p.value")

#select var numeriques
var <- names(speciesFW)[c(1:7)] 
pval_var <- data_frame(var,Pvaluespecies)

pval_var$pval_ajust <- p.adjust(pval_var$Pvaluespecies, 
                                method="BY")

#pvalue to stars ***
pval_var$stars <- cut(pval_var[[3]], breaks = c(-Inf, 0.001, 
                                                0.01, 0.05, Inf), 
                      labels = c("***", "**", "*", "n.s."), 
                      right = FALSE)

write_xlsx(pval_var,"pval_var.xlsx")

#closeFW
closeFW <- wikiflickrSERn[,c(4,6,11,12,15,20,21,23)]

#wilcoxon
Pvalueclose <- closeFW %>%
  select(-platform) %>% 
  map(~wilcox.test(.x~platform, 
                   paired = FALSE,
                   data=closeFW)) %>% 
  map_dbl("p.value")

#select var numeriques
var <- names(closeFW)[c(1:7)] 
pval_varC <- data_frame(var,Pvalueclose)

pval_varC$pval_ajust <- p.adjust(pval_varC$Pvalueclose, 
                                 method="BY")

#pvalue to stars ***
pval_varC$stars <- cut(pval_varC[[3]], breaks = c(-Inf, 0.001, 
                                                  0.01, 0.05, Inf), 
                       labels = c("***", "**", "*", "n.s."), 
                       right = FALSE)

write_xlsx(pval_varC,"pval_varC.xlsx")

#OpenFw
OpenFW <- wikiflickrSERn[,c(2:3,5,7,13,14,18,23)]

#wilcoxon
PvalueOpen <- OpenFW %>%
  select(-platform) %>% 
  map(~wilcox.test(.x~platform, 
                   paired = FALSE,
                   data=OpenFW)) %>% 
  map_dbl("p.value")

#select var numeriques
var <- names(OpenFW)[c(1:7)] 
pval_varO <- data_frame(var,PvalueOpen)

pval_varO$pval_ajust <- p.adjust(pval_varO$PvalueOpen, 
                                 method="BY")

#pvalue to stars ***
pval_varO$stars <- cut(pval_varO[[3]], breaks = c(-Inf, 0.001, 
                                                  0.01, 0.05, Inf), 
                       labels = c("***", "**", "*", "n.s."), 
                       right = FALSE)

write_xlsx(pval_varO,"pval_varO.xlsx")

#Flickr vs Wikiloc 
#speciesFW
speciesFW <- wikiflickrSER[,c(8:10,16:17,19,22:23)]

#wilcoxon
Pvaluespecies <- speciesFW %>%
  select(-platform) %>% 
  map(~wilcox.test(.x~platform, 
                   paired = FALSE,
                   data=speciesFW)) %>% 
  map_dbl("p.value")

#select var numeriques
var <- names(speciesFW)[c(1:7)] 
pval_var <- data_frame(var,Pvaluespecies)

pval_var$pval_ajust <- p.adjust(pval_var$Pvaluespecies, 
                                method="BY")

#pvalue to stars ***
pval_var$stars <- cut(pval_var[[3]], breaks = c(-Inf, 0.001, 
                                                0.01, 0.05, Inf), 
                      labels = c("***", "**", "*", "n.s."), 
                      right = FALSE)

write_xlsx(pval_var,"pval_var.xlsx")


#closeFW
closeFW <- wikiflickrSER[,c(4,6,11,12,15,20,21,23)]

#wilcoxon
Pvalueclose <- closeFW %>%
  select(-platform) %>% 
  map(~wilcox.test(.x~platform, 
                   paired = FALSE,
                   data=closeFW)) %>% 
  map_dbl("p.value")

#select var numeriques
var <- names(closeFW)[c(1:7)] 
pval_varC <- data_frame(var,Pvalueclose)

pval_varC$pval_ajust <- p.adjust(pval_varC$Pvalueclose, 
                                 method="BY")

#pvalue to stars ***
pval_varC$stars <- cut(pval_varC[[3]], breaks = c(-Inf, 0.001, 
                                                  0.01, 0.05, Inf), 
                       labels = c("***", "**", "*", "n.s."), 
                       right = FALSE)

write_xlsx(pval_varC,"pval_varC.xlsx")

#OpenFw
OpenFW <- wikiflickrSER[,c(2:3,5,7,13,14,18,23)]

#wilcoxon
PvalueOpen <- OpenFW %>%
  select(-platform) %>% 
  map(~wilcox.test(.x~platform, 
                   paired = FALSE,
                   data=OpenFW)) %>% 
  map_dbl("p.value")

#select var numeriques
var <- names(OpenFW)[c(1:7)] 
pval_varO <- data_frame(var,PvalueOpen)

pval_varO$pval_ajust <- p.adjust(pval_varO$PvalueOpen, 
                                 method="BY")

#pvalue to stars ***
pval_varO$stars <- cut(pval_varO[[3]], breaks = c(-Inf, 0.001, 
                                                  0.01, 0.05, Inf), 
                       labels = c("***", "**", "*", "n.s."), 
                       right = FALSE)

write_xlsx(pval_varO,"pval_varO.xlsx")


#SPECIES
#Flickr_species
Flickrspecies <- wfSPECIESn %>%
  filter(platform == "flickr")
Flickrspecies1 <- Flickrspecies[,c(2:50)]

#wilcoxon test_Flickr
Flickrspecies1 <- as.matrix(Flickrspecies1)

pvalue <- matrix(nrow = length(colnames(Flickrspecies1)), 
                 ncol = (length(colnames(Flickrspecies1))))  

for(i in 1:length(colnames(Flickrspecies1))){
  for(j in 1:length(colnames(Flickrspecies1))){
    pvalue[i,j]<-wilcox.test(Flickrspecies1[,i], 
                             Flickrspecies1[,j], 
                             paired=TRUE)$p.value 
    colnames(pvalue) <- colnames(Flickrspecies1)
    rownames(pvalue) <- colnames(Flickrspecies1)} }


#select desired paired 
#turn matrix to dataframe by keeping only non duplicate and NA paired
pvalue[upper.tri(pvalue)] = NA
df4 <- reshape2::melt(pvalue, na.rm=T)

#select wanted paired
idx <- sub('.*\\_', '', df4$Var1) == sub('.*\\_', '', df4$Var2)

df5 <- df4[idx,]

df6 <- df4 %>% 
  filter(str_sub(Var1,1,1)==str_sub(Var2,1,1))

df7 <- df6[c(1:4,8:9,14:16,20:21,26:67,71:72,77,81,82,
             90:91,96:103,106:110,113:130,133:137), ]

fpvalue <- rbind(df7, df5)

fpvalue$pval_ajust <- p.adjust(fpvalue$value, method="BY")

#pvalue to stars ***
fpvalue$stars <- cut(fpvalue[[4]], breaks = c(-Inf, 0.001, 
                                              0.01, 0.05, Inf), 
                     labels = c("***", "**", "*", "n.s."), 
                     right = FALSE)

write_xlsx(fpvalue,"speciesFpvalue.xlsx")

#Wikiloc species
wikilocspecies <- wfSPECIESn %>%
  filter(platform == "wikiloc")

wikilocspecies1 <- wikilocspecies[,c(2:50)]

#wilcoxon test_Flickr
wikilocspecies1 <- as.matrix(wikilocspecies1)

pvalue <- matrix(nrow = length(colnames(wikilocspecies1)), 
                 ncol = (length(colnames(wikilocspecies1))))  

for(i in 1:length(colnames(wikilocspecies1))){
  for(j in 1:length(colnames(wikilocspecies1))){
    pvalue[i,j]<-wilcox.test(wikilocspecies1[,i], 
                             wikilocspecies1[,j], 
                             paired=TRUE)$p.value 
    colnames(pvalue) <- colnames(wikilocspecies1)
    rownames(pvalue) <- colnames(wikilocspecies1)} }


#select desired paired 
#turn matrix to dataframe by keeping only non duplicate and NA paired
pvalue[upper.tri(pvalue)] = NA
df4 <- reshape2::melt(pvalue, na.rm=T)

#select wanted paired
idx <- sub('.*\\_', '', df4$Var1) == sub('.*\\_', '', df4$Var2)

df5 <- df4[idx,]

df6 <- df4 %>% 
  filter(str_sub(Var1,1,1)==str_sub(Var2,1,1))

df7 <- df6[c(1:4,8:9,14:16,20:21,
             26:65,69:70,75,79:80,
             88:89,94:101,104,107:110,111:121,124:128,
             133:137,142:157), ]

Wpvalue <- rbind(df7, df5)

Wpvalue$pval_ajust <- p.adjust(Wpvalue$value, method="BY")

#pvalue to stars ***
Wpvalue$stars <- cut(Wpvalue[[4]], breaks = c(-Inf, 0.001, 
                                              0.01, 0.05, Inf), 
                     labels = c("***", "**", "*", "n.s."), 
                     right = FALSE)

#################################################################################à
#compared Flicker vs Wikiloc species (Wilcoxon paired = False)
#NUMBERS
wfSPECIESn$platform <- as.factor(wfSPECIESn$platform)

#species type
wfSPECIESn1 <- wfSPECIESn[,c(5,8,24,26,39,46,49,51)]  #livestock
wfSPECIESn1 <- wfSPECIESn[,c(12,15,17,19,22,37,43,51)]  #bird
wfSPECIESn1 <- wfSPECIESn[,c(2,25,27,35,36,44,48,51)]  #fungi
wfSPECIESn1 <- wfSPECIESn[,c(3,4,7,11,18,21,47,51)]  #plant & flower
wfSPECIESn1 <- wfSPECIESn[,c(10,13,16,20,23,38,45,51)]  #invertebrate



#wilcoxon
comparFW <- wfSPECIESn1 %>%
  dplyr::select(-platform) %>% 
  map(~wilcox.test(.x~platform, 
                   paired = FALSE,
                   data=wfSPECIESn1)) %>% 
  map_dbl("p.value")

#select var numeriques
var <- names(wfSPECIESn1)[c(1:7)] 
pval_var <- data_frame(var,comparFW)

pval_var$pval_ajust <- p.adjust(pval_var$comparFW, 
                                method="BY")

#pvalue to stars ***
pval_var$stars <- cut(pval_var[[3]], breaks = c(-Inf, 0.001, 
                                                0.01, 0.05, Inf), 
                      labels = c("***", "**", "*", "n.s."), 
                      right = FALSE)

write_xlsx(pval_var,"pval_var.xlsx")

#################################################################################
#create tables 1, 2, 3 and 4
#table statistics with significant letters---------------
#column
#service_Flickr and Wikiloc
idx <- sub('.*\\_', '', serviceFpvalue$Var1) == sub('.*\\_', '', 
                                                    serviceFpvalue$Var2)

df5 <- serviceFpvalue[idx,]

species <- subset(df5,grepl("^.+(openland)$",Var1))  #select species, close or open 


#significance by letters

species$comparaison <- paste(species$Var1, 
                             species$Var2, sep="-")
diff <- species$pval_ajust     #select the pvalues
names(diff) <- species$comparaison  #define names

diff_let <-  multcompLetters(diff,
                             compare = "<",
                             threshold = 0.05)

diff_let


#rows 

df6 <- serviceFpvalue %>% 
  filter(str_sub(Var1,1,1)==str_sub(Var2,1,1))
land1 <- subset(df6,grepl("^9",Var1))

#significance by letters

land1$comparaison <- paste(land1$Var1, 
                           land1$Var2, sep="-")
diff <- land1$pval_ajust     #select the pvalues
names(diff) <- land1$comparaison  #define names

diff_let <-  multcompLetters(diff,
                             compare = "<",
                             threshold = 0.05)

diff_let


#species Flickr and Wikiloc
#columns
idx <- sub('.*\\_', '', speciesFpvalue$Var1) == sub('.*\\_', '', 
                                                    speciesFpvalue$Var2)

df5 <- speciesFpvalue[idx,]

species <- subset(df5,grepl("^.+(invertebrate)$",Var1))

#significance by letters

species$comparaison <- paste(species$Var1, 
                             species$Var2, sep="-")
diff <- species$pval_ajust     #select the pvalues
names(diff) <- species$comparaison  #define names

diff_let <-  multcompLetters(diff,
                             compare = "<",
                             threshold = 0.05)

diff_let



#rows (forest_bird vs forest_fungus)
df6 <- speciesFpvalue %>% 
  filter(str_sub(Var1,1,1)==str_sub(Var2,1,1))

land28 <- subset(df6,grepl("^1",Var1))

#significance by letters

land28$comparaison <- paste(land28$Var1, 
                            land28$Var2, sep="-")
diff <- land28$pval_ajust     #select the pvalues
names(diff) <- land28$comparaison  #define names

diff_let <-  multcompLetters(diff,
                             compare = "<",
                             threshold = 0.05)

diff_let
######################################################################################
#stat nature vs supply ----
#summary 
Flickr <- NATURE2PLATLAND %>%
  filter(platform == "flickr")

summary(Flickr)

wikiloc <- NATURE2PLATLAND %>%
  filter(platform == "wikiloc")

summary(wikiloc)

#one sample Wilcoxon signed rank test: PLATFORM VS AUVERGNE PONDERE)
#FLICKR VS AUVERGNE

FLI_AUV <- NATURE2PLATLAND %>%
  filter(platform == "flickr" | platform == "auvergne")

WIC_AUV <- NATURE2PLATLAND %>%
  filter(platform == "wikiloc" | platform == "auvergne")

#test
P_Flickr <- sapply(X = FLI_AUV[,2:8],
                   FUN = function(x) wilcox.test(x = x[-which(as.factor(FLI_AUV$platform)=="auvergne")],
                                                 y = x[which(as.factor(FLI_AUV$platform)=="auvergne")],
                                                 paired = F)$p.value)

P_Flickr
pvalueflickr<- as.data.frame(P_Flickr)


P_Wikiloc <- sapply(X = WIC_AUV[,2:8],
                    FUN = function(x) wilcox.test(x = x[-which(as.factor(WIC_AUV$platform)=="auvergne")],
                                                  y = x[which(as.factor(WIC_AUV$platform)=="auvergne")],
                                                  paired = F)$p.value)

P_Wikiloc

pvalueWIKILO <- as.data.frame(P_Wikiloc)

#pvalue to stars ***
pvalueWIKILO$stars <- cut(pvalueWIKILO[[1]], breaks = c(-Inf,0.001,
                                                        0.01, 0.05, Inf), 
                          labels = c("***", "**", "*", "n.s."), 
                          right = FALSE)

write_xlsx(pval_var,"pval_var.xlsx")


#####################################################################################################
#visualization graphs 
#figure 3: maps and monthly distribution 
#open shape auv
AuvSHAPE <- sf::st_read(file.path("C:/Users/User/Desktop/Pepers work 2024/imageARTICLE/GIS2024/GIS sur R/AuvergneLimit.shp")) %>%
  st_transform(4326)
AuvSHAPE <- AuvSHAPE[,1] 

#open grid shapefile 
grid_auv <-  sf::st_read(file.path("C:/Users/User/Desktop/Pepers work 2024/imageARTICLE/GIS2024/GIS sur R/grid_auv.shp")) %>%
  st_transform(4326)

#open shape parks
park_auv <- sf::st_read(file.path("C:/Users/User/Desktop/Pepers work 2024/imageARTICLE/GIS2024/GIS sur R/PNR_A.shp")) %>%
  st_transform(4326)


#REMOVE UNSED DATA HERE TO CLEAN A BIT
rm(grid_auv_join)


#social media data
#Wikiloc 
Wikiloc_CLOSE <- fliwikDATA %>%
  filter(platform == "wikiloc") %>%
  filter(servicetyp == "closeland")

Wikiloc_OPEN <- fliwikDATA %>%
  filter(platform == "wikiloc") %>%
  filter(servicetyp == "openland")

Wikiloc_SPECIE <- fliwikDATA %>%
  filter(platform == "wikiloc") %>%
  filter(servicetyp == "specie")

# turn it into an sf object, for spatial plotting
my_sf_wiki_C <- Wikiloc_CLOSE %>%          
  st_as_sf(coords = c('longitude', 'latitude')) %>%
  st_set_crs(4326) # using 4326 for lat/lon decimal 

my_sf_wiki_O <- Wikiloc_OPEN %>%          
  st_as_sf(coords = c('longitude', 'latitude')) %>%
  st_set_crs(4326)

my_sf_wiki_S <- Wikiloc_SPECIE %>%          
  st_as_sf(coords = c('longitude', 'latitude')) %>%
  st_set_crs(4326)

#spatial join images of close, open and specie with polygon grid

grid_auv$n_countCW <- st_intersects(grid_auv, my_sf_wiki_C) %>%
  lengths()

grid_auv$n_countOW <- st_intersects(grid_auv, my_sf_wiki_O) %>%
  lengths()

grid_auv$n_countSW <- st_intersects(grid_auv, my_sf_wiki_S) %>%
  lengths()

#CREATE 2 CATEGORIES 0 AND 1

grid_auv$countCW <- ifelse(grid_auv$n_countCW>0, 1, 0)
grid_auv$countOW <- ifelse(grid_auv$n_countOW>0, 1, 0)
grid_auv$countSW <- ifelse(grid_auv$n_countSW>0, 1, 0)


#Flickr had issues with coordinates _ join done in arcgis
dataFlickr$countCF <- ifelse(dataFlickr$n_countCF>0, 1, 0)
dataFlickr$countOF <- ifelse(dataFlickr$n_countOF>0, 1, 0)
dataFlickr$countSF <- ifelse(dataFlickr$n_countSF>0, 1, 0)

grid_auv_join <- inner_join(grid_auv, 
                            dataFlickr, 
                            by = "OBJECTID")


#Bivarite
databiv_open <- bi_class(grid_auv_join, 
                         x = countOW, 
                         y = countOF, 
                         style = "equal", 
                         dim = 2)

databiv_close <- bi_class(grid_auv_join, 
                          x = countCW, 
                          y = countCF, 
                          style = "equal", 
                          dim = 2)

databiv_bio <- bi_class(grid_auv_join, 
                        x = countSW, 
                        y = countSF, 
                        style = "equal", 
                        dim = 2)
# create maps open close et biodiversity
#close map
map_close <- ggplot() +
  geom_sf(data = databiv_close, mapping = aes(fill = bi_class), 
          color = NA, size = 0.1, show.legend = T) +
  geom_sf(data = park_auv, aes(color= "black"), linewidth = 1, fill=NA)+
  scale_x_continuous(breaks = seq(2.5, 4.5, by = 1))+
  scale_fill_manual(labels = c("No data", "Flickr only", "Wikiloc only", "Both platforms"),
                    values = c("#D6D2C7","#49A5D0","#ECAA53","#BC4749"))+
  labs(
    title = "Landscape aesthetics (close view)") +
  theme_minimal()+
  guides(color = guide_legend(override.aes = list(color = "black"))) +
  scale_color_identity(labels = c(black = "Nature parks"), guide = "legend")+
  theme(legend.title= element_blank(),
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 45, color = "black"),
        plot.title = element_text(size=55, face = "bold"))


#open map
map_open <- ggplot() +
  geom_sf(data = databiv_open, mapping = aes(fill = bi_class), 
          color = NA, size = 0.1, show.legend = T) +
  geom_sf(data = park_auv, aes(color = "black"), linewidth = 1, fill=NA)+
  scale_x_continuous(breaks = seq(2.5, 4.5, by = 1))+
  scale_fill_manual(labels = c("No data", "Flickr only", "Wikiloc only", "Both platforms"),
                    values = c("#D6D2C7","#49A5D0","#ECAA53","#BC4749"))+
  labs(
    title = "Landscape aesthetics (open view)") +
  theme_minimal()+
  guides(color = guide_legend(override.aes = list(color = "black",  linewidth = 1, fill=NA))) +
  scale_color_identity(labels = c(black = "Nature parks"), guide = "legend")+
  theme(legend.title= element_blank(),
        legend.text = element_text(size=55),
        legend.position = "right",
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 45, color = "black"),
        plot.title = element_text(size=55, face = "bold"),
        legend.key.height= unit(1, 'cm'),
        legend.key.width= unit(1.5, 'cm'))


# Add scale and North arrow
map_open1 <- map_open+
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(0.3, "cm"), pad_y = unit(0.3, "cm"),
    height = unit(2.5, "cm"),
    width = unit(2.5, "cm"),
    style = ggspatial::north_arrow_orienteering(
      fill = c("black", "white"),
      text_size = 25,
      line_col = "black"))

#bio map
map_bio <- ggplot() +
  geom_sf(data = databiv_bio, mapping = aes(fill = bi_class), 
          color = NA, size = 0.1, show.legend = T) +
  geom_sf(data = park_auv, aes(color = "black"), linewidth = 1, fill=NA)+
  scale_x_continuous(breaks = seq(2.5, 4.5, by = 1))+
  scale_fill_manual(labels = c("No data", "Flickr only", "Wikiloc only", "Both platforms"),
                    values = c("#D6D2C7","#49A5D0","#ECAA53","#BC4749"))+
  labs(
    title = "Species observation", tag = "(a)") +
  theme_minimal()+
  guides(color = guide_legend(override.aes = list(color = "black"))) +
  scale_color_identity(labels = c(black = "Nature parks"), guide = "legend")+
  theme(legend.title= element_blank(),
        text = element_text(size = 55),
        legend.position = "none",
        axis.text.y = element_text(size = 45, color = "black"),
        axis.text.x = element_text(size = 45, color = "black"),
        plot.title = element_text(size=55, face = "bold"))
map_bio



#merge 3 maps
gg_maps <- (map_bio + map_close +  map_open1+ 
              plot_layout(ncol = 3))
gg_maps

?ggsave

#temporal distribution
#set the language to English
Sys.setlocale("LC_TIME", "English")

#keep only the day month and year
tempoDATA$datee <- str_sub(tempoDATA$Date, start=1, end= 10)

#too platform have differents date type
tempoWikiloc <- tempoDATA %>%
  filter(platform == "wikiloc")

tempoWikiloc$date_name <- format(as.Date(tempoWikiloc$datee),'%b')

tempoWikiloc <- tempoWikiloc[,c(4,5,7)]

tempofli <- tempoDATA %>%
  filter(platform == "flickr")

tempofli$date_name <- format(as.Date(tempofli$datee),'%b')

tempofli <- tempofli[,c(4,5,7)]


tempomonth <- rbind(tempofli, tempoWikiloc)

stat_tempo <- tempomonth %>%
  group_by(platform, servicetyp, date_name) %>% 
  tally()


write_xlsx(stat_tempo,"stat_tempo.xlsx")

#FROM stat_tempo 
percetage <- stat_tempo %>%
  group_by(platform, servicetyp) %>% 
  mutate(percent = n/sum(n) * 100)

#stat descri by season
saisonDATA <- stat_tempo %>%
  group_by(platform, servicetyp) %>% 
  mutate(season = fct_collapse(
    .f = date_name,
    Spring = c("Mar", "Apr", "May"),
    Summer = c("Jun", "Jul", "Aug"),
    Autumn = c("Sep", "Oct", "Nov"),
    Winter = c("Dec", "Jan", "Feb")))

saisonDATA$season <- as.character(saisonDATA$season)

nSAISON <- saisonDATA %>%
  group_by(season, servicetyp, platform) %>%
  mutate(nSEASON = sum(n))


percentageSAISON <- nSAISON %>%
  group_by(platform, servicetyp) %>%
  distinct(season, nSEASON)%>%
  mutate(percent = nSEASON/sum(nSEASON) * 100)



#graph tempo
percetage$date_name <- factor(percetage$date_name , c("Jan","Feb","Mar",
                                                      "Apr","May","Jun","Jul","Aug",
                                                      "Sep","Oct","Nov","Dec"))
#biodi
tempo_bio <- percetage %>%
  filter(servicetyp == "specie")

Graphmonth_bio <- tempo_bio %>%
  ggplot(aes(x=date_name, y=percent, fill = platform)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  labs(
    y= "% of images",
    x = "")+
  scale_fill_manual(labels = c("Flickr", "Wikiloc"),
                    values = c("#49A5D0","#ECAA53"))+
  labs(
    title = "", tag = "(b)") +
  theme_minimal()+
  theme(legend.title= element_blank(),
        legend.position = "none",
        text = element_text(size = 55),
        axis.line = element_line(color = "black", linewidth = 1.2),
        axis.title.y = element_text(size = 50, color = "black", face = "bold"),
        axis.text.y = element_text(size = 45, color = "black"),
        axis.text.x = element_text(size = 45, color = "black", angle = 60, vjust = 1, hjust=1))+
  scale_y_continuous(expand = expansion(0),
                     limits = c(0,35),
                     breaks = seq(0,35,10))

Graphmonth_bio

#close land
tempo_close <- percetage %>%
  filter(servicetyp == "closeland")

Graphmonth_close <- tempo_close %>%
  ggplot(aes(x=date_name, y= percent, fill = platform)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  labs(
    y= "",
    x = "")+
  scale_fill_manual(labels = c("Flickr", "Wikiloc"),
                    values = c("#49A5D0","#ECAA53"))+
  labs(
    title = "") +
  theme_minimal()+
  theme(legend.title= element_blank(),
        legend.position = "none",
        axis.line.x = element_line(color = "black", linewidth = 1.2),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 45, color = "black", angle = 60, vjust = 1, hjust=1))+
  scale_y_continuous(expand = expansion(0),
                     limits = c(0,35),
                     breaks = seq(0,35,10))

Graphmonth_close

#open land
tempo_open <- percetage %>%
  filter(servicetyp == "openland")

Graphmonth_open <- tempo_open %>%
  ggplot(aes(x=date_name, y= percent, fill = platform)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  labs(
    y= "",
    x = "")+
  scale_fill_manual(labels = c("Flickr", "Wikiloc"),
                    values = c("#49A5D0","#ECAA53"))+
  labs(
    title = "") +
  theme_minimal()+
  theme(legend.title= element_blank(),
        legend.text = element_text(size=55),
        legend.key.height= unit(1, 'cm'),
        legend.key.width= unit(1.5, 'cm'),
        axis.text.y = element_blank(),
        axis.line.x = element_line(color = "black", linewidth = 1.2),
        axis.text.x = element_text(size = 45, color = "black", 
                                   angle = 60, vjust = 1, hjust=1))+
  scale_y_continuous(expand = expansion(0),
                     limits = c(0,35),
                     breaks = seq(0,35,10))

Graphmonth_open


#rassembler 
gg_tot <- (map_bio + map_close +  map_open1+
             Graphmonth_bio + Graphmonth_close + Graphmonth_open + 
             plot_layout(heights = c(2, 1)))  &
  theme(legend.justification = "left")
gg_tot

ggsave("distrimap.png",width=49.5,height=35, dpi = 300)


################################################################################
#Fig 4 nature images vs Auvergne 

supply$orderPLAT = factor(supply$plat, 
                          levels=c('Weighted availability in Auvergne',
                                   'Flickr', 'Wikiloc'))

supply$orderLAND = factor(supply$land, 
                          levels=c('Forests','Shrublands','Grasslands',
                                   'Heterogeneous agriculture',
                                   'Arable land','Rocks','Water and wetlands'))

Fig_supply <- supply %>%
  ggplot(aes(x=orderLAND, y= val, fill = orderPLAT))+
  geom_bar(stat = "identity", colour = "black", 
           width = .75, size=0.04, position=position_dodge())+
  scale_x_discrete(labels = wrap_format(10))+
  labs(y = "Percent of land covers", x = "")+
  theme(
    strip.text = element_text(size=30, face = "bold", ), 
    plot.margin = unit(c(0.5, .5, -1, .5), "cm"),
    panel.background = element_blank(),
    axis.line = element_line(color = "black", linewidth = 1.2),
    axis.text.y = element_text(size = 30, color = "black"),
    axis.title = element_text(size= 30, color="black", face = "bold"),
    axis.text.x = element_text(angle = 60,vjust = 1, hjust=1,
                               size = 30, color = "black"))+
  scale_fill_manual(values = c("#999999","#49A5D0","#ECAA53"))+
  scale_y_continuous(limits = c(0,45),
                     breaks = seq(0,45,10),
                     expand = expansion(mult= 0))+
  theme(legend.position =c(0.79, 0.92),
        legend.title= element_blank(),
        legend.text = element_text(size=27),
        legend.key.height= unit(1, 'cm'),
        legend.key.width= unit(1, 'cm'))


#add p value stars

Fig_supply1 <- Fig_supply +
  geom_signif(
    y_position = c(31, 5.2, 41, 19.5, 12, 0.7, 4.2), 
    xmin = c(0.7, 1.7, 2.7, 3.7, 4.7, 5.7, 6.7), 
    xmax = c(1, 2, 3, 4, 5, 6, 7),
    annotation = c("ns", "*", "ns", "ns", "***", "***", "**"), 
    tip_length = 0, textsize = 7.5, size = 1) 


Fig_supply2 <- Fig_supply1 +
  geom_signif(
    y_position = c(36.2, 7.6, 43, 22, 14.7, 3, 6.7), 
    xmin = c(0.7, 1.7, 2.7, 3.7, 4.7, 5.7, 6.7), 
    xmax = c(1.25, 2.25, 3.25 , 4.25 , 5.25, 6.25, 7.25),
    annotation = c("ns", "*", "ns", "ns", "***", "***", "***"), 
    tip_length = 0, textsize = 7.5, size = 1) 
Fig_supply2

ggsave("supply.png",width=17,height=10, dpi = 400)


#################################################################################
#Fig 5 Flickr and Wikiloc for species observation and landscape aesthetics
DifFLIwikinonsig$orderservice = factor(DifFLIwikinonsig$service, 
                                       levels=c('Species observation',
                                                'Landscape aesthetics (close view)',
                                                'Landscape aesthetics (open view)',
                                                'Bird','Fungi','Invertebrate',
                                                'Livestock','Plant & flower'))
DifFLIwikinonsig$ordertyp= factor(DifFLIwikinonsig$typ, 
                                  levels=c('CES','Species groups'))

DifFLIwiki <- DifFLIwikinonsig %>%
  filter(land == "Grasslands" | land == "Forests")

DifFLIwiki1 <- DifFLIwikinonsig %>%
  filter(land == "Water and wetlands" | land == "Heterogeneous agriculture")

#graphs
plot1 <- DifFLIwiki %>%
  ggplot(aes(x=orderservice, y= val, fill = plat)) +
  geom_bar(stat = "identity", colour = "black", 
           width = .65, size=0.07)+
  scale_fill_manual(values = c("#559CAD","#559CAD"))+
  facet_nested(~land+ ordertyp, scale = "free_x", space = "free_x")+
  #scale_x_discrete(labels = function(x) str_wrap(x, width = 14))+
  scale_y_continuous(limits=c(-0.5,0.5), 
                     breaks = c(-0.5,-0.25,0,0.25,0.5))+
  geom_text(DifFLIwiki, mapping = aes(label=star),
            stat="identity", 
            position = "stack", size = 30, hjust = 0.5,
            angle =0, vjust = -0.15, color = "black")+
  geom_hline(yintercept = 0, linewidth = 3)+
  labs(x= NULL,y = NULL)+
  theme(
    strip.text = element_text(size=54, face = "bold", ), 
    strip.background = element_rect(fill="#D5D4CE"),
    legend.position = "none",
    plot.margin = unit(c(0.5, .5, .5, .5), "cm"),
    panel.background = element_blank(),
    axis.line.y = element_line(color = "black", size = 2),
    axis.text.y = element_text(size = 55, color = "black"),
    axis.text.x = element_blank())

plot1+ theme(panel.spacing = unit(3, "lines"))

plot1


plot2 <- DifFLIwiki1 %>%
  ggplot(aes(x=orderservice, y= val, fill = plat)) +
  geom_bar(stat = "identity", colour = "black", 
           width = .65, size=0.07)+
  scale_fill_manual(values = c("#559CAD","#559CAD"))+
  facet_nested(~land+ ordertyp, scale = "free_x", space = "free_x")+
  scale_x_discrete(labels = wrap_format(26))+
  scale_y_continuous(limits=c(-0.5,0.5), 
                     breaks = c(-0.5,-0.25,0,0.25,0.5))+
  geom_text(DifFLIwiki1, mapping = aes(label=star),
            stat="identity", 
            position = "stack", size = 30, hjust = 0.5,
            angle =0, vjust = -0.15, color = "black")+
  geom_hline(yintercept = 0, linewidth = 3)+
  labs(x= NULL,y = NULL)+
  theme(
    strip.text = element_text(size=54, face = "bold", ), 
    strip.background = element_rect(fill="#D5D4CE"),
    legend.position = "none",
    plot.margin = unit(c(0.5, .5, .5, .5), "cm"),
    panel.background = element_blank(),
    axis.line = element_line(color = "black", size = 2),
    axis.text.y = element_text(size = 55, color = "black"),
    axis.text.x = element_text(angle = 60,vjust = 1, hjust=1,
                               size = 55, color = "black"))

plot2+ theme(panel.spacing = unit(3, "lines"))

plot2 


#merga figures 
ggp<- (plot1 + plot2 + plot_layout(nrow = 2))
ggp

ggsave("difplat.png",width=38,height=35, dpi = 400)



