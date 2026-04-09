#Updating packages in metadigitise 
update.packages(ask = FALSE)

#Installing metadigitise 
install.packages("devtools")
devtools::install_github("joelpick/metaDigitise")
1
#Installing Shiny Digitise 
devtools::install_github("EIvimeyCook/ShinyDigitise")
#removing se_to_sd before loading in metadigitise due to errors 
rm(se_to_sd)
#loading meta and shiny 
library(metaDigitise)
library(shinyDigitise)

#Running Shiny app 
df<-shinyDigitise("C:/Users/megdc/OneDrive/Documents/Eco-evo change/Meta-analysis/Data (Raw)")
#Extracting data from metadigitise into object 'data'
data<-metaDigitise::getExtracted("C:/Users/megdc/OneDrive/Documents/Eco-evo change/Meta-analysis/Data (Raw)", summary= FALSE)
data
data2<-metaDigitise::getExtracted("C:/Users/megdc/OneDrive/Documents/Eco-evo change/Meta-analysis/Data (Raw)", summary= TRUE)
data2
#Saving data 
saveRDS(data, "extracted_data.rds")
saveRDS(data2, "extracted_data.rds")
data$scatterplot$`Garel(2007)_appdC_hornL_year_age5.png`
#Practice attempts with Joel
extr1<-getExtracted("C:/Users/megdc/OneDrive/Documents/Eco-evo change/Meta-analysis/Data (Raw)", summary= FALSE)
extr1$scatterplot$'Corlatti(2017)_HornL_Yearofcull.jpg'

df<-shinyDigitise("C:/Users/megdc/OneDrive/Documents/Eco-evo change/Meta-analysis/Data (Raw)")

extr2<-getExtracted("C:/Users/megdc/OneDrive/Documents/Eco-evo change/Meta-analysis/Data (Raw)", summary= FALSE)

#data has been extracted
#Creating from supplementary data - raw data from Douhard 
Douhard2016 <- read.delim("C:/Users/megdc/OneDrive/Documents/Eco-evo change/Meta-analysis/Data (webimg)/Douhard(2016) Data/data/data-Figure6.txt")
Douhard2016
names(Douhard2016)
#Converting horn length from mm-cm and converting SE - this is to match other data which is in cm 
Douhard2016$mean.horn.length..mm. <- Douhard2016$mean.horn.length..mm. / 10
Douhard2016$standard.error.of.horn.length <- Douhard2016$standard.error.of.horn.length / 10
Douhard2016
#Renaming columns to fit with data - for ease in model writing 
names(Douhard2016)[names(Douhard2016) == "cohort"] <- "x"
names(Douhard2016)[names(Douhard2016) == "mean.horn.length..mm."] <- "y"
names(Douhard2016)[names(Douhard2016) == "standard.error.of.horn.length"] <- "se"
names(Douhard2016)[names(Douhard2016) == "age.class"]<- "Age"
names(Douhard2016)[names(Douhard2016) == "region"]<- "Pop"

Douhard2016
#Now adding file name - helps separate for running model to get effect size - splitting the two locations into seperate names so we can get an effect size for each 
Douhard2016$filename <- paste("Douhard2016", Douhard2016$Pop, sep = "_")
Douhard2016

#Moving onto scatterplot data - Organising that into similar structure 
#Binding all the scatterplots together first - so I can restructure 
scatter_data <- do.call(rbind, data$scatterplot)
head(scatter_data)
#making sure all the studies are there
length(unique(scatter_data$id))
#names(unique(scatter_data$id)) - didnt do what I thought 
#Extracting age from some of the id filenames - so it can be included as a variable in analyses 
scatter_data$filename <- scatter_data$id
scatter_data
scatter_data$age <- ifelse(grepl("age", scatter_data$filename),as.numeric(sub(".*age", "", scatter_data$filename)),NA)
scatter_data
#Making sure everything is in standard format 
scatter_data$se<-NA
scatter_data<- scatter_data[, c("x","y","se","age","filename")]
#Adding pop column to the scatterplots 
scatter_data$Pop <- ifelse(
  grepl("Corlatti", scatter_data$filename), "Italy",
  ifelse(
    grepl("Garel", scatter_data$filename), "France",
    ifelse(
      grepl("Pelle", scatter_data$filename), "Alberta Canada",
      NA
    )))
#Removing the pop column because spelling needs to be standardised between the two tables before merge 
scatter_data$pop<-NULL
Douhard2016<- Douhard2016[,c("x","y","se","Age","filename","Pop")]
Douhard2016$age<-Douhard2016$Age
Douhard2016$Age<-NULL
Douhard2016<- Douhard2016[,c("x","y","se","age","filename","Pop")]
#Both Scatterplots and Raw data are in standardised format 
#Now moving to xy mean charts 
#pairing rows together - to deal with the alternating row thing from metadigitise 
library(dplyr)
xydata<- data2 %>%
  group_by(filename, group_id) %>%
  mutate(row_id = row_number())
#Assigns 1s to all the rows w y's and se's and 2s to all x values 
#now separate x and y to format them properly before rejoining them 
y_data <- xydata %>%
  group_by(filename, group_id) %>%
  mutate(row_id = row_number()) %>%
  filter(row_id == 1)

x_data <- xydata %>%
  group_by(filename, group_id) %>%
  mutate(row_id = row_number()) %>%
  filter(row_id == 2)
#then recombine...
xy_data <- left_join(
  y_data,
  x_data,
  by = c("filename", "group_id"),
  suffix = c("_y", "_x")
)
#cleaning up data 
xy_data <- xy_data %>%
  transmute(
    filename,
    group_id,
    x = mean_x,
    y = mean_y,
    se = se_y
  )
#removing last 2 rows - they are in the scatterplot 
xy_data <- xy_data[-c(nrow(xy_data)-1, nrow(xy_data)), ]
#its summary data that needs to be removed 
scatter_files <- c("Corlatti(2017)_HornL_Yearofcull.jpg", 
                   "Garel(2007)_appdC_hornL_year_age5.png",
                   "Garel(2007)_appdC_hornL_year_age6.png",
                   "Garel(2007)_appdC_hornL_year_age7.png",
                   "Garel(2007)_appdC_hornL_year_age8.png")

xy_data <- xy_data %>%
  filter(!filename %in% scatter_files)
#removing Junk rows 
xy_data <- xy_data %>%
  filter(!is.na(group_id))
#Adding age column with NA
xy_data$age<-NA
#adding Pop column 
xy_data$Pop <- ifelse(
  grepl("Col(2003)", xy_data$filename), "Alberta Canada",
  ifelse(
    grepl("MA_Strucsize_Year_Mon(2013)", xy_data$filename), "North America",
      NA
    ))
#Need to standardise MA_Struct study 
xy_data$filename <- gsub("MA_Strucsize", "MA_Structsize", xy_data$filename)
#doesnt work with the parenthesis 
xy_data$Pop <- ifelse(
  grepl("Col\\(2003\\)", xy_data$filename), "Alberta Canada",
  ifelse(
    grepl("MA_Structsize", xy_data$filename),"North America",
    NA
  ))
#trying to standardise group_id - to eventually get rid of single id's which I didn't want in this study 
xy_data$group_id <- tolower(xy_data$group_id)
xy_data$group_id <- gsub(" ", "", xy_data$group_id)
xy_data$group_id <- gsub("^([a-z]+)_([0-9]+)$", "\\1_p\\2", xy_data$group_id)
#checking
unique(xy_data$group_id)
#Looks okay now going to remove single data point from graphs - I couldnt extract data from 
library(dplyr)
xy_data <- xy_data %>%
  group_by(filename) %>%
  filter(n_distinct(group_id) > 1) %>%
  ungroup()
#UNFORTUNATELY I need species - So I will use a mix of auto and manual coding to do this - Some files have species attached in their names - while some will need to be entered manually 
#here we go 
#Autoextract from those with species listed 
xy_data$species <- sub(".*_", "", xy_data$filename)
xy_data$species <- gsub("\\.jpg", "", xy_data$species)
#okay that didnt work - going to try creating a look-up table 
species_lookup <- data.frame(
  pattern = c(
    "Col",
    "AY_moose",
    "BG_Caribo",
    "C_moose",
    "C_NT_mule_deer",
    "C_T_mule_deer",
    "C_White-Tdeer",
    "CB_G_caribou",
    "M_caribou",
    "NT_Aelk",
    "NT_C_whiteTdeer",
    "QL_caribo",
    "R_elk",
    "Smoose",
    "TC_whiteT_deer",
    "TS_blackT_deer",
    "W_caribou",
    "Bighorn-sheep",
    "Bison",
    "D_sheep",
    "Dalls_sheep",
    "Muskox",
    "RM_goat",
    "Stone_sheep",
    "Corlatti",
    "Garel",
    "Douhard"),
  species = c(
    "bighorn_sheep",
    "Alaska-Yukon moose",
    "Barren Ground caribou",
    "Canada moose",
    "Non typical mule deer",
    "Typical mule deer",
    "Typical coues white tail deer",
    "Central barren ground caribou",
    "Mountain caribou",
    "Non typical American Elk",
    "Non typical coues white tail deer",
    "Quebec labrador caribou",
    "Roosevelts elk",
    "Shiras moose",
    "Typical columbia black tail deer",
    "Typical sitka black tail deer",
    "Woodland caribou",
    "Bighorn sheep",
    "Bison",
    "Desert sheep",
    "Dalls sheep",
    "Muskox",
    "Rocky mountain goat",
    "Stones sheep",
    "Chamois",
    "Mouflon",
    "Stones sheep")
)
#Calling in assign function 
assign_species <- function(df, lookup) {
  df$species <- NA
  
  for (i in 1:nrow(lookup)) {
    df$species[grepl(lookup$pattern[i], df$filename)] <- 
      lookup$species[i]
  }
  
  return(df)
}
#now assigning the look-up table to all the datasets
xy_data <- assign_species(xy_data, species_lookup)
scatter_data <- assign_species(scatter_data, species_lookup)
Douhard2016 <- assign_species(Douhard2016, species_lookup)
#I cant believe that worked 
#just to be a pain going to add study column so I can add it in as a moderator 
xy_data$study <- sub("^(.*?\\(\\d{4}\\)).*", "\\1", xy_data$filename)
scatter_data$study<-sub("^(.*?\\(\\d{4}\\)).*", "\\1", scatter_data$filename)
scatter_data$study <- sub("_age.*", "", scatter_data$filename)
Douhard2016$study<-"Douhard(2016)"
#Saving the cleaned Data 
write.csv(xy_data, "xy_cleaned.csv", row.names = FALSE)
write.csv(scatter_data, "scatter_cleaned.csv", row.names = FALSE)
write.csv(Douhard2016, "douhard_cleaned.csv", row.names = FALSE)
#The Modeling
rm(se_to_sd)
library(metaDigitise)
library(shinyDigitise)
library(metafor)
read.csv("xy_cleaned.csv")
read.csv("scatter_cleaned.csv")
read.csv("douhard_cleaned.csv")
#combining data sets that will run throught the same lm's 
error_data <- rbind(xy_data, Douhard2016)
#error - not same number of columns 
xy_data$group_id<-NULL
xy_data<- xy_data[,c("x","y","se","age","filename","Pop","species","study")]
#trying again 
error_data<-rbind(xy_data,Douhard2016)
#splitting by filname - because we need 1 effect size per study/graph 
scatter_split <- split(scatter_data, scatter_data$filename)
mean_split <- split(error_data, error_data$filename)
scatter_split
#model for scatterplots - extracting se and slope 
fit_scatter <- function(df) {
  
  model <- lm(y ~ x, data = df)
  
  coef_summary <- summary(model)$coefficients
  
  c(
    slope = coef_summary["x", "Estimate"],
    se = coef_summary["x", "Std. Error"]
  )
}
#weighted model for error data 
fit_mean <- function(df) {
  
  df <- df[df$se > 0, ]   # remove zero SE
  
  model <- lm(y ~ x, data = df, weights = 1 / (df$se^2))
  
  coef_summary <- summary(model)$coefficients
  
  c(
    slope = coef_summary["x", "Estimate"],
    se = coef_summary["x", "Std. Error"]
  )
}
#applying the models 
scatter_results <- lapply(scatter_split, fit_scatter)
mean_results <- lapply(mean_split, fit_mean)
#looking at results 
scatter_results
mean_results
#combining results into dataframes - for later use in metafor
scatter_df <- as.data.frame(do.call(rbind, scatter_results))
scatter_df$filename <- names(scatter_results)
scatter_df$type <- "scatter"

mean_df <- as.data.frame(do.call(rbind, mean_results))
mean_df$filename <- names(mean_results)
mean_df$type <- "mean"
#combining all the effect sizes 
effects_df <- rbind(scatter_df, mean_df)
#checking
effects_df
#YAY - now merging back metadata of study, pop etc 
meta_info <- unique(rbind(
  scatter_data[, c("filename","study","species","Pop")],
  error_data[, c("filename","study","species","Pop")]
))

effects_df <- merge(effects_df, meta_info, by="filename")
#double checking nothing is out of normal 
summary(effects_df$slope)
summary(effects_df$se)

table(effects_df$age_controlled)
#creating an age control column so it can be added as a moderator 
effects_df$age_controlled <- ifelse(
  grepl("_age", effects_df$filename),
  "yes",
  "no"
)


effects_df$age_controlled[
  grepl("Douhard", effects_df$filename)
] <- "yes"

effects_df$age_controlled <- as.factor(effects_df$age_controlled)

#now meta-analysis
#General model - to see overall effects and heterogeneity 
meta <- rma(
  yi = slope,
  sei = se,
  data = effects_df
)

summary(meta)

#Species model
meta_species <- rma(
  yi = slope,
  sei = se,
  mods = ~ species,
  data = effects_df
)

summary(meta_species)

#Warning - 2 studies with NAs omitted and fishers scoring algorithm stuck at a local maximum 
#checking both sources of warning 
effects_df[is.na(effects_df$species), ]
#Pelle missing species - adding them manually 
effects_df$species[
  grepl("Pelle", effects_df$filename)
] <- "bighorn_sheep" 
#checking plot for second warning 
profile(meta_species)
#Running species model again 
meta_species <- rma(
  yi = slope,
  sei = se,
  mods = ~ species,
  data = effects_df)
summary(meta_species)
#Age model 
meta_age <- rma(
  yi = slope,
  sei = se,
  mods = ~ age_controlled,
  data = effects_df
)
summary(meta_age)
#Data type - model 
meta_data <- rma(
  yi = slope,
  sei = se,
  mods = ~ type,
  data = effects_df
)
summary(meta_data)

#Final full model - explaining heterogeneity 
meta_full <- rma(
  yi = slope,
  sei = se,
  mods = ~ species + age_controlled + type,
  data = effects_df
)
summary(meta_full)

#PLOTS
#Forest plot
forest(meta)

#funnel plot 
funnel(meta)

#eggers test 
regtest(meta)

#Making a study table for the methods
Studydata<-(effects_df)
Studydata$slope<-NULL
Studydata$se<-NULL
write.csv(Studydata, "Study_table.csv", row.names = FALSE)
effects_Table<-effects_df
effects_Table$filename<-NULL
effects_Table$species<-NULL
effects_Table$Pop<-NULL
effects_Table$age_controlled<-NULL
write.csv(effects_Table, "effects_table.csv", row.names = FALSE)
