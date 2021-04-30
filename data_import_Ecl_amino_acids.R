library(here)
library(tidyverse)
library(broom)

folder <- here("Data")
files <- list.files(folder)
file_names <- sub(".txt","",files)

#import 23 April 2021 data
#make sure to not import only those files needed
data <- lapply(paste0(folder,"/",files)[6], read.delim2, header=FALSE,sep=",", stringsAsFactors = FALSE)

#name dfs in list
names(data) <- file_names[6]

#remove metadata from spectrophotometer
data <- lapply(data, function(x) {
  x <-  x[-c(1:4),-ncol(x)] #remove first 4 rows and last column
  return(x)
}
) 

#make first row colnames
#then remove first row, it's now the columns' names
data <- lapply(data, function(x) {
  colnames(x) <- (x[1,])
  x <- x[-1,]
  return(x)
} 
)

#name first colum
data <- lapply(data, function(x){
  names(x)[1] <- "well"
  return(x)
}
)


#add meta-data columns
data <- mapply(cbind,data, 
               "sample_ID" = NA, 
               "species" = NA, 
               "colony" = NA, 
               "community" = NA, 
               "carbon_source" = NA, 
               "bio_replicate" = NA,
               "tech_replicate" = NA, 
               "species_richness" = NA,
               SIMPLIFY = FALSE)

#fill in meta-data columns: sample IDs
data[["23April2021"]]$sample_ID[grepl("^(B|C|D)(01)$",data[["23April2021"]]$well)] <- "B ABCD R1 c1"
data[["23April2021"]]$sample_ID[grepl("^(B|C|D)(02)$",data[["23April2021"]]$well)] <- "B ABCD R1 c2"
data[["23April2021"]]$sample_ID[grepl("^(B|C|D)(03)$",data[["23April2021"]]$well)] <- "B ABCD R1 c3"
data[["23April2021"]]$sample_ID[grepl("^(B|C|D)(04)$",data[["23April2021"]]$well)] <- "B ABCD R2 c1"
data[["23April2021"]]$sample_ID[grepl("^(B|C|D)(05)$",data[["23April2021"]]$well)] <- "B ABCD R2 c2"
data[["23April2021"]]$sample_ID[grepl("^(B|C|D)(06)$",data[["23April2021"]]$well)] <- "B ABCD R2 c3"
data[["23April2021"]]$sample_ID[grepl("^(B|C|D)(07)$",data[["23April2021"]]$well)] <- "B ABCD R3 c1"
data[["23April2021"]]$sample_ID[grepl("^(B|C|D)(08)$",data[["23April2021"]]$well)] <- "B ABCD R3 c2"
data[["23April2021"]]$sample_ID[grepl("^(B|C|D)(09)$",data[["23April2021"]]$well)] <- "B ABCD R3 c3"
data[["23April2021"]]$sample_ID[grepl("^(B|C|D)(10)$",data[["23April2021"]]$well)] <- "B ABC R1 c1"
data[["23April2021"]]$sample_ID[grepl("^(B|C|D)(11)$",data[["23April2021"]]$well)] <- "B ABC R1 c2"
data[["23April2021"]]$sample_ID[grepl("^(B|C|D)(12)$",data[["23April2021"]]$well)] <- "B ABC R1 c3"
data[["23April2021"]]$sample_ID[grepl("^(B|C|D)(13)$",data[["23April2021"]]$well)] <- "B ABD R1 c1"
data[["23April2021"]]$sample_ID[grepl("^(B|C|D)(14)$",data[["23April2021"]]$well)] <- "B ABD R1 c2"
data[["23April2021"]]$sample_ID[grepl("^(B|C|D)(15)$",data[["23April2021"]]$well)] <- "B ABD R1 c3"
data[["23April2021"]]$sample_ID[grepl("^(B|C|D)(16)$",data[["23April2021"]]$well)] <- "B ABD R2 c1"
data[["23April2021"]]$sample_ID[grepl("^(B|C|D)(17)$",data[["23April2021"]]$well)] <- "B ABD R2 c3"
data[["23April2021"]]$sample_ID[grepl("^(B|C|D)(18)$",data[["23April2021"]]$well)] <- "B ABD R2 c4"
data[["23April2021"]]$sample_ID[grepl("^(B|C|D)(19)$",data[["23April2021"]]$well)] <- "B ABD R3 c1"
data[["23April2021"]]$sample_ID[grepl("^(B|C|D)(20)$",data[["23April2021"]]$well)] <- "B ABD R3 c3"
data[["23April2021"]]$sample_ID[grepl("^(B|C|D)(21)$",data[["23April2021"]]$well)] <- "B ABD R3 c4"
data[["23April2021"]]$sample_ID[grepl("^(B|C|D)(22)$",data[["23April2021"]]$well)] <- "B BCD R1 c1"
data[["23April2021"]]$sample_ID[grepl("^(B|C|D)(23)$",data[["23April2021"]]$well)] <- "B BCD R1 c4"
data[["23April2021"]]$sample_ID[grepl("^(B|C|D)(24)$",data[["23April2021"]]$well)] <- "B BCD R1 c5"
data[["23April2021"]]$sample_ID[grepl("^(F|G|H)(01)$",data[["23April2021"]]$well)] <- "B BCD R2 c1"
data[["23April2021"]]$sample_ID[grepl("^(F|G|H)(02)$",data[["23April2021"]]$well)] <- "B BCD R2 c2"
data[["23April2021"]]$sample_ID[grepl("^(F|G|H)(03)$",data[["23April2021"]]$well)] <- "B BCD R2 c3"
data[["23April2021"]]$sample_ID[grepl("^(F|G|H)(04)$",data[["23April2021"]]$well)] <- "B BCD R3 c1"
data[["23April2021"]]$sample_ID[grepl("^(F|G|H)(05)$",data[["23April2021"]]$well)] <- "B BCD R3 c2"
data[["23April2021"]]$sample_ID[grepl("^(F|G|H)(06)$",data[["23April2021"]]$well)] <- "B BCD R3 c3"
data[["23April2021"]]$sample_ID[grepl("^(F|G|H)(07)$",data[["23April2021"]]$well)] <- "B AB R2 c1"
data[["23April2021"]]$sample_ID[grepl("^(F|G|H)(08)$",data[["23April2021"]]$well)] <- "B AB R2 c2"
data[["23April2021"]]$sample_ID[grepl("^(F|G|H)(09)$",data[["23April2021"]]$well)] <- "B AB R2 c3"
data[["23April2021"]]$sample_ID[grepl("^(F|G|H)(10)$",data[["23April2021"]]$well)] <- "B BC R2 c1"
data[["23April2021"]]$sample_ID[grepl("^(F|G|H)(11)$",data[["23April2021"]]$well)] <- "B BC R2 c2"
data[["23April2021"]]$sample_ID[grepl("^(F|G|H)(12)$",data[["23April2021"]]$well)] <- "B BC R2 c3"
data[["23April2021"]]$sample_ID[grepl("^(F|G|H)(13)$",data[["23April2021"]]$well)] <- "B BC R3 c1"
data[["23April2021"]]$sample_ID[grepl("^(F|G|H)(14)$",data[["23April2021"]]$well)] <- "B BC R3 c2"
data[["23April2021"]]$sample_ID[grepl("^(F|G|H)(15)$",data[["23April2021"]]$well)] <- "B BC R3 c3"

data[["23April2021"]]$sample_ID[grepl("^(F|G|H)(16)$",data[["23April2021"]]$well)] <- "B BD R1 c1"
data[["23April2021"]]$sample_ID[grepl("^(F|G|H)(17)$",data[["23April2021"]]$well)] <- "B BD R1 c2"
data[["23April2021"]]$sample_ID[grepl("^(F|G|H)(18)$",data[["23April2021"]]$well)] <- "B BD R1 c3"

data[["23April2021"]]$sample_ID[grepl("^(F|G|H)(19)$",data[["23April2021"]]$well)] <- "B BD R2 c1"
data[["23April2021"]]$sample_ID[grepl("^(F|G|H)(20)$",data[["23April2021"]]$well)] <- "B BD R2 c2"
data[["23April2021"]]$sample_ID[grepl("^(F|G|H)(21)$",data[["23April2021"]]$well)] <- "B BD R2 c3"

data[["23April2021"]]$sample_ID[grepl("^(F|G|H)(22)$",data[["23April2021"]]$well)] <- "B BD R3 c1"
data[["23April2021"]]$sample_ID[grepl("^(F|G|H)(23)$",data[["23April2021"]]$well)] <- "B BD R3 c2"
data[["23April2021"]]$sample_ID[grepl("^(F|G|H)(24)$",data[["23April2021"]]$well)] <- "B BD R3 c3"

data[["23April2021"]]$sample_ID[grepl("^(J|K|L)(01)$",data[["23April2021"]]$well)] <- "B B R1 c1"
data[["23April2021"]]$sample_ID[grepl("^(J|K|L)(02)$",data[["23April2021"]]$well)] <- "B B R1 c2"
data[["23April2021"]]$sample_ID[grepl("^(J|K|L)(03)$",data[["23April2021"]]$well)] <- "B B R1 c3"

data[["23April2021"]]$sample_ID[grepl("^(J|K|L)(04)$",data[["23April2021"]]$well)] <- "B B R2 c1"
data[["23April2021"]]$sample_ID[grepl("^(J|K|L)(05)$",data[["23April2021"]]$well)] <- "B B R2 c2"
data[["23April2021"]]$sample_ID[grepl("^(J|K|L)(06)$",data[["23April2021"]]$well)] <- "B B R2 c3"

data[["23April2021"]]$sample_ID[grepl("^(J|K|L)(07)$",data[["23April2021"]]$well)] <- "B B R3 c1"
data[["23April2021"]]$sample_ID[grepl("^(J|K|L)(08)$",data[["23April2021"]]$well)] <- "B B R3 c2"
data[["23April2021"]]$sample_ID[grepl("^(J|K|L)(09)$",data[["23April2021"]]$well)] <- "B B R3 c3"

data[["23April2021"]]$sample_ID[grepl("^(J|K|L)(10)$",data[["23April2021"]]$well)] <- "cntl cntl cntl cntl"
data[["23April2021"]]$sample_ID[grepl("^(J|K|L)(11)$",data[["23April2021"]]$well)] <- "cntl cntl cntl cntl"
data[["23April2021"]]$sample_ID[grepl("^(J|K|L)(12)$",data[["23April2021"]]$well)] <- "cntl cntl cntl cntl"
data[["23April2021"]]$sample_ID[grepl("^(J|K|L)(13)$",data[["23April2021"]]$well)] <- "cntl cntl cntl cntl"

data[["23April2021"]]$sample_ID[grepl("^(M|N|O)(13|14|15|16|17)$",data[["23April2021"]]$well)] <- "B BC R2 c2"
data[["23April2021"]]$sample_ID[grepl("^(M|N|O)(18|19|20|21|22)$",data[["23April2021"]]$well)] <- "cntl cntl cntl cntl"


#fill in meta-data columns: carbon sources
data[["23April2021"]]$carbon_source[grepl("B|C|D|F|G|H|J|K|L",data[["23April2021"]]$well)] <- "AA"
data[["23April2021"]]$carbon_source[grepl("^(M|N|O)(13|18)$",data[["23April2021"]]$well)] <- "4 C-src"
data[["23April2021"]]$carbon_source[grepl("^(M|N|O)(14|19)$",data[["23April2021"]]$well)] <- "FRU"
data[["23April2021"]]$carbon_source[grepl("^(M|N|O)(15|20)$",data[["23April2021"]]$well)] <- "GLU"
data[["23April2021"]]$carbon_source[grepl("^(M|N|O)(16|21)$",data[["23April2021"]]$well)] <- "SUC"
data[["23April2021"]]$carbon_source[grepl("^(M|N|O)(17|22)$",data[["23April2021"]]$well)] <- "LAC"


#fill in meta-data columns: technical replicates
data[["23April2021"]]$tech_replicate[grepl("B|F|J|M",data[["23April2021"]]$well)] <- "R1"
data[["23April2021"]]$tech_replicate[grepl("C|G|K|N",data[["23April2021"]]$well)] <- "R2"
data[["23April2021"]]$tech_replicate[grepl("D|H|L|O",data[["23April2021"]]$well)] <- "R3"

#fill in most empty meta-data columns
data <- lapply(data, separate, sample_ID, into = c("species", "community", "bio_replicate", "colony"), sep = " ", fill = "left", remove = FALSE)

#fill in species_richness column
data <- lapply(data, function(x) {
  x$species_richness <- ifelse(nchar(x$community) == 1, "1",
                               ifelse(nchar(x$community) == 2, "2",
                                      ifelse(nchar(x$community) == 3, "3",
                                             ifelse(nchar(x$community) == 4, "4",
                                                    ifelse(nchar(x$community) == 5, "Ansc", NA)
                                      ))))
  return(x)
})

#get rid of now usless columns
data <- lapply(data, function(x){
  x <-  x[,-c(1:2)]
  return(x)
}
)





#plot all growth curves
data %>% 
  reduce(full_join) %>%
  pivot_longer(-c("sample_ID","species", "community", "bio_replicate", "colony", "carbon_source", "tech_replicate", "species_richness"),names_to="time_sec",values_to="OD600") %>%
  mutate(OD600 = as.numeric(OD600),
         time_sec = as.numeric(time_sec),
         time_hrs = time_sec*(1/60)*(1/60)) %>%
  unite("ID", sample_ID:tech_replicate,remove = FALSE) %>%
  filter(species != "A" & time_hrs < 30) %>%
  ggplot(.,aes(x = time_hrs,y = OD600, group = ID, color = ID)) +
  geom_line(size = 0.5) +
  facet_grid(cols=vars(carbon_source), rows = vars(community)) +
  ylab(expression(OD["600nm"]))+
  xlab("Time (hrs)")+
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none") 



##cut data
#split all samples into seperate lists
data_uncut <-
  data %>%
  reduce(full_join) %>%
  pivot_longer(-c("sample_ID","species", "community", "bio_replicate", "colony", "carbon_source", "tech_replicate", "species_richness"),names_to="time_sec",values_to="OD600") %>%
  mutate(OD600 = as.numeric(OD600),
         time_sec = as.numeric(time_sec),
         time_hrs = time_sec*(1/60)*(1/60)) %>%
  filter(species == "B") %>%
  unite("ID", sample_ID:tech_replicate,remove = FALSE) %>%
  group_split(species, community,colony, carbon_source,bio_replicate)


print(data_uncut[[1]], n = 1000)




data_cut <- lst()

for(i in seq_along(data_uncut)){
  
  plot <-
    #plot based on replicates
    ggplot(data_uncut[[i]], aes(x = time_hrs, y = OD600, group = ID, color = ID)) + 
    geom_line(size = 0.5) +
    scale_x_continuous(breaks = seq(0, 50, by = 2))+
    ylab(expression(OD["600nm"]))+
    xlab("Time (hrs)")+
    ggtitle(paste(data_uncut[[i]]$species[1],
                  as.character(data_uncut[[i]]$carbon_source[2]),
                  data_uncut[[i]]$bio_replicate[1]
    )) + 
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
  
  
  print(plot)
  
  begin = as.numeric(readline(prompt="begin"))
  
  end = as.numeric(readline(prompt="end"))
  
  #save data back to seperate lists for each sample
  x <- data_uncut[[i]] %>%
    group_split(species, community, carbon_source, bio_replicate) 
  
  #cut each sample (i.e., list) at chosen times; save to container 
  data_cut[[i]] <- lapply(x, filter, time_hrs > begin & time_hrs < end)
  
}

#save_data
save(data_cut, file = "data_cut_AA_23April2021.RData")


#load data(since it's already saved)
data_cut <- load("data_cut_AA_23April2021.RData")

#flatten the list
y <- do.call(c,data_cut)
#bind the list to one tibble
z <- do.call(rbind,y)

data_clean <- z



#examine the cut data
data_clean %>%
  filter(species_richness == "Ansc") %>%
  ggplot(.,aes(x = time_hrs, y = OD600), group = ID, color = ID) +
  geom_line()+
  geom_point()+
  facet_grid(rows = vars(carbon_source), cols = vars(tech_replicate))




#calculate rowth rate
res <- data_clean %>% 
  #pivot_longer(-c("sample_ID","species", "community", "bio_replicate", "colony", "carbon_source", "tech_replicate", "species_richness"),names_to="time_sec",values_to="OD600") %>%
  #unite("ID", community:bio_replicate,remove = FALSE) %>%
  group_by(sample_ID, community,bio_replicate,colony,carbon_source,tech_replicate,species_richness) %>%
  #filter(species == "B" & OD600 > 0.5 & OD600 < 0.75 & time_hrs < 12) %>%
  nest() %>%
  mutate(
    fit = map(data, ~lm(log(OD600) ~ time_hrs, data = .x)),
    tidied = map(fit,tidy)
  ) %>%
  unnest(tidied) 

#plot growth rate by community
res %>%
  separate(sample_ID, into = c("species", "community", "bio_replicate", "colony"), remove = FALSE) %>%
  filter(term != "(Intercept)" & p.value < 0.05) %>%
  ggplot(., aes(x = term, y = estimate, group = sample_ID, color = bio_replicate))+
  geom_point(size = 2)+
  geom_hline(yintercept = .25, linetype = "dashed", color = "red")+
  facet_grid(rows=vars(community),cols = vars(carbon_source))

#plot growth rate by species richness
res %>%
  separate(sample_ID, into = c("species", "community", "bio_replicate", "colony"), remove = FALSE) %>%
  filter(term != "(Intercept)" & p.value < 0.05 & community != "cntl") %>%
  ggplot(., aes(x = species_richness, y = estimate, group = species_richness, color = species_richness))+
  geom_boxplot(alpha = 0.1, width = 0.5)+
  geom_point(size = 2, position = position_jitterdodge(jitter.width = 0.2))+
  geom_hline(yintercept = .25, linetype = "dashed", color = "red")+
  facet_grid(rows=vars(carbon_source))

res %>%
  filter(p.value > 0.05)



data_clean[data_clean$species_richness == "Ansc",] %>%
  ggplot(., aes(x = time_hrs, y = OD600, group = ID))+
  geom_line()+
  facet_grid(rows=vars(carbon_source))
  

