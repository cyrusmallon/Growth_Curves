#visualize
df <-res %>%
  filter(term != "(Intercept)") %>%
  group_by(species, nutrient_mix) %>%
  summarize(u_avg = mean(estimate)) %>%
  mutate(species = sub("Ptund","A",species)) %>%
  mutate(species = sub("Ecl","B",species)) %>%
  mutate(species = sub("Mfol","C",species)) %>%
  mutate(species = sub("Smalt","D",species)) %>%
  ungroup()



communities <- 
  c("ABCD",
    "ABC","ABD","ACD", "BCD",
    "AB","AC","AD","BC","BD","CD",
    "A","B","C","D")


res %>%
  filter(term != "(Intercept)") %>%
  group_by(species, nutrient_mix) %>%
  summarize(u_avg = mean(estimate)) %>%
  mutate(species = sub("Ptund","A",species)) %>%
  mutate(species = sub("Ecl","B",species)) %>%
  mutate(species = sub("Mfol","C",species)) %>%
  mutate(species = sub("Smalt","D",species)) %>%
  expand_grid(species)

list <- list()

for(i in 1:length(communities)){
  print(communities[i])
  
 if( grepl "")
  
  
}


if(grepl("A|B|C|D",communities[1])){ 
    x <- as_tibble(df) %>% filter(species == "A|B|C|D")
} else {
  
}




ABCD <- df %>% mutate(community = "ABCD")

ABC <- df %>% filter(!grepl("[D]",species)) %>% mutate(community = "ABC")
ABD <- df %>% filter(!grepl("[C]",species)) %>% mutate(community = "ABD")
ACD <- df %>% filter(!grepl("[B]",species)) %>% mutate(community = "ACD")
BCD <- df %>% filter(!grepl("[A]",species)) %>% mutate(community = "BCD")

AB <- df %>% filter(grepl("[A|B]",species)) %>% mutate(community = "AB")
AC <- df %>% filter(grepl("[A|C]",species)) %>% mutate(community = "AC")
AD <- df %>% filter(grepl("[A|D]",species)) %>% mutate(community = "AD")
BC <- df %>% filter(grepl("[B|C]",species)) %>% mutate(community = "BC")
BD <- df %>% filter(grepl("[B|D]",species)) %>% mutate(community = "BD")
CD <- df %>% filter(grepl("[C|D]",species)) %>% mutate(community = "CD")

A <- df %>% filter(species == "A") %>% mutate(community = "A")
B <- df %>% filter(species == "B") %>% mutate(community = "B")
C <- df %>% filter(species == "C") %>% mutate(community = "C")
D <- df %>% filter(species == "D") %>% mutate(community = "D")

#make one df
all <- bind_rows(ABCD, ABC, ABD, ACD, BCD, AB, AC, AD, BC, BD, CD, A, B, C, D)

all %>% 
  group_by(community) %>%
  group_map(~ min_rank(.x$u_avg))

all %>% 
  group_by(community) %>%
  group_modify( ~ {
    .x %>%
      mutate(u_avg = sort(u_avg, decreasing = TRUE))
  }) %>%
  group_map (~ unique(.x$species)) 


all %>% 
  group_by(community) %>%
  group_map(~ min_rank(.x$u_avg))

all %>% 
  group_by(community) %>%
  group_modify( ~ {
    .x %>%
      mutate(u_avg = sort(u_avg, decreasing = TRUE))
  }) %>%
  group_modify( ~ {
  
    unique(.x$u_avg) %>%
      tibble::enframe(name = "unique", species = .x%)
  }) %>%
  print(n = 100)

