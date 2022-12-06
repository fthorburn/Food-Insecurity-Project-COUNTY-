
#-----------------RACE---------------------------------------------------------

# returns a data_frame that has the percentage of each race that is food insecure or not
d2 <- Merged_DataSet %>% filter(!is.na(Race),!is.na(FI_orNot)) %>% 
  group_by(Race, FI_orNot) %>% 
  summarise(count = n()) %>% 
  mutate(perc_FIofRace = count/sum(count))

#ggplot for percentage of each race that is food insecure

d2 %>% filter(FI_orNot == "Food Insecure") %>% 
  ggplot(aes(x = fct_reorder(Race, perc_FIofRace, .desc = TRUE), y = perc_FIofRace*100)) +
  geom_bar(stat = "identity", width = .7) +
  theme(axis.text.x = element_text(angle=70, vjust=1, hjust=1)) +
  labs(title = "Percentage of each Race that is Food Insecure",y = "Percentage ", 
       caption = "Source: Current Population Survey Food Security Supplements") 
  
  

#-----------------SEX---------------------------------------------------------

# returns a data_frame that has the percentage of each Sex that is food insecure or not
d3 <- Merged_DataSet %>% filter(!is.na(Sex),!is.na(FI_orNot)) %>% 
  group_by(Sex, FI_orNot) %>% 
  summarise(count = n()) %>% 
  mutate(perc_FIofSex = count/sum(count))

#ggplot for percentage of each Sex that is food insecure

d3 %>% filter(FI_orNot == "Food Insecure") %>% 
  ggplot(aes(x = Sex, y = perc_FIofSex*100)) +
  geom_bar(stat = "identity", width = .7) +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
  labs(title = "Percentage of each Sex that is Food Insecure",y = "Percentage ",
       caption = "Source: Current Population Survey Food Security Supplements")

#-----------------COUNTY---------------------------------------------------------


#Creating d5 which holds the 10 counties with the highest percentage of food insecure
d4 <- Merged_DataSet %>% filter(!is.na(FIPS_code),!is.na(FI_orNot)) %>% 
  group_by(Stabr,FIPS_code, FI_orNot) %>% 
  summarise(count = n()) %>% 
  mutate(perc_FI = count/sum(count))


d5 <- d4 %>% filter(FI_orNot == "Food Insecure") %>% arrange(desc(perc_FI))

d5 <- head(d5, 10)

#Top ten food insecure counties I have data on
d5 %>% filter(FI_orNot == "Food Insecure") %>% 
  ggplot(aes(x = fct_reorder(FIPS_code, perc_FI, .desc = TRUE), y = perc_FI*100)) +
  geom_bar(stat = "identity", width = .7, fill = "black") +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
  labs(title = "Top ten food insecure counties by percentage of surveyed Population",y = "Percentage ",
       caption = "Source: Current Population Survey Food Security Supplements")



#now find the racial makeup of those counties, d6 holds the racial makeup for all counties with data
d6 <- Merged_DataSet %>% filter(!is.na(FIPS_code),!is.na(FI_orNot), !is.na(Race)) %>% 
  group_by(Stabr,FIPS_code, Race) %>% 
  summarise(count = n()) %>% 
  mutate(perc_RaceCounty = count/sum(count))

#Merging data sets to create
Merged_FICounty_Race <- d5 %>% inner_join(d6,by="FIPS_code")


#Racial Makeup of 10 most food insecure counties by percentage
countyMergedData %>%
  ggplot(aes(x = fct_reorder(FIPS_code, perc_FI, .desc = TRUE), y = perc_RaceCounty*100, fill = Race)) +
  geom_bar(stat = "identity", width = .7, color = "black") +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
  labs(title = "Racial Makeup of 10 most food insecure counties by percentage of Survery Pop.",
       x = "County FIPS Code in Descending order of Food Insecurity",
       y = "Percentage ",
       caption = "Source: Current Population Survey Food Security Supplements") +
  scale_fill_colorblind() + 
  theme_bw() 
  

#This racial data does not take into account being hispanic


#creating a data frame that has the percentage hispanic in each in all counties with data
d7 <- Merged_DataSet %>% filter(!is.na(FIPS_code),!is.na(FI_orNot), !is.na(Hispanic)) %>% 
  group_by(Stabr,FIPS_code, Hispanic) %>% 
  summarise(count = n()) %>% 
  mutate(perc_HispanicCounty = count/sum(count))

d7 <- d7 %>% filter(Hispanic == "Hispanic")


#13063
#34011
#39103
#22063


#Merging to create a new data from top 10 most food insecure counties with 
Merged_FICounty_Hispanic_Race <- d5 %>% inner_join(d7,by="FIPS_code")


#Hispanic or not Makeup of 10 most food insecure counties with data
Merged_FICounty_Hispanic_Race %>%
  ggplot(aes(x = fct_reorder(FIPS_code, perc_FI, .desc = TRUE), y = perc_HispanicCounty*100)) +
  geom_bar(stat = "identity", width = .7, fill = "red", color = "black") +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
  labs(title = "Percent of County that identifies as Hispanic",y = "Percentage ",
       x = "County FIPS Code in Descending order of Food Insecurity",
       caption = "Source: Current Population Survey Food Security Supplements") +
  scale_fill_colorblind() + 
  theme_bw() 
  

#merging data to have both race and Hispanic percentages in a datafile, i
merged_hisp_race_data <- countyMerged_RaceData %>% full_join(Merged_FICounty_Hispanic_Race,by="FIPS_code")





#Racial and Hispanic Makeup of 10 most food insecure counties by percentage
merged_hisp_race_data %>%
  ggplot(aes(x = fct_reorder(FIPS_code, perc_FI.x, .desc = TRUE),y = perc_RaceCounty*100, fill = Race)) +
  geom_col(stat = "identity", width = .7, color = "black") +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
  labs(title = "Non-white and Hispanic Proportion (separately) of the 10 Most Food Insecure Counties",
       x = "County FIPS Code in Descending order of Food Insecurity",
       y = "Percentage ",
       caption = "Source: Current Population Survey Food Security Supplements") +
  scale_fill_colorblind() + 
  theme_few() +
  geom_col(aes(y = perc_HispanicCounty*100, fill = "Hispanic"), 
         width = 0.2,
         position = position_nudge(x = 0.3),
         color = "black")


#only way to improve this is maybe to find a way to shift the hispanic bar up or down depending on the percentage 
#white only that is hispanic

#A table to look at the Hispanic population breakdown by race  for a specific county
data_dec21 %>% filter(FIPS_code == "48439") %>% 
  tabyl(Race, Hispanic) %>% 
  adorn_percentages("row") %>% 
  adorn_pct_formatting(digits = 1)

