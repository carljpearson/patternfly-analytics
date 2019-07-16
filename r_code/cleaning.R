library(tidyverse)
library(rjson)

#testing first json -----

json_3scale <- file("./stats/3scale.json")
raw_3scale <- fromJSON(paste(readLines(json_3scale,warn=F), collapse=""))
df_3scale <- as.data.frame(unlist(raw_3scale,use.names = T))

json_OpenShift.Console <- file("./stats/OpenShift-Console.json")
raw_OpenShift.Console <- fromJSON(paste(readLines(json_OpenShift.Console,warn=F), collapse=""))
df_OpenShift.Console <- as.data.frame(unlist(raw_OpenShift.Console,use.names = T))


file_name <-  "OpenShift-Console.json"
r_name <- gsub("-","_",file_name)
r_name <- gsub(" ","_",r_name)
r_name <- gsub("\\.json","",r_name)

json <- file(paste0("./stats/",file_name))
raw<- fromJSON(paste(readLines(json,warn=F), collapse=""))
df <- as.data.frame(unlist(raw,use.names = T))

df$product <- r_name


#function to read and slightly clean and combine all txt files----

json_files <- list.files("./stats/") #get json file list
dfc <- map_df(json_files, function(i) {
  
  file_name <-  i #get file name
  
  #convert file name to r-friendly
  r_name <- gsub("-","_",file_name) 
  r_name <- gsub(" ","_",r_name)
  r_name <- gsub("\\.json","",r_name)
  
  #define file path based on file name and wd
  file_path <- paste0("./stats/",file_name)
  
  #connect json file
  json <- file(file_path)
  
  #json to r
  raw<- fromJSON(paste(readLines(json,warn=F), collapse="")) #read json into list
  df <- as.data.frame(unlist(raw,use.names = T)) #unlist into df
  
  df$product <- r_name #keep product name from file name
  
  df <- rownames_to_column(df) #get list definitions to row
  
  df
}) 

dfc %>%
  rename(raw_json_names=rowname, value=`unlist(raw, use.names = T)`) %>%
  filter(str_detect(raw_json_names,"classes.pf-c-")) %>%
  mutate(value=as.numeric(as.character(value))) %>%
  mutate(element=raw_json_names) -> test

test %>%
  group_by(product) %>%
  summarise(total_element_use = sum(value)) %>%
  ggplot(aes(x=product, y=total_element_use,fill=product)) +
  geom_bar(stat='identity') +
  coord_flip()

