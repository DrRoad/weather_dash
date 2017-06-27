require(rvest)
web_address <- 'https://www.ncei.noaa.gov/data/global-hourly/access/2016/'
web_page <- read_html(web_address)

list_datasets <- html_nodes(web_page, 'a') %>% html_attr('href')

list_datasets <- list_datasets[c(6, 7:8)] # temp code

all_data <- vector('list', length(list_datasets))
position = 0
#for(dset in list_datasets){
for(i in seq_along(list_datasets)){
  all_data[[i]] <- read.csv(paste0(web_address, list_datasets[i]))  
}

# loop through appropriate years

# insert data into database?

