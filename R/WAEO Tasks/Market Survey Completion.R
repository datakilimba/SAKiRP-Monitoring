library(tidyverse)
library(readxl)
library(httr)
library(RPostgres)
library(lubridate)

con = DBI::dbConnect(odbc::odbc(), "PostgreSAKiRP")
waeo = dbReadTable(con,"waeos") %>% 
  filter(active == 1)
district = dbReadTable(con,"districts")
ward = dbReadTable(con,"wards")

source("WAEO Tasks/Market Survey DQA.R")

# kobo_server_url = "https://kc.humanitarianresponse.info/"
# form_id = "919626"
# url = paste0(kobo_server_url,"api/v1/data/",form_id,".csv")
# rawdata = GET(url,authenticate("tmda","tmda@2021"))
# content = content(rawdata,"raw",encoding="UTF-8")
# 
# survey = read_csv(content)

market_survey2 = read_xlsx(
  "C:/Users/tnkil/OneDrive/Enabel - SAKiRP/M&E/Data Collection/S07 - Market Survey/Data/Market_Survey_-_all_versions_-_False_-_2021-10-26-12-52-31.xlsx",
  sheet = "Market Survey"
)

market_survey = market_survey2 %>% 
  janitor::clean_names() %>% 
  rename(
    bean_cess = ends_with("crop_cess_beans"),
    cassava_cess = ends_with("crop_cess_cassava"),
    sunflower_cess_ = ends_with("crop_cess_sunflower"),
    red_bean_price_ = ends_with("bean_red_price_market"),
    improved_yellow_price = ends_with("bean_improvedyellow_price_market"),
    local_yellow_price = ends_with("bean_yellow_price_market"),
    dried_cassava_price = ends_with("cassava_dried_price_market"),
    fresh_cassava_price = ends_with("cassava_fresh_price_market"),
    cassava_flour_price = ends_with("cassava_flour_price_market"),
    sf_seed_price = ends_with("sunflower_seeds_price_market"),
    sf_cake_price = ends_with("sunflower_cake_price_market"),
    sf_oil_price = ends_with("sunflower_oil_price_market"),
    working_capital = ends_with("working_capital"),
    quantity_beans = ends_with("quantity_beans"),
    quantity_cassava = ends_with("quantity_cassava"),
    #quantity_sf = ends_with("quantity_sf"),
  ) %>% 
  select(
    `uuid`,endtime,district,ward,enumname,
    bean_cess,cassava_cess,sunflower_cess_,red_bean_price_,improved_yellow_price,
    local_yellow_price,dried_cassava_price,fresh_cassava_price,cassava_flour_price,
    sf_seed_price,sf_cake_price,sf_oil_price,working_capital,quantity_beans,
    quantity_cassava
  ) %>% 
  mutate(
    sunflower_cess = case_when(
      sunflower_cess_=="n/a"~NA_character_,
      TRUE~sunflower_cess_
    ),
    sunflower_cess = as.integer(sunflower_cess),
    
    red_bean_price_ = case_when(
      red_bean_price_=="n/a"~NA_character_,
      TRUE~red_bean_price_
    ),
    red_bean_price = as.integer(red_bean_price_),
    
    improved_yellow_price = case_when(
      improved_yellow_price=="n/a"~NA_character_,
      TRUE~improved_yellow_price
    ),
    improved_yellow_price = as.integer(improved_yellow_price),
    
    local_yellow_price = case_when(
      local_yellow_price=="n/a"~NA_character_,
      TRUE~local_yellow_price
    ),
    local_yellow_price = as.integer(local_yellow_price),
    
    dried_cassava_price = case_when(
      dried_cassava_price=="n/a"~NA_character_,
      TRUE~dried_cassava_price
    ),
    dried_cassava_price = as.integer(dried_cassava_price),
    
    fresh_cassava_price = case_when(
      fresh_cassava_price=="n/a"~NA_character_,
      TRUE~fresh_cassava_price
    ),
    fresh_cassava_price = as.integer(fresh_cassava_price),
    
    cassava_flour_price = case_when(
      cassava_flour_price=="n/a"~NA_character_,
      TRUE~cassava_flour_price
    ),
    cassava_flour_price = as.integer(cassava_flour_price),
    
    # sf_seed_price = case_when(
    #   is.na(sf_seed_price)~0,
    #   TRUE~as.double(sf_seed_price)
    # ),
    sf_seed_price = as.integer(sf_seed_price),
    
    sf_cake_price = case_when(
      is.na(sf_cake_price)~0,
      TRUE~as.double(sf_cake_price)
    ),
    sf_cake_price = as.integer(sf_cake_price),
    
    sf_oil_price = case_when(
      is.na(sf_oil_price)~0,
      TRUE~as.double(sf_oil_price)
    ),
    sf_oil_price = as.integer(sf_oil_price),
    
    working_capital = case_when(
      working_capital=="n/a"~NA_character_,
      TRUE~working_capital
    ),
    working_capital = as.integer(working_capital),
    
    quantity_beans = case_when(
      quantity_beans=="n/a"~NA_character_,
      TRUE~quantity_beans
    ),
    quantity_beans = as.integer(quantity_beans),
    
    quantity_cassava = case_when(
      quantity_cassava=="n/a"~NA_character_,
      TRUE~quantity_cassava
    ),
    quantity_cassava = as.integer(quantity_cassava)
  ) %>% select(-red_bean_price_)

get_monthly_market_data = function(data,year,month){
  data %>% 
    filter(
      lubridate::year(as.Date(endtime))==year,
      lubridate::month(as.Date(endtime))==month
    ) %>% 
    mutate(
      week = lubridate::week(endtime)
    )
}


market_data = market_survey %>% 
  get_monthly_market_data(year = 2021,month=9) %>% 
  mutate(
    district=as.integer(district),
    ward=as.integer(ward),
    enumname=as.integer(enumname)
  ) %>% 
  left_join(district, by=c("district"="id")) %>% 
  left_join(ward,by=c("ward"="id")) %>% 
  left_join(waeo,by=c("enumname"="id")) %>% 
  select(district=district.y,ward=ward.y,waeo,bean_cess:week)

get_monthly_completion = function(data,target = 4){
  data %>% 
    group_by(district,ward,enumname,week) %>%
    count() %>% 
    group_by(district,ward,enumname) %>% 
    summarise(
      n = n(),
      market_survey_completion_rate = case_when(
        round((n/target),2) > 1 ~ 1,
        TRUE ~ round((n/target),2)
      )
    ) %>% 
    ungroup() %>% 
    mutate(enumname=as.integer(enumname)) %>% 
    right_join(waeo,by=c("enumname"="id")) %>%
    left_join(district,by=c("district_id"="id")) %>%
    left_join(ward,by=c("ward_id"="id")) %>% 
    select(
      district=district.y,
      ward=ward.y,
      waeo,
      market_survey_completion_rate
    )%>% 
    mutate(
      market_survey_completion_rate=case_when(
        is.na(market_survey_completion_rate)~0,
        TRUE~market_survey_completion_rate
      )
    )
}

monthly_market_completion = get_monthly_completion(
  data = get_monthly_market_data(market_survey,year = 2021,month = 9)
)

clean_market_data = market_survey_dqa(market_data)

market_error_rate = market_error_fun(my_failed_rows) %>% 
  arrange(district,waeo) %>% 
  mutate(
    error_incident = case_when(
      is.na(error_incident)~as.integer(0),
      TRUE~error_incident
    ),
    DQ_score = round((1-(2*error_incident)/24),2)
  ) 

market_error_rate2 = waeo %>% 
  left_join(district,by=c("district_id"="id")) %>% 
  left_join(ward,by=c("ward_id"="id")) %>% 
  mutate(
    error_incident=0,
    DQ_score=1
  ) %>% 
  filter(!(waeo %in% market_error_rate$waeo)) %>% 
  select(district,ward,waeo,error_incident,DQ_score) %>% 
  rbind(market_error_rate) %>% 
  arrange(DQ_score)



monthly_market_outcome = market_task_outcome(monthly_market_completion) %>% 
  mutate(
    market_task_score=(DQ_score*market_survey_completion_rate)*100
  ) %>% 
  arrange(market_task_score) %>% 
  rename(completion_rate=market_survey_completion_rate)

