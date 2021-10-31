library(tidyverse)
library(httr)
library(RPostgres)
library(plotly)

con = dbConnect(odbc::odbc(),"PostgreSAKIRP")
ward = dbReadTable(con,"wards")
district = dbReadTable(con,"districts")

save_localRDS = function(data){
  saveRDS(data, "./data/market_data.rds")
}

read_localRDS = function(){
  try(
    readRDS("./data/market_data.rds")
  )
}


get_data = function(form){
  
  # Get data from Kobo online, unless no internet access, in which case use the 
  # last available local copy of the data
  default = read_localRDS()
  
  kobo_server_url = "https://kc.humanitarianresponse.info/"
  #form_id = "516148"
  form_id = form
  url = paste0(kobo_server_url,"api/v1/data/",form_id,".csv")
  
  try({
    
    #rawdata = GET(url,authenticate(Sys.getenv("sakirp_user"),Sys.getenv("sakirp_pw")))
    content = content(rawdata,"raw",encoding="UTF-8")
    default = read_csv(content)
    save_localRDS(default)
    
  },silent = T)
  default
}

# Tidy market data

tidy_market_data = function(market_data){
  market = market_data %>% 
    select(
      `Market Name` = ends_with("market_name"),
      `Revenue Collector` = ends_with("revenue_collector"),
      `Collector Phone` = ends_with("phone_rev_collector"),
      `Beans Cess` = ends_with("crop_cess_beans"),
      `Cassava Cess` = ends_with("crop_cess_cassava"),
      `Sunflower Cess` = ends_with("crop_cess_sunflower"),
      `Red Beans Variety` = ends_with("beans_variety_red"),
      `Red Bean Market Price` = ends_with("bean_red_price_market"),
      `Red Bean Farmgate` =  ends_with("bean_red_price_farm"),
      `Yellow Beans Variety` = ends_with("beans_variety_improvedyellow"),
      `Yellow Bean Market Price` = ends_with("bean_improvedyellow_price_market"),
      `Yellow Bean Farmgate` = ends_with("bean_improvedyellow_price_farm"),
      `Local Yellow Variety` = ends_with("beans_variety_yellow"),
      `Local Yellow Market Price` = ends_with("bean_yellow_price_market"),
      `Local Yellow Farmgate` = ends_with("bean_yellow_price_farm"),
      `Other Bean Variety` = ends_with("beans_variety_other"),
      `Other Bean Variety Market Price` = ends_with("bean_other_price_market"),
      `Other Bean Variety Farmgate` = ends_with("bean_other_price_farm"),
      `Cassava Types Sold` = ends_with("cassava_types_sold"),
      `Cassava Variety (Dried)` = ends_with("cassava_dried_variety"),
      `Cassava Market Price (Dried)` = ends_with("cassava_dried_price_market"),
      `Cassava Farmgate (Dried)` = ends_with("cassava_dried_price_farm"),
      `Cassava Variety (Fresh)` = ends_with("cassava_variety_fresh"),
      `Cassava Market Price (Fresh)` = ends_with("cassava_fresh_price_farm"),
      `Cassava Variety (Flour)` = ends_with("cassava_variety_flour"),
      `Cassava Market Price (Flour)` = ends_with("cassava_flour_price_market"),
      `Cassava Farmgate (Flour)` = ends_with("cassava_flour_price_farm"),
      endtime,
      `survey_info/district`,
      `survey_info/ward`,
      longitude = `survey_info/_gps_start_longitude`,
      latitude =`survey_info/_gps_start_latitude`
    ) %>% 
    mutate(
      endtime = lubridate::ymd(as.Date(endtime)),
      year = lubridate::year(as.Date(endtime)),
      month = lubridate::month(as.Date(endtime)),
      day = lubridate::day(as.Date(endtime))
    ) %>% 
    left_join(district, by = c("survey_info/district" = "id")) %>% 
    left_join(ward, by = c("survey_info/ward" = "id"))
  
  
  market = as.data.frame(apply(market,2, function(x){
    if_else(x == "n/a",NA_character_,x)})) 
  
  market
}

get_market_long = function(data){
  market_dat_long = data %>% 
    pivot_longer(cols = c("Beans Cess":"Cassava Farmgate (Flour)"),names_to = "item",
                 values_to = "value") %>% 
    mutate(
      year = lubridate::year(endtime),
      month = lubridate::month(endtime),
      week = lubridate::week(endtime),
      value = as.double(value)
    ) %>% filter(!is.na(value),
                 !is.na(endtime))
  
  market_dat_long
}

get_district_data = function(data){
  
  district_dat = data %>% 
    group_by(district,item,year,month) %>% 
    summarise(median_val = median(value))
}

get_region_data = function(data){
  region_dat = data %>% 
    group_by(item,year,month) %>% 
    summarise(median_val = median(value))
}

get_ward_data = function(data){
  
  ward_dat = data %>% 
    group_by(district,ward,item,year,month) %>% 
    summarise(median_val = median(value))
  
  ward_dat
}


# getWardPlot = function(data, kata, product){
#   use_data = data %>% 
#     filter(ward == kata)
#   
#   p = ggplot(use_data %>% filter(!(is.na(product))), aes(x = endtime, y = `Yellow Bean Market`)) +
#     geom_smooth() +
#     scale_x_date(date_labels = "%Y %b %d")
#   
#   p
# }
# 
# ward_facets = function(data, dstrct, product){
#   use_data = data %>% 
#     filter(!(is.na(value)),
#            district == dstrct,
#            item == product)
#   
#   p = ggplot(use_data, aes(x = endtime, y = value)) +
#     geom_smooth() +
#     scale_x_date(date_labels = "%Y %b %d") + 
#     theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#     facet_wrap(~ward) 
#   
#   p
# }
# 
# ward_highchart = function(kata,data,product){
#   dat = data %>% 
#     filter(ward == kata,
#            item == product) 
#   
#   hchart(dat,hcaes(x = month,y = median_val,group=year),type = 'line')
# }
# 
# district_highchart = function(dstrct,product,data){
#   
#   dat = data %>% 
#     filter(district==dstrct,
#            item==product)
#   
#   hchart(dat,type='line',hcaes(x = month,y = median_val,group=year)) +
#     coord_cartesian(ylim = c(700,2800))
# }
# 
# region_highchart = function(product,data){
#   dat = data %>% 
#     filter(item == product)
#   
#   hchart(dat,"line",hcaes(x = month,y = median_val,group=year)) %>% 
#     hc_yAxis(min = 700,max = 2800)
# }
# 
# region_highchart_bxplot = function(product){
#   
#   dat = market_dat_long %>% 
#     filter(item==product) %>% 
#     ungroup() 
#   
#   
#   boxplot_dat = data_to_boxplot(data=dat,variable=value,group_var = month,
#                                 group_var2 = year,add_outliers = T)
#   
#   highchart() %>%
#     hc_xAxis(type = "category") %>%
#     hc_add_series_list(boxplot_dat)
# }
# 
# region_ggplot_boxplot = function(product){
#   dat = market_dat_long %>% 
#     filter(item==product)
#   
#   p = ggplot(dat, aes(x=as.factor(month),y = value, fill=as.factor(year))) + 
#     geom_boxplot() + 
#     coord_cartesian(ylim = quantile(market_dat_long$value, c(0.1, 0.9)))
#   
#   p
# }
# 
# region_line_plot = function(data_long, product){
#   ggplot() + 
#     geom_smooth(data = data_long %>% 
#                   filter(item==product,year==2020),
#                 mapping = aes(x=month,y=value,color=as.factor(year))) + 
#     geom_smooth(data= data_long %>% 
#                   filter(item==product,year==2021), 
#                 mapping = aes(x=month,y=value,color=as.factor(year))) +
#     scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12)) +
#     xlab(NULL) +
#     ylab(NULL) +
#     labs(color = "Year") +
#     theme(legend.position=c(0.1,0.85), legend.box = "horizontal") +
#     coord_cartesian(ylim = c(700,2800))
#   
# }
# 
# get_district_dygraph = function(data = market_dat_long,dist,product){
#   .GlobalEnv$district_dat = data %>% 
#     filter(district == dist,item == product) %>% 
#     group_by(district,year,month) %>% 
#     summarise(median_price = median(value)) %>% 
#     mutate(date = as.Date(paste0(year,"-",month,"-","15")))
#   
#   dygraph(xts::xts(.GlobalEnv$district_dat$median_price,.GlobalEnv$district_dat$date),
#           main = paste0(dist," District - ",product)) %>% 
#     dySeries("V1",label = "District Price", strokeWidth = 4, strokePattern = "dashed")
#   
# }
# 
# get_ward_dygraph = function(data = market_dat_long,dist,wrd,product){
#   
#   browser()
#   dat = data %>% 
#     filter(district==dist,ward==wrd,item==product) %>% 
#     group_by(ward,year,month) %>% 
#     summarise(median_price = median(value)) %>% 
#     mutate(date = as.Date(paste0(year,"-",month,"-","15")))
#   
#   district = xts::xts(.GlobalEnv$district_dat$median_price,.GlobalEnv$district_dat$date)
#   ward = xts::xts(dat$median_price,dat$date)
#   
#   series = cbind(district,ward)
#   
#   dygraph(series,main = paste0(wrd," - ",product)) %>% 
#     dySeries("district",label = "District Price", strokeWidth = 4, strokePattern = "dashed") %>% 
#     dySeries("ward",label = "Ward Price",strokeWidth = 3)
# }

get_region_dygraph = function(data = market_dat_long,product,dstrct,wrd){
  region_dat = data %>% 
    filter(item==product) %>% 
    group_by(year,month) %>% 
    summarise(median_price = median(value)) %>% 
    mutate(date = as.Date(paste0(year,"-",month,"-","15")))
  
  district_dat = data %>% 
    filter(district == dstrct,item == product) %>% 
    group_by(district,year,month) %>% 
    summarise(median_price = median(value)) %>% 
    mutate(date = as.Date(paste0(year,"-",month,"-","15")))
  
  ward_dat = data %>% 
    filter(district==dstrct,ward==wrd,item==product) %>% 
    group_by(ward,year,month) %>% 
    summarise(median_price = median(value)) %>% 
    mutate(date = as.Date(paste0(year,"-",month,"-","15")))
  
  region = xts::xts(region_dat$median_price,region_dat$date)
  district = xts::xts(district_dat$median_price,district_dat$date)
  ward = xts::xts(ward_dat$median_price,ward_dat$date)
  
  series = cbind(region,district,ward)
  
  dygraph(series,main = 
            paste0(wrd," - ",product)) %>% 
    dySeries("region",label = "Region Price",strokeWidth = 3) %>% 
    dySeries("district",label = "District Price",strokeWidth = 3,
             strokePattern = "dashed") %>% 
    dySeries("ward", label = "Ward Price",strokeWidth = 3, 
             strokePattern = "dotted")
}

load_data = function(){
  market <<- tidy_market_data(read_localRDS())#tidy_market_data(get_data("516148"))
  market_dat_long <<- get_market_long(market)
  district_dat <<- get_district_data(market_dat_long)
  ward_dat <<- get_ward_data(market_dat_long)
}

