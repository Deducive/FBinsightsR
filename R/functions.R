# Require packages

require(httr)
require(lubridate)
require(tidyverse)

# Set functions

# Insights by age & gender function

FBins_age_gen <- function(sdate, udate, report_level){
  #set variables
  sstring <- paste0('"','since','"')
  ustring <- paste0('"','until','"')
  #paste together JSON string
  range_content <- paste0(sstring,':','"',sdate,'"',',',ustring,':','"',udate,'"')
  time_range <- paste0("{",range_content,"}")
  #call insights
  report <- GET('https://graph.facebook.com/v2.10/act_224743358/insights',
                query = list(
                  access_token = access_token,
                  time_range = time_range,
                  level= report_level,
                  fields = "campaign_id, adset_id, adset_name, ad_id, ad_name, impressions, cpm, reach, clicks, unique_clicks, ctr, cpc, unique_ctr, cost_per_unique_click, estimated_ad_recall_rate, cost_per_estimated_ad_recallers, spend, canvas_avg_view_time, canvas_avg_view_percent",
                  time_increment="1",
                  limit = "10000",
                  breakdowns = "age, gender"
                ),
                encode = "json",
                verbose())
}

# Summary insights function

FB_ins_summ <- function(sdate, udate, report_level, time_increment){
  #set strings
  sstring <- paste0('"','since','"')
  ustring <- paste0('"','until','"')
  #paste together JSON string
  range_content <- paste0(sstring,':','"',sdate,'"',',',ustring,':','"',udate,'"')
  time_range <- paste0("{",range_content,"}")
  #call insights
  report <- GET('https://graph.facebook.com/v2.10/act_224743358/insights',
                query = list(
                  access_token = access_token,
                  time_range = time_range,
                  level= report_level,
                  fields = "campaign_id, adset_id, adset_name, ad_id, ad_name, impressions, cpm, reach, frequency, clicks, unique_clicks, ctr, cpc, unique_ctr, cost_per_unique_click, estimated_ad_recall_rate, cost_per_estimated_ad_recallers, spend, canvas_avg_view_time, canvas_avg_view_percent",
                  time_increment=time_increment,
                  limit = "10000"
                ),
                encode = "json",
                verbose())
}

# Extract into a data frame

FB_extract <- function(report){
  #extract data and name
  content_result <- content(report)
  content_result[["paging"]] <- NULL
  data.frame(content_result$data %>% reduce(bind_rows))
}
