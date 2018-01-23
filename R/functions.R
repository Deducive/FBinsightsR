# Set functions

#' Get FB insights by age & gender.
#' @description This returns all available FB insights per day including age and gender breakdown to the specified report level, and place into a data frame. When reporting by age and gender the FB API can timeout when reporting on only a few days. This function allows you to pull one day at a time if needed.
#' @param start_date The first full day to report, in the format "YYYY-MM-DD" .
#' @param until_date The last full day to report, in the format "YYYY-MM-DD" .
#' @param report_level One of "ad", "adset", "campaign" or "account" .
#' @param fb_access_token This must be a valid access token with sufficient privileges. Visit the Facebook API Graph Explorer to acquire one.
#' @keywords facebook insights api
#' @export
#' @examples
#' fbins_ag("start_date", "until_date", "report_level", "fb_access_token")
#' fbins_ag("2017-01-20", "2017-01-22", "ad", "ABCDEFG1234567890ABCDEFG")

fbins_ag <- function(start_date, until_date, report_level, fb_access_token){
  #set variables
  sstring <- paste0('"','since','"')
  ustring <- paste0('"','until','"')
  #paste together JSON string
  range_content <- paste0(sstring,':','"',start_date,'"',',',ustring,':','"',until_date,'"')
  time_range <- paste0("{",range_content,"}")
  #call insights
  content_result <- content(GET('https://graph.facebook.com/v2.10/act_224743358/insights',
                query = list(
                  access_token = fb_access_token,
                  time_range = time_range,
                  level= report_level,
                  fields = "campaign_id, adset_id, adset_name, ad_id, ad_name, impressions, cpm, reach, clicks, unique_clicks, ctr, cpc, unique_ctr, cost_per_unique_click, estimated_ad_recall_rate, cost_per_estimated_ad_recallers, spend, canvas_avg_view_time, canvas_avg_view_percent",
                  time_increment="1",
                  limit = "10000",
                  breakdowns = "age, gender"
                ),
                encode = "json",
                verbose()))
  #extract data and name
  content_result[["paging"]] <- NULL
  data.frame(content_result$data %>% reduce(bind_rows))
}

#' FB summary insights function.
#' This returns, in a data frame, a summary of FB insights with no breakdown
#' @param start_date The first full day to report, in the format "YYYY-MM-DD" .
#' @param until_date The last full day to report, in the format "YYYY-MM-DD" .
#' @param report_level One of "ad", "adset", "campaign" or "account" .
#' @param time_increment An integer representing the reporting increment. If blank, defaults to the entire reporting period.
#' @param fb_access_token This must be a valid access token with sufficient privileges. Visit the Facebook API Graph Explorer to acquire one.
#' @keywords facebook insights api
#' @export
#' @examples
#' fbins_summ("start_date", "until_date", "report_level", "time_increment", "fb_access_token")
#' fbins_summ("2017-01-20", "2017-01-22", "ad", "1", "ABCDEFG1234567890ABCDEFG")
#' fbins_summ("2017-01-20", "2017-01-22", "ad", "", "ABCDEFG1234567890ABCDEFG")

fbins_summ <- function(start_date, until_date, report_level, time_increment, fb_access_token){
  #set strings
  sstring <- paste0('"','since','"')
  ustring <- paste0('"','until','"')
  #paste together JSON string
  range_content <- paste0(sstring,':','"',start_date,'"',',',ustring,':','"',until_date,'"')
  time_range <- paste0("{",range_content,"}")
  #call insights
  content_result <- content(GET('https://graph.facebook.com/v2.10/act_224743358/insights',
                query = list(
                  access_token = fb_access_token,
                  time_range = time_range,
                  level = report_level,
                  fields = "campaign_id, adset_id, adset_name, ad_id, ad_name, impressions, cpm, reach, frequency, clicks, unique_clicks, ctr, cpc, unique_ctr, cost_per_unique_click, estimated_ad_recall_rate, cost_per_estimated_ad_recallers, spend, canvas_avg_view_time, canvas_avg_view_percent",
                  time_increment=time_increment,
                  limit = "10000"
                ),
                encode = "json",
                verbose()))
  #extract data and name
  content_result[["paging"]] <- NULL
  data.frame(content_result$data %>% reduce(bind_rows))
}
