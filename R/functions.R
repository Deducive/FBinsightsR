# Set functions

###### Function 1 - fbins_ag

#' Get FB insights by age & gender.
#' @description This returns all available FB insights per day including age and gender breakdown to the specified report level, and place into a data frame. When reporting by age and gender the FB API can timeout when reporting on only a few days. This function allows you to pull one day at a time if needed.
#' @param start_date The first full day to report, in the format "YYYY-MM-DD" .
#' @param until_date The last full day to report, in the format "YYYY-MM-DD" .
#' @param report_level One of "ad", "adset", "campaign" or "account" .
#' @param fb_access_token This must be a valid access token with sufficient privileges. Visit the Facebook API Graph Explorer to acquire one.
#' @param account This is the ad account, campaign, adset or ad ID to be queried.
#' @keywords facebook insights api
#' @export
#' @examples
#' fbins_ag("start_date", "until_date", "report_level", "fb_access_token", "account")
#' fbins_ag("2017-01-20", "2017-01-22", "ad", "ABCDEFG1234567890ABCDEFG", "act_12345678")

fbins_ag <- function(start_date, until_date, report_level, fb_access_token, account){
  #set variables
  sstring <- paste0('"','since','"')
  ustring <- paste0('"','until','"')
  #paste together JSON string
  range_content <- paste0(sstring,':','"',start_date,'"',',',ustring,':','"',until_date,'"')
  time_range <- paste0("{",range_content,"}")
  #paste together URL
  api_version <- "v3.3"
  url_stem <- "https://graph.facebook.com/"
  URL <- paste0(url_stem, api_version, "/", account, "/insights")
  
  #call insights
  content_result <- content(GET(
                  URL,
                  query = list(
                    access_token = fb_access_token,
                    time_range = time_range,
                    level= report_level,
                    fields = "campaign_name, campaign_id, objective, adset_id, adset_name, ad_id, ad_name, impressions, cpm, reach, clicks, unique_clicks, ctr, cpc, unique_ctr, cost_per_unique_click, estimated_ad_recall_rate, cost_per_estimated_ad_recallers, spend, canvas_avg_view_time, canvas_avg_view_percent",
                    time_increment="1",
                    limit = "1000",
                    breakdowns = "age, gender"
                    ),
                  encode = "json",
                  verbose()))
  
  # Show and return error if exists
  if ("error" %in% names(content_result)){
    message(paste(
      paste0("ERROR ", content_result$error$code, " (",
             content_result$error$type, "):"),
      content_result$error$message
    ))
    # Very useful for Shiny apps
    invisible(return(content_result$error))
  }
  
  #extract data and name
  result_df <- data.frame(content_result$data %>% reduce(bind_rows))
  
  # Condition to detect the next page
  if(exists("next", content_result$paging) == TRUE){
    # Checking from the originally returned list
    result <- fromJSON(content_result$paging$`next`)
    result_df <- bind_rows(result_df, as.tibble(result$data))
    
    # Looping through subsequent returned pages
    while(exists("next", result$paging) == TRUE){
      result <- fromJSON(result$paging$`next`)
      result_df <- bind_rows(result_df, as.tibble(result$data))
    }
  }
  result_df <- result_df
}

###### Function 2 - fbins_summ

#' FB summary insights function.
#' @description This returns, in a data frame, a summary of FB insights with no breakdown
#' @param start_date The first full day to report, in the format "YYYY-MM-DD" .
#' @param until_date The last full day to report, in the format "YYYY-MM-DD" .
#' @param report_level One of "ad", "adset", "campaign" or "account" .
#' @param time_increment An integer representing the reporting increment. If blank, defaults to the entire reporting period.
#' @param fb_access_token This must be a valid access token with sufficient privileges. Visit the Facebook API Graph Explorer to acquire one.
#' @param account This is the ad account, campaign, adset or ad ID to be queried.
#' @keywords facebook insights api
#' @export
#' @examples
#' fbins_summ("start_date", "until_date", "report_level", "time_increment", "fb_access_token", "account")
#' fbins_summ("2017-01-20", "2017-01-22", "ad", "1", "ABCDEFG1234567890ABCDEFG", "act_12345678")
#' fbins_summ("2017-01-20", "2017-01-22", "ad", "", "ABCDEFG1234567890ABCDEFG", "act_12345678")

fbins_summ <- function(start_date, until_date, report_level, time_increment, fb_access_token, account){
  #set strings
  sstring <- paste0('"','since','"')
  ustring <- paste0('"','until','"')
  #paste together JSON string
  range_content <- paste0(sstring,':','"',start_date,'"',',',ustring,':','"',until_date,'"')
  time_range <- paste0("{",range_content,"}")
  #paste together URL
  api_version <- "v3.3"
  url_stem <- "https://graph.facebook.com/"
  URL <- paste0(url_stem, api_version, "/", account, "/insights")
  
  #call insights
  content_result <- content(GET(URL,
                query = list(
                  access_token = fb_access_token,
                  time_range = time_range,
                  level = report_level,
                  fields = "campaign_name, campaign_id, objective, adset_id, adset_name, ad_id, ad_name, impressions, cpm, reach, frequency, clicks, unique_clicks, ctr, cpc, unique_ctr, cost_per_unique_click, estimated_ad_recall_rate, cost_per_estimated_ad_recallers, spend, canvas_avg_view_time, canvas_avg_view_percent",
                  time_increment=time_increment,
                  limit = "1000"
                ),
                encode = "json",
                verbose()))
  #extract data and name
  result_df <- data.frame(content_result$data %>% reduce(bind_rows))
  
  # Condition to detect the next page
  if(exists("next", content_result$paging) == TRUE){
    # Checking from the originally returned list
    result <- fromJSON(content_result$paging$`next`)
    result_df <- bind_rows(result_df, as.tibble(result$data))
    
    # Looping through subsequent returned pages
    while(exists("next", result$paging) == TRUE){
      result <- fromJSON(result$paging$`next`)
      result_df <- bind_rows(result_df, as.tibble(result$data))
    }
  }
  result_df <- result_df
}

###### Function 3 - fbins_page

#' FB page basic insights.
#' @description This returns, in a data frame, a summary of FB page insights within the specified time period
#' @param start_date The first full day to report, in the format "YYYY-MM-DD" .
#' @param until_date The last full day to report, in the format "YYYY-MM-DD" .
#' @param period One of "day", "week", "days_28", "month", "lifetime", "total_over_range" .
#' @param page_access_token This must be a valid page access token with sufficient privileges. Visit the Facebook API Graph Explorer to acquire one.
#' @param page_account This is the fb page to be queried.
#' @keywords facebook insights api
#' @export
#' @examples
#' fbins_page("start_date", "until_date", "time_period", "page_access_token", "page_account")
#' fbins_page("2017-01-20", "2017-01-22", "day", "ABCDEFG1234567890ABCDEFG", "1234567890123")

fbins_page <- function(start_date, until_date, time_period, page_access_token, page_account){
  #paste together URL
  api_version <- "v3.3"
  url_stem <- "https://graph.facebook.com/"
  URL <- paste0(url_stem, api_version, "/", page_account, "/insights")
  
  #call insights
  content_result <- content(GET
                            (URL,
                              query = list(
                                metric = "page_impressions,page_impressions_organic,page_impressions_paid,page_post_engagements,page_fans",
                                period = time_period,
                                access_token = page_access_token,
                                since = start_date,
                                until = until_date),
                              encode = "json",
                              verbose()))
  
  ## Create df with extracted date range
  result_df <- data.frame(content_result$data[[1]][["values"]] %>%
                            reduce(bind_rows)) %>%
    select("end_time")
  
  ## Loop through, extract and bind to date only frame
  for(i in seq_along(content_result$data)){
    result_temp <- data.frame(content_result$data[[i]][["values"]] %>%
                                reduce(bind_rows)) %>%
      rename(!!content_result[["data"]][[i]][["name"]] := value)
    result_df <- merge(result_df, result_temp, by = "end_time")
  }
  rm(result_temp)
  
  ## Trim date character, new column in date class, delete old col and reorder
  result_df <- result_df %>%
    mutate(date=ymd(substr(end_time, start = 1, stop = 10))) %>%
    select(-end_time) %>% 
    select(ncol(result_df), 1:ncol(result_df))
}

###### Function 4 - fbins_insta

#' Instagram basic insights.
#' @description This returns, in a data frame, a summary of Instagram insights within the specified time period. 
#' Only 30 days' data can be retrieved through the API and this is the full scope of variables.
#' @param start_date The first full day to report, in the format "YYYY-MM-DD" .
#' @param until_date The last full day to report, in the format "YYYY-MM-DD" .
#' @param period One of "day", "week", "days_28", "month", "lifetime", "total_over_range" .
#' @param page_access_token This must be a valid page access token with sufficient privileges. Visit the Facebook API Graph Explorer to acquire one.
#' @param insta_account This is the Instagram business account to be queried.
#' @keywords facebook insights api instagram
#' @export
#' @examples
#' fbins_insta("start_date", "until_date", "period", "page_access_token", "insta_account")
#' fbins_insta("2017-01-20", "2017-01-22", "day", "ABCDEFG1234567890ABCDEFG", "1234567890123")

fbins_insta <- function(start_date, until_date, time_period, page_access_token, insta_account){
  #paste together URL
  api_version <- "v3.3"
  url_stem <- "https://graph.facebook.com/"
  URL <- paste0(url_stem, api_version, "/", insta_account, "/insights")
  
  #call insights
  content_result <- content(GET
                            (URL,
                              query = list(
                                metric = "reach, impressions, follower_count, profile_views, website_clicks",
                                period = time_period,
                                access_token = page_access_token,
                                since = start_date,
                                until = until_date),
                              encode = "json",
                              verbose()))
  
  ## Create df with extracted date range
  result_df <- data.frame(content_result$data[[1]][["values"]] %>%
                            reduce(bind_rows)) %>%
    select("end_time")
  
  ## Loop through, extract and bind to date only frame
  for(i in seq_along(content_result$data)){
    result_temp <- data.frame(content_result$data[[i]][["values"]] %>%
                                reduce(bind_rows)) %>%
      rename(!!content_result[["data"]][[i]][["name"]] := value)
    result_df <- merge(result_df, result_temp, by = "end_time")
  }
  rm(result_temp)
  
  ## Trim date character, new column in date class, delete old col and reorder
  result_df <- result_df %>%
    mutate(date=ymd(substr(end_time, start = 1, stop = 10))) %>%
    select(-end_time) %>% 
    select(ncol(result_df), 1:ncol(result_df))
}

###### Function 5 - fbins_pxa

#' Facebook Ads Pixel Insights.
#' This returns, in a data frame, pixel actions such as conversions or adds-to-cart together with a complement of ads stats such as impressions and clicks.
#' This is applies pagination through the API - so it may take a time to return the results.
#' @param start_date The first full day to report, in the format "YYYY-MM-DD" .
#' @param until_date The last full day to report, in the format "YYYY-MM-DD" .
#' @param report_level One of "ad", "adset", "campaign" or "account" .
#' @param fb_access_token This must be a valid access token with sufficient privileges. Visit the Facebook API Graph Explorer to acquire one.
#' @param account This is the ad account, campaign, adset or ad ID to be queried.
#' @keywords facebook insights api
#' @export
#' @examples
#' fbins_pxa("start_date", "until_date", "report_level", "time_increment", "fb_access_token", "account")
#' fbins_pxa("2017-01-20", "2017-01-22", "ad", "ABCDEFG1234567890ABCDEFG", "act_12345678")
#' fbins_pxa("2017-01-20", "2017-01-22", "ad", "ABCDEFG1234567890ABCDEFG", "act_12345678")

fbins_pxa <- function(start_date, until_date, report_level, fb_access_token, account){
  fbins_rev <- function(start_date, until_date, report_level, fb_access_token, account){
    #set variables
    sstring <- paste0('"','since','"')
    ustring <- paste0('"','until','"')
    #paste together JSON string
    range_content <- paste0(sstring,':','"',start_date,'"',',',ustring,':','"',until_date,'"')
    time_range <- paste0("{",range_content,"}")
    #paste together URL
    api_version <- "v3.3"
    url_stem <- "https://graph.facebook.com/"
    URL <- paste0(url_stem, api_version, "/", account, "/insights")
    
    #call insights
    content_result <- content(GET(
      URL,
      query = list(
        access_token = fb_access_token,
        time_range = time_range,
        level= report_level,
        fields = "campaign_id, objective, adset_id, adset_name, ad_id, ad_name, impressions, actions",
        time_increment="1",
        limit = "50",
        action_breakdowns = "action_type",
        breakdowns = "age, gender"
      ),
      encode = "json",
      verbose()))
    
  }
  get_pix_actions <- function(content_result){
    # Extract & create 
    nested_df <- map_dfr(content_result$data, ~modify_at(.,"actions", compose(list,bind_rows)))
    missing_actions <- lengths(nested_df$actions) == 0
    nested_df$actions[missing_actions] <- replicate(sum(missing_actions),
                                                    tibble(action_device = NA, action_type = NA, value = NA), F)
    
    # Filter for only pixel actions
    unnest(nested_df) %>%
      filter(action_type == "offsite_conversion.fb_pixel_add_to_cart" |
               action_type == "offsite_conversion.fb_pixel_custom" |
               action_type == "offsite_conversion.fb_pixel_purchase")
  }
  
  # Extract
  content_result <- fbins_rev(start_date, until_date, report_level, fb_access_token, account)
  result_df <- get_pix_actions(content_result)
  
  # Condition to detect the next page
  if(exists("next", content_result$paging) == TRUE){
    # GET from the nextURL if it exists
    content_result2 <- content(GET(content_result$paging$`next`))
    result2_df <- get_pix_actions(content_result2)
    result_df <- bind_rows(result_df, result2_df)
    
    # Looping through subsequent returned pages
    while(exists("next", content_result2$paging) == TRUE){
      content_result2 <- content(GET(content_result2$paging$`next`))
      result2_df <- get_pix_actions(content_result2)
      result_df <- bind_rows(result_df, result2_df)
    }
  }
  result_df <- result_df
}

