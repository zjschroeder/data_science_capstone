---
title: Scraping Twitter Data
author: Zach Schroeder
date: '2023-03-01'
slug: []
categories: []
tags: []
featured_image: 'images/twitter_mining.png'
description: ''
editor_options: 
  chunk_output_type: console
---



In the article below, I walk through our process for collecting data using Twitter's API. 

>NOTE: I collected data prior to Twitter changing API access. As such, much (if not all) of this information may not be useful to you if you wish to use Twitter data in your own research. At the time of writing this, Twitter has proposed that access to the API may cost $42,000 per month - way too much for a measly graduate student like me.'

I secured API access by applying for a researcher account. This entailed writing a short blurb about this project and agreeing to terms/conditions for data privacy (as well as promising I'm not a corporation). The researcher account then gave me a *bearer token* - a unique code that acted as our key to get into the API from R. 
In R, I scraped Tweets using the [academictwitteR](https://github.com/cjbarrie/academictwitteR) package. 
I first added our bearer token to the R environment using the function


```r
set_bearer()
```

I then wrote a wrapper function to integrate several functions from the academictwitteR package. 


```r
get_me_some_tweets <- function(QUERY_TEXT, DATA_LOCATION, get_new = TRUE, resume = FALSE, update = FALSE, end_time = Sys.time()){
  
  # Set Variables
  
  DATE <- end_time

  require(academictwitteR)
  
  if(get_new == TRUE){
    
    # Set Query
    
    QUERY <- build_query(
      query = QUERY_TEXT,
      is_retweet = FALSE,
      lang = "en"
    )
  
    # Collect Tweets
    
    OUTPUT <-
      get_all_tweets(
        query = QUERY,
        start_tweets = "2007-01-01T00:00:00Z", # This is the beginning of Twitter
        end_tweets = DATE,
        data_path = DATA_LOCATION,
        bind_tweets = FALSE,
        n = Inf,
        export_query = TRUE
      )
  }else if(resume == TRUE){
    
    # Resume Collection
    
    resume_collection(data_path = DATA_LOCATION,
                      n = Inf,
                      bind_tweets = FALSE)
  }else if(update == TRUE){
    
    # Update Collection
    update_collection(data_path = DATA_LOCATION, 
                      end_tweets = DATE)
  }
}
```

Below is a brief walk through of the arguments for this function in an example query pulling Tweets cancelling Kanye (Ye) West. The search terms are saved as QUERY_TEXT and a folder location for the .json files is saved as DATA_LOCATION.


```r
QUERY_TEXT = c("#FuckYe", "#kanyewestcancelled", "#cancelkanyewest",
            "kanyeisoverparty", "cancelkanye", "kanyecancelled")

DATA_LOCATION = here::here("kanye/")

get_me_some_tweets(QUERY_TEXT, DATA_LOCATION)
```

There are then three additional arguments that you can specify:


```r
get_new = TRUE
resume = FALSE 
update = FALSE
```

get_new starts a new search and pulls Tweets from the beginning of Twitter to the current day. Setting resume = TRUE and get_new = FALSE allows you to resume a prior search from the most recent Tweet. A query can take between 45 minutes and 16 hours to complete. If your connection is interrupted, you can then resume from where you left off. Finally, update = TRUE acts similar to resume = TRUE but instead starts scraping Tweets from the most recent in the current folder. This allows me to go back and get more recent Tweets after the first scrape.

After scraping Tweets you'll be left with a folder of loose .json files. We then bind these tweets into a data frame using the bind_tweets function


```r
kanye <- bind_tweets(data_path = "kanye/", output_format = "tidy")
```


