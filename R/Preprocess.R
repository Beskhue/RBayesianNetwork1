# Preprocess the model data
preprocess = function(d) {
  var_names = get_var_names();
  
  # apply log transformations
  for(v in get_names_to_log()) {
    log_var_name = paste('log_', v, sep='');
    mindv = min(d[[v]]);
    if(mindv < 1) {
      d[[log_var_name]] = log(d[[v]] + (1-mindv)); # change min. to 1 to avoid log issues
    }
    else {
      d[[log_var_name]] = log(d[[v]]);
    }
  }
  
  # convert to zero mean, unit variance
  for(v in get_names_to_standardise()) {
    d[[v]] = scale(d[[v]]);
    if(is.element(v, get_names_to_log())) {
      log_var_name = paste('log_', v, sep='');
      d[[log_var_name]] = scale(d[[log_var_name]]);
    }
  }
  
  # partition data frame into a fiting and testing set
  # 80% of the data set is used for fitting
  smp_size <- floor(0.80*nrow(d));
  
  # seed for reproducability
  set.seed(42);
  
  fit_indices <- sample(seq_len(nrow(d)), size = smp_size);
  
  fit <- data.frame(d[fit_indices, ]);
  test <- data.frame(d[-fit_indices, ]);
  
  # Set ordered data as being ordered
  #categorical_var_names <- get_categorical_var_names();
  #fit[,categorical_var_names] <- lapply(fit[,categorical_var_names], ordered);
  #test[,categorical_var_names] <- lapply(test[,categorical_var_names], ordered);
  
  
  partition <- list(fit, test);
  names(partition) <- c('fit', 'test')
  
  return(partition);
}

get_names_to_standardise = function() {
   v <- get_var_names();
   do_not_standardise <- c('url', 
                           'data_channel_is_lifestyle', 'data_channel_is_entertainment',
                           'data_channel_is_bus', 'data_channel_is_socmed',
                           'data_channel_is_tech', 'data_channel_is_world',
                           'weekday_is_monday', 'weekday_is_tuesday', 'weekday_is_wednesday',
                           'weekday_is_thursday', 'weekday_is_friday', 'weekday_is_saturday',
                           'weekday_is_sunday', 'is_weekend');
   # Perhaps add timedelta to do_not_standardise list?
   return(v[!(v %in% do_not_standardise)]);
}

get_names_to_log = function() {
  return(c('n_tokens_title', 'n_tokens_content', 'n_non_stop_unique_tokens',
           'n_non_stop_words', 'n_unique_tokens', 'num_hrefs', 'num_self_hrefs', 
           'num_imgs', 'num_videos', 'average_token_length', 'num_keywords', 'kw_min_min', 
           'kw_max_min', 'kw_avg_min', 'kw_min_max', 'kw_max_max', 'kw_avg_max', 
           'kw_min_avg', 'kw_max_avg', 'kw_avg_avg', 'self_reference_min_shares', 
           'self_reference_max_shares', 'self_reference_avg_sharess', 'shares'));
}

get_var_names = function() {
  return(c('url', 'timedelta', 'n_tokens_title', 'n_tokens_content',
           'n_unique_tokens', 'n_non_stop_words', 'n_non_stop_unique_tokens',
           'num_hrefs', 'num_self_hrefs', 'num_imgs', 'num_videos', 
           'average_token_length', 'num_keywords', 'data_channel_is_lifestyle', 
           'data_channel_is_entertainment', 'data_channel_is_bus', 
           'data_channel_is_socmed', 'data_channel_is_tech', 'data_channel_is_world',
           'kw_min_min', 'kw_max_min', 'kw_avg_min', 'kw_min_max', 'kw_max_max',
           'kw_avg_max', 'kw_min_avg', 'kw_max_avg', 'kw_avg_avg',
           'self_reference_min_shares', 'self_reference_max_shares',
           'self_reference_avg_sharess', 'weekday_is_monday', 'weekday_is_tuesday',
           'weekday_is_wednesday', 'weekday_is_thursday', 'weekday_is_friday',
           'weekday_is_saturday', 'weekday_is_sunday', 'is_weekend', 'LDA_00',
           'LDA_01', 'LDA_02', 'LDA_03', 'LDA_04', 'global_subjectivity', 
           'global_sentiment_polarity', 'global_rate_positive_words', 
           'global_rate_negative_words', 'rate_positive_words', 
           'rate_negative_words', 'avg_positive_polarity', 'min_positive_polarity',
           'max_positive_polarity', 'avg_negative_polarity', 'min_negative_polarity',
           'max_negative_polarity', 'title_subjectivity', 'title_sentiment_polarity',
           'abs_title_subjectivity', 'abs_title_sentiment_polarity', 'shares'));
}

get_categorical_var_names = function() {
  return(c('data_channel_is_lifestyle', 
           'data_channel_is_entertainment', 'data_channel_is_bus', 
           'data_channel_is_socmed', 'data_channel_is_tech', 'data_channel_is_world',
           'weekday_is_monday', 'weekday_is_tuesday',
           'weekday_is_wednesday', 'weekday_is_thursday', 'weekday_is_friday',
           'weekday_is_saturday', 'weekday_is_sunday', 'is_weekend'));
}