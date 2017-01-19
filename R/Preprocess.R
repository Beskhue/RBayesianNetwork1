library(arules)

remove_data_errors = function(d) {
  d <- d[d$n_non_stop_unique_tokens <= 1, ]
  
  return(d)
}

remove_empty_articles = function(d) {
  d <- d[d$n_tokens_content >= 1, ]
  
  return(d)
}

# Preprocess the model data
preprocess = function(d) {
  var_names = get_var_names();
  
  # invert stop word rate
  d[['n_non_stop_words']] <- 1 - d$n_non_stop_words
  
  # apply log transformations
  for(v in get_names_to_log()) {
    log_var_name = paste('log_', v, sep='');
    mindv = min(d[[v]]);
    if(mindv <= 0) {
      d[[log_var_name]] = log(d[[v]] + (1-mindv)); # change min. to 1 to avoid log issues
    }
    else {
      d[[log_var_name]] = log(d[[v]]);
    }
  }
  
  # calculate length ratios
  for(v in get_names_to_turn_into_ratio()) {
    ratio_var_name <- paste('ratio_', v, sep='');
    ratios <- d[[v]] / d[['n_tokens_content']];
    ratios[!is.finite(ratios)] <- 0;
    d[[ratio_var_name]] <- ratios;
  }
  
  # calculate log length ratios
  for(v in get_names_to_turn_into_ratio()) {
    if(!v %in% get_names_to_log()) {
      next
    }
    
    logratio_var_name <- paste('logratio_', v, sep='');
    log_var_name <- paste('log_', v, sep='');
    
    d[[logratio_var_name]] <- d[[log_var_name]] - d[['log_n_tokens_content']];
  }
  
  # discretize
  for(v in get_names_to_discretize()) {
    discretized_var_name <- paste('discrete_', v, sep='');
    d[[discretized_var_name]] <- discretize(d[[v]], categories = 4, method = 'frequency', ordered = TRUE)
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

filter_for_structural_inference = function(d) {
  featureList <- get_names_for_structural_inference();
  d$fit <- d$fit[,featureList];
  d$test <- d$test[,featureList];
  return(d);
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

get_names_to_turn_into_ratio = function() {
  return(c('n_unique_tokens', 'n_non_stop_words', 'n_non_stop_unique_tokens',
           'num_hrefs', 'num_self_hrefs',
           'num_imgs', 'num_videos'));
}

get_names_to_discretize = function() { 
  return(c('n_tokens_title', 'n_tokens_content',
           'n_unique_tokens', 'n_non_stop_unique_tokens',
           'num_hrefs', 'num_self_hrefs', 'num_imgs', 'num_videos', 
           'average_token_length', 
           'kw_min_min', 'kw_max_min', 'kw_avg_min', 'kw_min_max', 
           'kw_avg_max', 'kw_min_avg', 'kw_max_avg', 'kw_avg_avg',
           'self_reference_min_shares', 'self_reference_max_shares',
           'self_reference_avg_sharess', 
           'LDA_00', 'LDA_01', 'LDA_02', 'LDA_03', 'LDA_04', 'global_subjectivity', 
           'global_sentiment_polarity', 'global_rate_positive_words', 
           'global_rate_negative_words', 'rate_positive_words', 
           'rate_negative_words', 'avg_positive_polarity', 'min_positive_polarity',
           'avg_negative_polarity', 'min_negative_polarity',
           'max_negative_polarity', 'title_sentiment_polarity',
           'abs_title_sentiment_polarity', 'shares'));
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

get_names_for_structural_inference = function() {
  return(c('log_shares', 'log_n_tokens_title', 'log_n_tokens_content', 'log_self_reference_avg_sharess',
           'log_num_imgs', 'log_num_videos', 'log_num_hrefs', 'log_num_self_hrefs', 'log_average_token_length',
           'log_n_unique_tokens', 'log_n_non_stop_words', 'log_n_non_stop_unique_tokens',
           'log_kw_avg_avg', 'log_kw_max_avg', 'log_kw_avg_max', 'log_kw_avg_min',
           'title_subjectivity', 'global_subjectivity', 'abs_title_subjectivity', 'title_sentiment_polarity',
           'global_sentiment_polarity', 'abs_title_sentiment_polarity', 'global_rate_positive_words',
           'rate_positive_words', 'avg_positive_polarity', 'global_rate_negative_words', 'rate_negative_words',
           'avg_negative_polarity', 'LDA_00', 'LDA_01', 'LDA_02', 'LDA_03', 'LDA_04'));
           # N.B. data channels removed
  
  #return(c('log_n_tokens_title', 'log_n_tokens_content', 'log_self_reference_avg_sharess',
           #'log_num_imgs', 'log_num_videos', 'log_num_hrefs', 'log_num_self_hrefs',
           #'data_channel_is_lifestyle', 'data_channel_is_entertainment', 'data_channel_is_bus',
           #'data_channel_is_socmed', 'data_channel_is_tech', 'data_channel_is_world'));
}

get_display_names_for_structural_inference = function() {
  lst <- get_names_for_structural_inference();
  lst <- gsub('_', ' ', lst);
  lst <- gsub('log ', '', lst);
  
  lst <- gsub('sharess', 'shares', lst);
  lst <- gsub('data channel is ', 'chan. ', lst);
  lst <- gsub('reference', 'ref.', lst);
  lst <- gsub('tokens', 'tok.', lst);
  lst <- gsub('token', 'tok.', lst);
  lst <- gsub('positive', 'pos.', lst);
  lst <- gsub('negative', 'neg.', lst);
  lst <- gsub('subjectivity', 'subj.', lst);
  lst <- gsub('polarity', 'pol.', lst);
  lst <- gsub('sentiment ', '', lst);
  lst <- gsub('global', 'gl.', lst);
  lst <- gsub('rate ', '', lst);
  lst <- gsub('average', 'avg.', lst);
  lst <- gsub(' words', '', lst);
  lst <- gsub('unique', 'un.', lst);
  return(lst);
}