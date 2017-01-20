lavaan_to_plot = function() {
  complexity <- abbreviate(c('log_average_token_length', 'n_unique_tokens', 'n_non_stop_unique_tokens', 'log_n_non_stop_words'))
  popularity <- abbreviate(c('log_kw_avg_avg', 'log_kw_max_avg', 'log_kw_avg_max', 'log_kw_avg_min'))
  subjectivity <- abbreviate(c('title_subjectivity', 'global_subjectivity', 'abs_title_subjectivity'))
  polarity <- abbreviate(c('title_sentiment_polarity', 'global_sentiment_polarity', 'abs_title_sentiment_polarity'))
  positivity <- abbreviate(c('global_rate_positive_words', 'rate_positive_words', 'avg_positive_polarity'))
  negativity <- abbreviate(c('global_rate_negative_words', 'rate_negative_words', 'avg_negative_polarity'))
  
  latents <- list(complexity, popularity, subjectivity, polarity, positivity, negativity)
  
  other <- abbreviate(c('log_n_tokens_title', 'log_n_tokens_content', 'log_self_reference_avg_sharess', 'log_num_imgs', 'log_num_videos', 'log_num_hrefs', 'log_num_self_hrefs'))
  
  edges <- matrix(nrow = 0, ncol = 2)
  
  for (latent in latents) {
    for (i in 1:length(latent)) {
      var1 <- latent[i]
      if (i < length(latent)) {
        for (j in (i+1):length(latent)) {
          var2 <- latent[j]
          edges <- rbind(edges, c(var1, var2))
        }
      }
      
      edges <- rbind(edges, c(var1, 'shares'))
    }
  }
  
  for (var in other) {
    edges <- rbind(edges, c(var, 'shares'))
  }
  
  return(edges);
}